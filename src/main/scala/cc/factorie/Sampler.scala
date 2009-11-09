package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer,HashMap,PriorityQueue}
import scalala.tensor.Vector
import cc.factorie.util.Implicits._
import cc.factorie.util.Hooks1

/** Samplers that key off of particular contexts.  Subclasses implement "process1(context:C)" */
trait Sampler[C] {
  //if (contextClass == classOf[Nothing]) throw new Error("Constructor for class "+this.getClass.getName+" must be given type argument");
  /** The number of calls to process(numIterations:Int) or process(contexts:C,numIterations:Int). */
	var iterationCount = 0
	/** The number of calls to process(context:C) */
	var processCount = 0
	/** The number of calls to process that resulted in a change (a non-empty DiffList) */
	var changeCount = 0
	/** Do one step of sampling.  This is a method intended to be called by users.  It manages hooks and processCount. */
	final def process(context:C): DiffList = {
  	val c = preProcessHook(context)
  	val d = process1(c)
  	processCount += 1
  	postProcessHook(c, d)
  	diffHook(d)
  	if (d != null && d.size > 0) changeCount += 1
  	d
  }
  /** If true, calls to "next" will create a new DiffList to describe the changes they made, otherwise "next" will not track the changes, and will return null. */
  var makeNewDiffList = true
  /** Convenient method for setting makeNewDiffList to false, and returning this. */
  def noDiffList: this.type = { makeNewDiffList = false; this }
  /** In your implementation of "process1" use this method to optionally create a new DiffList, obeying "makeNewDiffList". */
  def newDiffList = if (makeNewDiffList) new DiffList else null
  /** The underlying protected method that actually does the work.  Use this.newDiffList to optionally create returned DiffList.
   		Needs to be defined in subclasses. */
	def process1(context:C): DiffList
	protected final def processN(contexts:Iterable[C]): Unit = { contexts.foreach(process(_)); iterationCount += 1; if (!postIterationHook) return }
	def process(contexts:Iterable[C], numIterations:Int): Unit = for (i <- 0 to numIterations) processN(contexts)
	def process(count:Int): Unit = for (i <- 0 to count) process(null.asInstanceOf[C]) // TODO Why is this cast necessary?;
  // Guard is removed by erasure: 
  //def process00(x:AnyRef): DiffList = if (x.isInstanceOf[C]) process(x.asInstanceOf[C]) else null // x match { case c:C => process(c); case _ => null }
 
  // Hooks
  /** Called just before each step of sampling.  Return an alternative variable if you want that one sampled instead. */
  def preProcessHook(context:C): C = context 
  /** Call just after each step of sampling. */
  def postProcessHook(context:C, difflist:DiffList): Unit = {}
  /** An alternative to postProcessHook that does not require the type C. */ // TODO Really have both?  Remove 'context:C' from postProcessHook?
  def diffHook(difflist:DiffList): Unit = {}
  /** Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
  def postIterationHook: Boolean = true
}


// So we can call super in traits that override these methods
// TODO Is there a better way to do this?
// TODO Remove this and put ProposalSampler instead?  But then problems with SampleRank trait re-overriding methods it shouldn't?  Look into this. 
trait ProposalSampler0 {
	def proposalsHook(proposals:Seq[Proposal]): Unit
  def proposalHook(proposal:Proposal): Unit
}

/** Samplers that generate a list of Proposal objects, and select one log-proportionally to their modelScore. */
trait ProposalSampler[C] extends Sampler[C] with ProposalSampler0 {
  var temperature = 1.0 // Not used here, but used in subclasses; here for uniformity
  def proposals(context:C): Seq[Proposal]
  def process1(context:C): DiffList = {
    val props = proposals(context)
    proposalsHook(props)
    val proposal = if (props.size == 1) props.first else props.sampleExpProportionally(_.modelScore)
    proposal.diff.redo
    proposalHook(proposal)
    proposal.diff
  }
  val proposalsHooks = new Hooks1[Seq[Proposal]] // Allows non-overriders to add hooks
	def proposalsHook(proposals:Seq[Proposal]): Unit = proposalsHooks(proposals)
	val proposalHooks = new Hooks1[Proposal]
  def proposalHook(proposal:Proposal): Unit = proposalHooks(proposal)
}

/** Tries each one of the settings in the Iterator provided by the abstract method "settings(C), 
    scores each, builds a distribution from the scores, and samples from it. */
abstract class SamplerOverSettings[C](val model:Model, val objective:Model) extends ProposalSampler[C] {
  def this(m:Model) = this(m, null)
  // This method must be implemented in sub-classes
  def settings(context:C) : SettingIterator
  // TODO Some faster alternative to "toList" below?
  def proposals(context:C): Seq[Proposal] = {
    // 'map's call to 'next' is actually what causes the change in state to happen
    val s = settings(context).map(d => {val (m,o) = d.scoreAndUndo(model,objective); new Proposal(d, m/temperature, o)}).toList
    //if (s.exists(p=>p.modelScore > 0.0)) { s.foreach(p => println(p.modelScore+" "+model)); println("SamplerOverSettings^") }
    s
  } 
}


/** Manage and use a queue to more often revisit low-scoring factors and re-sample their variables. */
trait FactorQueue[C] extends Sampler[C] {
  var useQueue = true
  var maxQueueSize = 1000
  lazy val queue = new PriorityQueue[Factor]
  /** The proportion of sampling process steps to take from the queue, versus from the standard source of contexts. */
  var queueProportion = 0.5
  
  /** Overide to provide the generic sampler that can potentially deal with arbitrary variables coming from Factors */
  def process0(x:AnyRef): DiffList
  def model: Model
  
  override def postProcessHook(context:C, diff:DiffList): Unit = {
    super.postProcessHook(context, diff)
    if (useQueue) {
      var queueDiff: DiffList = new DiffList
      if (queueProportion > 1.0 && !queue.isEmpty) {
        for (i <- 0 until (queueProportion.toInt)) if (!queue.isEmpty) {
        	val qd = sampleFromQueue
        	if (qd != null) queueDiff ++= qd
        }
      } else if (!queue.isEmpty && Global.random.nextDouble < queueProportion) {
        val qd = sampleFromQueue
        if (qd != null) queueDiff ++= qd
      }
      if (maxQueueSize > 0) {
        queue ++= model.factors(diff)
        if (queue.size > maxQueueSize) queue.reduceToSize(maxQueueSize)
      }
      diff appendAll queueDiff
    }
  }
  def sampleFromQueue : DiffList = {
    val factor = queue.dequeue // TODO consider proportionally sampling from the queue instead
    for (variable <- factor.variables.toSeq.shuffle; if (!variable.isConstant)) {
      val difflist = process0(variable) // TODO This is not type-safe!
      if (difflist != null && difflist.size > 0) return difflist
    }
    null
  }
}

// TODO But I haven't been able to make this existential typing work in practice yet.
/*
trait AlternativeFactorQueue extends Sampler[C forSome {type C <: Variable}] {
	override def diffHook(diff:DiffList): Unit = {
   println("foo") 
	}
}
*/
