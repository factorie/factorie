package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer,HashMap}
import cc.factorie.util.Implicits._
import scalala.tensor.Vector

/** Samplers that key off of particular contexts.  Subclasses implement "process1(context:C) */
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
  	postProcessHook(context, d)
  	if (d.size > 0) changeCount += 1
  	d
  }
  /** The underlying protected method that actually does the work.  Needs to be defined in subclasses. */
	def process1(context:C) : DiffList
	protected final def processN(contexts:Iterable[C]): Unit = { contexts.foreach(process(_)); iterationCount += 1; if (!postIterationHook) return }
	def process(contexts:Iterable[C], numIterations:Int): Unit = for (i <- 0 to numIterations) processN(contexts)
	def process(count:Int): Unit = for (i <- 0 to count) process(null.asInstanceOf[C]) // TODO Why is this cast necessary?;
 
  // Hooks
  /** Called just before each step of sampling.  Return an alternative variable if you want that one sampled instead. */
  def preProcessHook(context:C): C = context 
  /** Call just after each step of sampling. */
  def postProcessHook(context:C, difflist:DiffList) : Unit = {}
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
  def proposals(context:C): Seq[Proposal]

  def process1(context:C): DiffList = {
    val props = proposals(context)
    proposalsHook(props)
    val proposal = if (props.size == 1) props.first else props.sampleExpProportionally(_.acceptanceScore)
    proposal.diff.redo
    proposalHook(proposal)
    proposal.diff
  }

  def proposalsHook(proposals:Seq[Proposal]): Unit = {}
  def proposalHook(proposal:Proposal): Unit = {}
}

/** Tries each one of the settings in the Iterator provided by the abstract method "settings(C), 
    scores each, builds a distribution from the scores, and samples from it. */
abstract class SamplerOverSettings[C](val model:Model, val objective:Model) extends ProposalSampler[C] {
  def this(m:Model) = this(m, null)
  // This method must be implemented in sub-classes
  def settings(context:C) : SettingIterator
  // Meta-parameters
  var temperature = 1.0
  // TODO Some faster alternative to "toList" below?
  def proposals(context:C): Seq[Proposal] =
    settings(context).map(d => {val (m,o) = d.scoreAndUndo(model,objective); new Proposal(d, m, o, m/temperature)}).toList
}




