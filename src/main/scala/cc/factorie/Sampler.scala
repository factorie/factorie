package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer,HashMap}
import cc.factorie.util.Implicits._
import scalala.tensor.Vector

trait Inferencer {
  def infer(vs:Seq[Variable]): Unit
}

trait Optimizer {
  def optimize: Unit
  def optimize(numIterations:Int): Unit
}

/** Samplers that key off of particular contexts.  Subclasses implement "process1(context:C) */
trait Sampler[C] {
  //if (contextClass == classOf[Nothing]) throw new Error("Constructor for class "+this.getClass.getName+" must be given type argument");
  /** The number of calls to process(numIterations:Int) or process(contexts:C,numIterations:Int). */
	var iterationCount = 0
	/** The number of calls to process(context:C) */
	var processCount = 0
	// TODO Fix this process0 so it will work with Int?;
	/** Do one step of sampling.  This is a method intended to be called by users.  It manages hooks and processCount. */
	final def process(context:C): DiffList = { val c = preProcessHook(context); val d = process1(c); processCount += 1; postProcessHook(context, d); d }
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
// TODO Remove this and put ProposalSampler instead
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
    val proposal = if (props.size == 1) props.first else props.sampleExpProportionally(_.modelScore)
    proposal.diff.redo
    proposalHook(proposal)
    proposal.diff
  }
	def proposalsHook(proposals:Seq[Proposal]): Unit = {}
  def proposalHook(proposal:Proposal): Unit = {}
}



// Currently unused below, but needs to be re-considered

/** A collection of variables to be sampled together. */
trait Block extends Product with Iterable[Block] {
  // TODO Default value of d should be "new DiffList"
  def sample : DiffList
  def samplePriority : Double = 1.0
  def template: Sampler[_]
  def numVariables: Int
  def variable(i:Int): Variable
  def variables: Iterable[Variable] = for (i <- 0 until numVariables force) yield variable(i)
  def elements = Iterator.single(this)
  def ::(that: Block) = List(that, this)
  def compare(that:Block) = {
    val s = this.samplePriority - that.samplePriority
    if (s > 0) 1 else if (s < 0) -1 else 0
  }
  def canEqual(other: Any) = other.isInstanceOf[Block];
  override def equals(other: Any): Boolean = {
  	//if (this eq other) return true
  	if (!canEqual(other)) return false
  	val fother = other.asInstanceOf[Block];
  	(this.numVariables == fother.numVariables &&
  		(0 until numVariables).forall(i => this.variable(i).hashCode == fother.variable(i).hashCode &&
  				this.variable(i) == fother.variable(i)))
  }
  var _hashCode = -1
  override def hashCode: Int = {if (_hashCode == -1) _hashCode = variables.sumInts(_ hashCode); _hashCode}
}


trait Blocks[C] {
  this : Sampler[C] =>
  def process1(context:C) : DiffList = {
    val blocks = block(context)
    blocks match {
      case Nil => new DiffList
      case b:Block => b.sample
      case _ => { val d = new DiffList; d ++= blocks.flatMap(_.sample); d } 
    }
  }
  def block(context:C) : Iterable[Block]
}

/** Sample variables in blocks of size 1.  Blocks are created from an arbitrary context of type C using the method block(). */
trait Blocks1[C,V1<:Variable] extends Blocks[C] {
  this : Sampler[C] =>
  //val vc1 = m1.erasure; // if (m1.erasure == classOf[Null]) class
  def _sample(b:Block) : DiffList = sample(b.v1)
  def sample(v1:V1) : DiffList
  case class Block(v1:V1) extends cc.factorie.Block with Iterable[Block] {
    def template = Blocks1.this
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => v1; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}

/** Sample variables in blocks of size 1.  The context for block creatation is simply the variable itself. */
trait Blocks1Variable[V1<:Variable] extends Blocks1[V1,V1] {
  this : Sampler[V1] =>
	def block(v1:V1) : Iterable[Block] = Block(v1)
}

/** Sample variables in blocks of size 2.  Blocks are created from an arbitrary context of type C using the method block(). */
trait Blocks2[C,V1<:Variable,V2<:Variable] extends Blocks[C] {
  this : Sampler[C] =>
  def _sample(b:Block) = sample(b.v1, b.v2)
  def sample(v1:V1, v2:V2) : DiffList
  case class Block(v1:V1,v2:V2) extends cc.factorie.Block with Iterable[Block] {
    def template = Blocks2.this
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => v1; case 1 => v2; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}

/** Sample variables in blocks of size 2.  The context for block creatation is the first variable in the block. */
trait Blocks2Variables[V1<:Variable,V2<:Variable] extends Blocks2[V1,V1,V2] {
  this : Sampler[V1] =>
}


