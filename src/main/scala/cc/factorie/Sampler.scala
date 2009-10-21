package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer,HashMap}
import cc.factorie.util.Implicits._


/** A specialized sampler for particular variables.  
 * Subclasses implement "sample(V1) */
abstract class Sampler[C](implicit mc:Manifest[C]) {
  val contextClass = mc.erasure
  //if (contextClass == classOf[Nothing]) throw new Error("Constructor for class "+this.getClass.getName+" must be given type argument");
  // TODO Think carefully about "iterations", and its different meanings in different subclasses.  It is possible to have maintenance here in superclass?
	var iterations = 0
	// TODO Fix this process0 so it will work with Int !!	
	def process0[T<:AnyRef](context:T): DiffList = if (contextClass.isAssignableFrom(context.getClass)) process(context.asInstanceOf[C]) else null
	def process(context:C): DiffList
	protected def _process(contexts:Iterable[C]): Unit = { iterations += 1; contexts.foreach(process(_)); if (!postIterationHook(this)) return }
	def process(contexts:Iterable[C], numIterations:Int): Unit = for (i <- 0 to numIterations) _process(contexts)
	def process(numIterations:Int): Unit = for (i <- 0 to numIterations) process(null.asInstanceOf[C]) // TODO Why is this cast necessary?
  /**Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
  def postIterationHook(s:Sampler[C]): Boolean = true
  /**Called just before each variable is sampled.  Return an alternative variable if you want that one sampled instead. */
  def preProcessHook(context:C): C = context // TODO Not yet used
}


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


abstract class BlockSampler[C](implicit mc:Manifest[C]) extends Sampler[C]()(mc) {
  def process(context:C) : DiffList = {
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
abstract class BlockSampler1[C,V1<:Variable](cm:Manifest[C], m1:Manifest[V1]) extends BlockSampler[C]()(cm) {
  val vc1 = m1.erasure; // if (m1.erasure == classOf[Null]) class
  def _sample(b:Block) : DiffList = sample(b.v1)
  def sample(v1:V1) : DiffList
  case class Block(v1:V1) extends cc.factorie.Block with Iterable[Block] {
    def template = BlockSampler1.this
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => v1; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}

/** Sample variables in blocks of size 1.  The context for block creatation is simply the variable itself. */
abstract class VariableSampler1[V1<:Variable](m1:Manifest[V1]) extends BlockSampler1[V1,V1](m1,m1) {
	def block(v1:V1) : Iterable[Block] = Block(v1)

}

/** Sample variables in blocks of size 2.  Blocks are created from an arbitrary context of type C using the method block(). */
abstract class BlockSampler2[C,V1<:Variable,V2<:Variable](cm:Manifest[C], m1:Manifest[V1],m2:Manifest[V2]) extends BlockSampler[C]()(cm) {
  val vc1 = m1.erasure
  val vc2 = m2.erasure
  def _sample(b:Block) = sample(b.v1, b.v2)
  def sample(v1:V1, v2:V2) : DiffList
  case class Block(v1:V1,v2:V2) extends cc.factorie.Block with Iterable[Block] {
    def template = BlockSampler2.this
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => v1; case 1 => v2; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}

/** Sample variables in blocks of size 1.  The context for block creatation is the first variable in the block. */
abstract class VariableSampler2[V1<:Variable,V2<:Variable](m1:Manifest[V1], m2:Manifest[V2]) extends BlockSampler2[V1,V1,V2](m1,m1,m2)


