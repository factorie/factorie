package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import cc.factorie.util.Implicits._

/** A collection of variables to be sampled together. */
trait Block extends Product with Iterable[Block] {
  // TODO Default value of d should be "new DiffList"
  def sample : DiffList
  def samplePriority : Double = 1.0
  def template: Sampler
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

/** A specialized sampler for particular variables.  
 * Subclasses implement "sample(V1) */
trait Sampler {
	var iterations = 0
	def step : DiffList 
  def step(numIterations:Int): Unit = {
    for (iteration <- 0 until numIterations) {
      step
      if (!postIterationHook(this)) return
    }
  }
  /**Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
  def postIterationHook(s:Sampler): Boolean = true
  /**Called just before each variable is sampled.  Return an alternative variable if you want that one sampled instead. */
  //def preVariableSampleHook(v:Variable): Variable = v
}

trait VariableSampler extends Sampler {
  def step : DiffList = throw new Error("Not yet figured out unified architecture for Gibbs and MH sampling")
 	val variables = new ArrayBuffer[Variable];
	/** Return null if this Sampler does not operate on this v */
  def process(v:Variable) : DiffList
  def process(vs:Iterable[Variable]) : Unit = vs.foreach(process(_))
  def process(vs:Iterable[Variable], numIterations:Int) : Unit =  {
    for (i <- 1 to numIterations) {
      vs.foreach(process(_))
      iterations += 1
      if (!postIterationHook(this)) return
    }
  }
}

abstract class Sampler1[V1<:Variable](m1:Manifest[V1]) extends VariableSampler{
  val vc1 = m1.erasure
  def process(v:Variable) : DiffList = {
    val blocks = if (vc1.isAssignableFrom(v.getClass)) unroll1(v.asInstanceOf[V1]) 
                 else return null 
    blocks match {
      case Nil => new DiffList
      case b:Block => b.sample
      case _ => { val d = new DiffList; d ++= blocks.flatMap(_.sample); d } 
    }
  }
  def unroll1(v1:V1) : Iterable[Block] = Block(v1)
  def _sample(b:Block) : DiffList = sample(b.v1)
  def sample(v1:V1) : DiffList
  case class Block(v1:V1) extends cc.factorie.Block with Iterable[Block] {
    def template = Sampler1.this
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => v1; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}

abstract class Sampler2[V1<:Variable,V2<:Variable](m1:Manifest[V1],m2:Manifest[V2]) extends VariableSampler {
  val vc1 = m1.erasure
  val vc2 = m2.erasure
  def process(v:Variable) : DiffList = {
    var blocks = new ListBuffer[Block]
    var response = false
    if (vc1.isAssignableFrom(v.getClass)) { blocks ++ unroll1(v.asInstanceOf[V1]); response = true } 
    if (vc2.isAssignableFrom(v.getClass)) { blocks ++ unroll2(v.asInstanceOf[V2]); response = true }
    if (!response) return null
    if (blocks.length == 1) blocks.first.sample
    else { val d = new DiffList; d ++= blocks.flatMap(_.sample); d } 
  }
  def unroll1(v1:V1) : Block
  def unroll2(v2:V2) : Block
  def _sample(b:Block) = sample(b.v1, b.v2)
  def sample(v1:V1, v2:V2) : DiffList
  case class Block(v1:V1,v2:V2) extends cc.factorie.Block with Iterable[Block] {
    def template = Sampler2.this
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => v1; case 1 => v2; case _ => throw new IndexOutOfBoundsException(i.toString)}
    def sample = _sample(this)
  } 
}


