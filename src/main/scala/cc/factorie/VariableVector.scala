package cc.factorie
import scala.collection.mutable.ArrayBuffer
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import scala.util.Sorting

trait VectorVariable extends IndexedVariable with TypedVariable {
  def vector : Vector
  def indices: Collection[Int]
}


/**A variable whose value is a SparseBinaryVector; immutable. */
// I considered renaming this VectorObservation, but then I realized that methods such as += change its value. -akm
// TODO Rename to BinaryVectorVariable?
abstract class BinaryVectorVariable[T](initVals:Iterable[T]) extends VectorVariable {
	//def this(iv:T*) = this(iv:Seq[T])
	def this() = this(null)
	type ValueType = T
	type VariableType <: BinaryVectorVariable[T]
  class DomainInSubclasses
  protected var indxs = new ArrayBuffer[Int]()
  private var _vector: Vector = null // TODO Can we make this more memory efficient?  Avoid having both Vector and ArrayBuffer?;
  if (initVals ne null) this ++= initVals
  def indices : Seq[Int] = indxs // TODO project to ensure no changes, even with casting?  But this would involve allocating the Projection
  def values : Seq[T] = { val d = this.domain; indxs.map(d.get(_)) }
  override def vector = {
  	if (_vector == null || _vector.size != domain.allocSize) {
  		val indices = indxs.toArray
  		Sorting.quickSort(indices)
  		_vector = new SparseBinaryVector(domain.allocSize, indices)
  	}
  	_vector
  }
  // TODO when we have Scala 2.8, add to the method below difflist argument with default value null
  // But will a += b syntax with with default arguments?
  def +=(value: T) : Unit = {
  	val idx = domain.index(value);
  	if (idx == IndexedDomain.NULL_INDEX) throw new Error("VectorVariable += value " + value + " not found in domain " + domain)
  	indxs += idx
  	_vector = null
  }
  //def +(value: T) = {this += value; this} // TODO Shouldn't this method actually return a new VectorVariable, leaving old one unchanged?  Yes.
  def ++=(vals: Iterable[T]) : Unit = vals.foreach(v => this += v)
  //def ++(vals: Iterable[T]) = {this ++= vals; this} // TODO this method should return a new Vector
  override def toString = {
    val s = new StringBuilder(printName + "(")
    val iter = vector.activeDomain.elements
    if (iter.hasNext) { val i:Int = iter.next ; s ++= (domain.get(i).toString + "=" + i) }
    while (iter.hasNext) {
    	val i:Int = iter.next
      s ++= ("," + domain.get(i).toString + "=" + i)
    }
    s ++= ")"
    s.toString
  }
}

/** A vector of */
abstract class RealVectorVariable[T](initVals:Iterable[(T,Double)]) extends VectorVariable {
  def this() = this(null)
	type ValueType = T
	type VariableType <: RealVectorVariable[T]
  class DomainInSubclasses
  lazy val vector: Vector = new SparseVector(domain.allocSize)
  if (initVals ne null) this ++= initVals
  def indices : Collection[Int] = if (vector == null) Nil else vector.activeDomain
  def +=(pair:(T,Double)) = vector((domain.index(pair._1))) = pair._2
  def ++=(pairs:Iterable[(T,Double)]) = pairs.foreach(pair => this += pair)
}

