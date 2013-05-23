package cc.factorie

import scala.util.MurmurHash
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap
import cc.factorie.la._

/** Functions used inside HashFeatureVectorVariable,
    also available here for outside use. */
object HashFeatureVectorVariable {
  def hash(c: Any): Int = c match {
    case s:String => MurmurHash.stringHash(s)
    case _ => c.hashCode()
  }
  def index(c: Any, size: Int): Int = hash(c) % size
  def sign(c: Any): Int = hash(c) >> 31
}

/** A variable whose value is a SparseTensor1 whose length matches the size of a DiscreteDomain,
    and whose dimensions each correspond to the result of running a hash function on elements
    added to the vector using +=.
    These can be used as feature vectors where one wants to avoid a large or growing CategoricalDomain.
    The 'dimensionDomain' is abstract.
    @author Andrew McCallum */
abstract class HashFeatureVectorVariable extends DiscreteDimensionTensorVariable {
  override def domain: DiscreteDomain
  def this(initVals:Iterable[Any]) = { this(); initVals.map(this.+=_) }
  def ++=(cs: Iterable[Any]): Unit = cs.foreach(this.+= _)
  def +=(c: Any): Unit = {
    val hash = HashFeatureVectorVariable.hash(c)
    val sign = math.signum(hash >> 31)
    tensor.update(math.abs(hash % domain.size), sign)
  }
  set(new SparseIndexedTensor1(domain.size))(null)
}
