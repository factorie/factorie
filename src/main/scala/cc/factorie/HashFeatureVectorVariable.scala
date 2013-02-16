package cc.factorie

import scala.util.MurmurHash
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap
import cc.factorie.la._

/*
 * A Domain which does not preserve the mapping from Category to Value,
 * instead using the hash of C to index.
 * 
 * @author Brian Martin
 * @date 10/7/2012
 * @since 1.0
 */


object HashHelper {
  def hash(s: String) = MurmurHash.stringHash(s)
  def hash(c: Any) = c.hashCode()
  def index(s: String, size: Int) = math.abs(MurmurHash.stringHash(s) % size)
  def index(c: Any, size: Int) = math.abs(c.hashCode() % size)
}


abstract class ObjectHashFeatureVectorVariable[C] extends DiscreteDimensionTensorVariable {
  override def domain: DiscreteDomain
  def ++=(cs: Iterable[C]) = cs.map(this.+= _)
  def +=(c: C) = {
    val hash = HashHelper.hash(c)
    val sign = math.signum(hash >> 31)
    tensor.update(math.abs(hash % domain.size), sign)
  }
  def this(initVals:Iterable[C]) = { this(); initVals.map(this.+=_) }
  set(new SparseIndexedTensor1(domain.size))(null)
}

abstract class HashFeatureVectorVariable extends ObjectHashFeatureVectorVariable[String] {
  override def +=(c: String) {
    val hash = HashHelper.hash(c)
    val sign = math.signum(hash >> 31)
    tensor.update(math.abs(hash % domain.size), sign)
  }
}
