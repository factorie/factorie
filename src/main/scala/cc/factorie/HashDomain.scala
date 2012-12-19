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

class HashDomain[C](size: Int) extends DiscreteDomain(size) {
  def index(c: C) = math.abs(c.hashCode() % size)
}

class StringHashDomain(size: Int) extends HashDomain[String](size) {
  override def index(s: String) = math.abs(MurmurHash.stringHash(s) % size)
}

// Call them all either "Hash" or "Hashing", but don't mix the names. 

abstract class BinaryHashFeatureVectorVariable[C] extends DiscreteTensorVariable {
  override def domain: HashDomain[C]
  def ++=(cs: Iterable[C]) = cs.map(c => tensor.update(domain.index(c), 1.0))
  def +=(c: C) = tensor.update(domain.index(c), 1.0)
  def this(initVals:Iterable[C]) = { this(); initVals.map(c => domain.index(c)).foreach(this.tensor.+=(_, 1.0)) }
  set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))(null)
}

abstract class HashFeatureVectorVariable[C] extends DiscreteTensorVariable {
  override def domain: HashDomain[C]
  def ++=(cs: Iterable[C]) = cs.map(c => tensor.update(domain.index(c), 1.0))
  def +=(c: C) = tensor.update(domain.index(c), 1.0)
  def this(initVals:Iterable[C]) = { this(); initVals.map(c => domain.index(c)).foreach(this.tensor.+=(_, 1.0)) }
  set(new GrowableSparseTensor1(domain.dimensionDomain))(null)
}
