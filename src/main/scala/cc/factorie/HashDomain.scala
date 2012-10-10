package cc.factorie

import scala.util.MurmurHash
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap

class HashDomain2[C](size:Int) extends DiscreteDomain(size) {
  def index(category:C) = category.hashCode % size
}


/*
 * A CategoricalDomain which does not preserve the mapping from Category to Value,
 * intead using the hash of the category to index.
 * 
 * HashDomains for String and Int categories are provided.
 * 
 * @author Brian Martin
 * @date 10/7/2012
 * @since 1.0
 */

// Hash should be specialized for types other than String.
class Hash[C](var c: C = null.asInstanceOf[C]) {
  val hash: Int = if (c == null.asInstanceOf[C]) 0 else { val h = MurmurHash.stringHash(c.toString); c = null.asInstanceOf[C]; h }
  override def hashCode() = hash
}

abstract class HashDomain[C](val hashLength: Int) extends CategoricalDomain[Hash[C]] with ValueBound[CategoricalValue[Hash[C]]] with Domain[CategoricalValue[Hash[C]]] {
  def constructHash(i: Int): Hash[C]
  
  private val __elements = new ArrayBuffer[this.Value](hashLength)
  override def _elements = __elements // this only necessary for specifying the length of the ArrayBuffer
  // _indices isn't needed because the category (Hash[C]) already knows its index in _elements
  override def _indices = null.asInstanceOf[HashMap[Hash[C], this.Value] with SynchronizedMap[Hash[C], this.Value]]
  
  @inline final def adj(h: Int) = math.abs(h % hashLength)
  
  def fill() = {
    for (i <- 0 to hashLength) {
      val category = constructHash(i)
      val e: Value = newCategoricalValue(i,category).asInstanceOf[Value]
      _elements += e
    }
	super.freeze()
  }
  
  override def category(idx:Int): Hash[C] = constructHash(idx)
  override def index(h: Hash[C]): Int = adj(h.hash)
  override def getIndex(h :Hash[C]): Int = index(h)
  override def value(h: Hash[C]) = _elements(index(h))
}

class StringHashDomain(length: Int) extends HashDomain[String](length) {
  def constructHash(i: Int) = new Hash[String]() { override val hash = adj(i) }
  fill()
}

class IntHashDomain(length: Int) extends HashDomain[Int](length) {
  def constructHash(i: Int) = new Hash[Int]() { override val hash = adj(i) }
  fill()
}

abstract class HashingBinaryFeatureVectorVariable[C] extends BinaryFeatureVectorVariable[Hash[C]] {
  def this(initVals:Iterable[Hash[C]]) = { this(); this.++=(initVals) }
  override def toString: String = activeCategories.mkString(printName+"(", ",", ")")
  override def domain: HashDomain[C] = null
}
