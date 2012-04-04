/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie
import cc.factorie.la._
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer,HashMap}
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/** A Domain for variables whose value is a Tensor whose length matches the size of a DiscreteDomain. 
    This domain has a non-negative integer size.  The method 'dimensionDomain' is abstract. */
trait DiscreteTensorDomain[+T<:Tensor] extends TensorDomain with ValueType[T] {
  def dimensionDomain: DiscreteDomain2
  /** The maximum size to which this domain will be allowed to grow.  
      The 'dimensionDomain.size' method may return values smaller than this, however.
      This method is used to pre-allocate a Template's parameter arrays and is useful for growing domains. */
  def dimensionSize: Int = dimensionDomain.size
  def dimensionName(i:Int): String = i.toString
  def freeze(): Unit = dimensionDomain.freeze
  override def save(dirname: String, gzip: Boolean = false) {
    // TODO: Note that if multiple domains have same dimension domains, it will be written multiple times
    dimensionDomain.save(dirname, gzip)
  }
  override def load(dirname: String, gzip: Boolean = false) {
    // TODO: Note that the dimensionDomain might get read multiple times
    if(!dimensionDomain.frozen) dimensionDomain.load(dirname, gzip)
  }
}

// TODO Just a placeholder for now
trait CategoricalTensorDomain[+T<:Tensor,C] extends DiscreteTensorDomain[T] {
  def dimensionDomain: CategoricalDomain2[C]
}

trait DiscreteTensorVar[+A<:Tensor] extends TensorVar[A] with VarAndValueType[DiscreteTensorVar[A],A] {
  def domain: DiscreteTensorDomain[A]
}

/** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
abstract class DiscreteTensorVariable[A<:Tensor] extends TensorVariable[A] with DiscreteTensorVar[A] {
  //thisVariable =>
  //_set(new SparseVector(domain.dimensionSize) with DiscreteVectorValue { def domain = thisVariable.domain })
}

trait CategoricalTensorVar[+T<:Tensor,C] extends DiscreteTensorVar[T] {
  def domain: CategoricalTensorDomain[T,C]
}
abstract class CategoricalTensorVariable[T<:Tensor,C] extends DiscreteTensorVariable[T] with CategoricalTensorVar[T,C]

abstract class SparseCategoricalTensorVariable1[C] extends CategoricalTensorVariable[GrowableSparseTensor1,C] {
  _set(new GrowableSparseTensor1(domain.dimensionDomain))
}


// A sketch for the future:

trait DiscreteValue2 extends SingletonBinaryTensorLike1 {
  def domain: DiscreteDomain2
  @inline final def intValue: Int = singleIndex // TODO Consider swapping singleIndex <-> intValue
  @inline final def dim1 = domain.size
}

trait CategoricalValue2[C] extends DiscreteValue2 {
  def domain: CategoricalDomain2[C]
  def category: C
}

class DiscreteDomain2(sizeProxy:Iterable[Any]) extends IndexedSeq[DiscreteValue2] with DiscreteTensorDomain[DiscreteValue2] with ValueType[DiscreteValue2] {
  thisDomain =>
  def this(size:Int) = { this(null.asInstanceOf[Iterable[Any]]); _size = size }
  def dimensionDomain: DiscreteDomain2 = this
  /** If true, do not allow this domain to change. */
  protected var _frozen = false
  override def freeze(): Unit = _frozen = true
  /** Can new category values be added to this Domain? */
  def frozen = _frozen
  def allocSize = size // TODO Remove this?
  var maxRequestedInt: Int = 0

  /** Maps from integer index to the DiscreteValue objects */
  private val __elements = new scala.collection.mutable.ArrayBuffer[ValueType]
  def _elements = __elements // Define this way so that _elements can be overridden

  // If _size >= 0 it is used to determine DiscreteDomain.size, otherwise _sizeProxy.size is used. 
  private var _size = -1
  private val _sizeProxy = sizeProxy
  def length = if (_size >= 0) _size else _sizeProxy.size
  def apply(index:Int): ValueType = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) {
      _elements synchronized { for (i <- _elements.size to index) _elements += new DiscreteValue(i) }
    } //.asInstanceOf[Value] // Here new a DiscreteValue gets created
    _elements(index)
  }
  def unapply(value:ValueType): Option[Int] = if (value.domain == this) Some(value.intValue) else None
  override def iterator = _elements.iterator
  def getAll(c: Iterator[Int]) = c map apply
  def getAll(c: List[Int]) = c map apply
  def getAll(c: Array[Int]) = c map apply
  def getAll(c: Set[Int]) = c map apply

  protected class DiscreteValue(val singleIndex:Int) extends cc.factorie.DiscreteValue2 {
    def domain = thisDomain
    override def toString = intValue.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue2 => this.intValue == other.intValue; case _ => false }
  }
  
  // Serialization
  override def save(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val writer = new PrintWriter(new BufferedOutputStream({
      if (gzip)
        new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(f)))
      else
        new FileOutputStream(f)
    }))

    writer.println(size)
    writer.close
  }

  override def load(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val reader = new BufferedReader(new InputStreamReader({
      if (gzip)
        new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
      else
        new FileInputStream(f)
    }))
    val line = reader.readLine
    val readSize = Integer.parseInt(line)
    require(size == readSize)
  }
  
}


class CategoricalDomain2[C] extends DiscreteDomain2(0) with IndexedSeq[CategoricalValue2[C]] with CategoricalTensorDomain[CategoricalValue2[C],C] with ValueType[CategoricalValue2[C]] with cc.factorie.util.ProtectedIntArrayBuffer {
  thisDomain =>
  def this(values:Iterable[C]) = { this(); values.foreach(value(_)) }
  //private val _elements = new ArrayBuffer[ValueType]
  private val _indices = new HashMap[C,ValueType] with collection.mutable.SynchronizedMap[C, ValueType] //new HashMap[C,ValueType]
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weights;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  override def dimensionDomain: CategoricalDomain2[C] = this
  @inline final override def length = _elements.length
  def value(category:C): ValueType = {
    if (_frozen) _indices.getOrElse(category, null.asInstanceOf[ValueType])
    else {
      if (_indices.contains(category)) { // double-tap locking necessary to ensure only one thread adds to _indices
        _indices.synchronized({
          if (_indices.get(category).isEmpty) {
            val m = _elements.size
            if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
            val e: ValueType = new CategoricalValue(m, category).asInstanceOf[ValueType]
            _elements += e
            _indices(category) = e
          }
        })
      }
      _indices.getOrElse(category, null)
    }
  }
  override def apply(i:Int): ValueType = _elements(i)
  def category(i:Int): C = _elements(i).category
  def categories: Seq[C] = _elements.map(_.category)
  /** Return the integer associated with the category, do not increment the count of category, even if gatherCounts is true. */
  def indexOnly(category:C): Int = {
    val v = value(category)
    if (v eq null) -1 else v.intValue
  }
  /** Return the integer associated with the category, and also, if gatherCounts is true, also increment the count of category. */
  def index(category:C): Int = {
    val i = indexOnly(category)
    if (gatherCounts && i != -1) incrementCount(i)
    i
  }
  /** Like index, but throw an exception if the category is not already there. */
  def getIndex(category:C): Int = _indices.getOrElse(category, throw new Error("Category not present; use index() to cause the creation of a new value.")).intValue
  
  def +=(x:C) : Unit = this.value(x)
  def ++=(xs:Traversable[C]) : Unit = xs.foreach(this.index(_))
  /** Wipe the domain and its indices clean */
  def clear(): Unit = { _frozen = false; _elements.clear(); _indices.clear() }
  // Separate argument types preserves return collection type
  def indexAll(c: Iterator[C]) = c map index;
  def indexAll(c: List[C]) = c map index;
  def indexAll(c: Array[C]) = c map index;
  def indexAll(c: Set[C]) = c map index;

  override def dimensionName(i:Int): String = category(i).toString
  override def toString = "CategoricalDomain2[]("+size+")"

  protected class CategoricalValue(val singleIndex:Int, val category:C) extends CategoricalValue2[C] {
    def domain = thisDomain
  }

  override def save(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    if (f.exists) return // Already exists, don't write it again.  // TODO Careful about trying to re-write to the same location, though.
    val writer = new PrintWriter(new BufferedOutputStream({
      if (gzip)
        new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(f)))
      else
        new FileOutputStream(f)
    }))
    if (frozen) writer.println("#frozen = true") else writer.println("#frozen = false")
    for (e <- iterator) {
      if (e.toString.contains("\n")) throw new Error("Cannot save Domain with category String containing newline.")
      writer.println(e.toString)
    }
    writer.close
  }

  override def load(dirname:String, gzip: Boolean = false): Unit = {
    if (size > 0) return // Already initialized, don't read again
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val reader = new BufferedReader(new InputStreamReader({
      if (gzip)
        new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
      else
        new FileInputStream(f)
    }))
    var line = reader.readLine
    var willFreeze = false
    if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
    while ({line = reader.readLine; line != null})
      this.index(line.asInstanceOf[C]) // TODO What if C isn't a String?  Fix this.
    if (willFreeze) freeze()
    reader.close
  }

  var gatherCounts = false
  def count(i:Int): Int = _apply(i)
  def count(category:C): Int = _apply(indexOnly(category))
  def counts: Seq[Int] = _toSeq.take(length)
  def countsTotal: Int = _sum    
  def incrementCount(i:Int): Unit = _increment(i, 1)
  def incrementCount(category:C): Unit = incrementCount(indexOnly(category))
  private def someCountsGathered: Boolean = { var i = 0; while (i < _length) { if (_apply(i) > 0) return true; i += 1 }; return false }
  /** Returns the number of unique entries trimmed */
  def trimBelowCount(threshold:Int): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _elements.clone
    clear() // TODO Should we override reset to also set gatherCounts = true?  I don't think so.
    gatherCounts = false
    for (i <- 0 until origEntries.size)
      if (_apply(i) >= threshold) indexOnly(origEntries(i).category.asInstanceOf[C])
    _clear // We don't need counts any more; allow it to be garbage collected.
    freeze()
    origEntries.size - size
  }
  /** Return the number of unique entries with count equal to 'c'. */
  def sizeAtCount(c:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = math.min(size, _length)
    for (i <- 0 until min) if (_apply(i) == c) ret += 1
    ret
  }
  /** Return the number of unique entries with count greater than or equal to 'threshold'. 
      This returned value will be the size of the Domain after a call to trimBelowCount(threshold). */
  def sizeAtOrAboveCount(threshold:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = math.min(size, _length)
    for (i <- 0 until min) if (_apply(i) >= threshold) ret += 1
    ret
  }
  /** Return the number of unique entries with count below 'threshold'. */
  def sizeBelowCount(threshold:Int): Int = size - sizeAtOrAboveCount(threshold)  
  /** Returns the count threshold below which entries were discarded. */
  def trimBelowSize(target:Int): Int = {
    assert(!frozen)
    var threshold = 2
    while (sizeAtOrAboveCount(threshold) >= target) threshold += 1
    trimBelowCount(threshold)
    threshold
  }
}

