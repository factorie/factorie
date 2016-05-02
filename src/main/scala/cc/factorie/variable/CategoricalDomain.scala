/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.variable

import cc.factorie.util.Cubbie
import cc.factorie.{util, variable}

import scala.collection.mutable

// For single categorical values

/** A value in a CategoricalDomain.  
    Each value is assigned an intValue in the range 0...size-1.
    Each value has a category of type C.
    These are the values used to map from words to integer parameter indices, etc.
    @author Andrew McCallum */
trait CategoricalValue[C] extends DiscreteValue {
  def domain: CategoricalDomain[C]
  def category: C
  override def toString: String = category.toString
}

/** A domain for categorical variables.  It stores not only a size,
    but also the mapping from category values (of type T = this.CategoryType)
    to densely packed integers suitable for indices into parameter
    vectors.  For example, a common use case is mapping Strings (NLP or 
    document classification words) into indices, and back. 

    Furthermore if domain.gatherCounts = true, this domain will count
    the number of calls to 'index'.  Then you can reduce the size of
    the Domain by calling 'trimBelowCount' or 'trimBelowSize', which
    will recreate the new mapping from categories to densely-packed
    non-negative integers (making the old mapping no longer valid).  
    Thus, in typical usage you would (1) read in the data, 
    (2) trim the domain, (3) re-read the data with the new
    mapping, creating variables.

    @author Andrew McCallum
    */
class CategoricalDomain[C] extends DiscreteDomain(0) with IndexedSeq[CategoricalValue[C]] with CategoricalVectorDomain[C] with Domain with cc.factorie.util.ProtectedIntArrayBuffer {
  protected class CategoricalValue(val singleIndex:Int, val category:C) extends variable.CategoricalValue[C] {
    override def copy = this
    def domain = CategoricalDomain.this
    def dim1 = CategoricalDomain.this.size
  }
  type Value <: variable.CategoricalValue[C]
  def this(values:Iterable[C]) = { this(); values.foreach(value(_)); freeze() }

  private val __indices: java.util.HashMap[C,Value] = new java.util.HashMap[C,Value]
  def _indices = __indices
  private val lock = new util.RWLock
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weightsSet;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  override def dimensionDomain: CategoricalDomain[C] = this
  @inline final override def length = lock.withReadLock { _elements.length }
  var growPastMaxSize: Boolean = true
  /** Return the CategoricalValue associated with the given category. 
      If the category is not already in this CategoricalDomain and 'frozen' is false,
      and 'mazSize' will not be exceeded,
      then add the category to this CategoricalDomain.
      This method is thread-safe so that multiple threads may read and index data simultaneously. */
  def value(category: C): Value = {
    if (category == null) throw new Error("Null is not a valid category.")
    if (_frozen) {
      __indices.get(category)
    } else {
      lock.withReadLock {
        var thisIndex = __indices.get(category)
        if (thisIndex eq null) { // double-tap locking necessary to ensure only one thread adds to _indices
          lock.readUnlock()
          lock.writeLock()
          try {
            thisIndex = __indices.get(category)
            if (thisIndex eq null) {
              val m = _elements.length
              if (maxSize > 0 && m >= maxSize) {
                if (growPastMaxSize)
                  throw new Error("Index size exceeded maxSize")
                else {
                  println("Warning - max domain size %d exceeded! Freezing." format maxSize)
                  freeze()
                  return null.asInstanceOf[Value]
                }
              }
              // TODO Consider calling "new String(category)" here to avoid substring memory leak: http://stackoverflow.com/questions/15612157/substring-method-in-string-class-causes-memory-leak 
              val e: Value = newCategoricalValue(m, category).asInstanceOf[Value]
              _elements += e
              __indices.put(category, e)
              thisIndex = e
            }
          } finally {
            lock.writeUnlock()
            lock.readLock()
          }
        }
        thisIndex
      }
    }
  }
  /** Return the CategoricalValue at index i. */
  override def apply(i:Int): Value = _elements(i)
  def category(i:Int): C = lock.withReadLock {_elements(i).category.asInstanceOf[C]}
  def categories: Seq[C] = lock.withReadLock { _elements.map(_.category.asInstanceOf[C]) }
  /** Return the integer associated with the category, do not increment the count of category, even if gatherCounts is true. */
  def indexOnly(category:C): Int = {
    val v = value(category)
    if (v eq null) -1 else v.intValue
  }
  /** Return the integer associated with the category, and also, if gatherCounts is true, also increment the count of category.
      If the category is not already in this CategoricalDomain and 'frozen' is false,
      and 'mazSize' will not be exceeded,
      then add the category to this CategoricalDomain.
      This method is thread-safe so that multiple threads may read and index data simultaneously. */
  def index(category:C): Int = {
    val i = indexOnly(category)
    if (gatherCounts && i != -1) incrementCount(i)
    i
  }
  /** Return the integer associated with the category,
      and also (whether or not 'gatherCounts' is true')
      increment by 'count' the number of times this Domain says the category has been seen.
      If the category is not already in this CategoricalDomain and 'frozen' is false,
      and 'maxSize' will not be exceeded,
      then add the category to this CategoricalDomain.
      This method is thread-safe so that multiple threads may read and index data simultaneously. */
  def indexWithCount(category:C, count:Int): Int = {
    val i = indexOnly(category)
    this synchronized { _increment(i, count) }
    i
  }
  /** Like indexOnly, but throw an exception if the category is not already there. */
  def getIndex(category:C): Int = lock.withReadLock({
    val v = __indices.get(category)
    if (v ne null) v.intValue else throw new Error("Category not present; use index() to cause the creation of a new value.")
  })
  /** Like indexOnly, but return -1 if the category is not already there. */
  def indexOrNegativeOne(category:C): Int = lock.withReadLock({
    val v = __indices.get(category)
    if (v eq null) -1 else { v.intValue }
  })
  override def freeze(): Unit = {
    _frozen = true
  }

  def +=(x:C) : Unit = this.index(x)
  def ++=(xs:Traversable[C]) : Unit = xs.foreach(this += _)
  /** Wipe the domain, its elements, indices and counts clean */
  def clear(): Unit = { _frozen = false; _elements.clear(); lock.withWriteLock { _indices.clear(); _clear() } }
  // Separate argument types preserves return collection type
  def indexAll(c: Iterator[C]) = c map index
  def indexAll(c: List[C]) = c map index
  def indexAll(c: Array[C]) = c map index
  def indexAll(c: Set[C]) = c map index

  override def dimensionName(i:Int): String = category(i).toString
  override def toString() = "CategoricalDomain[]("+size+")"

  protected def newCategoricalValue(i:Int, e:C) = new CategoricalValue(i, e)

  /** If type T is not string, this should be overridden to provide de-serialization */
  override def stringToCategory(s:String): C = s.asInstanceOf[C]

  // Code for managing occurrence counts
  /** If true, then each call to CategoricalDomain.index will increment a count associated with value in the domain.
      This count can then later be used to trim the set of domain values by various thresholds. */
  var gatherCounts = false
  def count(i:Int): Int = _apply(i)
  def count(category:C): Int = _apply(indexOnly(category))
  def counts: cc.factorie.util.IntSeq = _takeAsIntSeq(length) // _toSeq.take(length)
  private var cachedCountsTotal: Long = -1
  def countsTotal: Long =
    if (frozen && cachedCountsTotal >= 0)
      cachedCountsTotal
    else {
      var total: Long = 0
      var i = 0
      val len = _length
      while (i < len) {
        total += _apply(i)
        i += 1
      }
      cachedCountsTotal = total
      total
    }
  def incrementCount(i:Int): Unit = this synchronized { _increment(i, 1) }
  def incrementCount(category:C): Unit = incrementCount(indexOnly(category))
  private def someCountsGathered: Boolean = { var i = 0; while (i < _length) { if (_apply(i) > 0) return true; i += 1 }; false }
  /** Returns the number of unique entries trimmed */
  def trimBelowCount(threshold:Int, preserveCounts:Boolean = false): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _elements.clone()
    val origCounts = _toArray
    clear() // This will also clear the counts
    gatherCounts = false
    if (preserveCounts) { for (i <- 0 until origEntries.size) if (origCounts(i) >= threshold) indexWithCount(origEntries(i).category.asInstanceOf[C], origCounts(i)) }
    else { for (i <- 0 until origEntries.size) if (origCounts(i) >= threshold) indexOnly(origEntries(i).category.asInstanceOf[C]) }
    freeze()
    origEntries.size - size
  }
  /** Returns the number of unique entries trimmed */
  def trimAboveCount(threshold:Int): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _elements.clone()
    clear()
    gatherCounts = false
    for (i <- 0 until origEntries.size)
      if (_apply(i) <= threshold) indexOnly(origEntries(i).category.asInstanceOf[C])
    _clear()
    freeze()
    origEntries.size - size
  }
  /** Returns the count threshold below which entries were discarded. */
  def trimBelowSize(target:Int): Int = {
    assert(!frozen)
    var threshold = 2
    while (sizeAtOrAboveCount(threshold) >= target) threshold += 1
    trimBelowCount(threshold)
    threshold
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
}


object CategoricalDomain {
  val NULL_INDEX = -1
}

class CategoricalDomainCubbie[T](val cd: CategoricalDomain[T]) extends Cubbie {
  // This cubbie automatically writes into the underlying CategoricalDomain instead of
  // using an intermediate HashMap representation
  setMap(new mutable.Map[String, Any] {
    override def update(key: String, value: Any): Unit = {
      val isFrozen = cd.frozen
      if (key == "size") { /* cd.size = value.asInstanceOf[Int] */ }
      else if (key == "frozen") { if (value.asInstanceOf[Boolean]) cd.freeze() }
      else if (key == "categories") {
        cd.unfreeze()
        val categories = value.asInstanceOf[Iterable[String]]
        //categories.map(c => if (cd.string2T != null) cd.string2T(c) else c.asInstanceOf[T]).foreach(cd.value(_))
        categories.map(c => cd.stringToCategory(c)).foreach(cd.value(_))
        if (isFrozen) cd.freeze()
      } else sys.error("Unknown cubbie slot key: \"%s\"" format key)
    }
    def += (kv: (String, Any)): this.type = { update(kv._1, kv._2); this }
    def -= (key: String): this.type = sys.error("Can't remove slots from cubbie map!")
    def get(key: String): Option[Any] =
      if (key == "size") Some(cd.size)
      else if (key == "frozen") Some(cd.frozen)
      else if (key == "categories") Some(cd.categories.map(_.toString)) // toString because not all categories are already Strings
      else None //{ println("CategoricalDomainCubbie.get key="+key); None }
    def iterator: Iterator[(String, Any)] = List("size", "frozen", "categories").map(s => (s, get(s).get)).iterator
  })
}

/* CategoricalDomain also facilitates counting occurrences of entries, and trimming the Domain size.
   WARNING: Any indices that you use and store before trimming will not be valid after trimming!
   Typical usage:
   <pre>
   class Token(s:String) extends CategoricalVariable(s)
   data.readAndIndex
   Domain[Token].trimBelowSize(100000) // this also automatically turns off counting
   data.readIndexAndCreateVariables // again
   </pre>
   */

// TODO Consider categorical remapping interface in the future.
///** To be used to avoid re-reading the data after CategoricalDomain trimming, 
//    but not yet implemented. */
//trait CategoricalRemapping { def remapCategories(fn:(Int)=>Int) }
