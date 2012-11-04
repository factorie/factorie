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
import scala.collection.mutable.{Map,ArrayBuffer, HashMap, ListBuffer}
import scala.util.Random
import cc.factorie.la._
import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}


// For single categorical values

/** A value in a CategoricalDomain */
trait CategoricalValue[C] extends DiscreteValue {
  def domain: CategoricalDomain[C]
  def category: C
  override def toString: String = category.toString
}

/** A domain for categorical variables.  It stores not only a size,
    but also the mapping from category values (of type T = this.CategoryType)
    to densely packed integers suitable for indices into parameter
    vectors.

    Furthermore if domain.gatherCounts = true, this domain will count
    the number of calls to 'index'.  Then you can reduce the size of
    the Domain by calling 'trimBelowCount' or 'trimBelowSize', which
    will recreate the new mapping from categories to densely-packed
    non-negative integers.  In typical usage you would (1) read in the
    data, (2) trim the domain, (3) re-read the data with the new
    mapping, creating variables.

    @author Andrew McCallum
    */
class CategoricalDomain[C] extends DiscreteDomain(0) with IndexedSeq[CategoricalValue[C]] with CategoricalTensorDomain[C] with Domain[CategoricalValue[C]] with cc.factorie.util.ProtectedIntArrayBuffer {
  thisDomain =>
  def this(values:Iterable[C]) = { this(); values.foreach(value(_)) }
  //def this(values:C*) = { this(); values.foreach(value(_)) }
  //private val _elements = new ArrayBuffer[ValueType]
  private val __indices = new HashMap[C,Value] with collection.mutable.SynchronizedMap[C, Value] //new HashMap[C,ValueType]
  def _indices = __indices
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weights;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  override def dimensionDomain: CategoricalDomain[C] = this
  @inline final override def length = _elements.length
  def value(category:C): Value = {
    if (_frozen) _indices.getOrElse(category, null.asInstanceOf[Value])
    else {
      if (!_indices.contains(category)) { // double-tap locking necessary to ensure only one thread adds to _indices
        _indices.synchronized({
          if (_indices.get(category).isEmpty) {
            val m = _elements.size
            if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
            val e: Value = newCategoricalValue(m, category).asInstanceOf[Value]
            _elements += e
            _indices(category) = e
          }
        })
      }
      //_indices.getOrElse(category, null)
      _indices(category)
    }
  }
  override def apply(i:Int): Value = _elements(i)
  def category(i:Int): C = _elements(i).category.asInstanceOf[C]
  def categories: Seq[C] = _elements.map(_.category.asInstanceOf[C]) // TODO Can we avoid this cast.
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
  override def toString = "CategoricalDomain[]("+size+")"

  protected def newCategoricalValue(i:Int, e:C) = new CategoricalValue(i, e)
  protected class CategoricalValue(val singleIndex:Int, val category:C) extends cc.factorie.CategoricalValue[C] {
    override def copy = this
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

  var string2T: (String) => C = null  // if T is not string, this should be overridden to provide deserialization
  // TODO Use this instead: -akm
  //def stringToCategory(s:String): C = s.asInstanceOf[C]
  var _frozenByLoader = false
  override def load(dirname:String, gzip: Boolean = false): Unit = {
    if (_frozenByLoader) return // Already initialized by loader, don't read again
    else if (size > 0) throw new Error("Attempted to load a non-empty domain. Did you mean to load the model before creating variables?")
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val reader = new BufferedReader(new InputStreamReader({
      if (gzip)
        new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
      else
        new FileInputStream(f)
    }))
    this.loadFromReader(reader)
    reader.close()
  }

  override def loadFromReader(reader: BufferedReader): Unit = {
    var line = reader.readLine
    var willFreeze = false
    if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
    if (string2T eq null) {
     while ({line = reader.readLine; line != null})
       this.index(line.asInstanceOf[C]) // this cast shouldn't be necessary
   }
   else {
     while ({line = reader.readLine; line != null})
       this.index(string2T(line))
   }

    
//    while ({line = reader.readLine; line != null})
//      this.index(stringToCategory(line))
    if (willFreeze) { freeze(); _frozenByLoader = true }
  }

  // Code for managing occurrence counts
  var gatherCounts = false
  def count(i:Int): Int = _apply(i)
  def count(category:C): Int = _apply(indexOnly(category))
  def counts: cc.factorie.util.IntSeq = _takeAsIntSeq(length) // _toSeq.take(length)
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


object CategoricalDomain {
  val NULL_INDEX = -1
}

import cc.factorie.util._
class CategoricalDomainCubbie extends Cubbie {
  val size = IntSlot("size")
  val frozen = BooleanSlot("frozen")
  val categories = StringListSlot("end")
    def store(d:CategoricalDomain[String]): this.type = {
    size := d.size
    frozen := d.frozen
    categories := d.categories
    this
  }
  def fetch(d:CategoricalDomain[String]): CategoricalDomain[String] = {
    for (c <- categories.value) d.index(c)
    if (frozen.value) d.freeze()
    d
  }
}




/* CategoricalDomain also facilitates counting occurences of entries, and trimming the Domain size.
   WARNING: Any indices that you use and store before trimming will not be valid after trimming!
   Typical usage:
   <pre>
   class Token(s:String) extends CategoricalVariable(s)
   data.readAndIndex
   Domain[Token].trimBelowSize(100000) // this also automatically turns off counting
   data.readIndexAndCreateVariables // again
   </pre>
   */
