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
import java.io.{File,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}

// TODO Also make a randomized-representation CategoricalDomain, with hashes?

/** A value in a CategoricalVectorDomain */
//trait CategoricalVectorValue[T] extends DiscreteVectorValue with DomainType[CategoricalVectorDomain[T]]

trait CategoricalsValue[T] extends DiscretesValue with DomainType[CategoricalVectorDomain[T]]

/** Domain for CategoricalsVar, e.g. FeatureVectorVariable.
    @author Andrew McCallum */
trait CategoricalVectorDomain[T] extends DiscreteVectorDomain with ValueType[CategoricalsValue[T]] with DimensionDomainType[CategoricalDomain[T]] {
  thisDomain =>
  type CategoryType = T
  lazy val dimensionDomain: CategoricalDomain[T] = new CategoricalDomain[T]
  def size: Int = dimensionDomain.size
}


// For single categorical values

/** A value in a CategoricalDomain */
trait CategoricalValue[T] extends CategoricalsValue[T] with DiscreteValue with DomainType[CategoricalDomain[T]] {
  // TODO Rename this "category"
  def entry: T
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
class CategoricalDomain[T] extends DiscreteDomain with CategoricalVectorDomain[T] with ValueType[CategoricalValue[T]] {
  def this(values:Iterable[T]) = { this(); values.foreach(getValue(_)); freeze() }
  override lazy val dimensionDomain = this
  /** The size others might want to allocate to hold data relevant to this Index.  
      If maxSize is set can be bigger than size. 
      @see maxSize */
  override def allocSize = if (maxSize < 0) size else maxSize
  override def size = _indices.size
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weights;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  // TODO consider putting the following method back in later -akm
  //override def maxSize_=(s:Int) : Unit = if (maxSize >= size) maxSize = s else throw new Error("Trying to set maxSize smaller than size.")

  /** If true, do not allow this domain to change. */
  private var _frozen = false
  /** Can new category values be added to this Domain? */
  def frozen = _frozen
  override def freeze() = _frozen = true

  /** Map from entry back to int index */
  private var _indices = Map[T, Value]()
  /** Wipe the domain and its indices clean */
  def reset(): Unit = {
    _frozen = false
    _elements.clear()
    _indices = Map[T,Value]()
  }
  /** An alias for reset(). */
  def clear() = reset()

  class CategoricalValue(override val index:Int, val entry:T) extends DiscreteValue(index) with cc.factorie.CategoricalValue[T] {
    override def toString = entry.toString
  }
  protected def newCategoricalValue(i:Int, e:T): Value = new CategoricalValue(i, e) //.asInstanceOf[Value]

  def contains(entry: Any) = _indices.contains(entry.asInstanceOf[T])
  /* entry match { case e:T => _indices.contains(e); case _ => false } */

  /** Return an object at the given position or throws an exception if it's not found. */
  def getEntry(index: Int): T = _elements(index).entry // TODO Consider changing this name to getElement or getEntry or get
  override def getValue(index: Int): Value = _elements(index)
  // _indices.getOrElse(entry, null).asInstanceOf[Value]
  def getValue(entry:T): Value = {
    def nextMax: Value = {
      val m = _elements.size
      if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
      val e: Value = newCategoricalValue(m, entry) // Here is the place that new CategoricalValue gets created
      // TODO Can we make the above cast unnecessary??
      _elements += e
      _indices(entry) = e
      e
    }
    if (_frozen) _indices.getOrElse(entry, null.asInstanceOf[Value])
    else _indices.getOrElseUpdate(entry, nextMax)
  }

  /** Return a densely-packed positive integer index for the given object.  
      By default, allocate a new index (at the end) if the object was not found, 
      but if immutable may return -1 */
  // TODO Is this the right interface choice for gatherCounts?  What if some code (inefficiently) indexes more than once for the same data point?
  def indexOnly(entry: T): Int = {
    val value = getValue(entry)
    if (value eq null) -1 else value.index
  }
  def index(entry: T): Int = {
    val i = indexOnly(entry)
    if (gatherCounts && i != -1) incrementCount(i)
    i
  }

 
  /** Like index, but throw an exception if the entry is not already there. */
  def getIndex(entry:T) : Int = _indices.getOrElse(entry, throw new Error("Entry not present; use index() to cause the creation of a new entry.")).index

  //def indexOf[B >: Value](elem: B): Int = elem.index // elem.asInstanceOf[Value].index //index(elem.asInstanceOf[T]) // TODO Try to get rid of this cast!!!

  // Separate argument types preserves return collection type
  def indexAll(c: Iterator[T]) = c map index;
  def indexAll(c: List[T]) = c map index;
  def indexAll(c: Array[T]) = c map index;
  def indexAll(c: Set[T]) = c map index;
  def getAllValues(c: Iterator[Int]) = c map getValue
  def getAllValues(c: List[Int]) = c map getValue
  def getAllValues(c: Array[Int]) = c map getValue
  def getAllValues(c: Set[Int]) = c map getValue
  //def indexKeys[V](c: scala.collection.Map[T, V]) = Map[T, V]() ++ c.map {case (a, b) => (index(a), b)}
  //def indexValues[K](c: scala.collection.Map[K, T]) = Map[K, T]() ++ c.map {case (a, b) => (a, index(b))}

  def randomEntry(random:Random): T = getEntry(random.nextInt(size))
  def randomEntry: T = randomEntry(cc.factorie.random)
  def randomValue(random:Random): Value = getValue(random.nextInt(size))
  def randomValue: Value = randomValue(cc.factorie.random)
  def +=(x:T) : Unit = this.index(x)
  def ++=(xs:Traversable[T]) : Unit = xs.foreach(this.index(_))
 
  override def toString = "CategoricalDomain[]("+size+")"
  override def vectorDimensionName(i:Int): String = getEntry(i).toString

  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    if (f.exists) return // Already exists, don't write it again.  // TODO Careful about trying to re-write to the same location, though.
    val s = new PrintWriter(new FileWriter(f))
    //if (elements.next.asInstanceOf[AnyVal].getClass != classOf[String]) throw new Error("Only know how to save CategoryType String.")
    if (frozen) s.println("#frozen = true") else s.println("#frozen = false")
    for (e <- iterator) {
      if (e.toString.contains("\n")) throw new Error("Cannot save Domain with entry containing newline.")
      s.println(e.toString)
    }
    s.close
  }
  override def load(dirname:String): Unit = {
    if (size > 0) return // Already initialized, don't read again
    val f = new File(dirname+"/"+filename)
    val s = new BufferedReader(new FileReader(f))
    var line = s.readLine
    var willFreeze = false
    if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
    while ({line = s.readLine; line != null}) {
      //println("Domain load got "+line)
      this.index(line.asInstanceOf[T]) // TODO What if T isn't a String?  Fix this.
    }
    if (willFreeze) freeze()
    s.close
    //println("Loading Domain["+filename+"].size="+size)
  }
  
  // Code for managing occurrence counts
  var gatherCounts = false
  private val _countsInitialSize = 64
  private var _counts = new Array[Int](_countsInitialSize)
  protected def ensureSize(n:Int): Unit = {
    if (_counts.length - 1 < n) {
      var newsize = _counts.length * 2
      while (newsize < n) newsize *= 2
      val new_counts = new Array[Int](newsize)
      Array.copy(_counts, 0, new_counts, 0, _counts.size)
      _counts = new_counts
    }
  }
  def count(i:Int): Int = _counts(i)
  def count(entry:T): Int = _counts(indexOnly(entry))
  def incrementCount(i:Int): Unit = { ensureSize(i); _counts(i) += 1 }
  def incrementCount(entry:T): Unit = incrementCount(indexOnly(entry))
  private def someCountsGathered: Boolean = { for (i <- 0 until _counts.size) if (_counts(i) > 0) return true; return false }
  /** Returns the number of unique entries trimmed */
  def trimBelowCount(threshold:Int): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _elements
    reset() // TODO Should we override reset to also set gatherCounts = true?  I don't think so.
    gatherCounts = false
    for (i <- 0 until origEntries.size)
      if (_counts(i) >= threshold) indexOnly(origEntries(i).entry)
    _counts = null // We don't need counts any more; allow it to be garbage collected.  Note that if
    freeze()
    origEntries.size - size
  }
  /** Return the number of unique entries with count equal to 'c'. */
  def sizeAtCount(c:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) == c) ret += 1
    ret
  }
  /** Return the number of unique entries with count greater than or equal to 'threshold'. 
      This returned value will be the size of the Domain after a call to trimBelowCount(threshold). */
  def sizeAtOrAboveCount(threshold:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) >= threshold) ret += 1
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

/** To be used to avoid re-reading the data, as described in above comment, 
    but not yet implemented. */
trait CategoricalRemapping {
  def remapCategories(fn:(Int)=>Int)
}


/** A Categorical domain with enumerated values.  Provides convenient intialization to known values, 
    with value members holding those known values.  For example:
    object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value }
    Each of the defined val will have Int type.  Their corresponding String category values 
    will be the name of the variable (obtained through reflection). */
class EnumDomain extends CategoricalDomain[String] {
  /* For all member variables, if its type is Int, set its value to its name, 
     and intern in the Domain.  Usage: 
     object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value } */
  private def stringFields = this.getClass.getDeclaredFields.filter(f => { /* println("stringFields "+f); */  f.getType == classOf[Int] })
  private var stringFieldsIterator: Iterator[java.lang.reflect.Field] = _
  def Value: Int = {
    if (stringFieldsIterator == null) stringFieldsIterator = stringFields.iterator
    assert(stringFieldsIterator.hasNext)
    val field = stringFieldsIterator.next
    //println("StringDomain Value got "+field.getName)
    //checkFields // TODO Re-add this and make sure example/DirichletDemo works
    index(field.getName) // Add it to the index
  } 
  private def checkFields: Unit = {
    for (field <- stringFields) {
      val fieldName = field.getName
      println("StringDomain.checkFields "+field+" fieldName="+fieldName)
      //getClass.getMethods.foreach(m => println(m.toString))
      val fieldMethod = getClass.getMethod(fieldName) // was with ,null)
      val fieldValue = fieldMethod.invoke(this).asInstanceOf[Int]
      //println("Field "+fieldName+" has value "+fieldValue)
      if (fieldValue != 0 && this.getEntry(fieldValue) != fieldName) throw new Error("Somehow StringDomain category "+fieldName+" got the wrong String value "+fieldValue+" ("+this.getEntry(fieldValue)+").")
    }
  }
}

// TODO Create a EnumVectorDomain
