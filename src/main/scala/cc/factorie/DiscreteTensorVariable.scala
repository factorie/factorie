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
  def contains(index:Int): Boolean = tensor.apply(index) != 0.0
}

/** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
abstract class DiscreteTensorVariable[A<:Tensor] extends TensorVariable[A] with DiscreteTensorVar[A] {
  def this(initialValue:A) = { this(); _set(initialValue) }
  //thisVariable =>
  //_set(new SparseVector(domain.dimensionSize) with DiscreteVectorValue { def domain = thisVariable.domain })
}

trait CategoricalTensorVar[+T<:Tensor,C] extends DiscreteTensorVar[T] {
  def domain: CategoricalTensorDomain[T,C]
  /** If false, then when += is called with a value (or index) outside the Domain, an error is thrown.
      If true, then no error is thrown, and request to add the outside-Domain value is simply ignored. */
  def skipNonCategories = false
  protected def doWithIndexSafely(elt:C, v:Double, update:Boolean): Unit = {
    val i = domain.dimensionDomain.index(elt)
    if (i == CategoricalDomain.NULL_INDEX) {
      if (!skipNonCategories) throw new Error("CategoricalTensorVar += value " + value + " not found in domain " + domain)
    } else {
      if (update) tensor.update(i, v)
      else tensor.+=(i, v)
    }
  }
  def update(elt:C, newValue:Double): Unit = doWithIndexSafely(elt, newValue, true)
  def +=(elt:C, incr:Double): Unit = doWithIndexSafely(elt, incr, false)
  def +=(elt:C): Unit = +=(elt, 1.0)
  def ++=(elts:Iterable[C]): Unit = elts.foreach(this.+=(_))
  @deprecated("This method may be removed.") def zero(): Unit = tensor.zero()
  def activeCategories: Seq[C] = tensor.activeDomain.toSeq.map(i => domain.dimensionDomain.category(i))
}
abstract class CategoricalTensorVariable[A<:Tensor,C] extends DiscreteTensorVariable[A] with CategoricalTensorVar[A,C] {
  def this(initialValue:A) = { this(); _set(initialValue) }
}

// TODO rename BinaryFeatureVectorVariable
abstract class BinaryFeatureVectorVariable2[C] extends CategoricalTensorVariable[GrowableSparseBinaryTensor1,C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
  _set(new GrowableSparseBinaryTensor1(domain.dimensionDomain))
}

// TODO rename FeatureVectorVariable
abstract class FeatureVectorVariable2[C] extends CategoricalTensorVariable[GrowableSparseTensor1,C] {
  def this(initVals:Iterable[C]) = { this(); this.++=(initVals) }
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


/** A single discrete variable */
trait DiscreteVar2 extends DiscreteTensorVar[DiscreteValue2] with VarAndValueType[DiscreteVar2,DiscreteValue2] {
  def domain: DiscreteDomain2
  def intValue = value.intValue
  override def toString = printName+"("+intValue+")"
}

/** A single discrete variable whose value can be changed. */
trait MutableDiscreteVar2 extends DiscreteVar2 with MutableVar {
  def set(newValue:Value)(implicit d:DiffList): Unit
  def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.apply(newInt)/*.asInstanceOf[ValueType]*/)(d)
  @inline final def :=(i:Int): Unit = set(i)(null)
  def setRandomly(random:Random = cc.factorie.random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
}


/** A concrete single discrete variable whose value can be changed. */
abstract class DiscreteVariable2 extends MutableDiscreteVar2 with IterableSettings {
  def this(initialValue:Int) = { this(); __value = initialValue }
  def this(initialValue:DiscreteValue) = { this(); require(initialValue.domain == domain); _set(initialValue.intValue) }
  private var __value: Int = 0
  protected def _value = __value
  protected def _set(newValue:Int): Unit = __value = newValue
  //final protected def _set(newValue:ValueType): Unit = _set(newValue.intValue)
  override def intValue = __value
  def value: Value = domain.apply(__value)
  @inline final def set(newValue:ValueType)(implicit d:DiffList): Unit = set(newValue.intValue)(d)
  override def set(newValue:Int)(implicit d:DiffList): Unit = if (newValue != __value) {
    assert(newValue < domain.size)
    if (d ne null) d += new DiscreteVariableDiff(__value, newValue)
    __value = newValue
  }
  def settings = new SettingIterator {
    // TODO Base this on a domain.iterator instead, for efficiency
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable: DiscreteVariable2.this.type = DiscreteVariable2.this
  }
  /** Return the distribution over values of this variable given the model and given that all other variables' values are fixed. */
  def proportions(model:Model): Proportions = {
    val origIntValue = intValue
    val l = domain.size 
    val distribution = new DenseTensor1(l)
    var i = 0
    while (i < l) {
      //model.factors(Seq(this)).sumBy(_.values.set(this, i).score) // a version that doesn't change the value of this variable
      __value = i
      distribution(i) = model.score(this)  // compute score of variable with value 'i'
      i += 1
    }
    distribution.expNormalize()
    __value = origIntValue
    new DenseProportions1(distribution)
  }

  case class DiscreteVariableDiff(oldValue: Int, newValue: Int) extends Diff {
    @inline final def variable: DiscreteVariable2 = DiscreteVariable2.this
    @inline final def redo = DiscreteVariable2.this.set(newValue)(null)
    @inline final def undo = DiscreteVariable2.this.set(oldValue)(null)
    override def toString = variable match { 
      case cv:CategoricalVar2[_] if (oldValue >= 0) => "DiscreteVariableDiff("+cv.domain.category(oldValue)+"="+oldValue+","+cv.domain.category(newValue)+"="+newValue+")"
      case _ => "DiscreteVariableDiff("+oldValue+","+newValue+")"
    }
  }
}


/** A DiscreteVar whose integers 0...N are associated with an categorical objects of type A.
    Concrete implementations include CategoricalVariable and CategoricalObservation. 
    @author Andrew McCallum */
trait CategoricalVar2[A] extends DiscreteVar2 with CategoricalTensorVar[CategoricalValue2[A],A] with VarAndValueType[CategoricalVar2[A],CategoricalValue2[A]] {
  def domain: CategoricalDomain2[A]
  def categoryValue: A = if (value ne null) value.category else null.asInstanceOf[A]
  override def toString = printName + "(" + (if (categoryValue == null) "null" else if (categoryValue == this) "this" else categoryValue.toString) + "=" + intValue + ")" // TODO Consider dropping the "=23" at the end.
}

trait MutableCategoricalVar2[A] extends CategoricalVar2[A] with MutableDiscreteVar2 {
  def setCategory(newCategory:A)(implicit d: DiffList): Unit = set(domain.index(newCategory))(d)
}

/** A DiscreteVariable whose integers 0...N are associated with an object of type A. 
    @author Andrew McCallum */
abstract class CategoricalVariable2[A] extends DiscreteVariable2 with MutableCategoricalVar2[A] {
  def this(initialCategory:A) = { this(); _set(domain.index(initialCategory)) }
  //def this(initalValue:ValueType) = { this(); _set(initialValue) }
}





// Because DiscreteDomain2 is an IndexedSeq it can be passed as a sizeProxy
class DiscreteDomain2(sizeProxy:Iterable[Any]) extends IndexedSeq[DiscreteValue2] with DiscreteTensorDomain[DiscreteValue2] /*with ValueType[DiscreteValue2]*/ {
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
  def category(i:Int): C = _elements(i).category.asInstanceOf[C]
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

