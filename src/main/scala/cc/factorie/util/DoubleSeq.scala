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

package cc.factorie.util
import scala.util.Random

/** We are so desperate for efficient @specialized Seq[Double], that we created our own. 
    This could inherit from IndexedSeq[Double] but we would pass on significant risk of inefficiencies hidden to the user. */
trait DoubleSeq {
  def apply(i:Int): Double
  def length: Int
  final def size = length // Just an alias // Not final so we can mix Tensor with FlatHashMap in WeightsTensor
  def foreach(f:(Double)=>Unit): Unit = { val l = length; var i = 0; while (i < l) { f(apply(i)); i += 1 } }
  def foreachElement(f:(Int,Double)=>Unit): Unit = { val l = length; var i = 0; while (i < l) { f(i, apply(i)); i += 1 } }
  def foreachActiveElement(f:(Int,Double)=>Unit): Unit
  def forallElements(f:(Int,Double)=>Boolean): Boolean = { val l = length; var i = 0; while (i < l) { if (!f(i, apply(i))) return false; i += 1 }; true }
  def contains(d:Double): Boolean = { val l = length; var i = 0; while (i < l) { if (d == apply(i)) return true; i += 1 }; false }
  def forall(f:Double=>Boolean): Boolean = { val l = length; var i = 0; while (i < l) { if (!f(apply(i))) { println("DoubleSeq.forall "+apply(i)); return false }; i += 1 }; true }
  def foldLeft[B](z:B)(f:(B,Double)=>B): B = { var acc = z; foreach(el => acc = f(acc, el)); acc }
  def map(f:(Double)=>Double): DoubleSeq = { val l = length; val a = new Array[Double](l); var i = 0; while (i < l) { a(i) = f(apply(i)); i += 1 }; new ArrayDoubleSeq(a) }
  final def =+(a:Array[Double]): Unit = =+(a, 0, 1.0)
  final def =+(a:Array[Double], offset:Int): Unit = =+(a, offset, 1.0)
  final def =+(a:Array[Double], f:Double): Unit = =+(a, 0, f)
  /** Increment given array (starting at offset index) with contents of this DoubleSeq, multiplied by factor f. */
  def =+(a:Array[Double], offset:Int, f:Double): Unit = { val len = length+offset; var i = offset; while (i < len) { a(i) += f * apply(i); i += 1 } }
  def indexOf(d:Double): Int = { val l = length; var i = 0; while (i < l) { if (d == apply(i)) return i; i += 1 }; -1 }
  def max: Double = { var m = Double.NaN; val l = length; var i = 0; while (i < l) { if (!(m >= apply(i))) m = apply(i); i += 1 }; m }
  def min: Double = { var m = Double.NaN; val l = length; var i = 0; while (i < l) { if (!(m <= apply(i))) m = apply(i); i += 1 }; m }
  def sum: Double = { var s = 0.0; val l = length; var i = 0; while (i < l) { s += apply(i); i += 1 }; s }
  // Don't do this here because in sparse WeightsMap missing entries would be treated as NegativeInfinity, but are treated in all other operations as 0.0
  // Sum of two doubles expressed in log space 
  //def sumLog: Double = { var s = 0.0; val l = length; var i = 0; while (i < l) { s = cc.factorie.maths.sumLogProb(s, apply(i)); i += 1 }; s }
  def oneNorm: Double = { val l = length; var result = 0.0; var i = 0; while (i < l) { result += math.abs(apply(i)); i += 1}; result }
  //def absNorm: Double = { var result = 0.0; var i = 0; while (i < length) { result += math.abs(apply(i)); i += 1}; result }
  def twoNormSquared: Double = { val l = length; var result = 0.0; var i = 0; var v = 0.0; while (i < l) { v = apply(i); result += v * v; i += 1}; result }
  final def twoNorm: Double = math.sqrt(twoNormSquared)
  final def infinityNorm = { var m = Double.NaN; val l = length; var i = 0; while (i < l) { val cur = math.abs(apply(i)); if (!(m >= cur)) m = cur; i += 1 }; m }
  def l2Similarity(t:DoubleSeq): Double = {
    // TODO This will be inefficient for sparse this or t
    var sum = 0.0; var i = 0; var diff = 0.0; val len = length; assert(len == t.length)
    while (i < len) {
      diff = apply(i) - t(i)
      sum += diff * diff
      i += 1
    }
    math.sqrt(sum)
  }
  def different(t:DoubleSeq, threshold:Double): Boolean = { require(length == t.length); val l = length; var i = 0; while (i < l) { if (math.abs(apply(i) - t(i)) > threshold) return true; i += 1}; false } // TODO Inefficient when sparse
  def maxIndex: Int = { val l = length; var i = 1; var j = 0; while (i < l) { if (apply(j) < apply(i)) j = i; i += 1 }; j }
  def maxIndex2: (Int, Int) = {
    val l = length; var i = 1
    var max1 = 0; var max2 = 0
    while (i < l) {
      if (apply(max1) < apply(i)) { max2 = max1; max1 = i }
      else if (apply(max2) < apply(i)) max2 = i
      i += 1
    }
    (max1, max2)
  }
  def containsNaN: Boolean = { val l = length; var i = 0; while (i < l) { if (apply(i) != apply(i)) return true; i += 1 }; false }  // TODO Why wouldn't apply(i).isNaN compile?
  /** Return records for the n elements with the largest values. */
  def top(n:Int): TopN[String] = new cc.factorie.util.TopN(n, this)
  // For proportions; implemented here so that different Tensor subclasses can override it for efficiency
  def sampleIndex(normalizer:Double)(implicit r:Random): Int = {
    assert(normalizer > 0.0, "normalizer = "+normalizer)
    val l = length; var b = 0.0; val s = r.nextDouble * normalizer; var i = 0
    while (b <= s && i < l) { assert(apply(i) >= 0.0); b += apply(i); i += 1 }
    assert(i > 0)
    i - 1
  }
  /** Careful, for many subclasses this is inefficient because it calls the method "sum" to get the normalizer. */ // TODO Should this method be removed for this reason?
  def sampleIndex(implicit r:Random): Int = sampleIndex(sum)(r)
  /** Assumes that the values are already normalized to sum to 1. */
  def entropy: Double = {
    var result = 0.0
    var sum = 0.0; val l = length; var i = 0; var pv = 0.0
    while (i < l) {
      pv = apply(i)
      sum += pv
      require(pv >= 0.0, pv)
      require(pv <= 1.000001)
      if (pv > 0.0)
        result -= pv * math.log(pv)
      i += 1
    }
    assert(sum > 0.9999 && sum < 1.0001, sum)
    result / cc.factorie.maths.log2
  }
  /** Assumes that the values in both DoubleSeq are already normalized to sum to 1. */
  def klDivergence(p:DoubleSeq): Double = {
    assert(length == p.length)
    var klDiv = 0.0
    var sum1 = 0.0; var sum2 = 0.0
    val l = length; var i = 0; var p1 = 0.0; var p2 = 0.0
    while (i < l) {
      p1 = apply(i)
      p2 = p(i)
      sum1 += p1
      sum2 += p2
      if (p1 != 0.0)
        klDiv += p1 * math.log(p1 / p2)
      i += 1
    }
    assert(sum1 > 0.9999 && sum1 < 1.0001)
    assert(sum2 > 0.9999 && sum2 < 1.0001)
    klDiv / cc.factorie.maths.log2
  }
  /** Assumes that the values are already normalized to sum to 1. */
  def jsDivergence(p:DoubleSeq): Double = {
    assert(length == p.length)
    val average = DoubleSeq(length)
    val l = length; var i = 0
    while (i < l) { average(i) += (apply(i) + p(i)) / 2.0; i += 1 }
    (this.klDivergence(average) + p.klDivergence(average)) / 2.0
  }
  //def pr(i:Int, normalizer:Double): Double = apply(i) / normalizer
  
  /** Return the values as an Array[Double].  Guaranteed to be a copy, not just a pointer to an internal array that would change with changes to the DoubleSeq */
  def toArray: Array[Double] = { val a = new Array[Double](length); var i = 0; while (i < length) { a(i) = apply(i); i += 1 }; a }
  def asArray: Array[Double] = toArray // Can be overridden for further efficiency
  /** With uncopied contents */
  def asSeq: Seq[Double] = new IndexedSeq[Double] {
    def length = DoubleSeq.this.length
    def apply(i:Int): Double = DoubleSeq.this.apply(i)
  }
  /** With copied contents */
  def toSeq: Seq[Double] = new ArrayIndexedSeqDouble(toArray)
  /** Append a string representation of this DoubleSeq to the StringBuilder. */
  def addString(b:StringBuilder, start:String, sep:String, end:String): StringBuilder = {
    var first = true
    b.append(start)
    val len = length; var i = 0
    while (i < len) {
      if (first) { b.append(apply(i)); first = false } else { b.append(sep); b.append(apply(i)) }
      i += 1
    }
    b.append(end); b
  }
  def mkString(start:String, sep:String, end:String): String =  addString(new StringBuilder(), start, sep, end).toString
  def mkString(sep:String): String = mkString("", sep, "")
  def mkString: String = mkString("")
}

/** Used to iterate over the contents of a DoubleSeq, with access to both the index and the value,
    but, unlike an Iterator[(Int,Double)], not needing to allocate memory for each iteration. 
    Typical usage:  for (e <- myDoubleSeq.elements) println("index="+e.index+" value="+e.value) */
// TODO Put this into usage, replacing foreachActiveElement.  -akm
trait DoubleSeqIterator extends Iterator[DoubleSeqIterator] {
  def index: Int
  def value: Double
}


/** An IndexedSeq[Double] backed by an array, just used as a return type for DoubleSeq.toSeq. */
class ArrayIndexedSeqDouble(val array:Array[Double]) extends IndexedSeq[Double] {
  def length: Int = array.length
  def apply(i:Int): Double = array(i)
}

object DoubleSeq {
  def apply(seqDouble:Seq[Double]): DoubleSeq = new DoubleSeq {
    def foreachActiveElement(f: (Int, Double) => Unit) { foreachElement(f) }
    private val sd = seqDouble
    def length = sd.length
    def apply(i:Int) = sd(i)
  }
  def apply(array:Array[Double]): MutableDoubleSeq = new MutableDoubleSeq {
    def foreachActiveElement(f: (Int, Double) => Unit) { foreachElement(f) }
    private val a = array
    def length = a.length
    def apply(i:Int) = a(i)
    def +=(i:Int, v:Double) = a(i) += v
    def zero(): Unit = java.util.Arrays.fill(a, 0.0)
    def update(i:Int, v:Double) = a(i) = v
  }
  def apply(contents:Double*): ArrayDoubleSeq = new ArrayDoubleSeq(contents.toArray) 
  def apply(len:Int): MutableDoubleSeq = apply(new Array[Double](len))
}


// TODO Find out if it is faster to use "foreachActiveElement" or "activeDomain...while" 
trait SparseDoubleSeq extends DoubleSeq {
  def activeDomain: IntSeq
  def activeDomainSize: Int
  // TODO Remove this when DoubleSeqIterator is put in place. -akm
  // We are relying on "f" getting properly @specialized
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  // TODO Should we also override def map(f:(Double)=>Double): DoubleSeq  ???
  //override def max:  Double = { var m = Double.NaN; val d = activeDomain; val l = d.length; var i = 0; while (i < l) { val v = apply(d(i)); if (!(m >= v)) m = v; i += 1 }; m }
  override def max: Double = { var m = Double.NaN; foreachActiveElement((i,v) => { if (!(m >= v)) m = v }); m }
  override def min: Double = { var m = Double.NaN; foreachActiveElement((i,v) => { if (!(m <= v)) m = v }); m }
  override def sum: Double = { var s = 0.0; foreachActiveElement((i,v) => { s += v }); s }
  override def oneNorm: Double = { var s = 0.0; foreachActiveElement((i,v) => { s += math.abs(v) }); s }
  override def twoNormSquared: Double = { var s = 0.0; foreachActiveElement((i,v) => { s += v * v }); s }
  override def contains(d:Double): Boolean = { foreachActiveElement((i,v) => if (d == v) return true); false }
  override def different(t:DoubleSeq, threshold:Double): Boolean = t match {
    case t:SparseDoubleSeq => super.different(t, threshold) // TODO Do something clever here
    case t:DoubleSeq => super.different(t, threshold)
  }
  override def maxIndex: Int = { var j = 0; var m = Double.NaN; foreachActiveElement((i,v) => { if (!(m >= v)) { m = v; j = i } }); j }
  override def containsNaN: Boolean = { foreachActiveElement((i,v) => if (v != v) return true); false } 
  override def sampleIndex(normalizer:Double)(implicit r:Random): Int = {
    assert(normalizer > 0.0, "normalizer = "+normalizer)
    var b = 0.0; val s = r.nextDouble * normalizer
    foreachActiveElement((i,v) => { assert(v >= 0.0); if (b > s) return i - 1; b += v })
    length - 1
  }
  /** Assumes that the values are already normalized to sum to 1. */
  override def entropy: Double = {
    var result = 0.0; var sum = 0.0
    foreachActiveElement((i,v) => { sum += v; require(v >= 0.0, v); require(v <= 1.000001); if (v > 0.0) result -= v * math.log(v) })
    assert(sum > 0.9999 && sum < 1.0001)
    result / cc.factorie.maths.log2
  }
  /** Assumes that the values in both DoubleSeq are already normalized to sum to 1. */
  override def klDivergence(p:DoubleSeq): Double = {
    assert(length == p.length)
    var klDiv = 0.0
    var sum1 = 0.0
    var p2 = 0.0
    foreachActiveElement((i,p1) => { p2 = p(i); sum1 += p1; if (p1 != 0.0) klDiv += p1 * math.log(p1 / p2) })
    assert(sum1 > 0.9999 && sum1 < 1.0001)
    klDiv / cc.factorie.maths.log2
  }
  /** Assumes that the values are already normalized to sum to 1. */
  override def jsDivergence(p:DoubleSeq): Double = {
    // TODO We should make this efficient for sparsity
    assert(length == p.length)
    val average = DoubleSeq(length)
    val l = length; var i = 0
    while (i < l) { average(i) += (apply(i) + p(i)) / 2.0; i += 1 }
    (this.klDivergence(average) + p.klDivergence(average)) / 2.0
  }

}

trait IncrementableDoubleSeq extends DoubleSeq {
  def +=(i:Int, incr:Double): Unit
  def zero(): Unit
  def +=(d:Double): Unit = { val l = length; var i = 0; while (i < l) { +=(i, d); i += 1 }}
  final def +=(ds:DoubleSeq): Unit = this +=(ds, 1.0) 
  /*ds match {
    case ds:SparseDoubleSeq => { ds.foreachActiveElement((i,v) => +=(i,v)) }
    case ds:DoubleSeq => { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, ds(i)); i += 1 }}
  }*/
  def +=(a:Array[Double]): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { +=(i, a(i)); i += 1 }}
  def +=(ds:DoubleSeq, factor:Double): Unit = ds match {
    case ds:SparseDoubleSeq => { ds.foreachActiveElement((i,v) => +=(i,v*factor)) }
    case ds:DoubleSeq => { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, factor*ds(i)); i += 1 }} 
  }
  def +=(a:Array[Double], factor:Double): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { +=(i, factor*a(i)); i += 1 }}
  /** Increment by the element-wise product of ds and factor. */
  def +=(ds:DoubleSeq, factor:DoubleSeq): Unit = ds match {
    case ds:SparseDoubleSeq => { ds.foreachActiveElement((i,v) => +=(i,v*factor(i))) }
    case ds:DoubleSeq => { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, factor(i)*ds(i)); i += 1 }} 
  }
//    /** Increment by the element-wise product of ds, factor. */
//  def +=(ds:DoubleSeq, factor:DoubleSeq,scalar:Double): Unit = ds match {
//    case ds:SparseDoubleSeq => { ds.foreachActiveElement((i,v) => +=(i,v*factor(i)*scalar)) }
//    case ds:DoubleSeq => { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, factor(i)*ds(i)*scalar); i += 1 }}
//  }
  def -=(i:Int, incr:Double): Unit = +=(i, -incr)
  final def -=(d:Double): Unit = +=(-d)
  def -=(ds:DoubleSeq): Unit = +=(ds, -1.0)
}

trait MutableDoubleSeq extends IncrementableDoubleSeq {
  def update(i:Int, v:Double): Unit
  // Although the next two methods could be implemented here, they are usually implemented in a superclass that inherits from IncrementableDoubleSeq
  def +=(i:Int, incr:Double): Unit // = update(i, apply(i)+incr)
  def zero(): Unit // = this := 0.0 //{ var i = 0; while (i < length) { update(i, 0.0); i += 1 }}
  // Concrete methods, efficient for dense representations
  def substitute(oldValue:Double, newValue:Double): Unit = { var i = 0; while (i < length) { if (apply(i) == oldValue) update(i, newValue); i += 1 } }
  def :=(d:Double): Unit = { val l = length; var i = 0; while (i < l) { update(i, d); i += 1 }}
  def :=(ds:DoubleSeq): Unit = ds match {
    case ds:SparseDoubleSeq => { zero(); /* Use defaultValue instead? */ ds.foreachActiveElement((i,v) => update(i, v)) }
    case ds:DoubleSeq => { val l = length; require(ds.length == l); var i = 0; while (i < l) { update(i, ds(i)); i += 1 }}
  }
  def :=(a:Array[Double]): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { update(i, a(i)); i += 1 }}
  def :=(a:Array[Double], offset:Int): Unit = { val len = length; var i = 0; while (i < len) { update(i, a(i+offset)); i += 1 }} 
  def *=(i:Int, incr:Double): Unit = update(i, apply(i)*incr)
  final def /=(i:Int, incr:Double): Unit = *=(i, 1.0/incr)
  def *=(d:Double): Unit = { val l = length; var i = 0; while (i < l) { *=(i, d); i += 1 }}
  final def /=(d:Double): Unit = *=(1.0/d)
  def *=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { *=(i, ds(i)); i += 1 }}
  def /=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { /=(i, ds(i)); i += 1 }}
  def abs: Unit = { val l = length; var i = 0; while (i < l) { val d = apply(i); if (d < 0.0) update(i, math.abs(d)); i += 1 }}
  def normalize(): Double = { val n = oneNorm; /=(n); n }
  def oneNormalize(): Double = normalize()
  def twoNormalize(): Double = { val n = twoNorm; /=(n); n }
  def twoSquaredNormalize(): Double = { val n = twoNormSquared; /=(n); n }
  //def absNormalize(): Double = { val n = absNorm; /=(n); n }
  /** Exponentiate the elements of the array, and then normalize them to sum to one. */
  def expNormalize(): Double = {
    var max = Double.MinValue
    var i = 0; val l = length
    while (i < l) { if (max < apply(i)) max = apply(i); i += 1 }
    var sum = 0.0
    i = 0
    while (i < l) {
      update(i, math.exp(apply(i) - max))
      sum += apply(i)
      i += 1
    }
    this /= sum
    sum
    //i = 0; while (i < l) { apply(i) /= sum; i += 1 }; sum
  }
  def exponentiate(): Unit = {
    var i = 0; val l = length
    while (i < l) {
      update(i, math.exp(apply(i)))
      i += 1
    }
  }
  // Finds the maximum element of the array and sets it to 1, while setting all others to zero
  def maxNormalize() {
    var max = Double.MinValue
    var maxi = 0
    var i = 0
    val l = length
    while (i < l) {
      if (apply(i) > max) {
        max = apply(i)
        maxi = i
      }
      update(i, 0)
      i += 1
    }
    update(maxi, 1)
  }
  /** Exponential the elements of the array such that they are normalized to sum to one,
      but do so efficiently by providing logZ.  Note that to maximize efficiency, this method
      does not verify that the logZ value was the correct one to cause proper normalization. */
  def expNormalize(logZ:Double): Unit = {
    val l = length; var i = 0
    while (i < l) {
      update(i, math.exp(apply(i) - logZ))
      i += 1
    }
  }
  /** expNormalize, then put back into log-space. */
  def normalizeLogProb(): Double = {
    // normalizeLogProb: [log(a), log(b), log(c)] --> [log(a/Z), log(b/Z), log(c/Z)] where Z = a+b+c
    // expNormalize: [log(a), log(b), log(c)] --> [a/Z, b/Z, c/Z] where Z=a+b+c
    val n = expNormalize()
    var i = 0; val l = length; while (i < l) { update(i, math.log(apply(i))); i += 1 }
    n
  }
}

// Some simple concrete classes

// TODO For Scala 2.10 make this implicit final class ArrayDoubleSeq
final class ArrayDoubleSeq(override val asArray:Array[Double]) extends MutableDoubleSeq {
  def foreachActiveElement(f: (Int, Double) => Unit) { foreachElement(f) }
  def this(contents:Double*) = this(contents.toArray)
  def length: Int = asArray.length
  def apply(i:Int) = asArray(i)
  def update(i:Int, v:Double): Unit = asArray(i) = v
  def zero(): Unit = java.util.Arrays.fill(asArray, 0.0)
  def +=(i:Int, v:Double): Unit = asArray(i) += v
}

final class TruncatedArrayDoubleSeq(val array:Array[Double], val length:Int) extends DoubleSeq {
  def foreachActiveElement(f: (Int, Double) => Unit) { foreachElement(f) }
  def apply(i:Int): Double = array(i)
  override def toArray = { val a = new Array[Double](length); System.arraycopy(array, 0, a, 0, length); a }
}

final class SubArrayDoubleSeq(val array:Array[Double], val start:Int, val length:Int) extends DoubleSeq {
  def foreachActiveElement(f: (Int, Double) => Unit) { foreachElement(f) }
  def apply(i:Int): Double = array(i+start)
  override def toArray = { val a = new Array[Double](length); System.arraycopy(array, start, a, 0, length); a }
}
