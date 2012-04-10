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
// TODO Consider if we should move activeDomain from Tensor to here, so that there could be more efficient implementations centralized here.
//  But my current thinking is not to do this because many methods should have even more highly specialized implementations anyway. -akm
trait DoubleSeq {
  def apply(i:Int): Double
  def length: Int
  final def size = length // Just an alias
  def foreach(f:(Double)=>Unit): Unit = { val l = length; var i = 0; while (i < l) { f(apply(i)); i += 1 } }
  def forElements(f:(Int,Double)=>Unit): Unit = { val l = length; var i = 0; while (i < l) { f(i, apply(i)); i += 1 } }
  def forallElements(f:(Int,Double)=>Boolean): Boolean = { val l = length; var i = 0; while (i < l) { if (!f(i, apply(i))) return false; i += 1 }; return true }
  def contains(d:Double): Boolean = { val l = length; var i = 0; while (i < l) { if (d == apply(i)) return true; i += 1 }; false }
  def forall(f:Double=>Boolean): Boolean = { val l = length; var i = 0; while (i < l) { if (!f(apply(i))) return false; i += 1 }; true }
  def foldLeft[B<:AnyRef](z:B)(f:(B,Double)=>B): B = throw new Error("Not yet implemented.")
  def indexOf(d:Double): Int = { val l = length; var i = 0; while (i < l) { if (d == apply(i)) return i; i += 1 }; -1 }
  def max: Double = { var m = Double.NaN; val l = length; var i = 0; while (i < l) { if (!(m >= apply(i))) m = apply(i); i += 1 }; m }
  def min: Double = { var m = Double.NaN; val l = length; var i = 0; while (i < l) { if (!(m <= apply(i))) m = apply(i); i += 1 }; m }
  def sum: Double = { var s = 0.0; val l = length; var i = 0; while (i < l) { s += apply(i); i += 1 }; s }
  def oneNorm: Double = { val l = length; var result = 0.0; var i = 0; while (i < l) { result += math.abs(apply(i)); i += 1}; result }
  //def absNorm: Double = { var result = 0.0; var i = 0; while (i < length) { result += math.abs(apply(i)); i += 1}; result }
  def twoNormSquared: Double = { val l = length; var result = 0.0; var i = 0; var v = 0.0; while (i < l) { v = apply(i); result += v * v; i += 1}; result }
  def twoNorm: Double = math.sqrt(twoNormSquared)
  final def infinityNorm = max
  def cosineSimilarity(t:DoubleSeq): Double = {
    val numerator:Double = this dot t
    val denominator:Double = this.twoNorm * t.twoNorm
    if (denominator == 0.0 || denominator != denominator) 0.0 else numerator/denominator
  }
  def different(t:DoubleSeq, threshold:Double): Boolean = { require(length == t.length); val l = length; var i = 0; while (i < l) { if (math.abs(apply(i) - t(i)) > threshold) return true; i += 1}; return false }
  def dot(t:DoubleSeq): Double = { assert(length == t.length); val l = length; var result = 0.0; var i = 0; while (i < l) { result += apply(i) * t(i); i += 1 }; result }
  def maxIndex: Int = { val l = length; var i = 1; var j = 0; while (i < l) { if (apply(j) < apply(i)) j = i; i += 1 }; j }
  def containsNaN: Boolean = { val l = length; var i = 0; while (i < l) { if (apply(i) != apply(i)) return true; i += 1 }; false }  // TODO Why wouldn't apply(i).isNaN compile?
  // For proportions
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
    var sum = 0.0
    val l = length
    var i = l - 1
    var pv = 0.0
    while (i < l) {
      pv = apply(i)
      sum += pv
      require(pv >= 0.0, pv)
      require(pv <= 1.000001)
      if (pv > 0.0)
        result -= pv * math.log(pv)
      i += 1
    }
    assert(sum > 0.9999 && sum < 1.0001)
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
    }
    assert(sum1 > 0.9999 && sum1 < 1.0001)
    assert(sum2 > 0.9999 && sum2 < 1.0001)
    klDiv / cc.factorie.maths.log2
  }
  /** Assumes that the values are already normalized to sum to 1. */
  def jsDivergence(p:DoubleSeq): Double = {
    assert(length == p.length);
    val average = DoubleSeq(length)
    var i = 0
    while (i < length) average(i) += (apply(i) + p(i)) / 2.0
    (this.klDivergence(average) + p.klDivergence(average)) / 2.0
  }
  //def pr(i:Int, normalizer:Double): Double = apply(i) / normalizer
  
  // def copy: DoubleSeq = DoubleSeq(this.toArray) // TODO We really should make a version of this that uses CanBuildFrom[] tricks to preserve return type
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
}

class ArrayIndexedSeqDouble(val array:Array[Double]) extends IndexedSeq[Double] {
  def length = array.length
  def apply(i:Int) = array(i)
}

object DoubleSeq {
  def apply(seqDouble:Seq[Double]): DoubleSeq = new DoubleSeq {
    private val sd = seqDouble
    def length = sd.length
    def apply(i:Int) = sd(i)
  }
  def apply(array:Array[Double]): MutableDoubleSeq = new MutableDoubleSeq {
    private val a = array
    def length = a.length
    def apply(i:Int) = a(i)
    def +=(i:Int, v:Double) = a(i) += v
    def zero(): Unit = java.util.Arrays.fill(a, 0.0)
    def update(i:Int, v:Double) = a(i) = v
  }
  def apply(len:Int): MutableDoubleSeq = apply(new Array[Double](len))
}

trait IncrementableDoubleSeq extends DoubleSeq {
  def +=(i:Int, incr:Double): Unit
  def zero(): Unit
  def +=(d:Double): Unit = { val l = length; var i = 0; while (i < l) { +=(i, d); i += 1 }}
  def +=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, ds(i)); i += 1 }}
  def +=(a:Array[Double]): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { +=(i, a(i)); i += 1 }}
  def +=(ds:DoubleSeq, factor:Double): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { +=(i, factor*ds(i)); i += 1 }}
  def +=(a:Array[Double], factor:Double): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { +=(i, factor*a(i)); i += 1 }}
  def -=(i:Int, incr:Double): Unit = +=(i, -incr)
  def -=(d:Double): Unit = +=(-d)
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
  def :=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { update(i, ds(i)); i += 1 }}
  def :=(a:Array[Double]): Unit = { val l = length; require(a.length == l); var i = 0; while (i < l) { update(i, a(i)); i += 1 }}
  def *=(i:Int, incr:Double): Unit = update(i, apply(i)*incr)
  final def /=(i:Int, incr:Double): Unit = *=(i, 1.0/incr)
  def *=(d:Double): Unit = { val l = length; var i = 0; while (i < l) { *=(i, d); i += 1 }}
  final def /=(d:Double): Unit = *=(1.0/d)
  def *=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { *=(i, ds(i)); i += 1 }}
  def /=(ds:DoubleSeq): Unit = { val l = length; require(ds.length == l); var i = 0; while (i < l) { /=(i, ds(i)); i += 1 }}
  def normalize(): Double = { val n = oneNorm; /=(n); n }
  def oneNormalize(): Double = normalize
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
    i = 0
    while (i < l) { apply(i) /= sum; i += 1 }
    sum
  }
  /** expNormalize, then put back into log-space. */
  def normalizeLogProb(): Double = {
    // normalizeLogProb: [log(a), log(b), log(c)] --> [log(a/Z), log(b/Z), log(c/Z)] where Z = a+b+c
    // expNormalize: [log(a), log(b), log(c)] --> [a/Z, b/Z, c/Z] where Z=a+b+c
    val n = expNormalize
    var i = 0; val l = length; while (i < l) { update(i, math.log(apply(i))); i += 1 }
    n
  }
}

