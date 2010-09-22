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

package cc.factorie.maths
import cc.factorie._

trait ArrayOps {
  type A = Array[Double]
  def absNorm(s:A): Double = { var result = 0.0; forIndex(s.length)(i => result += math.abs(s(i))); result }
  def oneNorm(s:A): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i)); result }
  def twoNorm(s:A): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i) * s(i)); math.sqrt(result) }
  def twoNormSquared(s:A): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i) * s(i)); result }
  def infinityNorm(s:A): Double = { var result = s(0); forIndex(s.length)(i => if (math.abs(s(i)) > result) result = math.abs(s(i))); result }
  def +=(s:A, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) + d)
  def -=(s:A, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) - d)
  def *=(s:A, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) * d)
  def /=(s:A, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) / d)
  def incr(s:A, t:A): Unit = { require(s.length == t.length); forIndex(s.length)(i => s(i) += t(i)) }
  def incr(s:A, t:A, factor:Double): Unit = { require(s.length == t.length); forIndex(s.length)(i => s(i) += t(i) * factor) }
  def different(s:A, t:A, threshold:Double): Boolean = { require(s.length == t.length); forIndex(s.length)(i => if (math.abs(s(i) - t(i)) > threshold) return true); return false }
  def dot(s:A, t:A): Double = { assert(s.length == t.length); var result = 0.0; forIndex(s.length)(i => result += s(i) * t(i)); result }
  /** Divide each element of the array by the sum of the elements. */
  def normalize(s:A): Double = { val sum = oneNorm(s); forIndex(s.length)(i => s(i) /= sum); sum }
  def oneNormalize(s:A): Double = normalize(s)
  def twoNormalize(s:A): Double = { val norm = twoNorm(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def twoSquaredNormalize(s:A): Double = { val norm = twoNormSquared(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def absNormalize(s:A): Double = { val norm = absNorm(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def contains(s:A, d:Double): Boolean = { forIndex(s.length)(i => if (s(i) == d) return true); false }
  def maxIndex(a:Array[Double]): Int = { var i = 0; var j = 0; for (i <- 0 until a.length) if (a(j) < a(i)) j = i; j }
  def isNaN(s:A): Boolean = contains(s, Double.NaN)
  def substitute(s:A, oldValue:Double, newValue:Double): Unit = forIndex(s.length)(i => if (s(i) == oldValue) s(i) = newValue)
  def copy(s:A): Array[Double] = { val result = new Array[Double](s.length); set(result, s); result }
  def set(s:A, t:A): Unit = {
    require(s.length == t.length)
    forIndex(t.length)(i => s(i) = t(i))
  }
  /** Exponentiate the elements of the array, and then normalize them to sum to one. */
  def expNormalize(a:Array[Double]): Double = {
    var max = Double.MinValue
    for (i <- 0 until a.length) if (max < a(i)) max = a(i)
    var sum = 0.0
    for (i <- 0 until a.length) {
      a(i) = math.exp(a(i) - max)
      sum += a(i)
    }
    for (i <- 0 until a.length) a(i) /= sum
    sum
  }

  /** expNormalize, then put back into log-space. */
  def normalizeLogProb(a:Array[Double]): Double = {
    // normalizeLogProb: [log(a), log(b), log(c)] --> [log(a/Z), log(b/Z), log(c/Z)] where Z = a+b+c
    // expNormalize: [log(a), log(b), log(c)] --> [a/Z, b/Z, c/Z] where Z=a+b+c
    val n = expNormalize(a)
    for (i <- 0 until a.length) a(i) = math.log(a(i))
    n
  }

}

object ArrayOps extends ArrayOps

trait ArrayImplicits {

  implicit def array2ArrayOps(s:Array[Double]) = new {
    //def length = s.length
    //def apply(i:Int) = s(i)
    //def update(i:Int, d:Double): Unit = s(i) = d
    def oneNorm: Double = ArrayOps.oneNorm(s)
    def twoNorm: Double = ArrayOps.twoNorm(s)
    def twoNormSquared: Double = ArrayOps.twoNormSquared(s)
    def infinityNorm: Double = ArrayOps.infinityNorm(s)
    def +=(d:Double): Unit = ArrayOps.+=(s, d)
    def -=(d:Double): Unit = ArrayOps.-=(s, d)
    def *=(d:Double): Unit = ArrayOps.*=(s, d)
    def /=(d:Double): Unit = ArrayOps./=(s, d)
    def incr(t:A, factor:Double): Unit = ArrayOps.incr(s, t, factor)
    def different(t:A, threshold:Double): Boolean = ArrayOps.different(s, t, threshold)
    def dot(t:A): Double = ArrayOps.dot(s, t)
    def normalize(): Double = ArrayOps.normalize(s)
    def oneNormalize(): Double = ArrayOps.oneNormalize(s)
    def twoNormalize(): Double = ArrayOps.twoNormalize(s)
    def twoSquaredNormalize(): Double = ArrayOps.twoSquaredNormalize(s)
    def absNormalize(): Double = ArrayOps.absNormalize(s)
    def expNormalize(): Double = ArrayOps.expNormalize(s)
    def normalizeLogProb(): Double = ArrayOps.normalizeLogProb(s)
    def contains(d:Double): Boolean = ArrayOps.contains(s, d)
    def maxIndex: Int = ArrayOps.maxIndex(s)
    def isNaN: Boolean = ArrayOps.isNaN(s)
    def substitute(oldValue:Double, newValue:Double): Unit = ArrayOps.substitute(s, oldValue, newValue)
    def copy: Array[Double] = ArrayOps.copy(s)
    def set(t:A): Unit = ArrayOps.set(s, t)
  }

}
