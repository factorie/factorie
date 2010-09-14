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

package cc.factorie.la
import cc.factorie._

// Some classes to help manage IndexedSeq[Double] as simple vectors
// Used, for example, in cc.factorie.optimize

/** Various simple linear algebra functions that can operate on an Array[Double], 
    hence the name "Array Linear Algebra" == "ArrayLA".
    @author Andrew McCallum */
object ArrayLA {
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
  def normalize(s:A): Double = { val sum = oneNorm(s); forIndex(s.length)(i => s(i) /= sum); sum }
  def oneNormalize(s:A): Double = normalize(s)
  def twoNormalize(s:A): Double = { val norm = twoNorm(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def twoSquaredNormalize(s:A): Double = { val norm = twoNormSquared(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def absNormalize(s:A): Double = { val norm = absNorm(s); forIndex(s.length)(i => s(i) /= norm); norm }
  def contains(s:A, d:Double): Boolean = { forIndex(s.length)(i => if (s(i) == d) return true); false }
  def isNaN(s:A): Boolean = contains(s, Double.NaN)
  def substitute(s:A, oldValue:Double, newValue:Double): Unit = forIndex(s.length)(i => if (s(i) == oldValue) s(i) = newValue)
  def copy(s:A): Array[Double] = { val result = new Array[Double](s.length); set(result, s); result }
  def set(s:A, t:A): Unit = {
    require(s.length == t.length)
    forIndex(t.length)(i => s(i) = t(i))
  }

  /** Import the contents of this object, and you can call these methods directly on arrays, 
      e.g. val a = new Array[Double](2); a(0) = 1.0; a(1) = 2.0; val n = a.twoNorm */
  object Implicits {
    implicit def array2ArrayLA(s:Array[Double]) = new {
      //def length = s.length
      //def apply(i:Int) = s(i)
      //def update(i:Int, d:Double): Unit = s(i) = d
      def oneNorm: Double = ArrayLA.oneNorm(s)
      def twoNorm: Double = ArrayLA.twoNorm(s)
      def twoNormSquared: Double = ArrayLA.twoNormSquared(s)
      def infinityNorm: Double = ArrayLA.infinityNorm(s)
      def +=(d:Double): Unit = ArrayLA.+=(s, d)
      def -=(d:Double): Unit = ArrayLA.-=(s, d)
      def *=(d:Double): Unit = ArrayLA.*=(s, d)
      def /=(d:Double): Unit = ArrayLA./=(s, d)
      def incr(t:A, factor:Double): Unit = ArrayLA.incr(s, t, factor)
      def different(t:A, threshold:Double): Boolean = ArrayLA.different(s, t, threshold)
      def dot(t:A): Double = ArrayLA.dot(s, t)
      def normalize(): Double = ArrayLA.normalize(s)
      def oneNormalize(): Double = ArrayLA.oneNormalize(s)
      def twoNormalize(): Double = ArrayLA.twoNormalize(s)
      def twoSquaredNormalize(): Double = ArrayLA.twoSquaredNormalize(s)
      def absNormalize(): Double = ArrayLA.absNormalize(s)
      def contains(d:Double): Boolean = ArrayLA.contains(s, d)
      def isNaN: Boolean = ArrayLA.isNaN(s)
      def substitute(oldValue:Double, newValue:Double): Unit = ArrayLA.substitute(s, oldValue, newValue)
      def copy: Array[Double] = ArrayLA.copy(s)
      def set(t:A): Unit = ArrayLA.set(s, t)
    }
  }

}


