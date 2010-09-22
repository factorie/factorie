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

trait IndexedSeqOps {

  private type IS = scala.collection.mutable.IndexedSeq[Double]

  def oneNorm(s:IS): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i)); result }
  def twoNorm(s:IS): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i) * s(i)); math.sqrt(result) }
  def twoNormSquared(s:IS): Double = { var result = 0.0; forIndex(s.length)(i => result += s(i) * s(i)); result }
  def infinityNorm(s:IS): Double = { var result = s(0); forIndex(s.length)(i => if (math.abs(s(i)) > result) result = math.abs(s(i))); result }
  def +=(s:IS, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) + d)
  def -=(s:IS, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) - d)
  def *=(s:IS, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) * d)
  def /=(s:IS, d:Double): Unit = forIndex(s.length)(i => s(i) = s(i) / d)
  def incr(s:IS, t:IS, factor:Double): Unit = { require(s.length == t.length); forIndex(s.length)(i => s(i) += t(i) * factor) }
  def different(s:IS, t:IS, threshold:Double): Boolean = { require(s.length == t.length); forIndex(s.length)(i => if (math.abs(s(i) - t(i)) > threshold) return true); return false }
  def dot(s:IS, t:IS): Double = { assert(s.length == t.length); var result = 0.0; forIndex(s.length)(i => result += s(i) * t(i)); result }

  // Lazy operations
  def timesEqualed(s:IS, d:Double): IS = new IS {
    def update(i:Int, d:Double): Unit = throw new Error("update not supported.")
    def length = s.length
    def apply(i:Int) = s(i) * d
  }

}

object IndexedSeqOps extends IndexedSeqOps

/** Import the contents of this object, 
    and you can call these methods directly on IndexedSeq's, 
    e.g. val a = new ArrayBuffer[Double](2); a(0) = 1.0; a(1) = 2.0; val n = a.twoNorm */
trait IndexedSeqImplicits {
  implicit def indexedSeq2IndexedSeqOps(s:scala.collection.mutable.IndexedSeq[Double]) = new scala.collection.mutable.IndexedSeq[Double] {
    type IS = scala.collection.mutable.IndexedSeq[Double]
    def length = s.length
    def apply(i:Int) = s(i)
    def update(i:Int, d:Double): Unit = s(i) = d
    def oneNorm: Double = IndexedSeqOps.oneNorm(s)
    def twoNorm: Double = IndexedSeqOps.twoNorm(s)
    def twoNormSquared: Double = IndexedSeqOps.twoNormSquared(s)
    def infinityNorm: Double = IndexedSeqOps.infinityNorm(s)
    def +=(d:Double): Unit = IndexedSeqOps.+=(s, d)
    def -=(d:Double): Unit = IndexedSeqOps.-=(s, d)
    def *=(d:Double): Unit = IndexedSeqOps.*=(s, d)
    def /=(d:Double): Unit = IndexedSeqOps./=(s, d)
    def *(d:Double): IS = IndexedSeqOps.timesEqualed(s, d)
    def incr(t:IS, factor:Double): Unit = IndexedSeqOps.incr(s, t, factor)
    def different(t:IS, threshold:Double): Boolean = IndexedSeqOps.different(s, t, threshold)
    def dot(t:IS): Double = IndexedSeqOps.dot(s, t)
  }
}
