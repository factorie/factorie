/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.la
import cc.factorie._

// Some classes to help manage IndexedSeq[Double] as simple vectors
// Used, for example, in cc.factorie.optimize

/** Various simple linear algebra functions that can operate on a IndexedSeq[Double]
    hence the name "IndexedSeq Linear Algebra" == "IndexedSeqLA".
    @author Andrew McCallum */
// TODO This class is not as complete as ArrayLA; the additional functions from there should be implemented here too
object IndexedSeqLA {
  type IS = scala.collection.mutable.IndexedSeq[Double]

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

  /** Import the contents of this object, and you can call these methods directly on IndexedSeq's, 
      e.g. val a = new ArrayBuffer[Double](2); a(0) = 1.0; a(1) = 2.0; val n = a.twoNorm */
  object Implicits {
    implicit def indexedSeq2IndexedSeqVector(s:IS) = new scala.collection.mutable.IndexedSeq[Double] {
      def length = s.length
      def apply(i:Int) = s(i)
      def update(i:Int, d:Double): Unit = s(i) = d
      def oneNorm: Double = IndexedSeqLA.oneNorm(s)
      def twoNorm: Double = IndexedSeqLA.twoNorm(s)
      def twoNormSquared: Double = IndexedSeqLA.twoNormSquared(s)
      def infinityNorm: Double = IndexedSeqLA.infinityNorm(s)
      def +=(d:Double): Unit = IndexedSeqLA.+=(s, d)
      def -=(d:Double): Unit = IndexedSeqLA.-=(s, d)
      def *=(d:Double): Unit = IndexedSeqLA.*=(s, d)
      def /=(d:Double): Unit = IndexedSeqLA./=(s, d)
      def *(d:Double): IS = IndexedSeqLA.timesEqualed(s, d)
      def incr(t:IS, factor:Double): Unit = IndexedSeqLA.incr(s, t, factor)
      def different(t:IS, threshold:Double): Boolean = IndexedSeqLA.different(s, t, threshold)
      def dot(t:IS): Double = IndexedSeqLA.dot(s, t)
    }
  }

}


