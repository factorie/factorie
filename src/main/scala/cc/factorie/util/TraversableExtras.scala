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
import scala.util.Sorting
import scala.reflect.ClassTag
import scala.annotation.tailrec

/** New functionality on Traversable instances, available by implicit conversion in the cc.factorie package object in cc/factorie/package.scala. */
final class TraversableExtras[A](val t: Traversable[A]) extends AnyVal {

    def split(pred:(A => Boolean)):Iterable[Traversable[A]] = {
      @tailrec def splitAcc(acc:List[Traversable[A]], elems:Traversable[A]):List[Traversable[A]] = {
        val (first, rest) = elems span pred
        if(elems.isEmpty) {acc}
        else if(rest.isEmpty && first.nonEmpty) {first :: acc}
        else if(first.isEmpty) {splitAcc(acc, rest.tail)}
        else {splitAcc(first :: acc, rest.tail)}
      }
      splitAcc(Nil, t).reverse
    }

  def indexSafe(i: Int): Option[A] = if (i < t.size && i >= 0) Some(t.toSeq(i)) else None

  def sumDoubles(extractor: A => Double): Double = t.foldLeft(0.0)((sum, x) => sum + extractor(x))
  def sumInts(extractor: A => Int): Int = t.foldLeft(0)((sum, x) => sum + extractor(x))

  def multiplyDoubles(extractor: A => Double): Double = t.foldLeft(1.0)((prod, x) => prod * extractor(x))
  def multiplyInts(extractor: A => Int): Int = t.foldLeft(1)((prod, x) => prod * extractor(x))

  def maxByDouble(extractor: A => Double): A = {
    val iter = t.toSeq.iterator
    if (!iter.hasNext) throw new Error("TraversableExtras.maxByDouble on empty Traversable")
    var result: A = iter.next()
    var value = extractor(result)
    while (iter.hasNext) {
      val x = iter.next(); val v = extractor(x)
      if (v > value) { result = x; value = v }
    }
    result
  }

  def indexOfMaxByDouble(extractor: A => Double): Int = {
    val seq = t.toSeq
    if (seq.isEmpty) throw new Error("TraversableExtras.indexOfMaxByDouble on empty Traversable")
    var result: Int = 0
    var value = extractor(seq(0))
    for(i <- 1 until seq.length) {
      val v = extractor(seq(i))
      if (v > value) { result = i; value = v }
    }
    result
  }

  def maxByInt(extractor: A => Int): A = {
    val iter = t.toSeq.iterator
    if (!iter.hasNext) throw new Error("TraversableExtras.maxByInt on empty Traversable")
    var result: A = iter.next()
    var value = extractor(result)
    while (iter.hasNext) {
      val x = iter.next(); val v = extractor(x)
      if (v > value) { result = x; value = v }
    }
    result
  }

  def minByDouble(extractor: A => Double): A = {
    val iter = t.toSeq.iterator
    if (!iter.hasNext) throw new Error("TraversableExtras.minByDouble on empty Traversable")
    var result: A = iter.next()
    var value = extractor(result)
    while (iter.hasNext) {
      val x = iter.next(); val v = extractor(x)
      if (v < value) { result = x; value = v }
    }
    result
  }
  def minByInt(extractor: A => Int): A = {
    val iter = t.toSeq.iterator
    if (!iter.hasNext) throw new Error("TraversableExtras.minByInt on empty Traversable")
    var result: A = iter.next()
    var value = extractor(result)
    while (iter.hasNext) {
      val x = iter.next(); val v = extractor(x)
      if (v < value) { result = x; value = v }
    }
    result
  }

  /**Returns both the maximum element and the second-to-max element */
  def max2ByDouble(extractor: A => Double): (A, A) = {
    val s1 = t.toSeq
    assert(s1.length > 1)
    var best1 = Double.NegativeInfinity
    var best1i: A = null.asInstanceOf[A]
    var best2 = Double.NegativeInfinity
    var best2i: A = null.asInstanceOf[A]
    var i = 0
    while (i < s1.length) {
      val x = extractor(s1(i))
      if (x > best1) {
        best2 = best1
        best2i = best1i
        best1 = x
        best1i = s1(i)
      } else if (x > best2) {
        best2 = x
        best2i = s1(i)
      }
      i += 1
    }
    (best1i, best2i)
  }

  /** Sorts with minimum first. */
  //@deprecated // use SeqLike sort instead?
  def sortForward(extractor: A => Double): Seq[A] =
    t.toSeq.sortWith((x1:A, x2:A) => extractor(x1) < extractor(x2))

  /** Sorts with maximum first.*/
  //@deprecated // use SeqLike sort instead?
  def sortReverse(extractor: A => Double): Seq[A] =
    t.toSeq.sortWith((x1:A, x2:A) => extractor(x1) > extractor(x2))

  def shuffle(implicit random: Random) : Seq[A] = {
    val s2 = t.map(x => (x, random.nextInt())).toSeq
    Sorting.stableSort(s2, (t1: (A, Int), t2: (A, Int)) => t1._2 > t2._2).map(t => t._1)
  }

  def split(ratio: Double): (Seq[A], Seq[A]) = {
    val s2 = t.toSeq
    if (ratio <= 0 || ratio > 1.0) throw new Error
    val index = (ratio * s2.size).toInt
    if (index >= s2.size)
      (s2, Seq.empty)
    else
      (s2.slice(0, index), s2.drop(index))
  }

  // TODO Make these preserve their correct return types rather than backing off to Traversable.
  def filterByType[T<:AnyRef](implicit m: ClassTag[T]): Traversable[T] = 
    t.filter(t1 => m.runtimeClass.isAssignableFrom(t1.asInstanceOf[AnyRef].getClass)).asInstanceOf[Traversable[T]]
  def filterByClass[C](c: Class[C]): Traversable[C] =
    t.filter(t1 => c.isAssignableFrom(t1.asInstanceOf[AnyRef].getClass)).asInstanceOf[Traversable[C]]

  def subseq(prob:Double)(implicit random: Random) = {
    t.flatMap((a:A) => if(random.nextDouble < prob) List(a) else Nil)
  }

  def sampleUniformly(implicit random: Random): A = {
    val s2 = t.toSeq
    if (s2.size == 1) s2.head
    else s2(random.nextInt(s2.size))
  }

  def sampleProportionally(extractor: A => Double)(implicit random:Random): A = {
    var sum = t.foldLeft(0.0)((total, x) => total + extractor(x))
    val r = random.nextDouble * sum
    sum = 0
    for (choice <- t) {
      val e = extractor(choice)
      if (e < 0.0) throw new Error("TraversableExtras sample extractor value " + e + " less than zero.  Sum=" + sum)
      sum += e
      if (sum >= r)
        return choice
    }
    throw new Error("TraversableExtras sample error: r=" + r + " sum=" + sum)
  }

  def sampleExpProportionally(extractor: A => Double)(implicit random:Random): A = {
    val maxValue : Double = t.foldLeft(Double.NegativeInfinity)((max,t) => {val x = extractor(t); assert(x==x); if (x>max) x else max})
    if (maxValue == Double.NegativeInfinity) throw new Error("Cannot sample from an empty list.")
    sampleProportionally(t1 => if (extractor(t1) == Double.NegativeInfinity) Double.NegativeInfinity else math.exp(extractor(t1) - maxValue))(random)
  }


}
