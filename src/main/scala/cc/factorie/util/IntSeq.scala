/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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

trait IntSeq {
  def apply(i:Int): Int
  def length: Int
  def toArray: Array[Int]
  final def size = length
  def map(f:Int=>Int): IntSeq = {
    val len = length; val a = new Array[Int](len); var i = 0
    while (i < len) { a(i) = f(apply(i)); i += 1 }
    new ArrayIntSeq(a)
  }
  def map[A](f:Int=>A): Seq[A] = {
    val len = length; var i = 0; if (len <= 0) return Nil
    val a = new scala.collection.mutable.ArrayBuffer[A](len)
    while (i < len) { a.append(f(apply(i))); i += 1 }
    a
  }
  def foreach(f:Int=>Unit): Unit = { val len = length; var i = 0; while (i < len) { f(apply(i)); i += 1 } }
  def forElements(f:(Int,Int)=>Unit): Unit = { val len = length; var i = 0; while (i < len) { f(i, apply(i)); i += 1 } }
  def contains(d:Int): Boolean = { val len = length; var i = 0; while (i < len) { if (d == apply(i)) return true; i += 1 }; false }
  def forall(f:Int=>Boolean): Boolean = { val len = length; var i = 0; while (i < len) { if (!f(apply(i))) return false; i += 1 }; true }
  def foldLeft[B<:AnyRef](z:B)(f:(B,Int)=>B): B = { var acc = z; this.foreach(el => acc = f(acc, el)); acc }
  def indexOf(d:Int): Int = { val len = length; var i = 0; while (i < len) { if (d == apply(i)) return i; i += 1 }; -1 }
  def slice(from:Int, until:Int): IntSeq = { require(until <= length); val a = new Array[Int](until-from); Array.copy(_rawArray, from, a, 0, until-from); new ArrayIntSeq(a) }
  def max: Int = { val len = length; var m = Int.MinValue; var i = 0; while (i < len) { if (!(m >= apply(i))) m = apply(i); i += 1 }; m }
  def min: Int = { val len = length; var m = Int.MaxValue; var i = 0; while (i < len) { if (!(m <= apply(i))) m = apply(i); i += 1 }; m }
  def asArray: Array[Int] = toArray // To be overridden for efficiency in some subclasses
  /** Return the values as an Array[Int] whose length may be longer than this.length. */
  def _rawArray: Array[Int] = toArray  // Careful.  _rawArray.length may not equal length
  def asSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    final def length = IntSeq.this.length
    final def apply(i:Int): Int = IntSeq.this.apply(i)
  }
  def toSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    private val a = IntSeq.this.toArray
    final def length = a.length
    final def apply(i:Int): Int = a(i)
  }
  def asDoubleSeq: DoubleSeq = new DenseDoubleSeq {
    final def length = IntSeq.this.length
    final def apply(i:Int): Double = IntSeq.this.apply(i)
  }
}

trait MutableIntSeq extends IntSeq {
  def update(index:Int, value:Int): Unit
}

// Some simple concrete classes implementing IntSeq

/** Integers from start, start+1, start+2 ... end-1. */ 
final class RangeIntSeq (val start:Int, val end:Int) extends IntSeq {
  val length = end - start
  def apply(i:Int) = i
  def toArray = Array.range(start, end)
}

final class SingletonIntSeq(val value:Int) extends IntSeq {
  def length = 1
  def apply(i:Int): Int = if (i == 0) value else throw new Error("Index out of range: "+i)
  def toArray = Array(value)
  override def map(f:Int=>Int): IntSeq = new SingletonIntSeq(f(value))
}

final class ArrayIntSeq(val array:Array[Int]) extends IntSeq {
  @inline def length = array.length
  @inline def apply(i:Int): Int = array(i)
  @inline def toArray = { val a = new Array[Int](length); System.arraycopy(array, 0, a, 0, length); a }
  @inline override def asArray = array
}

final class TruncatedArrayIntSeq(val array:Array[Int], val length:Int) extends IntSeq {
  def apply(i:Int): Int = array(i)
  def toArray = { val a = new Array[Int](length); System.arraycopy(array, 0, a, 0, length); a }
}

final class SubArrayIntSeq(val array:Array[Int], val start:Int, val length:Int) extends IntSeq {
  def apply(i:Int): Int = array(i+start)
  def toArray = { val a = new Array[Int](length); System.arraycopy(array, start, a, 0, length); a }
}

/** A sequence of (activeDomain) indices for a possibly sparse outer product of (the activeDomains of) WeightsMap. */
final class Outer2IntSeq(val dim1:Int, val dim2:Int, val intSeq1:IntSeq, val intSeq2:IntSeq) extends IntSeq {
  override val _rawArray = new Array[Int](intSeq1.length * intSeq2.length)
  def toArray = { val a = new Array[Int](length); System.arraycopy(_rawArray, 0, a, 0, _rawArray.length); a }
  override def asArray = _rawArray
  private def _init() = { var k = 0; for (i <- 0 until intSeq1.length; j <- 0 until intSeq2.length) { _rawArray(k) = i*dim2 + j; k += 1 } }
  _init
  def length = _rawArray.length
  def apply(i:Int): Int = _rawArray(i)
}

/** Note that this will cause the Int to be boxed and unboxed. */
final class SeqIntSeq(override val asSeq:IndexedSeq[Int]) extends IntSeq {
  def length = asSeq.length
  def apply(i:Int) = asSeq(i)
  override def toArray = asSeq.toArray
}
