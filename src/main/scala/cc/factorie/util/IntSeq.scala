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

trait IntSeq {
  def apply(i:Int): Int
  def length: Int
  def toArray: Array[Int]
  final def size = length
  def foreach(f:(Int)=>Unit): Unit = { var i = 0; while (i < length) { f(apply(i)); i += 1 } }
  def forElements(f:(Int,Int)=>Unit): Unit = { var i = 0; while (i < length) { f(i, apply(i)); i += 1 } }
  def contains(d:Int): Boolean = { var i = length; while (i >= 0) if (d == apply(i)) return true; false }
  def forall(f:(Int)=>Boolean): Boolean = { var i = length; while (i >= 0) if (!f(apply(i))) return false; true } 
  def foldLeft[B<:AnyRef](z:B)(f:(B,Int)=>B): B = throw new Error
  def indexOf(d:Int): Int = { var i = 0; while (i < length) { if (d == apply(i)) return i }; -1 }
  def max: Int = { var m = Int.MinValue; var i = 0; while (i < length) { if (!(m >= apply(i))) m = apply(i) }; m }
  def min: Int = { var m = Int.MinValue; var i = 0; while (i < length) { if (!(m <= apply(i))) m = apply(i) }; m }
  def asSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    final def length = IntSeq.this.length
    final def apply(i:Int): Int = IntSeq.this.apply(i)
  }
  def toSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    private val a = toArray
    final def length = a.length
    final def apply(i:Int): Int = a(i)
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
}

final class ArrayIntSeq(val array:Array[Int]) extends IntSeq {
  def length = array.length
  def apply(i:Int): Int = array(i)
  def toArray = array
}

final class TruncatedArrayIntSeq(val array:Array[Int], val length:Int) extends IntSeq {
  def apply(i:Int): Int = array(i)
  def toArray = { val a = new Array[Int](length); System.arraycopy(array, 0, a, 0, length); a }
}

final class SubArrayIntSeq(val array:Array[Int], val start:Int, val length:Int) extends IntSeq {
  def apply(i:Int): Int = array(i+start)
  def toArray = { val a = new Array[Int](length); System.arraycopy(array, start, a, 0, length); a }
}

/** Note that this will cause the Int to be boxed and unboxed. */
final class SeqIntSeq(override val asSeq:IndexedSeq[Int]) extends IntSeq {
  def length = asSeq.length
  def apply(i:Int) = asSeq(i)
  override def toArray = asSeq.toArray
}
