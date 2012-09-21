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

import scala.collection.mutable.ArrayBuffer
import scala.math
import java.util.Arrays

// Variables for dealing with sequences

// TODO Consider removing this?
trait ElementType[+ET] {
  type ElementType = ET
}

//@deprecated("Will be removed")
//trait VarAndElementType[+This<:Variable,+ET] extends VarAndValueType[This,IndexedSeq[ET]] with ElementType[ET]

/** A variable whose value is a Seq[E].  
    Note that this trait itself does not actually inherit from Seq[E] 
    because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity. */
trait SeqVar[+E] extends Variable with ValueBound[Seq[E]] with ElementType[E] {
  type Value <: Seq[E]
  def value: Seq[E]
  def iterator: Iterator[E]
  def foreach[U](f:(E)=>U): Unit
  def length: Int
  def apply(index:Int): E
  def exists(f:E=>Boolean): Boolean = { val iter = iterator; while (iter.hasNext) if (f(iter.next)) return true; return false }
  def contains(elem: Any): Boolean = exists(_ == elem)
  def toSeq: Seq[E] = value
}

/** A variable whose value is an IndexedSeq[E].  
    Note that this trait itself does not actually inherit from IndexedSeq[E] 
    because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity. */
trait IndexedSeqVar[+E] extends SeqVar[E] with ValueBound[IndexedSeq[E]] with ElementType[E] {
  type Value <: IndexedSeq[E]
  def value: IndexedSeq[E]
  // Some of the methods of Seq, for convenience
  def iterator: Iterator[E] = new Iterator[E] {
    var i = 0
    def hasNext: Boolean = i < length
    def next: E = { i += 1; apply(i-1) }
  }
  def foreach[U](f:(E)=>U): Unit = {
    var i = 0; val len = length
    while (i < 0) { f(apply(i)); i += 1 }
  }
  def map[B](f:E=>B): IndexedSeq[B] = {
    val len = length; val result = new ArrayBuffer[B](len); var i = 0
    while (i < 0) { result += f(apply(i)); i += 1 }; result
  }
  def head: E = apply(0)
  def last: E = apply(length-1)
  override def toSeq: IndexedSeq[E] = value
  def indexWhere(p:E => Boolean, from: Int): Int = { val len = length; var i = from; while (i < len) { if (p(apply(i))) return i; i += 1 }; return -1 }
  def indexWhere(p:E => Boolean): Int = indexWhere(p, 0)
  def indexOf[B>:E](elem:B): Int = indexOf(elem, 0)
  def indexOf[B>:E](elem:B, from:Int): Int = indexWhere(elem ==, from)
}

/** A variable containing a mutable sequence of other variables.  
    This variable stores the sequence itself, and tracks changes to the contents and order of the sequence. 
    @author Andrew McCallum */
trait MutableSeqVar[X] extends IndexedSeqVar[X] with MutableVar[IndexedSeq[X]] { // TODO This could be an IndexedSeqVar
  //type ElementType <: AnyRef
  //type Element = VariableType#ElementType
  type Element = X
  protected val _seq = new ArrayBuffer[Element] // TODO Consider using an Array[] instead so that SeqVar[Int] is efficient.
  final def value: IndexedSeq[Element] = _seq
  def set(newValue:Value)(implicit d:DiffList): Unit = { _seq.clear; _seq ++= newValue }
  def update(seqIndex:Int, x:Element)(implicit d:DiffList): Unit = UpdateDiff(seqIndex, x)
  def append(x:Element)(implicit d:DiffList) = AppendDiff(x)
  def prepend(x:Element)(implicit d:DiffList) = PrependDiff(x)
  def trimStart(n:Int)(implicit d:DiffList) = TrimStartDiff(n)
  def trimEnd(n: Int)(implicit d:DiffList) = TrimEndDiff(n)
  def remove(n:Int)(implicit d:DiffList) = Remove1Diff(n)
  def swap(i:Int,j:Int)(implicit d:DiffList) = Swap1Diff(i,j)
  def swapLength(pivot:Int,length:Int)(implicit d:DiffList) = for (i <- pivot-length until pivot) Swap1Diff(i,i+length)
  abstract class SeqVariableDiff(implicit d:DiffList) extends AutoDiff {override def variable = MutableSeqVar.this}
  case class UpdateDiff(i:Int, x:Element)(implicit d:DiffList) extends SeqVariableDiff {val xo = _seq(i); def undo = _seq(i) = xo; def redo = _seq(i) = x}
  case class AppendDiff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo = _seq.trimEnd(1); def redo = _seq.append(x)}
  case class PrependDiff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo = _seq.trimStart(1); def redo = _seq.prepend(x)}
  case class TrimStartDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.take(n); def undo = _seq prependAll (s); def redo = _seq.trimStart(n)}
  case class TrimEndDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.drop(_seq.length - n); def undo = _seq appendAll (s); def redo = _seq.trimEnd(n)}
  case class Remove1Diff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val e = _seq(n); def undo = _seq.insert(n,e); def redo = _seq.remove(n)}
  case class Swap1Diff(i:Int,j:Int)(implicit d:DiffList) extends SeqVariableDiff { def undo = {val e = _seq(i); _seq(i) = _seq(j); _seq(j) = e}; def redo = undo }
  // Limited support for Seq-related messages, but this object is not a scala.collection.Seq[] // TODO Consider removing these
  def length = _seq.length
  override def iterator = _seq.iterator
  def apply(index: Int) = _seq(index)
  // for changes without Diff tracking
  def +=(x:Element) = _seq += x
  def ++=(xs:Iterable[Element]) = _seq ++= xs
  //def update(index:Int, x:Element): Unit = _seq(index) = x
}

class SeqDomain[X] extends Domain[Seq[X]]
object SeqDomain extends SeqDomain[Variable]
class SeqVariable[X] extends MutableSeqVar[X] {
  def this(initialValue: Seq[X]) = { this(); _seq ++= initialValue }
  def domain = SeqDomain.asInstanceOf[SeqDomain[X]]
}

