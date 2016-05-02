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

package cc.factorie.variable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ArrayBuffer

// Variables for holding sequences.

/** A trait for setting the member type ElementType in SeqVar classes.
    @author Andrew McCallum */
trait ElementType[+ET] {
  type ElementType = (ET @uncheckedVariance)
}

/** A trait with many of the same methods as Seq, but not actually a Seq itself.
    This is necessary because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity.
    @author Andrew McCallum */
trait SeqSimilar[+E] extends Iterable[E] with ElementType[E] {
  def value: Seq[E]
  // Some of the methods of Seq, for convenience
  def length: Int = value.length
  def apply(index:Int): E = value.apply(index)
  def iterator: Iterator[E] = value.iterator
  def map[B](f:E=>B): Seq[B] = value.map(f)
  def contains(elem: Any): Boolean = value.contains(elem)
  def indexWhere(p:E => Boolean, from: Int): Int = value.indexWhere(p, from)
  def indexWhere(p:E => Boolean): Int = value.indexWhere(p)
  def indexOf[B>:E](elem:B): Int = value.indexOf(elem)
  def indexOf[B>:E](elem:B, from:Int): Int = value.indexOf(elem, from)
  // Methods overridden from Iterable
  override def toSeq: Seq[E] = value // TODO Should we also have "asSeq"? -akm
  override def foreach[U](f:(E)=>U): Unit = value.foreach(f)
  override def head: E = value.head
  override def last: E = value.last
  override def exists(f:E=>Boolean): Boolean = value.exists(f)
}

/** A trait with many of the same methods as IndexedSeq, but not actually a IndexedSeq itself.  
    This is necessary because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity.
    @author Andrew McCallum */
trait IndexedSeqSimilar[+E] extends SeqSimilar[E] {
  override def value: IndexedSeq[E]
  override def map[B](f:E=>B): IndexedSeq[B] = value.map(f)
  override def toSeq: IndexedSeq[E] = value
}

/** An abstract variable whose value is a Seq[E].  
    Note that this trait itself does not actually inherit from Seq[E] 
    because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity.
    @author Andrew McCallum */
trait SeqVar[+E] extends Var with SeqSimilar[E] {
  type Value <: Seq[E]
  def value: Value
}


/** An abstract variable whose value is an IndexedSeq[E].  
    Note that this trait itself does not actually inherit from IndexedSeq[E] 
    because Seq defines "equals" based on same contents, 
    but all variables must have equals based on identity.
    @author Andrew McCallum */
trait IndexedSeqVar[+E] extends SeqVar[E] with IndexedSeqSimilar[E] {
  type Value <: IndexedSeq[E]
  def value: Value
}


/** An abstract variable containing a mutable sequence of elements (which could be other variables).  
    This variable stores the sequence itself, and tracks changes to the contents and order of the sequence. 
    @author Andrew McCallum */
trait MutableSeqVar[E] extends IndexedSeqVar[E] with MutableVar {
  type Element = E
  type Value = IndexedSeq[E]
  protected val _seq = new ArrayBuffer[Element] // TODO Consider using an Array[] instead so that apply(Int) is more efficient.
  @inline final def value: Value = _seq.toIndexedSeq // Note that for efficiency we don't return a copy, but this means that this value could change out from under a saved "value" if this variable value is changed.
  def set(newValue:Value)(implicit d:DiffList): Unit = { _seq.clear(); _seq ++= newValue }
  def update(seqIndex:Int, x:Element)(implicit d:DiffList): Unit = UpdateDiff(seqIndex, x)
  def add(e:Element)(implicit d:DiffList): Unit = Append1Diff(e)
  def append(es:Element*)(implicit d:DiffList): Unit = AppendDiff(es:_*)
  def prepend(e:Element)(implicit d:DiffList): Unit = PrependDiff(e)
  def trimStart(n:Int)(implicit d:DiffList): Unit = TrimStartDiff(n)
  def trimEnd(n:Int)(implicit d:DiffList): Unit = TrimEndDiff(n)
  def remove(n:Int)(implicit d:DiffList): Unit = Remove1Diff(n)
  def remove(e:E)(implicit d:DiffList): Unit = remove(_seq.indexOf(e))
  def clear(implicit d:DiffList): Unit = for (i <- length-1 to 0) remove(i)
  def swap(i:Int,j:Int)(implicit d:DiffList) = Swap1Diff(i,j)
  def swapLength(pivot:Int,length:Int)(implicit d:DiffList) = for (i <- pivot-length until pivot) Swap1Diff(i,i+length)
  abstract class SeqVariableDiff(implicit d:DiffList) extends AutoDiff {override def variable = MutableSeqVar.this}
  case class UpdateDiff(i:Int, x:Element)(implicit d:DiffList) extends SeqVariableDiff {val xo = _seq(i); def undo() = _seq(i) = xo; def redo() = _seq(i) = x}
  case class Append1Diff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo() = _seq.trimEnd(1); def redo() = _seq.append(x)}
  case class AppendDiff(xs:Element*)(implicit d:DiffList) extends SeqVariableDiff {def undo() = _seq.trimEnd(xs.length); def redo() = _seq.append(xs:_*)}
  case class PrependDiff(x:Element)(implicit d:DiffList) extends SeqVariableDiff {def undo() = _seq.trimStart(1); def redo() = _seq.prepend(x)}
  case class TrimStartDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.take(n); def undo() = _seq prependAll s; def redo() = _seq.trimStart(n)}
  case class TrimEndDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = _seq.drop(_seq.length - n); def undo() = _seq appendAll s; def redo() = _seq.trimEnd(n)}
  case class Remove1Diff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val e = _seq(n); def undo() = _seq.insert(n,e); def redo() = _seq.remove(n)}
  case class Swap1Diff(i:Int,j:Int)(implicit d:DiffList) extends SeqVariableDiff { def undo() = {val e = _seq(i); _seq(i) = _seq(j); _seq(j) = e}; def redo() = undo() }
  // Override some methods for a slight gain in efficiency
  override def length = _seq.length
  override def iterator = _seq.iterator
  override def apply(index: Int) = _seq(index)
  // for changes without Diff tracking
  def +=(x:Element) = _seq += x
  def -=(x:Element) = _seq -= x
  def ++=(xs:Iterable[Element]) = _seq ++= xs
  //def update(index:Int, x:Element): Unit = _seq(index) = x // TODO What should this be named, since we already have an update method above? -akm
}

/** A concrete variable containing a mutable sequence of elements (which could be other variables).
    @author Andrew McCallum */
class SeqVariable[X] extends MutableSeqVar[X] {
  def this(initialValue: Seq[X]) = { this(); _seq ++= initialValue }
}

