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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable, DoubleLinkedList}
import scala.math

// Variables for dealing with sequences

/** Revert equals/hashCode behavior of Seq[A] to the default Object.
    WARNING: This doesn't actually satisfy commutativity with a Seq[A]. :-( */
trait SeqEqualsEq[+A] extends scala.collection.Seq[A] {
  override def equals(that:Any): Boolean = that match {
    case that:AnyRef => this eq that
    case _ => false
  }
  override def hashCode: Int = java.lang.System.identityHashCode(this)
}

trait IndexedSeqEqualsEq[+A] extends SeqEqualsEq[A] with IndexedSeq[A]

/** A variable containing a mutable sequence of other variables.  
    This variable stores the sequence itself, and tracks changes to the contents and order of the sequence. 
    @author Andrew McCallum */
abstract class SeqVariable[X](sequence: Seq[X]) extends Variable with TypedValues with SeqEqualsEq[X] {
  def this() = this(Nil)
  type ValueType = X
  type VariableType <: SeqVariable[X]
  //class XList[X](var elem:X, var prev:XList[X], var next:XList[X]) extends DoubleLinkedList[X,XList[X]] {
  //this(xs:Seq[X]) = this(xs.head, null, new XList(xs.drop(1)))
  //def prepend(x:X) : XList[X] = { val first = new XList(x, null, this); this.prev = first; first }
  //}
  private val seq = { val a = new ArrayBuffer[X](); a ++= sequence; a }
  def append(x:X)(implicit d:DiffList) = AppendDiff(x)
  def prepend(x:X)(implicit d:DiffList) = PrependDiff(x)
  def trimStart(n:Int)(implicit d:DiffList) = TrimStartDiff(n)
  def trimEnd(n: Int)(implicit d:DiffList) = TrimEndDiff(n)
  def remove(n:Int)(implicit d:DiffList) = Remove1Diff(n)
  def swap(i:Int,j:Int)(implicit d:DiffList) = Swap1Diff(i,j)
  def swapLength(pivot:Int,length:Int)(implicit d:DiffList) = for (i <- pivot-length until pivot) Swap1Diff(i,i+length)
  abstract class SeqVariableDiff(implicit d:DiffList) extends AutoDiff {override def variable = SeqVariable.this}
  case class AppendDiff(x:X)(implicit d:DiffList) extends SeqVariableDiff {def undo = seq.trimEnd(1); def redo = seq.append(x)}
  case class PrependDiff(x:X)(implicit d:DiffList) extends SeqVariableDiff {def undo = seq.trimStart(1); def redo = seq.prepend(x)}
  case class TrimStartDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = seq.take(n); def undo = seq prependAll (s); def redo = seq.trimStart(n)}
  case class TrimEndDiff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val s = seq.drop(seq.length - n); def undo = seq appendAll (s); def redo = seq.trimEnd(n)}
  case class Remove1Diff(n:Int)(implicit d:DiffList) extends SeqVariableDiff {val e = seq(n); def undo = seq.insert(n,e); def redo = seq.remove(n)}
  case class Swap1Diff(i:Int,j:Int)(implicit d:DiffList) extends SeqVariableDiff { def undo = {val e = seq(i); seq(i) = seq(j); seq(j) = e}; def redo = undo }
  // for Seq trait
  def length = seq.length
  def iterator = seq.iterator
  def apply(index: Int) = seq(index)
  // for changes without Diff tracking
  def +=(x:X) = seq += x
  def ++=(xs:Iterable[X]) = seq ++= xs
}

/** A variable containing a mutable (but untracked by Diff) sequence of variables; used in conjunction with VarInSeq.
    @author Andrew McCallum */
class VariableSeq[V <: Variable with VarInTypedSeq[V,_]](initialCapacity:Int = 8) extends IndexedSeqEqualsEq[V] with Variable {
  private val seq = new ArrayBuffer[V](initialCapacity)
  def +=(v: V) = {
    if (v.seq != null) throw new Error("Trying to add VarInSeq that is already assigned to another VariableSeq")
    seq += v
    v.setSeqPos(this, seq.size - 1)
  }
  def +(v: V) = {this += v; this} // TODO But according to Scala convension this should create a return a new sequence, right?  Remove this method?
  def ++=(vs: Iterable[V]) = vs.foreach(this += _)
  def ++(vs: Iterable[V]) = {this ++= vs; this}
  override def iterator = seq.iterator
  def length = seq.length
  def apply(i: Int) = seq.apply(i)
}


/** For use with variables that have immutable-valued .next and .prev in a sequence. 
    This tries to avoid the need for the self-type in VarInSeq, but I don't think it will work.
    Deprecated.  Use VarInSeq instead. */
@deprecated("Use VarInSeq or VarInTypedSeq instead.")
trait VarInSeq2 {
  this: Variable =>
    private var _seq: Seq[this.type] = null
  def seq = _seq
  private var _position = -1
  def position = _position
  def setSeqPos(s: Seq[Variable], p: Int) = {
    if (s(p) != this) throw new Error
    _seq = s.asInstanceOf[Seq[this.type]]
    _position = p
  }
  def hasNext = _seq != null && _position + 1 < _seq.length
  def next: this.type = if (_position + 1 < _seq.length) _seq(_position + 1) else null
  def hasPrev = _seq != null && _position > 0
  def prev: this.type = if (_position > 0) _seq(_position - 1) else null
}

trait AbstractVarInSeq[This <: AbstractVarInSeq[This]] {
  def hasNext: Boolean
  def hasPrev: Boolean
  def next: This
  def prev: This
}

/** For use with variables that have immutable-valued .next and .prev in a sequence. 
@author Andrew McCallum */
// TODO Reverse the order of type arguments V and S to match other places in which the "This" type comes last.
trait VarInTypedSeq[V /*>: Null*/ <: VarInTypedSeq[V,S] with Variable, S<:Seq[V]] extends AbstractVarInSeq[V] {
  this: V =>
  private var _seq: S = _
  var _position = -1
  def seq: S = _seq
  def position = _position
  def seqAfter = seq.drop(_position+1)
  def seqBefore = seq.take(_position)
  def setSeqPos(s: Seq[V], p: Int) = {
    if (s(p) != this) throw new Error
    _seq = s.asInstanceOf[S]
    _position = p
  }
  def hasNext = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else seq != null && _position + 1 < seq.length
  def next: V = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (_position + 1 < seq.length) seq(_position + 1) else null.asInstanceOf[V]
  def hasPrev = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else seq != null && _position > 0
  def prev: V = if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (_position > 0) seq(_position - 1) else null.asInstanceOf[V]
  def next(n:Int): V = { 
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set")
    val i = _position + n
    if (i >= 0 && i < seq.length) seq(i) else null.asInstanceOf[V]
  }
  def prev(n:Int): V = {
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") 
    val i = _position - n
    if (i >= 0 && i < seq.length) seq(i) else null.asInstanceOf[V]
  }
  def prevWindow(n:Int): Seq[V] = for (i <- math.max(_position-n, 0) to math.max(_position-1,0)) yield seq(i)
  def nextWindow(n:Int): Seq[V] = for (i <- math.min(_position+1, seq.length-1) to math.min(_position+n, seq.length-1)) yield seq(i)
  def window(n:Int): Seq[V] = for (i <- math.max(_position-n,0) to math.min(_position+n,seq.length-1)) yield seq(i)
  def windowWithoutSelf(n:Int): Seq[V] = for (i <- math.max(_position-n,0) to math.min(_position+n,seq.length-1); if (i != _position)) yield seq(i)
  def between(other:V): Seq[V] = {
    assert (other.seq == seq)
    if (other.position > _position)
      for (i <- _position until other.position) yield seq(i)
    else
      for (i <- other.position until _position) yield seq(i)
  } 
  def firstInSeq = seq(0)
}

trait VarInSeq[This <: VarInSeq[This] with Variable] extends VarInTypedSeq[This,Seq[This]] {
  this: This =>
}

/*trait VarInMutableSeq[This >: Null <: VarInMutableSeq[This]] extends cc.factorie.util.DLinkedList[VarInMutableSeq[This]] {
  this : This =>
}*/

/** For variables that have mutable-valued .next and .prev in a sequence.  
    Currently only change operation is 'swapWithVar', but more could be added.
    This is an odd class because it extends LinkList in which each element of 
    the collection represents both the element and the collection itself.
    This class may be deprecated in the future.
    @author Andrew McCallum */
trait VarInMutableSeq[This >: Null <: VarInMutableSeq[This] with cc.factorie.util.LinkList[This] with Variable] extends cc.factorie.util.LinkList[This] {
  this : This =>
    def swapWithVar(that:This)(implicit d:DiffList) : Unit = {
      this.swapWith(that)
      if (d ne null) {
        d += new VarInMutableSeqSwapDiff(this, that)
        d += new VarInMutableSeqOtherVar(that)
      }
    }
  /** Delete "this". */
  def deleteVar(implicit d:DiffList): Unit = {
    if (d ne null) {
      d += new VarInMutableSeqOtherVar(prev)
      d += new VarInMutableSeqOtherVar(next)
    }
    VarInMutableSeqDeleteDiff(prev, next)
  }
  /** Insert "that" before "this". */
  def insertVar(that:This)(implicit d:DiffList): Unit = {
    VarInMutableSeqPreInsertDiff(that)
    if (d ne null) {
      d += new VarInMutableSeqOtherVar(that)
      if (that.hasPrev) d += new VarInMutableSeqOtherVar(that.prev)
    }
  }
  case class VarInMutableSeqDeleteDiff(prevElt:This, nextElt:This)(implicit d:DiffList) extends Diff {
    if (d ne null) d += this
    var done = false
    redo
    def variable: This = if (!done) VarInMutableSeq.this else null.asInstanceOf[This]
    def redo = { assert(!done); done = true; remove }
    def undo = { assert(done); done = false; if (prevElt ne null) prevElt.postInsert(variable) else nextElt.preInsert(variable) }
  }
  case class VarInMutableSeqPreInsertDiff(that:This)(implicit d:DiffList) extends Diff {
    if (d ne null) d += this
    var done = false
    redo
    def variable: This = if (done) VarInMutableSeq.this else null.asInstanceOf[This]
    def redo = { assert(!done); done = true; preInsert(that) }
    def undo = { assert(done); done = false; that.remove }
    override def toString = "VarInMutableSeqDeleteDiff(prev="+VarInMutableSeq.this+",insert="+that+")"
  }
  case class VarInMutableSeqSwapDiff(ths:This, that:This) extends Diff {
    def variable: This = VarInMutableSeq.this
    def variables = List(ths, that) // TODO Consider handling this in the FACTORIE library
    def redo = ths.swapWith(that)
    def undo = redo
  }
  case class VarInMutableSeqOtherVar(that:This) extends Diff {
    def variable: This = that  // Just to put another variable on the difflist
    def redo = {}
    def undo = {}
  }
}

