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

/** A variable containing a mutable (but untracked by Diff) sequence of variables; used in conjunction with VarInSeq.
    @author Andrew McCallum */
class VariableSeq[V <: Variable with VarInTypedSeq[V,_]](initialCapacity:Int = 8) extends IndexedSeqEqualsEq[V] with Variable with VarAndValueGenericDomain[VariableSeq[V],Seq[V]] {
  def value = _seq.toSeq
  private val _seq = new ArrayBuffer[V](initialCapacity)
  def +=(v: V) = {
    if (v.seq != null) throw new Error("Trying to add VarInSeq that is already assigned to another VariableSeq")
    _seq += v
    v.setSeqPos(this, _seq.size - 1)
  }
  def +(v: V) = {this += v; this} // TODO But according to Scala convension this should create a return a new sequence, right?  Remove this method?
  def ++=(vs: Iterable[V]) = vs.foreach(this += _)
  def ++(vs: Iterable[V]) = {this ++= vs; this}
  override def iterator = _seq.iterator
  def length = _seq.length
  def apply(i: Int) = _seq.apply(i)
}

// TODO Use trait VariableSeqType[+A] { type VariableSeqType = A } to make this unnecessary.
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

