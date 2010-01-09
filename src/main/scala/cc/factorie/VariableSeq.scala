/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable,DoubleLinkedList}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging}
import cc.factorie.util.Implicits._

// Variables for dealing with sequences


  /** A variable containing a mutable sequence of other variables.  
      This variable stores the sequence itself, and tracks changes to the contets and order of the sequence. 
      @author Andrew McCallum */
  abstract class SeqVariable[X](sequence: Seq[X]) extends Variable with TypedValues with Seq[X] {
    def this() = this(Nil)
    type ValueType = X
    type VariableType <: SeqVariable[X]
    //class XList[X](var elem:X, var prev:XList[X], var next:XList[X]) extends DoubleLinkedList[X,XList[X]] {
    //this(xs:Seq[X]) = this(xs.first, null, new XList(xs.drop(1)))
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
    def elements = seq.elements
    def apply(index: Int) = seq(index)
    // for changes without Diff tracking
    def +=(x:X) = seq += x
    def ++=(xs:Iterable[X]) = seq ++= xs
  }

  /** A variable containing a mutable (but untracked by Diff) sequence of variables; used in conjunction with VarInSeq.
      @author Andrew McCallum */
  trait VariableSeq[V <: Variable with VarInTypedSeq[V,_]] extends RandomAccessSeq[V] with Variable {
    private val seq = new ArrayBuffer[V]
    def +=(v: V) = {
      if (v.seq != null) throw new Error("Trying to add VarInSeq that is already assigned to another VariableSeq")
      seq += v
      v.setSeqPos(this, seq.size - 1)
    }
    def +(v: V) = {this += v; this} // TODO But according to Scala convension this should create a return a new sequence, right?  Remove this method?
    def ++=(vs: Iterable[V]) = vs.foreach(this += _)
    def ++(vs: Iterable[V]) = {this ++= vs; this}
    override def elements = seq.elements
    def length = seq.length
    def apply(i: Int) = seq.apply(i)
  }


  /** For use with variables that have immutable-valued .next and .prev in a sequence. 
      Deprecated.  Use VarInSeq instead. */
  @deprecated
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

  
  /** For use with variables that have immutable-valued .next and .prev in a sequence. 
      @author Andrew McCallum */
  trait VarInTypedSeq[V >: Null <: VarInTypedSeq[V,S] with Variable, S<:Seq[V]] {
    this: V =>
    private var _seq: S = _
    def seq: S = _seq
    def seqAfter = seq.drop(position+1)
    def seqBefore = seq.take(position)
    var position = -1
    def setSeqPos(s: Seq[V], p: Int) = {
      if (s(p) != this) throw new Error
      _seq = s.asInstanceOf[S]
      position = p
    }
    def hasNext = if (position == -1) throw new IllegalStateException("VarInSeq position not yet set") else seq != null && position + 1 < seq.length
    def next: V = if (position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (position + 1 < seq.length) seq(position + 1) else null
    def hasPrev = if (position == -1) throw new IllegalStateException("VarInSeq position not yet set") else seq != null && position > 0
    def prev: V = if (position == -1) throw new IllegalStateException("VarInSeq position not yet set") else if (position > 0) seq(position - 1) else null
    def next(n:Int): V = { 
      if (position == -1) throw new IllegalStateException("VarInSeq position not yet set")
      val i = position + n
      if (i >= 0 && i < seq.length) seq(i) else null
    }
    def prev(n:Int): V = {
      if (position == -1) throw new IllegalStateException("VarInSeq position not yet set") 
      val i = position - n
      if (i >= 0 && i < seq.length) seq(i) else null
    }
    def prevWindow(n:Int): Seq[V] = for (i <- Math.max(position-n, 0) to Math.max(position-1,0)) yield seq(i)
    def nextWindow(n:Int): Seq[V] = for (i <- Math.min(position+1, seq.length-1) to Math.min(position+n, seq.length-1)) yield seq(i)
    def window(n:Int): Seq[V] = for (i <- Math.max(position-n,0) to Math.min(position+n,seq.length-1)) yield seq(i)
    def windowWithoutSelf(n:Int): Seq[V] = for (i <- Math.max(position-n,0) to Math.min(position+n,seq.length-1); if (i != position)) yield seq(i)
    def between(other:V): Seq[V] = {
      assert (other.seq == seq)
      if (other.position > position)
        for (i <- position until other.position) yield seq(i)
      else
        for (i <- other.position until position) yield seq(i)
    } 
    def firstInSeq = seq(0)
  }
  
  trait VarInSeq[V >: Null <: VarInSeq[V] with Variable] extends VarInTypedSeq[V,Seq[V]] {
    this: V =>
  }
 
  /*trait VarInMutableSeq[This >: Null <: VarInMutableSeq[This]] extends cc.factorie.util.DLinkedList[VarInMutableSeq[This]] {
    this : This =>
  }*/
 
  /** For variables that have mutable-valued .next and .prev in a sequence.  Currently only change operation is 'swapWithVar', but more could be added.
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
      def variable: This = if (!done) VarInMutableSeq.this else null
      def redo = { assert(!done); done = true; remove }
      def undo = { assert(done); done = false; if (prevElt ne null) prevElt.postInsert(variable) else nextElt.preInsert(variable) }
    }
    case class VarInMutableSeqPreInsertDiff(that:This)(implicit d:DiffList) extends Diff {
      if (d ne null) d += this
      var done = false
      redo
      def variable: This = if (done) VarInMutableSeq.this else null
      def redo = { assert(!done); done = true; preInsert(that) }
      def undo = { assert(done); done = false; that.remove }
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

