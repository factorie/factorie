/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
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

  abstract class ImmutableSpanVariable[X](val parent: Seq[X], initStart: Int, initLength: Int) extends Variable with TypedValues with RandomAccessSeq[X] {
    type ValueType = X
    type VariableType <: SpanVariable[X]
    assert(initStart >= 0)
    assert(initLength > 0)
    assert(initStart + initLength <= parent.length)
    override def elements = new Iterator[X] {
      var i = _start
      def hasNext = i < _start + _length
      def next: X = {i += 1; parent(i - 1)}
    }
    def apply(i: Int) = parent(i + _start)
    protected var _start = initStart
    def start = _start
    def end = _start + _length - 1
    protected var _length = initLength
    def length = _length
    def isAtStart = _start == 0

    def overlaps(that: ImmutableSpanVariable[_]) =
      (that.start <= this.start && that.end >= this.start) ||
              (this.start <= that.start && this.end >= that.start)

    def isAtEnd = _start + _length == parent.length
    def successor(i: Int) = parent(_start + _length - 1 + i)
    def predecessor(i: Int) = parent(_start - i)
    def phrase = if (length == 1) this.first.toString else this.foldLeft("")(_ + " " + _.toString).drop(1) // Span as a string
  }

  abstract class SpanVariable[X /*,S<:Seq[X]*/ ](aParent: Seq[X], initStart: Int, initLength: Int)(implicit d: DiffList)
          extends ImmutableSpanVariable(aParent, initStart, initLength)
  {
    //println("Model.this.SpanVariable constructor d.length="+d.length)
    if (d != null) new NewSpanVariable()(d)
    //val nsv : NewSpanVariable = new NewSpanVariable()(d)
    //println("NewSpanVariable "+nsv)
    //println("NewSpanVariable.variable "+nsv.variable)
    //println("Model.this.SpanVariable constructoy d.length="+d.length)
    var present = true
    def diffIfNotPresent = false
    def delete(implicit d: DiffList) = new DeleteSpanVariable()(d)
    def setLength(l: Int)(implicit d: DiffList) = new SetLength(_length, l)
    def trimStart(n: Int)(implicit d: DiffList) = new TrimStart(n)
    def trimEnd(n: Int)(implicit d: DiffList) = new TrimEnd(n)
    def prepend(n: Int)(implicit d: DiffList) = new Prepend(n)
    def append(n: Int)(implicit d: DiffList) = new Append(n)
    def canPrepend(n: Int) = _start >= n
    def canAppend(n: Int) = _start + _length + n <= parent.length
    case class NewSpanVariable(implicit d: DiffList) extends Diff {
      //println("NewSpanVariable d.length="+d.length)
      var done = false
      if (d != null) d += this
      redo
      def variable: SpanVariable[X] = {if (done || diffIfNotPresent) SpanVariable.this else null}
      def redo = {assert(!done); done = true; present = true; }
      def undo = {assert(done); done = false; present = false}
      override def toString = "NewSpanVariable " + variable
    }
    case class DeleteSpanVariable(implicit d: DiffList) extends AutoDiff {
      var done = false
      def variable: SpanVariable[X] = if (done && !diffIfNotPresent) null else SpanVariable.this
      def redo = {assert(!done); done = true; present = false}
      def undo = {assert(done); done = false; present = true}
    }
    case class SetStart(oldStart: Int, newStart: Int)(implicit d: DiffList) extends AutoDiff {
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = _start = newStart
      def undo = _start = oldStart
    }
    case class SetLength(oldLength: Int, newLength: Int)(implicit d: DiffList) extends AutoDiff {
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = _length = newLength
      def undo = _length = oldLength
    }
    case class TrimStart(n: Int)(implicit d: DiffList) extends AutoDiff {
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = {assert(n < _length); _start += n; _length -= n}
      def undo = {assert(_start - n >= 0); _start -= n; _length += n}
    }
    case class TrimEnd(n: Int)(implicit d: DiffList) extends AutoDiff {
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = {assert(n < _length); _length -= n}
      def undo = _length += n
    }
    case class Prepend(n: Int)(implicit d: DiffList) extends AutoDiff {
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = {assert(canPrepend(n)); _start -= n; _length += n}
      def undo = {_start += n; _length -= n}
    }
    case class Append(n: Int)(implicit d: DiffList) extends AutoDiff {
      //if (!canAppend(n)) { println("Append n="+n+" start="+variable.start+" length="+variable.length+" parent.length="+variable.parent.length) }
      def variable = if (present || diffIfNotPresent) SpanVariable.this else null
      def redo = {assert(canAppend(n)); _length += n}
      def undo = _length -= n
    }
  }

  /* A variable containing a mutable sequence of other variables.  
  *  This variable stores the sequence itself, and tracks changes to the contets and order of the sequence. */
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

  /**A variable containing a mutable (but untracked by Diff) sequence of variables; used in conjunction with VarInSeq */
  class VariableSeq[V <: Variable with VarInSeq[V]] extends RandomAccessSeq[V] with Variable {
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

  class VariableSeqWithSpans[X <: Variable with VarInSeq[X]] extends VariableSeq[X] {
    type SpanType >: Null <: SpanVariable[X]
    private val _spans = new ListBuffer[SpanType]
    def spans: Seq[SpanType] = _spans
    def spans(index: Int): Iterable[SpanType] = _spans.filter(s => s.start <= index && index < (s.start + s.length))
    def spansStartingAt(index: Int): Iterable[SpanType] = _spans.filter(s => s.start == index)
    abstract class SpanVariableInSeq(initStart: Int, initLength: Int)(implicit d: DiffList) extends SpanVariable[X](VariableSeqWithSpans.this, initStart, initLength)(d)
    {
      //this : SpanType =>
      protected def thisSpan: SpanType = this.asInstanceOf[SpanType] // TODO is there some cleaner way to get SpanVariable.this inside the Diff classes below?
      if (d != null) AddSpanVariable()(d)
      override def delete(implicit d: DiffList) = {RemoveSpanVariable()(d); val a = super.delete; a}
      // Needs def trueScore to not be abstract
      case class AddSpanVariable(implicit d: DiffList) extends Diff {
        var done = false
        if (d != null) d += this
        redo
        def variable = {if (done) thisSpan else null} // or VariableSeqWithSpans[X].this?
        def redo = {_spans.prepend(thisSpan); assert(!done); done = true}
        def undo = {_spans.-=(thisSpan); assert(done); done = false}
        override def toString = "AddSpanVariable variable " + variable
      }
      case class RemoveSpanVariable(implicit d: DiffList) extends Diff {
        var done = false
        if (d != null) d += this
        redo
        def variable = if (done) null else thisSpan // or VariableSeqWithSpans[X].this?
        def redo = {_spans.-=(thisSpan); assert(!done); done = true}
        def undo = {_spans.prepend(thisSpan); assert(done); done = false}
        override def toString = "RemoveSpanVariable variable " + variable
      }
    }
  }

  /**For use with variables that have immutable-valued .next and .prev in a sequence. */
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

  /**For use with variables that have immutable-valued .next and .prev in a sequence. */
  trait VarInSeq[V >: Null <: Variable] {
    this: Variable =>
    var seq: Seq[V] = null
    def seqAfter = seq.drop(position+1)
    def seqBefore = seq.take(position)
    var position = -1
    def setSeqPos(s: Seq[V], p: Int) = {
      if (s(p) != this) throw new Error
      seq = s.asInstanceOf[Seq[V]]
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
    def first = seq(0)
  }
 
  /*trait VarInMutableSeq[This >: Null <: VarInMutableSeq[This]] extends cc.factorie.util.DLinkedList[VarInMutableSeq[This]] {
    this : This =>
  }*/
 
  trait VarInMutableSeq[This >: Null <: VarInMutableSeq[This] with cc.factorie.util.LinkList[This] with Variable] extends cc.factorie.util.LinkList[This] {
    this : This =>
    def swapWithVar(that:This)(implicit d:DiffList) : Unit = {
      this.swapWith(that)
      if (d ne null) {
        d += new VarInMutableSeqSwapDiff(this, that)
        d += new VarInMutableSeqSwapDiff2(that)
      }
    }
    case class VarInMutableSeqSwapDiff(ths:This, that:This) extends Diff {
      def variable: This = VarInMutableSeq.this
      def variables = List(ths, that) // TODO Consider handling this in the FACTORIE library
      def redo = ths.swapWith(that)
      def undo = redo
    }
    case class VarInMutableSeqSwapDiff2(that:This) extends Diff {
      def variable: This = that  // Just to put another variable on the difflist
      def redo = {}
      def undo = {}
    }
  }

  // TODO Various tests below.  Remove them.
 /*
  class Elt2(val f:Int) extends cc.factorie.util.DLinkedList[Elt2];
  trait Elt3 extends cc.factorie.util.DLinkedList[Elt3];
  trait Elt4[N] extends cc.factorie.util.DLinkedList[Elt4[N]];
  trait Elt5[This<:Elt5[This]] extends cc.factorie.util.DLinkedList[Elt5[This]];
  trait Elt6[This >: Null <:Elt5[This] with cc.factorie.util.DLinkedList[This]] extends cc.factorie.util.DLinkedList[This] {
    this : This =>
  }
  class Elt extends DoubleLinkedList[Elt,Elt] {
    var prev : Elt = null
    var next : Elt = null
    var elem : Elt = this
  }*/

  /*trait VarInTypedSeq[X,S<:Seq[X]] extends VarInSeq {
  this : Variable =>
  override def seq : S = super.seq.asInstanceOf[S]
}*/

