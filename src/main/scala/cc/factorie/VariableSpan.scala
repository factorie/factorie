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

// Variables for dealing with spans of sequences

abstract class SpanValue[T](val seq: Seq[T], initStart: Int, initLength: Int) extends Variable with TypedValues with RandomAccessSeq[T] {
	type ValueType = T
	type VariableType <: SpanVariable[T];
	assert(initStart >= 0)
	assert(initLength > 0)
	assert(initStart + initLength <= seq.length)
	override def elements = new Iterator[T] {
		var i = _start
		def hasNext = i < _start + _length
		def next: T = {i += 1; seq(i - 1)}
	}
	def apply(i: Int) = seq(i + _start)
	protected var _start = initStart
	def start = _start
	def end = _start + _length - 1
	protected var _length = initLength
	def length = _length
	def isAtStart = _start == 0

	def overlaps(that: SpanValue[T]) =
		(that.start <= this.start && that.end >= this.start) ||
		(this.start <= that.start && this.end >= that.start)

	def isAtEnd = _start + _length == seq.length
	def successor(i: Int) = seq(_start + _length - 1 + i)
	def predecessor(i: Int) = seq(_start - i)
	/** Return a String representation of the span */
	def phrase = if (length == 1) this.first.toString else this.mkString(" ")
}

  
abstract class SpanVariable[T](seq: Seq[T], initStart: Int, initLength: Int)(implicit d: DiffList) extends SpanValue(seq, initStart, initLength) {
	//println("Model.this.SpanVariable constructor d.length="+d.length)
	if (d != null) new NewSpanVariable()(d)
	seq match { case s:VariableSeqWithSpans[T,SpanVariable[T]] => s.addSpan(this) }
	//val nsv : NewSpanVariable = new NewSpanVariable()(d)
	//println("NewSpanVariable "+nsv)
	//println("NewSpanVariable.variable "+nsv.variable)
	//println("Model.this.SpanVariable constructoy d.length="+d.length)
	var present = true
	/** If true, this SpanVariable will be scored by a difflist, even if it is in its deleted non-"present" state. */
	def diffIfNotPresent = false
	def delete(implicit d: DiffList) = {
    new DeleteSpanVariable()(d)
    seq match { case s:VariableSeqWithSpans[T,SpanVariable[T]] => s.removeSpan(this) }
	}
	def setLength(l: Int)(implicit d: DiffList) = new SetLength(_length, l)
	def trimStart(n: Int)(implicit d: DiffList) = new TrimStart(n)
	def trimEnd(n: Int)(implicit d: DiffList) = new TrimEnd(n)
	def prepend(n: Int)(implicit d: DiffList) = new Prepend(n)
	def append(n: Int)(implicit d: DiffList) = new Append(n)
	def canPrepend(n: Int) = _start >= n
	def canAppend(n: Int) = _start + _length + n <= seq.length
	case class NewSpanVariable(implicit d: DiffList) extends AutoDiff {
		//println("NewSpanVariable d.length="+d.length)
		var done = false
		def variable: SpanVariable[T] = {if (done || diffIfNotPresent) SpanVariable.this else null}
		def redo = {assert(!done); done = true; present = true }
		def undo = {assert(done); done = false; present = false}
		override def toString = "NewSpanVariable("+SpanVariable.this+")"
	}
	case class DeleteSpanVariable(implicit d: DiffList) extends AutoDiff {
		var done = false
		def variable: SpanVariable[T] = if (done && !diffIfNotPresent) null else SpanVariable.this
		def redo = { assert(!done); done = true; present = false }
		def undo = { assert(done); done = false; present = true }
		override def toString = "DeleteSpanVariable("+SpanVariable.this+")"
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


class VariableSeqWithSpans[T <: Variable with VarInTypedSeq[T,_],S<:SpanVariable[T]] extends VariableSeq[T] {
  private val _spans = new ListBuffer[S];
  def spans: Seq[S] = _spans
  def spansContaining(position: Int): Iterable[S] = _spans.filter(s => s.start <= position && position < (s.start + s.length))
  def spansStartingAt(position: Int): Iterable[S] = _spans.filter(s => s.start == position)
  def addSpan(s:S)(implicit d:DiffList): Unit = {
    require(s.seq == this)
    AddSpanVariable(s)
  }
  /** Remove the span from the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from SpanVariable.delete. */
  def removeSpan(s:S)(implicit d:DiffList): Unit = {
    require(s.seq == this)
    RemoveSpanVariable(s)
  }
  /** Remove the span from the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from the SpanVariable constructor. */
	case class AddSpanVariable(span:S)(implicit d: DiffList) extends AutoDiff {
  	var done = false
  	def variable: S = if (done) span else null.asInstanceOf[S]
  	def redo = { _spans.prepend(span); assert(!done); done = true }
  	def undo = { _spans.-=(span); assert(done); done = false }
  	override def toString = "AddSpanVariable("+variable+")"
  }
	case class RemoveSpanVariable(span:S)(implicit d: DiffList) extends AutoDiff {
		var done = false
		def variable: S = if (done) null.asInstanceOf[S] else span
		def redo = { _spans.-=(span); assert(!done); done = true }
		def undo = { _spans.prepend(span); assert(done); done = false }
		override def toString = "RemoveSpanVariable("+variable+")"
	}
}

@deprecated
class VariableSeqWithSpansOld[X <: Variable with VarInSeq[X]] extends VariableSeq[X] {
	type SpanType >: Null <: SpanVariable[X];
	private val _spans = new ListBuffer[SpanType];
	def spans: Seq[SpanType] = _spans
	def spansContaining(index: Int): Iterable[SpanType] = _spans.filter(s => s.start <= index && index < (s.start + s.length))
	def spansStartingAt(index: Int): Iterable[SpanType] = _spans.filter(s => s.start == index)
	abstract class SpanVariableInSeq(initStart: Int, initLength: Int)(implicit d: DiffList) extends SpanVariable[X](VariableSeqWithSpansOld.this, initStart, initLength)(d) {
		//this : SpanType =>
		protected def thisSpan: SpanType = this.asInstanceOf[SpanType] // TODO is there some cleaner way to get SpanVariable.this inside the Diff classes below?
		if (d != null) AddSpanVariable()(d)
		override def delete(implicit d: DiffList) = {RemoveSpanVariable()(d); val a = super.delete; a}
		case class AddSpanVariable(implicit d: DiffList) extends AutoDiff {
			var done = false
			def variable = {if (done) thisSpan else null} // or VariableSeqWithSpans[X].this?
			def redo = {_spans.prepend(thisSpan); assert(!done); done = true}
			def undo = {_spans.-=(thisSpan); assert(done); done = false}
			override def toString = "AddSpanVariable("+variable.toString+")"
		}
		case class RemoveSpanVariable(implicit d: DiffList) extends AutoDiff {
			var done = false
			def variable = if (done) null else thisSpan // or VariableSeqWithSpans[X].this?
			def redo = {_spans.-=(thisSpan); assert(!done); done = true}
			def undo = {_spans.prepend(thisSpan); assert(done); done = false}
			override def toString = "RemoveSpanVariable("+variable.toString+")"
		}
	}
}

//class LabelSeqWithSpans[T<:Variable { def label:Label }]

