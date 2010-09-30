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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable,DoubleLinkedList}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

// Variables for dealing with spans of sequences

abstract class SpanVar[T](theSeq: Seq[T], initStart: Int, initLength: Int) extends Variable with TypedValues with IndexedSeqEqualsEq[T] {
  type ValueType = T
  type VariableType <: SpanVar[T] //SpanVariable[T];
  assert(initStart >= 0)
  assert(initLength > 0)
  assert(initStart + initLength <= seq.length)
  def seq: Seq[T] = theSeq // Use "def" instead of "val seq" in constructor so that subclasses can override this
  protected var _start = initStart
  protected var _length = initLength
  override def iterator = new Iterator[T] {
    var i = _start
    def hasNext = i < _start + _length
    def next: T = {i += 1; seq(i - 1)}
  }
  def apply(i: Int) = seq(i + _start)
  def start = _start
  def end = _start + _length - 1
  def length = _length
  def isAtStart = _start == 0
  def overlaps(that: SpanVar[T]) = {
    assert(this.seq eq that.seq)
    (that.start <= this.start && that.end >= this.start) ||
    (this.start <= that.start && this.end >= that.start)
  }
  def isAtEnd = _start + _length == seq.length
  def hasSuccessor(i: Int) = (_start + _length - 1 + i) < seq.length
  def hasPredecessor(i: Int) = (_start - i) >= 0
  def successor(i: Int) = if (hasSuccessor(i)) seq(_start + _length - 1 + i) else null.asInstanceOf[T]
  def predecessor(i: Int) = if (hasPredecessor(i)) seq(_start - i) else null.asInstanceOf[T]
  /** Return a String representation of the span */
  def phrase = if (length == 1) this.head.toString else this.mkString(" ")
  def prevWindow(n:Int): Seq[T] = for (i <- math.max(0,start-n) until start) yield seq(i)
  def nextWindow(n:Int): Seq[T] = for (i <- end+1 until math.min(seq.length-1,end+n)) yield seq(i)
  def window(n:Int): Seq[T] = for (i <- math.max(0,start-n) to math.min(seq.length-1,end+n)) yield seq(i)
  def windowWithoutSelf(n:Int): Seq[T] = for (i <- math.max(0,start-n) to math.min(seq.length-1,end+n); if (i < start || i > end)) yield seq(i)
}

  
abstract class SpanVariable[T<:Variable](theSeq: Seq[T], initStart: Int, initLength: Int)(implicit d: DiffList) extends SpanVar(theSeq, initStart, initLength) {
  //println("Model.this.SpanVariable constructor d.length="+d.length)
  if (d != null) new NewSpanVariable()(d)
  //seq match { case s:VariableSeqWithSpans[T,SpanVariable[T]] => s.addSpan(this) }
  // TODO Consider changing type of theSeq in the constructor
  //val nsv : NewSpanVariable = new NewSpanVariable()(d)
  //println("NewSpanVariable "+nsv)
  //println("NewSpanVariable.variable "+nsv.variable)
  //println("Model.this.SpanVariable constructoy d.length="+d.length)
  var present = true
  /** If true, this SpanVariable will be scored by a difflist, even if it is in its deleted non-"present" state. */
  def diffIfNotPresent = false
  def preChange(implicit d:DiffList): Unit = {}
  def postChange(implicit d:DiffList): Unit = {}
  def delete(implicit d: DiffList): Unit = {
    preChange
    new DeleteSpanVariable()(d)
    //seq match { case s:VariableSeqWithSpans[T,SpanVariable[T]] => s.removeSpan(this) }
    //seq.asInstanceOf[VariableSeqWithSpans[T,SpanVariable[T]]].removeSpan(this)
    postChange
  }
  def setLength(l: Int)(implicit d: DiffList): Unit = if (l != length) { preChange; new SetLength(_length, l); postChange }
  def trimStart(n: Int)(implicit d: DiffList): Unit = if (n >= length) this.delete else if (n > 0) { preChange; new TrimStart(n); postChange }
  def trimEnd(n: Int)(implicit d: DiffList): Unit = if (n >= length) this.delete else if (n > 0) { preChange; new TrimEnd(n); postChange }
  def prepend(n: Int)(implicit d: DiffList): Unit = if (n > 0) { preChange; new Prepend(n); postChange }
  def append(n: Int)(implicit d: DiffList): Unit = if (n > 0) { preChange; new Append(n); postChange }
  def canPrepend(n: Int) = _start >= n
  def canAppend(n: Int) = _start + _length + n <= seq.length
  case class NewSpanVariable(implicit d: DiffList) extends Diff {
    // NewSpanVariable cannot be an AutoDiff because of initialization ordering, done will end up false. 
    // TODO But I should get rid of 'done' and just use 'present' instead.
    //println("NewSpanVariable d.length="+d.length)
    var done = false
    if (d != null) d += this
    redo
    def variable: SpanVariable[T] = {if (done || diffIfNotPresent) SpanVariable.this else null}
    def redo = {assert(!done); done = true; present = true }
    def undo = {assert(done); done = false; present = false}
    override def toString = "NewSpanVariable("+SpanVariable.this+")"
  }
  case class DeleteSpanVariable(implicit d: DiffList) extends Diff {
    // cannot be AutoDiff for same reasons as NewSpanVariable
    var done = false
    if (d != null) d += this
    redo
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
    override def toString = "TrimStart("+n+","+SpanVariable.this+")"
  }
  case class TrimEnd(n: Int)(implicit d: DiffList) extends AutoDiff {
    def variable = if (present || diffIfNotPresent) SpanVariable.this else null
    def redo = {assert(n < _length); _length -= n}
    def undo = _length += n
    override def toString = "TrimEnd("+n+","+SpanVariable.this+")"
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
    //override def toString = "Append("+n+","+(how do I reliably get the appended token)+")"
  }
}

/** A SpanVariable that describes a subsequence of a VariableSeqWithSpans, which in turn stores a pointer to this object. */
class SpanVariableInSeq[T >:Null <: Variable with VarInTypedSeq[T,_],S<:SpanVariableInSeq[T,S]](theSeq: VariableSeqWithSpans[T,S], initStart: Int, initLength: Int)(implicit d: DiffList) 
extends SpanVariable[T](theSeq, initStart, initLength) {
  this: S =>
  seq.addSpan(this)
  override def seq: VariableSeqWithSpans[T,S] = theSeq
  override def delete(implicit d: DiffList): Unit = {
    super.delete
    seq.removeSpan(this)
  }
}


trait VariableSeqWithSpans[T >:Null <: Variable with VarInTypedSeq[T,_],S<:SpanVariableInSeq[T,S]] extends VariableSeq[T] {
  private val _spans = new ListBuffer[S];
  def spans: Seq[S] = _spans
  def orderedSpans: Seq[S] = _spans.toList.sortWith((s1,s2) => s1.start < s2.start) // TODO Make this more efficient by avoiding toList
  def spansContaining(position: Int): Iterable[S] = _spans.filter(s => s.start <= position && position < (s.start + s.length))
  def spansStartingAt(position: Int): Iterable[S] = _spans.filter(s => s.start == position)
  def spansEndingAt(position: Int): Iterable[S] = _spans.filter(s => s.start + s.length - 1 == position)
  /** Add the span to the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from the SpanVariable constructor. */
  def addSpan(s:S)(implicit d:DiffList): Unit = {
    require(s.seq == this, "VariableSeqWithSpans.addSpan: span.seq="+s.seq+" != this="+this)
    AddSpanVariable(s)
  }
  /** Remove the span from the list of spans maintained by this VariableSeqWithSpans.
      Typically you would not call this yourself; it is called automatically from SpanVariable.delete. */
  def removeSpan(s:S)(implicit d:DiffList): Unit = {
    require(s.seq == this)
    RemoveSpanVariable(s)
  }
  def clearSpans: Unit = {
    // Make a copy of the collection of spans so its iterator doesn't get confused as we delete them
    spans.toList.foreach(_.delete(null))
  }
  case class AddSpanVariable(span:S)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: S = if (done) span else null.asInstanceOf[S]
    def redo = { _spans.prepend(span); assert(!done); done = true }
    def undo = { _spans.-=(span); assert(done); done = false }
    override def toString = "AddSpanVariable("+span+")"
  }
  case class RemoveSpanVariable(span:S)(implicit d: DiffList) extends Diff {
    // Cannot be an AutoDiff, because of initialization ordering 'done' will end up false
    var done = false
    if (d != null) d += this
    redo
    def variable: S = if (done) null.asInstanceOf[S] else span
    def redo = { _spans.-=(span); assert(!done); done = true }
    def undo = { _spans.prepend(span); assert(done); done = false }
    override def toString = "RemoveSpanVariable("+span+")"
  }
}
