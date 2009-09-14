package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging}
import cc.factorie.util.Implicits._

	/**A variable whose value is a set of other variables */
	abstract class SetVariable[X]() extends Variable with TypedVariable {
		type ValueType = X
		type VariableType <: SetVariable[X]
		private val _members = new HashSet[X]
		def members: Iterable[X] = _members
		def size = _members.size
		def add(x: X)(implicit d: DiffList): Unit = if (!_members.contains(x)) {
			if (d != null) d += new SetVariableAddDiff(x)
			_members += x
		}
		def remove(x: X)(implicit d: DiffList): Unit = if (_members.contains(x)) {
			if (d != null) d += new SetVariableRemoveDiff(x)
			_members -= x
		}
		case class SetVariableAddDiff(added: X) extends Diff {
			//        Console.println ("new SetVariableAddDiff added="+added)
			def variable: SetVariable[X] = SetVariable.this
			def redo = _members += added //if (_members.contains(added)) throw new Error else
			def undo = _members -= added
		}
		case class SetVariableRemoveDiff(removed: X) extends Diff {
			//        Console.println ("new SetVariableRemoveDiff removed="+removed)
			def variable: SetVariable[X] = SetVariable.this
			def redo = _members -= removed
			def undo = _members += removed //if (_members.contains(removed)) throw new Error else
		}
	}

	abstract class ImmutableSpanVariable[X](val parent: Seq[X], initStart: Int, initLength: Int) extends Variable with TypedVariable with RandomAccessSeq[X] {
		type ValueType = X
		type VariableType <: SpanVariable[X]
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
			def redo = {assert(n < _length); assert(_start - n >= 0); _start += n; _length -= n}
			def undo = {_start -= n; _length += n}
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

	/* A variable containing a mutable sequence of other variables.  This variable stores the sequence itself. */
	abstract class SeqVariable[X](sequence: Seq[X]) extends Variable with TypedVariable with Seq[X] {
		type ValueType = X
		type VariableType <: SeqVariable[X]
		//class XList[X](var elem:X, var prev:XList[X], var next:XList[X]) extends DoubleLinkedList[X,XList[X]] {
		//this(xs:Seq[X]) = this(xs.first, null, new XList(xs.drop(1)))
		//def prepend(x:X) : XList[X] = { val first = new XList(x, null, this); this.prev = first; first }
		//}
		private val seq = new ListBuffer[X]() ++ sequence
		def append(x: X)(implicit d: DiffList) = {if (d != null) d += AppendDiff(x); seq.append(x)}
		def prepend(x: X)(implicit d: DiffList) = {if (d != null) d += PrependDiff(x); seq.prepend(x)}
		def trimStart(n: Int)(implicit d: DiffList) = {if (d != null) d += TrimStartDiff(n); seq.trimStart(n)}
		def trimEnd(n: Int)(implicit d: DiffList) = {if (d != null) d += TrimEndDiff(n); seq.trimEnd(n)}
		//def split(n:Int)(implicit d:DiffList) = { if (d != null) d += SplitDiff(n); val seq2 = seq.drop(n); trimStart(seq.length-n); seq2 }
		trait SeqVariableDiff extends Diff {override def variable = SeqVariable.this}
		case class AppendDiff(x: X) extends SeqVariableDiff {def undo = seq.trimEnd(1); def redo = seq.append(x)}
		case class PrependDiff(x: X) extends SeqVariableDiff {def undo = seq.trimStart(1); def redo = seq.prepend(x)}
		case class TrimStartDiff(n: Int) extends SeqVariableDiff {val s = seq.take(n); def undo = seq prependAll (s); def redo = seq.trimStart(n)}
		case class TrimEndDiff(n: Int) extends SeqVariableDiff {val s = seq.drop(seq.length - n); def undo = seq appendAll (s); def redo = seq.trimEnd(n)}
		// for Seq trait
		def length = seq.length
		def elements = seq.elements
		def apply(index: Int) = seq(index)
	}

	/**A variable containing a mutable (but untracked by Diff) sequence of variables; used in conjunction with VarInSeq */
	class VariableSeq[V <: Variable with VarInSeq[V]] extends RandomAccessSeq[V] with Variable {
		private val seq = new ArrayBuffer[V]
		def +=(v: V) = {
			seq += v
			v.setSeqPos(this, seq.size - 1)
		}
		def +(v: V) = {this += v; this}
		def ++=(vs: Iterable[V]) = vs.foreach(this += _)
		def ++(vs: Iterable[V]) = {this ++= vs; this}
		override def elements = seq.elements
		def length = seq.length
		def apply(i: Int) = seq.apply(i)
		//type ValueType = V
		def trueScore = 0.0 // Changes to this variable are not tracked by Diffs and they have no truth
	}

	class VariableSeqWithSpans[X <: Variable with VarInSeq[X]] extends VariableSeq[X] {
		type SpanType >: Null <: SpanVariable[X]
		private val _spans = new ListBuffer[SpanType]
		def spans: Seq[SpanType] = _spans
		def spans(index: Int): Iterable[SpanType] = _spans.filter(s => s.start <= index && index < (s.start + s.length))
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
		var position = -1
		def setSeqPos(s: Seq[V], p: Int) = {
			if (s(p) != this) throw new Error
			seq = s.asInstanceOf[Seq[V]]
			position = p
		}
		def hasNext = seq != null && position + 1 < seq.length
		def next: V = if (position + 1 < seq.length) seq(position + 1) else null
		def hasPrev = seq != null && position > 0
		def prev: V = if (position > 0) seq(position - 1) else null
	}

	/*trait VarInTypedSeq[X,S<:Seq[X]] extends VarInSeq {
	this : Variable =>
	override def seq : S = super.seq.asInstanceOf[S]
}*/

