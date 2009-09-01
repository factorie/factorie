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

trait Variables requires Model {
  //this : Model =>
  //this : Model with Domains with Templates =>

	/**Abstract superclass of all variables.  Don't need to know its value type to use it. */
	trait Variable {
		type VariableType <: Variable
		def domain: Domain[VariableType] = Domain[VariableType](this.getClass)
		def trueScore: Double
		// TODO sometimes trueScores on variables might not be enough; we should consider trueScores in Template
		private def shortClassName = {
			val fields = this.getClass.getName.split('$')
			if (fields.last == "class")
				fields(fields.length - 2)
			else
				fields.last
		}
		def printName = shortClassName
		override def toString = printName + "(_)"
		def isConstant = false
		def factors: Iterable[Factor] = {
			var factors = new HashSet[Factor]
			// unroll all factors touching this variable
			modelTemplates.foreach(_.asInstanceOf[Neighbors].unroll0(this).foreach(f => factors += f))
			factors.toSeq
		}
	}

	/**For variables whose value has a type stored in type ValueType */
	trait TypedVariable extends Variable {
		type ValueType
	}

	/**A Variable with a Domain different than that of this.getClass, instead specified by constructor argument */
	// TODO would like to make this a trait later when traits can take constructor arguments
	abstract class VariableWithDomain[D <: TypedVariable](implicit variableOfDomain: Manifest[D]) extends TypedVariable {
		// put this.getClass in the Domain global hash.  // TODO but no need to do it again and again!
		Domain.set(this.getClass, Domain[D](variableOfDomain))
		override def domain: Domain[VariableType] = Domain[VariableType](variableOfDomain.erasure)
	}

	/**An IndexedVariable with a Domain different than that of this.getClass, instead specified by constructor argument */
	// TODO would like to make this a trait later when traits can take constructor arguments
	abstract class IndexedVariableWithDomain[D <: IndexedVariable](implicit variableOfDomain: Manifest[D]) extends IndexedVariable {
		// put this.getClass in the Domain global hash.  // TODO but no need to do it again and again!
		Domain.set(this.getClass, Domain[D](variableOfDomain))
		override def domain: IndexedDomain[VariableType] = IndexedDomain.get[VariableType](variableOfDomain.erasure)
	}


	/**For Variables that hold their list of Factors */
	trait FactorList {
		this: Variable =>
		private var factorList: List[Factor] = Nil
		def addFactor(f: Factor) = factorList = f :: factorList
		def clearFactors = factorList = Nil
		def factors: Iterable[Factor] = factorList
	}

	// TODO remove this now that we have Proposer
	/**A variable that can provide an Iterator over all of its values. */
	trait IterableValues[T] {
		// TODO Inherit from TypedVariable instead?
		this: Variable =>
		/**Values this variable could take on. */
		def iterableValues: Iterable[T]
		/**Possible alternative values, that is, values other than its current value. */
		def iterableOtherValues: Iterable[T]
	}

	// TODO remove this now that we have Proposer?
	/**A variable that can iterate over its possible configurations */
	trait IterableSettings {
		this: Variable =>
		/**Return an iterator over some/all configurations of this variable, each time returning simply the same Variable object. */
		def iterator: Iterator[this.type]
	}


	/**A variable with a single mutable (unindexed) value */
	abstract class PrimitiveVariable[T](initval: T) extends Variable with TypedVariable {
		type VariableType <: PrimitiveVariable[T]
		type ValueType = T
		protected var _value: T = _
		set(initval)(null) // initialize with method call because subclasses may do coordination in overridden set()()
		def value = _value

		def set(newValue: T)(implicit d: DiffList): Unit =
			if (newValue != _value) {
				if (d != null) d += new PrimitiveDiff(_value, newValue)
				_value = newValue
			}
		// Should we implement equals to compare values??
		// No, I don't think so because we might need to put multiple variables with the same values in a HashMap
		// But we can implement our own specialized equality method... (the shorter === overlaps with an implicit conversion in scalatest)
		def ====(other: PrimitiveVariable[T]) = _value == other._value
		def !===(other: PrimitiveVariable[T]) = _value != other._value
		override def toString = printName + "(" + value.toString + ")"
		case class PrimitiveDiff(oldValue: T, newValue: T) extends Diff {
			//        Console.println ("new PrimitiveDiff old="+oldValue+" new="+newValue)
			def variable: PrimitiveVariable[T] = PrimitiveVariable.this
			def redo = set(newValue)(null)
			def undo = set(oldValue)(null)
		}
	}

	trait PrimitiveTrueValue[T] {
		this: PrimitiveVariable[T] =>
		var trueValue: T = _
		def trueScore: Double = if (_value == trueValue) 1.0 else 0.0
		def isUnlabeled = trueValue == _
	}

	/**For use with variables whose values are mapped to dense integers */
	trait IndexedVariable extends Variable with TypedVariable {
		type VariableType <: IndexedVariable
		override def domain: IndexedDomain[VariableType] = IndexedDomain.get[VariableType](this.getClass)
		override def isConstant = true
		def vector: Vector
		// TODO These next methods are efficient for cycling through all values,
		// but perhaps should be simply collapsed into IterableValues or MultiProposer -akm
		def setFirstValue: Unit = throw new Error("Cannot set constant IndexedVariable")
		def hasNextValue = false
		def setNextValue: Unit = {}
	}

	trait SingleIndexedVariable[T] extends IndexedVariable with Proposer with MultiProposer {
		type VariableType <: SingleIndexedVariable[T]
		type ValueType = T
		//override def domain : IndexedDomain[VariableType] = IndexedDomain[VariableType](this.getClass)
		protected var indx = -1 //domain.index(initval) // but this provides no way to initialize with null
		// TODO Consider changing this method name to just "set"?  But then will code readers more easily get confused?
		def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
			if (newIndex < 0) throw new Error("SingleIndexedVariable setByIndex can't be negative.")
			if (newIndex != indx) {
				if (d != null) d += new SingleIndexedDiff(indx, newIndex)
				indx = newIndex
			}
		}
		def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
		override def propose(implicit d: DiffList) = {setByIndex(random.nextInt(domain.size)); 0.0}
		def multiPropose(difflist: DiffList) = for (i <- 0 until domain.size) yield {
			new AutoProposal(diff => setByIndex(i)(diff))
			// val d = new DiffList; setByIndex(i)(d); new CaseProposal(d.scoreAndUndo, d)
		}
		def index = indx
		def value: T = domain.get(indx)
		/**Tests equality of variable values, whereas == tests equality of variable objects themselves. */
		// TODO But these method names conflict with scalatest assert implicit conversions
		//def ===(other:SingleIndexedVariable[T]) = indx == other.indx
		//def !===(other:SingleIndexedVariable[T]) = indx != other.indx
		override def toString = printName + "(" + value.toString + "=" + indx + ")"
		override def vector = new SingletonBinaryVector(domain.allocSize, indx)
		def ====(other: SingleIndexedVariable[T]) = indx == other.indx
		def !===(other: SingleIndexedVariable[T]) = indx != other.indx
		case class SingleIndexedDiff(oldIndex: Int, newIndex: Int) extends Diff {
			def variable: SingleIndexedVariable[T] = SingleIndexedVariable.this
			def redo = setByIndex(newIndex)(null)
			def undo = setByIndex(oldIndex)(null)
		}
	}

	/**A variable whose value is a single indexed value; mutable */
	trait CoordinatedEnumVariable[T] extends SingleIndexedVariable[T] {
		type VariableType <: CoordinatedEnumVariable[T]
		override def domain: IndexedDomain[VariableType] = IndexedDomain.get[VariableType](this.getClass)
		// initialize the variable's value; using this method in case coordination in necessary
		//setByIndex(domain.index(initval))(null)
	}

	/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value, and whose trueScore is 1.0 iff they are equal and 0.0 otherwise. */
	trait TrueIndexedValue[T] {
		this: SingleIndexedVariable[T] =>
		/**The index of the true labeled value for this variable.  If unlabeled, set to -1 */
		var trueIndex: Int
		//private var _trueValue:T = domain.get(trueIndex)
		def trueValue = if (trueIndex >= 0) domain.get(trueIndex) else null // _trueValue
		def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
		def trueScore: Double = if (trueIndex >= 0 && indx == trueIndex) 1.0 else 0.0
		def isUnlabeled = trueIndex < 0
		def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex else throw new Error("Already unlabeled.")
	}

	/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method.  Ensuring no coordination is necessary for optimization of belief propagation. */
	abstract class EnumVariable[T](trueval:T) extends CoordinatedEnumVariable[T] with TrueIndexedValue[T] with IterableValues[T] {
		var trueIndex = domain.index(trueval)
		setByIndex(domain.index(trueval))(null)
		override final def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
		override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
		override def setFirstValue: Unit = setByIndex(0)(null)
		override def hasNextValue = indx < domain.size - 1
		override def isConstant = false
		override def setNextValue: Unit = if (hasNextValue) setByIndex(indx + 1)(null) else throw new Error("No next value")
		def iterableValues: Iterable[T] = domain
		def iterableOtherValues: Iterable[T] = domain.filter(_ != value)
	}

	/** The value of a Label variable.  
    * Previously we simply used String values in a EnumVariable, but here LabelValues can be compared much more efficiently than Strings. */
	trait LabelValue {
		def index: Int
		def domain: LabelDomain[_]
		def entry: String = domain.getString(index)
	}

	/**A variable whose value is a LabelValue, which in turn can be
	created or indexed through a String.  LabelValues can be
	efficiently compared. */
	//class Label(initval:String) extends SingleIndexedVariable[LabelValue] with TrueIndexedValue[LabelValue] with IterableValues[LabelValue] {
	class CoordinatedLabel(trueval: String) extends CoordinatedEnumVariable[LabelValue] with TrueIndexedValue[LabelValue] with IterableValues[LabelValue] {
		def this(trueval: LabelValue) = this (trueval.entry)
		var trueIndex = domain.index(trueval)
		setByIndex(domain.index(trueval))(null)
		//def this(initval:String) = this(LabelDomain.get[Label](this/*.getClass*/).get(initval))
		type VariableType <: CoordinatedLabel
		override def domain: LabelDomain[VariableType] = LabelDomain.get[VariableType](this.getClass)
		override def isConstant = false
		def set(s: String)(implicit d: DiffList) = setByIndex(domain.index(s))
		override def setFirstValue: Unit = setByIndex(0)(null)
		override def hasNextValue = indx < domain.size - 1
		override def setNextValue: Unit = if (hasNextValue) setByIndex(indx + 1)(null) else throw new Error("No next value")
		def iterableValues: Iterable[LabelValue] = domain
		def iterableOtherValues: Iterable[LabelValue] = domain.filter(_ != value)
		//type ValueType <: LabelDomain[VariableType]#Value
		/*trait Value {
	def index : Int
g      def domain : LabelDomain[_<:Label]
	def entry : String
}*/
		override def toString = printName + "(Value=" + value.entry + "=" + indx + ")"
		def ====(other: CoordinatedLabel) = value.index == other.value.index
		def !===(other: CoordinatedLabel) = value.index != other.value.index
	}

	class Label(trueval: String) extends CoordinatedLabel(trueval) {
		override final def set(newValue: String)(implicit d: DiffList) = super.set(newValue)(d)
		override final def set(newValue: LabelValue)(implicit d: DiffList) = super.set(newValue)(d)
		override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
	}

 
	/**A variable whose value is a SparseBinaryVector; immutable. */
	abstract class VectorVariable[T] extends IndexedVariable {
		type ValueType = T
		type VariableType <: VectorVariable[T]
		//def this (es:T*) = this(es.toArray)   TODO include this again later
		def trueScore: Double = 0 // VectorVariable is typically observed; doesn't have a trueValue
		override def isConstant = true
		protected var indxs = new ArrayBuffer[Int]()
		private var _vector: Vector = null // Can we make this more memory efficient?  Avoid having both Vector and ArrayBuffer?
		override def vector = {
			if (_vector == null || _vector.size != domain.allocSize) {
				val indices = indxs.toArray
				Sorting.quickSort(indices)
				_vector = new SparseBinaryVector(domain.allocSize, indices)
			}
			_vector
		}
		def +=(value: T) = {
			val idx = domain.index(value);
			if (idx == IndexedDomain.NULL_INDEX) throw new Error("VectorVariable += value " + value + " not found in domain " + domain)
			indxs += domain.index(value)
			_vector = null
		}
		def +(value: T) = {this += value; this}
		def ++=(values: Iterable[T]) = values.foreach(v => this += v)
		def ++(values: Iterable[T]) = {this ++= values; this}
		override def toString = printName + "(" + vector.activeDomain.foldLeft("")((s, i) => s + domain.get(i).toString + "=" + i + ",") + ")"
	}

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


	/**A variable class for boolean values, defined here for convenience.  If you have several different "types" of booleans, you might want to subclass this to enable type safety checks. */
	class Bool(b: Boolean) extends EnumVariable(b) {
		type VariableType = Bool

		def :=(b: Boolean) = set(b)(null)
	}

	object Bool {
		val t = new Bool(true)
		val f = new Bool(false)

		def apply(b: Boolean) = if (b) t else f
	}

	/**A variable class for real values. */
	class Real(v: Double) extends PrimitiveVariable(v) {
		type VariableType = Real

		def trueScore = 0.0
	}

	/* TODO Consider adding such a thing
class IntRange(i:Int) extends IndexedVariable {
	type VariableType = IntRange
	def trueScore = 0.0
}*/

	/**A variable class for string values. */
	class StringVariable(str: String) extends PrimitiveVariable(str) {
		type VariableType = StringVariable

		def trueScore = 0.0
	}

}
