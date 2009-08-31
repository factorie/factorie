/**FACTORIE: Factor graphs, imperative, extensible.  "Probabilistic Programming" */
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

class World {
	// import model
	//def worldVariables : Iterable[Variable]
	//def worldVariablesOf[V<:Variable](implicit m:Manifest[V])
}

trait Model {

	/**Various variables and methods that are global to a Model. */
	def randomSeed = 0

	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
	var _modelTemplates: List[Template] = Nil
	var _modelScoreTemplates: List[Template] = Nil

	def modelTemplates = _modelTemplates

	def addModelTemplate(temp: Template): Unit = addModelTemplates(List(temp))

	def addModelTemplates(temps: Template*): Unit = addModelTemplates(temps)

	def addModelTemplates(temps: Collection[Template]): Unit = {
		for (temp <- temps; if (!_modelTemplates.contains(temp)))
			_modelTemplates = temp :: _modelTemplates
		_modelScoreTemplates = _modelTemplates.filter(!_.isInstanceOf[NoScore]).toList
	}

	def clearModelTemplates = _modelTemplates = Nil

	def modelTemplatesOf[T <: Template](implicit m: Manifest[T]): Iterable[T] = {
		for (t <- modelTemplates; if (m.erasure.isAssignableFrom(t.getClass))) yield t.asInstanceOf[T]
	}

	def modelScoreTemplates = _modelScoreTemplates

	def modelMarginalSamplesTemplates = _modelTemplates.filter(!_.isInstanceOf[MarginalSamples])

	val modelDomains = new HashSet[Domain[_ <: Variable]]()

	def worldTrueScore(vars: Iterable[Variable]): Double = vars.sum(_ trueScore)

	def worldAccuracy(vars: Collection[Variable]): Double = worldTrueScore(vars) / vars.size

	def randomVariable[V](vars: RandomAccessSeq[V]): V = vars(random.nextInt(vars.size));
	def randomVariable[V](vars: RandomAccessSeq[V], test: V => boolean): V = {
		val filteredVars = vars.filter(test)
		filteredVars(random.nextInt(filteredVars.size))
	}


	/**The "domain" of a variable---also essentially serving as the variables' "type" */
	class Domain[V <: Variable](implicit m: Manifest[V]) {
		//def this(cls:Class[V]) = this(Manifest.classType[V](cls))
		if (Domain.forClass(m.erasure) != null) throw new Error("Domain[" + m.erasure.getName + "] already exists!")
		// If V is not an IndexedVariable, put this Domain in the Domain map; otherwise this will get done in IndexedVariable
		//if (!classOf[IndexedVariable].isAssignableFrom(m.erasure))
		Domain.set(m.erasure, this) // Why was the above condition there earlier? -akm  Perhaps I'm missing something now?
		private def shortClassName = {
			val fields = this.getClass.getName.split('$')
			if (fields.last == "class")
				fields(fields.length - 2)
			else
				fields.last
		}

		val printName = shortClassName
		modelDomains += this
	}

	abstract class IndexedDomain[V <: IndexedVariable](implicit m: Manifest[V]) extends Domain[V] with util.Index[V#ValueType] {
		//if (Domain.forClass(m.erasure) != null) throw new Error("IndexedDomain["+m.erasure.getName+"] already exists!")
		//Domain.set(m.erasure, this)
		def randomValue: V#ValueType = get(Model.this.random.nextInt(size))
	}

	class StringDomain[V <: IndexedVariable {type ValueType = String}](implicit m: Manifest[V]) extends IndexedDomain[V] {
		/* For all member variables, if its type is String and its name is all upper case or digits,
set its value to its name, and intern in the Domain.  Usage:
val PER, ORG, LOC, O = Value; internValues */
		def internValues /*(cls:Class[_])*/ : Unit = {
			val fields = this.getClass.getDeclaredFields()
			for (field <- fields; if (field.getType == classOf[String] && field.get(this) == Value)) {
				field.set(this, field.getName)
				index(field.getName)
			}
		}

		def Value = "__StringDomainValue__"
	}

	//class LabelDomain[V<:Label](implicit m:Manifest[V]) extends IndexedDomain[W forSome {type W <: LabelValue}]
	class LabelDomain[V <: CoordinatedLabel](implicit m: Manifest[V]) extends IndexedDomain[V] {
		private val stringIndex = new util.Index[String] {}
		//override def index(entry:Value) = entry.index
		def index(entry: String): Int = {
			val i = stringIndex.index(entry)
			if (i == this.size) this.index(new Value(i))
			i
		}

		def apply(entry: String) = index(entry)

		def getString(i: Int) = stringIndex.get(i)

		def get(s: String): V#ValueType = get(index(s))

		def internValues: Unit = {
			val fields = this.getClass.getDeclaredFields()
			for (field <- fields; if (field.getType.isAssignableFrom(classOf[LabelValue]) && Value == field.get(this))) {
				val i = index(field.getName)
				field.set(this, get(i))
			}
		}

		def Value = new Value(-1)

		final private def ldomain = this
		class Value(val index: Int) extends LabelValue with Ordered[Value] {
			if (index < size) throw new Error("LabelDomain Value for this index already exists.")
			// TODO make sure these are not created by the user, but only through the LabelDomain
			override def domain = ldomain
			//def entry : String = stringIndex.get(index)
			override def equals(other: Any) = other match {
				case label: Value => this.index == label.index
				case _ => false
			}

			def compare(other: Value) = other.index - this.index
		}
	}


	/**A static map from a Variable class to its Domain */
	object Domain {
		protected val _domains = new HashMap[Class[_], Any]() {
			override def default(c: Class[_]) = {
				//Console.println ("object Domain default with class "+c)
				if (classOf[CoordinatedLabel].isAssignableFrom(c)) {
					//Console.println ("object Domain default Label "+c)
					new LabelDomain[CoordinatedLabel]()(Manifest.classType[CoordinatedLabel](c))
				} else if (classOf[IndexedVariable].isAssignableFrom(c))
				//getOrElseUpdate(c, new IndexedDomain[IndexedVariable](c.asInstanceOf[Class[IndexedVariable]]))
				// Return a new IndexedDomain; its constructor will automatically put it in the Domain map
					new IndexedDomain[IndexedVariable]()(Manifest.classType[IndexedVariable](c)) {}
				else {
					//getOrElseUpdate(c, new Domain[Variable]())
					// Return a new Domain; its constructor will automatically put it in the Domain map
					new Domain[Variable]()(Manifest.classType[Variable](c))
				}
			}
		}
		// Enables syntax like Domain[Token]
		def apply[V <: Variable](v: V): Domain[V] = _domains(v.getClass).asInstanceOf[Domain[V]]

		def apply[V <: Variable](c: Class[_]): Domain[V] = _domains(c).asInstanceOf[Domain[V]]

		def apply[V <: Variable](implicit m: Manifest[V]): Domain[V] = _domains(m.erasure).asInstanceOf[Domain[V]]
		// TODO Next two methods don't have great names -akm
		def forClass[V <: Variable](c: Class[_]): Domain[V] = _domains.getOrElse(c, null).asInstanceOf[Domain[V]]

		def set[V <: Variable](c: Class[_], d: Domain[V])(implicit m: Manifest[V]): Unit = _domains.put(c, d)
		// TODO I want syntax like Domain[Token] = new IndexedDomain[Token] { ... }  def update should be able to do this.
		// def update[V<:Variable](c:Class[_], d:Domain[V])(implicit m:Manifest[V]) : Unit = _domains.put(c,d)
	}

	object IndexedDomain {
		/**The returned index indicating that the entry is not present in the Domain */
		val NULL_INDEX: Int = -1;
		// Enables syntax like IndexedDomain[Token]
		// TODO Why was I getting type errors when these two 'get' methods were 'apply'?
		def get[V <: IndexedVariable](v: V): IndexedDomain[V] = Domain(v.getClass).asInstanceOf[IndexedDomain[V]]

		def get[V <: IndexedVariable](c: Class[_]): IndexedDomain[V] = Domain(c).asInstanceOf[IndexedDomain[V]]

		def apply[V <: IndexedVariable](implicit m: Manifest[V]): IndexedDomain[V] = Domain(m.erasure).asInstanceOf[IndexedDomain[V]]
		// Enables syntax like IndexedDomain[Label] <-- new IndexedDomain("B", "I", "O").freeze
		// TODO Change this method name; <-- is too obscure; try to find alternative cleaner syntax in general
		//def <-- [V<:IndexedVariable,D<:IndexedDomain[V]](domain:D)(implicit m:Manifest[V]) =
		//if (Domain.forClass(m.erasure) != null) throw new Error ("Domain["+m+"] already created.")
		//else Domain.set(m.erasure,domain)
	}

	object LabelDomain {
		def get[V <: CoordinatedLabel](v: V): LabelDomain[V] = Domain(v.getClass).asInstanceOf[LabelDomain[V]]

		def get[V <: CoordinatedLabel](c: Class[_]): LabelDomain[V] = Domain(c).asInstanceOf[LabelDomain[V]]

		def apply[V <: CoordinatedLabel](implicit m: Manifest[V]): LabelDomain[V] = Domain(m.erasure).asInstanceOf[LabelDomain[V]]
	}

	object Domains {
		def apply[V <: Variable](implicit m: Manifest[V]): Array[Domain[_]] =
			Array(Domain(m.erasure))

		def apply[V1 <: Variable, V2 <: Variable](implicit m1: Manifest[V1], m2: Manifest[V2]): Array[Domain[_]] =
			Array(Domain(m1.erasure), Domain(m2.erasure))

		def apply[V1 <: Variable, V2 <: Variable, V3 <: Variable](implicit m1: Manifest[V1], m2: Manifest[V2], m3: Manifest[V3]): Array[Domain[_]] =
			Array(Domain(m1.erasure), Domain(m2.erasure), Domain(m3.erasure))
	}

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

	/**A variable that can provide an Iterator over all of its values. */
	trait IterableValues[T] {
		// TODO Inherit from TypedVariable instead?
		this: Variable =>

		/**Values this variable could take on. */
		def iterableValues: Iterable[T]

		/**Possible alternative values, that is, values other than its current value. */
		def iterableOtherValues: Iterable[T]
	}

	/**A variable that can iterate over its possible configurations */
	trait IterableSettings {
		this: Variable =>

		/**Return an iterator over some/all configurations of this variable, each time returning simply the same Variable object. */
		def iterator: Iterator[this.type]
	}

	/**For representing a proposed change to a possible world. */
	trait Proposal {
		def modelScore: Double

		def trueScore: Double

		def diff: DiffList
	}

	/**A simple implementation of the Proposal trait as a case class. */
	case class CaseProposal(modelScore: Double, trueScore: Double, diff: DiffList) extends Proposal {
		def this(score: Double, difflist: DiffList) = this (score, 0.0, difflist)
	}

	/**A proposal that makes no change. */
	case class EmptyProposal() extends CaseProposal(0.0, 0.0, new DiffList)

	/**A Proposal that automatically populates its diff, trueScore and score fields given a closure that makes the proposed change. */
	class AutoProposal(change: (DiffList) => Unit) extends Proposal {
		implicit val diff = new DiffList
		//println("Calling change")
		change(diff)
		val (modelScore, trueScore) = {
			//println("AutoProposal before diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
			//println("AutoProposal diff = " + diff)
			var tmpTrueScore = diff.trueScore;
			//println("true score delta before undo: " + tmpTrueScore)
			val tmpModelScore = diff.scoreAndUndo;
			//println("tmpModelScore=" + tmpModelScore)
			//println("AutoProposal after  diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
			tmpTrueScore -= diff.trueScore;
			//println("true score delta after undo: " + tmpTrueScore)
			(tmpModelScore, tmpTrueScore)
		}
		//private var tmpTrueScore = diff.trueScore
		//val modelScore = diff.scoreAndUndo
		//tmpTrueScore -= diff.trueScore
		//val trueScore = tmpTrueScore
		// Now modelScore is the different in model scores before and after the proposal; likewise for trueScore
	}

	/**A variable that can propose changes to itself, and possibly also other variables through variable value coordination */
	trait Proposer {
		this: Variable =>

		/**Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
		def propose(implicit d: DiffList): Double
	}

	/**A variable that can propose a menu of changes to itself.  Useful for considering all choices and choosing the max, or for parameter estimation in which we check the ranking of the best choice. */
	trait MultiProposer extends Proposer {
		this: Variable =>

		/**Make all possible proposals.
		The argument is not implicit because we do not want it implicitly passed to other methods that change variables. Instead a new DiffList should be created for each Proposal.  The only reason to pass the DiffList argument to
		this method is so that implementations can check the DiffList for circular changes. */
		def multiPropose(d: DiffList): Seq[Proposal]

		def propose(implicit d: DiffList): Double = {
			val proposals = multiPropose(d)
			val maxModelScore = proposals.max(_.modelScore).modelScore
			val proposal = proposals.sample(p => Math.exp(p.modelScore - maxModelScore))
			proposal.diff.redo
			d ++= proposal.diff
			0.0 // TODO Make an API for putting ratio of q's into multiPropose
		}
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
		// TODO Should we implement equals to compare values??
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

	trait TrueValue[T] {
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
	abstract class EnumVariable[T](trueval: T) extends CoordinatedEnumVariable[T] with TrueIndexedValue[T] with IterableValues[T] {
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
			if (d != null) d += SetVariableAddDiff(x)
			_members += x
		}

		def remove(x: X)(implicit d: DiffList): Unit = if (_members.contains(x)) {
			if (d != null) d += SetVariableRemoveDiff(x)
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

		def delete(implicit d: DiffList) = DeleteSpanVariable()(d)

		def setLength(l: Int)(implicit d: DiffList) = SetLength(_length, l)

		def trimStart(n: Int)(implicit d: DiffList) = TrimStart(n)

		def trimEnd(n: Int)(implicit d: DiffList) = TrimEnd(n)

		def prepend(n: Int)(implicit d: DiffList) = Prepend(n)

		def append(n: Int)(implicit d: DiffList) = Append(n)

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
	trait VariableSeq[V <: Variable with VarInSeq[V]] extends RandomAccessSeq[V] with Variable {
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
		abstract class SpanVariableInSeq(initStart: Int, initLength: Int)(implicit d: DiffList) extends Model.this.SpanVariable[X](VariableSeqWithSpans.this, initStart, initLength)(d)
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


	/*
abstract class GaussianFactor extends Factor3(Real, Real, Real) with HashSet[Real] {
def score(mean:Real, variance:Real, value:Real) =
}*/

	/**A single factor in a factor graph.  In other words, a factor
	template packaged with a set of variables neighboring the
	factor.  Factor inherits from Iterable[Factor] so that we can
	return a single Factor when an Iterable[Factor] is required. */
	trait Factor extends Product with Iterable[Factor] with Ordered[Factor] {
		//type TemplateType <: Template
		//def template : TemplateType
		def numVariables: Int = this.productArity

		def variable(index: Int): Variable = this.productElement(index).asInstanceOf[Variable]

		def variables: Iterable[Variable] = for (i <- 0 until productArity) yield variable(i)

		def randomVariable: Variable = variable(random.nextInt(productArity))

		def score: Double

		def vector: Vector

		/**A Factor can act as as singleton Iterable[Factor].  This makes it easier to return a single Factor from unroll* methods. */
		def elements: Iterator[Factor] = Iterator.single(this)

		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Factor) = List(that, this)

		/**Add this Factor to the FactorList of all the Variables that are this factor's neighbors */
		def addToVariables = variables.filter(_.isInstanceOf[FactorList]).map(_.asInstanceOf[FactorList].addFactor(this))
		// Implement Ordered, such that worst (lowest) scores are considered "high"
		def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
		// Implement equality based on class assignability and Variable contents equality
		def canEqual(other: Any) = other.isInstanceOf[Factor]

		override def equals(other: Any): Boolean = {
			//(this eq other) ||
			if (!canEqual(other)) return false
			val fother = other.asInstanceOf[Factor]
			(this.numVariables == fother.numVariables &&
							(0 until numVariables).forall(i => this.variable(i).hashCode == fother.variable(i).hashCode &&
											this.variable(i) == fother.variable(i)))
		}

		var _hashCode = -1

		override def hashCode: Int = {if (_hashCode == -1) _hashCode = variables.sumInts(_ hashCode); _hashCode}

		override def toString = this.getClass.getName + variables.foldLeft("(")(_ + _.toString + ",") + ")"
	}

	def unroll(variables: Iterable[Variable]): Iterable[Factor] = {
		var factors = new HashSet[Factor]
		// unroll all factors touching all v in variables
		for (v <- variables; t <- modelTemplates)
			t.asInstanceOf[Neighbors].unroll0(v).foreach(f => factors += f)
		// Make sure each variables factor list starts empty
		variables.filter(_.isInstanceOf[FactorList]).foreach(_.asInstanceOf[FactorList].clearFactors)
		// Add relevant factors to each relevant neighboring variable
		factors.foreach(_.addToVariables)
		factors.toSeq
	}

	// TODO not yet used
	trait Beliefs {
		this: Factor =>
		val belief: Vector
	}

	/**A container for the sufficient statistics of a Factor. */
	// TODO Make this also extend Product, support scoring, etc, like Factor
	trait Suff extends Iterable[Suff] {
		def template: Template

		def vector: Vector
		//def score : Double = template.weights dot vector

		/**A Suff can act as as singleton Iterable[Suff].
		This makes it easier to return a single Suff from unroll* methods. */
		def elements: Iterator[Suff] = Iterator.single(this)

		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Suff) = List(that, this)
	}

	trait LogLinearScoring extends Template {
		type TemplateType <: LogLinearScoring

		def weights: Vector

		def score(s: S): Double
	}

	trait DenseLogLinearScoring extends Template {
		type TemplateType <: DenseLogLinearScoring
		lazy val weights = {freezeDomains; new DenseVector(suffsize)}

		def score(s: S) = weights dot s.vector
	}

	trait SparseLogLinearScoring extends Template {
		type TemplateType <: SparseLogLinearScoring
		lazy val weights = new SparseVector(suffsize)

		def score(s: S) = weights dot s.vector
	}

	//trait LogLinearTemplate extends LogLinearScoring with Template

	//trait LogLinearTemplate extends Template with LogLinearScoring

	/**The template for many factors.  Stores its parameters and has methods for templating behavior */
	trait Template {
		//template =>
		type S <: Suff // The case class of sufficient statistics
		type N <: Factor // The case class of neighbors, defined in subtrait of Neighbors
		val sDomains: Array[IndexedDomain[_]]
		lazy val suffsize: Int = sDomains.productInts(_ allocSize)

		protected def freezeDomains: Unit = sDomains.foreach(_ freeze)

		def factors(difflist: Iterable[Diff]): Iterable[Factor] // TODO consider trying to make return type Iterable[N]
		//def vector(s:S) : Vector // TODO consider putting this here instead of in Factor, to allow Template subclasses to change behavior. -akm
		def score(s: S): Double

		type TemplateType <: Template
		//def template : TemplateType = this
		//def thisTemplate = this
		trait Factor extends Model.this.Factor {
			def template: TemplateType = Template.this.asInstanceOf[TemplateType]
		}
	}

	abstract class Template1[S1 <: IndexedVariable](implicit vm1: Manifest[S1]) extends Template {
		case class Suff(s1: S1) extends Model.this.Suff with Iterable[Suff] {
			override def template = Template1.this

			def productArity = 1

			def productElement(index: Int) = index match {case 0 => s1}

			def vector = s1.vector
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1))
		//def indices(s:S) = s.s1.indices
		//def vector(s:S) = s.s1.vector
	}

	private def flatOuter(vector1: Vector, vector2: Vector) = vector1 match {
		case v1: SingletonBinaryVector => vector2 match {
			case v2: SingletonBinaryVector =>
				new SingletonBinaryVector(v1.size * v2.size, v1.singleIndex * v2.size + v2.singleIndex)
			case v2: SparseBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v2.activeDomain.size);
						var i = 0;
						for (i2 <- v2.activeDomain) {
							arr(i) = v1.singleIndex * v2.size + i2;
							i += 1;
						};
						arr
					})
		}
		case v1: SparseBinaryVector => vector2 match {
			case v2: SingletonBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v1.activeDomain.size);
						var i = 0;
						for (i1 <- v1.activeDomain) {
							arr(i) = i1 * v2.size + v2.singleIndex;
							i += 1;
						};
						arr
					})
			case v2: SparseBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v1.activeDomain.size * v2.activeDomain.size);
						var i = 0;
						for (i1 <- v1.activeDomain; i2 <- v2.activeDomain) {
							arr(i) = i1 * v2.size + i2;
							i += 1;
						};
						arr
					})
		}
	}

	abstract class Template2[S1 <: IndexedVariable, S2 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2]) extends Template {
		case class Suff(s1: S1, s2: S2) extends Model.this.Suff with Iterable[Suff] {
			def template = Template2.this

			def productArity = 2

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2}
			//def indices = Template2.this.indices(this)
			//def vector = Template2.this.indices(this)
			def vector = flatOuter(s1.vector, s2.vector)
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2))
		//def indices(s:S) : Seq[Int] =  for (i <- s.s1.indices; j <- s.s2.indices) yield i * sDomains(1).size + j
	}

	abstract class Template3[S1 <: IndexedVariable, S2 <: IndexedVariable, S3 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2], vm3: Manifest[S3]) extends Template {
		case class Suff(s1: S1, s2: S2, s3: S3) extends Model.this.Suff with Iterable[Suff] {
			def template = Template3.this

			def productArity = 3

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2; case 2 => s3}
			//def indices = Template3.this.indices(this)
			def vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2), IndexedDomain(vm3))
		//def indices(s:S) = for (i <- s.s1.indices; j <- s.s2.indices; k <- s.s3.indices) yield i * sDomains(1).size * sDomains(2).size + j * sDomains(2).size + k
	}

	abstract class Template4[S1 <: IndexedVariable, S2 <: IndexedVariable, S3 <: IndexedVariable, S4 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2], vm3: Manifest[S3], vm4: Manifest[S4]) extends Template {
		case class Suff(s1: S1, s2: S2, s3: S3, s4: S4) extends Model.this.Suff with Iterable[Suff] {
			def template = Template4.this

			def productArity = 4

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2; case 2 => s3; case 3 => s4}
			//def indices = Template4.this.indices(this)
			def vector = flatOuter(s1.vector, flatOuter(s2.vector, flatOuter(s3.vector, s4.vector)))
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2), IndexedDomain(vm3), IndexedDomain(vm4))
		/*def indices(s:S) = for (i <- s.s1.indices; j <- s.s2.indices; k <- s.s3.indices; l <- s.s4.indices) yield
i * sDomains(1).size * sDomains(2).size * sDomains(3).size + j * sDomains(2).size * sDomains(3).size + k * sDomains(3).size + l */
	}

	class LinkedHashSet[A] extends scala.collection.mutable.Set[A] with FlatHashTable[A] {
		var list = List[A]()

		override def initialSize: Int = 32

		def contains(elem: A): Boolean = containsEntry(elem)

		def +=(elem: A) {add(elem)}

		def add(elem: A): Boolean = {
			if (addEntry(elem)) {
				list = elem :: list
				true
			} else false
		}

		def -=(elem: A) {remove(elem)}

		def remove(elem: A): Boolean = removeEntry(elem) match {
			case None => false
			case Some(elem) => list = list.filter(_ != elem); true
		}

		override def clear() {list = Nil; super.clear()}

		override def elements = list.elements
	}

	trait Neighbors extends Template {
		//this: Template =>
		val nDomains: Array[Domain[_]] // TODO Change this into a Tuple so that we can catch n-arg errors at compile time.

		/**A version of unroll0 that takes the Diff object instead of just the variable */
		def unroll(d: Diff): Iterable[N] = if (d.variable == null) Nil else unroll0(d.variable)

		def unroll0(v: Variable): Iterable[N]

		def factors(difflist: Iterable[Diff]): Iterable[N] = {
			var result = new LinkedHashSet[N]()
			difflist.foreach(diff => result ++= unroll(diff))
			result.toList
		}

		def _sufficient(f: N): Iterable[S]
		//def thisTemplate = this
		//type TemplateType <: Neighbors
		trait Factor extends super.Factor with Iterable[Factor] {
			//override def template : TemplateType = Neighbors.this
			//def template = thisTemplate
			private def thisn: N = this.asInstanceOf[N] // TODO Surely there is a way to avoid this silliness
			def score: Double = _sufficient(thisn) match {
				case single: Suff => Neighbors.this.score(single.asInstanceOf[S])
				case multi: Iterable[_] => multi.asInstanceOf[Iterable[S]].foldLeft(0.0)(_ + Neighbors.this.score(_))
			}

			def vector: Vector = _sufficient(thisn) match {
				case single: Suff => single.asInstanceOf[S].vector
				case multi: Iterable[_] => {
					val iter = multi.asInstanceOf[Iterable[S]].elements
					if (iter.hasNext) {
						val first: Vector = iter.next.vector
						var vec = new SparseVector(first.size) // TODO if 'first' is SparseBinaryVector this should be Sparse also
						vec += first
						while (iter.hasNext)
							vec += iter.next.vector
						vec
					} else {
						// an empty iterator over Suff's.  Just return a (sparse) vector of zero's.
						new SparseVector(suffsize)
					}
				}
			}
			// Support for BP message passing
			case class MessageTo(v: EnumVariable[_]) {
				lazy private val msg = new Array[Double](v.domain.size)

				def factor = Factor.this

				def message = msg

				def update: MessageTo = {
					if (!v.isConstant)
						for (i <- 0 until msg.length) msg(i) = 0.0 // TODO surely there is a faster way
					else {
						val msgNeighbors = variables.filter(v2 => v2 != v).map(v2 => v2.asInstanceOf[EnumVariable[_]]).toList
						def nextValues(vs: List[EnumVariable[_]]): Boolean = {
							if (vs.first.hasNextValue) {vs.first.setNextValue; true}
							else if (vs.tail != Nil) {if (!vs.first.isConstant) vs.first.setFirstValue; nextValues(vs.tail)}
							else false
						}
						for (i <- 0 until v.domain.size) {
							v.setByIndex(i)(null)
							msgNeighbors.foreach(_ setFirstValue)
							do {
								msg(i) += Factor.this.score * msgNeighbors.product(n => Factor.this.messageFrom(n).message(n.index))
							} while (nextValues(msgNeighbors))
						}
					}
					this // Return this so we can say messageTo(v).update.message
				}
			}
			case class MessageFrom(v: EnumVariable[_] with FactorList) {
				lazy private val msg = new Array[Double](v.domain.size)

				def factor = Factor.this

				def message = msg

				def update: MessageFrom = {
					for (i <- 0 until v.domain.size) {
						//msg(i) = v.factors.filter(_.!=(this)).product(_.messageTo(v)(i))
						msg(i) = 1.0
						for (f <- v.factors; if (f != this))
							msg(i) *= f.asInstanceOf[Factor].messageTo(v).message(i)
					}
					this // Return this so we can say messageFrom(v).update.message
				}
			}
			lazy private val _msgTo: Array[MessageTo] = variables.map(v => MessageTo(v.asInstanceOf[EnumVariable[_]])).toSeq.toArray
			lazy private val _msgFrom: Array[MessageFrom] = variables.map(v => MessageFrom(v.asInstanceOf[EnumVariable[_] with FactorList])).toSeq.toArray

			def messageTo(v: EnumVariable[_]) = _msgTo(variables.toSeq.indexOf(v))

			def messageFrom(v: EnumVariable[_]) = _msgFrom(variables.toSeq.indexOf(v))

			def messageTo(vi: Int) = _msgTo(vi)

			def messageFrom(vi: Int) = _msgFrom(vi)
			//def neighborDistribution : Array[Double]
			//def sufficientDistribution : Array[Double]
		}
	}


	trait MarginalSamples extends Template {
		lazy val samples = {freezeDomains; new Array[Double](suffsize)}

		def clearSamples = for (i <- 0 until samples.length) samples(i) = 0.0 // TODO surely there is a faster way
	}

	trait NoScore extends Template {
		override final def score(s: S) = 0
	}

	trait Neighbors1[N1 <: Variable] extends Neighbors {
		//this: Template =>
		type N = Factor
		//override val nDomains : Tuple1[Domain[N1]]  // I'm considering making it a Tuple, but for now its an Array -akm
		def unroll0(v: Variable): Iterable[N] =
			if (v.domain == nDomains(0)) unroll1(v.asInstanceOf[N1])
			else Nil

		def unroll1(v: N1): Iterable[Factor] = new Factor(v)

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.n1)

		def sufficient(v1: N1): Iterable[S]
		//case class Factor(n1:N1) extends super.Factor with Iterable[Factor]
		case class Factor(n1: N1) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors1[N1 <: IndexedVariable](implicit vm1: Manifest[N1])
					extends Template1[N1]()(vm1) with Neighbors1[N1]
	{
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]

		override def _sufficient(f: Factor): Iterable[S] = Suff(f.n1) // TODO consider a way to just return f??
		override def sufficient(v1: N1): Iterable[S] = Suff(v1)
	}

	trait Neighbors2[N1 <: Variable, N2 <: Variable] extends Neighbors {
		//this: Template =>
		type N = Factor
		//val n1 : Domain[N1] // = Domains.getFromClass[N1](Class[N1])
		//val n2 : Domain[N2] // = Domains.getFromClass[N2](Class[N2])
		def unroll0(v: Variable): Iterable[Factor] = {
			var ret = new ListBuffer[Factor]
			if (v.domain == nDomains(0)) ret ++= unroll1(v.asInstanceOf[N1])
			if (v.domain == nDomains(1)) ret ++= unroll2(v.asInstanceOf[N2])
			ret
		}

		def unroll1(v: N1): Iterable[Factor]

		def unroll2(v: N2): Iterable[Factor]

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.v1, f.v2)

		def sufficient(v1: N1, v2: N2): Iterable[S]
		case class Factor(v1: N1, v2: N2) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors2[N1 <: IndexedVariable, N2 <: IndexedVariable](implicit vm1: Manifest[N1], vm2: Manifest[N2])
					extends Template2[N1, N2]()(vm1, vm2) with Neighbors2[N1, N2]
	{
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]
		//val n1 : Domain[N1] = domain1; val n2 : Domain[N2] = domain2
		override def _sufficient(f: Factor): Iterable[S] = Suff(f.v1, f.v2)

		override def sufficient(v1: N1, v2: N2): Iterable[S] = Suff(v1, v2)
	}

	trait Neighbors3[N1 <: Variable, N2 <: Variable, N3 <: Variable] extends Neighbors {
		this: Template =>
		type N = Factor
		//val n1 : Domain[N1]; val n2 : Domain[N2]; val n3 : Domain[N3]
		def unroll0(v: Variable): Iterable[Factor] = {
			var ret = new ListBuffer[Factor]
			if (v.domain == nDomains(0)) ret ++= unroll1(v.asInstanceOf[N1])
			if (v.domain == nDomains(1)) ret ++= unroll2(v.asInstanceOf[N2])
			if (v.domain == nDomains(2)) ret ++= unroll3(v.asInstanceOf[N3])
			ret
		}

		def unroll1(v: N1): Iterable[Factor]

		def unroll2(v: N2): Iterable[Factor]

		def unroll3(v: N3): Iterable[Factor]

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.n1, f.n2, f.n3)

		def sufficient(v1: N1, v2: N2, v3: N3): Iterable[S]
		case class Factor(n1: N1, n2: N2, n3: N3) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors3[N1 <: IndexedVariable, N2 <: IndexedVariable, N3 <: IndexedVariable](implicit vm1: Manifest[N1], vm2: Manifest[N2], vm3: Manifest[N3]) extends Template3[N1, N2, N3]()(vm1, vm2, vm3) with Neighbors3[N1, N2, N3] {
		// val n1 : Domain[N1] = domain1; val n2 : Domain[N2] = domain2; val n3 : Domain[N3] = domain3
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]

		override def _sufficient(f: Factor): Iterable[S] = Suff(f.n1, f.n2, f.n3)

		override def sufficient(v1: N1, v2: N2, v3: N3): Iterable[S] = Suff(v1, v2, v3)
	}

	// TODO consider adding state that throws error if users try to "undo" or "redo" twice in a row.

	/**A change record for a variable, holding its old and new values */
	trait Diff {
		def variable: Variable

		def redo: Unit

		def undo: Unit

	}

	// TODO think about whether or not this is a good idea...
	//abstract class CreationDiff(implicit d:DiffList) extends Diff { var done = true }

	abstract class AutoDiff(implicit d: DiffList) extends Diff {
		if (d != null) d += this
		redo
		override def toString = this.getClass.toString
	}

	/**The default DiffList is null, and therefore calls to
	Variable.set that don't set a DiffList do not accumulate Diffs */
	//implicit val nullDiffList : DiffList = null

	/**A collection of changes to variables; the result of a "jump" in configuration */
	class DiffList extends ArrayBuffer[Diff] with ConsoleLogging {
		def redo: Unit = this.foreach(d => d.redo)

		def undo: Unit = this.reverse.foreach(d => d.undo)

		/**Return the sum of the trueScore's of all the changed variables. */
		def trueScore: Double = {
			//println("calculating true score of " + this);
			var sum = 0.0
			for (d <- this) {
				//println("variable " + d.variable)
				if (d.variable != null) {
					val s = d.variable.trueScore
					sum += s
				}
			}
			sum
			//this.sum(d => {println(d.variable); if (d.variable != null) d.variable.trueScore else 0.0})
		}

		/**Return the sum of scores of all factors that touch changed variables. */
		def factors: Iterable[Factor] = modelTemplates.flatMap(template => template.factors(this))

		def factorsOf[T <: Template](implicit m: Manifest[T]): Iterable[T#Factor] = modelTemplatesOf(m).flatMap(template => template.factors(this))

		def factorsFiltered(test: Template => Boolean): Iterable[Factor] =
			modelTemplates.filter(test).flatMap(template => template.factors(this))

		def scoreFactors: Iterable[Factor] = modelScoreTemplates.flatMap(_ factors (this))

		/**Gather the uniq'ed "neighbor invocations" of each factor and return the sum of their scores */
		//def modelScore : Double = scoreFactors.sum(_ score)
		def modelScore: Double = {
			//      var sum = 0.0
			//			for (template <- modelScoreTemplates){
			//				println("Template: " + template)
			//				for (factor <- template.factors(this)){
			//					println("Factor score of " + factor + ":" + factor.score)
			//
			//				}
			//			}
			val s = modelScoreTemplates.map(_ factors (this)).foldLeft(0.0)((total, fs) => total + fs.sum(_ score))
			//scoreFactors.sum(_ score)
			s
		}

		/**Return the sum of scores of all factors that touch changed variables, and whose Template's pass the test. */
		def modelScoreFilter(test: (Template) => Boolean): Double =
			modelScoreTemplates.filter(test).flatMap(_ factors (this)).sum(_ score)

		def scoreAndUndo: Double = {
			var s = modelScore
			//println("Score: " + s)
			log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
			this.undo
			// We need to re-calculate the Factors list because the structure may have changed
			s -= modelScore
			log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
			s
		}
	}

}
