package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

/**Abstract superclass of all variables.  Don't need to know its value type to use it. 
   The trait is abstract not because there are abstract method definitions; 
   it is just preventing users from trying to construct a Variable instance. */
abstract trait Variable {
  /** The type of this variable, especially used by this Variable's Domain. */
	type VariableType <: Variable
	/** The type of this.domain and Domain[MyVariable]*/
	type DomainType <: Domain[VariableType]
  /** When a Domain is constructed for this class, it will be the superclass of this inner class. */
  class DomainClass extends Domain[VariableType]
  /** When DomainInSubclasses appears as an inner class of a Variable class, 
      it simply ensures that the library will never create a Domain for this class, only its subclasses.
      If library users create their own new Variable classes, which will be subclassed, and wants each
      subclass to have its own Domain, then those new Variable classes must declare an inner class of this type. */
  class DomainInSubclasses
  final def domain = Domain.get[VariableType](this.getClass)
	private def shortClassName = {
	  var fields = this.getClass.getName.split('$')
	  if (fields.length == 1)
	  	fields = this.getClass.getName.split('.')
	  if (fields.last == "class")
	  	fields(fields.length - 2)
	  else if ("1234567890".contains(fields.last))
     fields(fields.length-2)
    else
      fields.last
	}
	def printName = shortClassName
	override def toString = printName + "(_)"
	def factors(model:Model): Iterable[Factor] = model.factors(this)
}

/** Used as a marker for Variables whose value does not change once created. */
abstract trait ConstantValue requires Variable

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed*/
trait NoVariableCoordination 
/** A marker for Variables that declare themselves not to automatically change which Factors are relevant when they are changed*/
trait NoFactorCoordination 


/*
trait ItemizedVariable extends Variable {
	type VariableType <: ItemizedVariable
	def domain: ItemizedDomain[VariableType] = Domain[VariableType](this.getClass).asInstanceOf[ItemizedDomain[VariableType]]
	                                                                                            domain.index(this) // Put this variable in the index.
}
*/

/**For variables whose value has a type stored in type ValueType */
abstract trait TypedVariable /*extends Variable*/ {
  this : Variable =>
	type ValueType
}



// TODO remove this now that we have Proposer
/**A variable that can provide an Iterator over all of its values. */
@deprecated
abstract trait IterableValues[T] {
	// TODO Inherit from TypedVariable instead?
			this: Variable =>
	/**Values this variable could take on. */
	def iterableValues: Iterable[T];
	/**Possible alternative values, that is, values other than its current value. */
	def iterableOtherValues: Iterable[T]
}

// TODO remove this now that we have Proposer?
/** A variable that can iterate over its possible configurations */
trait IterableSettings{
	this: Variable =>
  /** Return an iterator over some/all configurations of this variable, each time returning simply the same Variable object. */
  def settings: Iterator[{def set(d:DiffList):Unit}]
}


/**A variable with a single mutable (unindexed) value */
abstract class PrimitiveVariable[T](initval: T) extends Variable with TypedVariable {
	type VariableType <: PrimitiveVariable[T]
  type ValueType = T
  class DomainInSubclasses
  protected var _value: T = _
  set(initval)(null) // initialize with method call because subclasses may do coordination in overridden set()()
  def value = _value
  def set(newValue: T)(implicit d: DiffList): Unit =
    if (newValue != _value) {
      if (d != null) d += new PrimitiveDiff(_value, newValue)
      _value = newValue
    }
  // Should we implement equals here to compare Variable values??
  // No, I don't think so because we might need to put multiple variables with the same values in a HashMap
  // But we can implement our own specialized equality method... (the shorter === overlaps with an implicit conversion in scalatest)
  def ====(other: PrimitiveVariable[T]) = _value == other._value
  def !===(other: PrimitiveVariable[T]) = _value != other._value
  override def toString = printName + "(" + value.toString + ")"
	case class PrimitiveDiff(oldValue: T, newValue: T) extends Diff {
  	//        Console.println ("new PrimitiveDiff old="+oldValue+" new="+newValue)
  	def variable: PrimitiveVariable[T] = PrimitiveVariable.this
  	def redo = _value = newValue
  	def undo = _value = oldValue
  }
}

/** For variables that have a true value describable by a Scala type T. */
trait PrimitiveTrueValue[T] {
	this: PrimitiveVariable[T] =>
  var trueValue: T = _
  def isUnlabeled = trueValue == _
}

/**For use with variables whose values are mapped to dense integers */
abstract trait IndexedVariable extends Variable with TypedVariable {
	type VariableType <: IndexedVariable
	type DomainType <: IndexedDomain[VariableType]
	class DomainClass extends IndexedDomain[VariableType]
	class DomainInSubclasses
	def vector: Vector // TODO remove this method?  No perhaps not.
	// TODO These next methods are efficient for cycling through all values,
	// but perhaps should be simply collapsed into IterableValues or MultiProposer -akm
	//def setFirstValue: Unit = throw new Error("Cannot set constant IndexedVariable")
	//def hasNextValue = false
	//def setNextValue: Unit = {}
}

// TODO Consider making a ConstantSingleIndexedVariable, for use by MixtureComponent
// But how would it be enforced?

/** For variables whose values are associated with a an Int from an index. */
abstract trait SingleIndexedVariable extends IndexedVariable with Proposer with MultiProposer with IterableSettings {
	type VariableType <: SingleIndexedVariable
 	class DomainInSubclasses
	protected var _index = -1
	def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
		if (newIndex < 0) throw new Error("SingleIndexedVariable setByIndex can't be negative.")
		if (newIndex != _index) {
			if (d != null) d += new SingleIndexedDiff(_index, newIndex)
			_index = newIndex
		}
	}
	def setRandomly(implicit random:Random, d:DiffList) : Unit = setByIndex(random.nextInt(domain.size))
	def setRandomly(implicit random:Random) : Unit = setByIndex(random.nextInt(domain.size))(null)
	def setRandomly : Unit = setRandomly(cc.factorie.Global.random)
	def settings = new Iterator[{def set(d:DiffList):Unit}] {
	  var d : DiffList = _
	  var i = -1
	  val max = domain.size - 1
	  def hasNext = i < max
	  def set(d:DiffList) : Unit = setByIndex(i)(d)
	  def next = { i += 1; this }
	  //def next = { if (d != null) d.undo; d = new DiffList; setByIndex(i)(d); i += 1; d }
	}
	def propose(d: DiffList)(implicit random:Random) = {setByIndex(random.nextInt(domain.size))(d); 0.0}
	// The reason for the "toList" (now changed to "force"), see 
	// http://stackoverflow.com/questions/1332574/common-programming-mistakes-for-scala-developers-to-avoid
	// http://creativekarma.com/ee.php/weblog/comments/the_scala_for_comprehension_from_a_java_perspective/
  // TODO Look at this issue more carefully and turn on printing in Implicits.bonusIterables to look for additional efficiencies 
	def multiPropose(model:Model, objective:Model, difflist: DiffList) = {
	  val aps = for (i <- 0 until domain.size force) yield {
	  	//println("SingleIndexedVariable multiPropose " +i) // TODO check this for repeated evaluation
	  	val ap = new AutoProposal(model, objective, diff => setByIndex(i)(diff))
	  	//println("SingleIndexedVariable.multiPropose i="+i+" modelScore="+ap.modelScore)
	  	ap
    }
		// val d = new DiffList; setByIndex(i)(d); new CaseProposal(d.scoreAndUndo, d)
		aps
	}
	def index = _index
	override def toString = printName + "(" + _index + ")"
	override def vector = new SingletonBinaryVector(domain.allocSize, _index)
	/** Tests equality of variable values, whereas == tests equality of variable objects themselves. */
	def ====(other: SingleIndexedVariable) = _index == other._index
	def !===(other: SingleIndexedVariable) = _index != other._index
	case class SingleIndexedDiff(oldIndex: Int, newIndex: Int) extends Diff {
		@scala.inline final def variable: SingleIndexedVariable = SingleIndexedVariable.this
		@scala.inline final def redo = _index = newIndex
		@scala.inline final def undo = _index = oldIndex
	}
}

/** For variables put in a index, and whose value is the variable itself. */
abstract trait ItemizedVariable[This <: ItemizedVariable[This]] extends SingleIndexedVariable {
	this : This =>
  type VariableType = This
  type ValueType = This
  class DomainInSubclasses
  domain.index(this) // Put the variable in the index
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
abstract trait TypedSingleIndexedVariable[T] extends SingleIndexedVariable with TypedVariable {
	type VariableType <: TypedSingleIndexedVariable[T]
  type ValueType = T
  class DomainInSubclasses
  def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
  def value: T = domain.get(_index)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + _index + ")"
}	

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.

/**A variable whose value is a single indexed value; mutable */
abstract trait CoordinatedEnumVariable[T] extends TypedSingleIndexedVariable[T] {
	type VariableType <: CoordinatedEnumVariable[T]
  class DomainInSubclasses
  // initialize the variable's value; using this method in case coordination in necessary
  //setByIndex(domain.index(initval))(null)
}

/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value. */
//trait TrueIndexedValue[T] extends TypedSingleIndexedVariable[T] 
trait TrueIndexedValue /*extends SingleIndexedVariable*/ {
  this : SingleIndexedVariable =>
	//type VariableType <: TrueIndexedValue // TODO Try to make this work, so that "trueValue" returns the right type
  /** The index of the true labeled value for this variable.  If unlabeled, set to -1 */
  var trueIndex: Int
  //private var _trueValue:T = domain.get(trueIndex)
  def trueValue /*:ValueType*/= if (trueIndex >= 0) domain.get(trueIndex) else null // _trueValue
  def isUnlabeled = trueIndex < 0
  def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex else throw new Error("Already unlabeled.")
}

// TODO consider moving TrueIndexedValue to inside with this:TrueIndexedValue => ?
abstract trait TypedTrueIndexedValue[T] extends TrueIndexedValue with TypedSingleIndexedVariable[T] {
  class DomainInSubclasses
	def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
}

abstract class TrueIndexedValueTemplate[V<:SingleIndexedVariable with TrueIndexedValue](implicit m:Manifest[V]) extends TemplateWithExpStatistics1[V]()(m) {
  def score(s:Stat) = if (s.s1.index == s.s1.trueIndex) 1.0 else 0.0
}

/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method.  Ensuring no coordination is necessary for optimization of belief propagation. */
abstract class EnumVariable[T](trueval:T) extends CoordinatedEnumVariable[T] with TypedTrueIndexedValue[T] with IterableValues[T] {
	type VariableType <: EnumVariable[T]
  class DomainInSubclasses
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
	override final def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
	/*final*/ override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d) // TODO uncomment final
	//override def setFirstValue: Unit = setByIndex(0)(null)
	//override def hasNextValue = _index < domain.size - 1
	//override def setNextValue: Unit = if (hasNextValue) setByIndex(_index + 1)(null) else throw new Error("No next value")
	def iterableValues: Iterable[T] = domain
	def iterableOtherValues: Iterable[T] = domain.filter(_ != value)
	override def trueValue : T = domain.get(trueIndex) 
}

//trait MyFoo { def myfoo = 3 }
//class MyEnumVariable[T](trueval:T) extends EnumVariable[T](trueval) with MyFoo

class TrueEnumTemplate[V<:EnumVariable[_]](implicit m:Manifest[V]) extends TrueIndexedValueTemplate[V]()(m)

// TODO We can get rid of LabelValue
/** The value of a Label variable.  
* Previously we simply used String values in a EnumVariable, but here LabelValues can be compared much more efficiently than Strings. */
trait LabelValue {
	def index: Int
	def domain: LabelDomain[_]
  def entry: String = domain.getString(index)
  override def toString = "LabelValue("+entry+")"
}

/**A variable whose value is a LabelValue, which in turn can be
created or indexed through a String.  LabelValues can be
efficiently compared. */
//class Label(initval:String) extends SingleIndexedVariable[LabelValue] with TrueIndexedValue[LabelValue] with IterableValues[LabelValue] {
class CoordinatedLabel(trueval: String) extends CoordinatedEnumVariable[LabelValue] with TypedTrueIndexedValue[LabelValue] with IterableValues[LabelValue] {
	type VariableType <: CoordinatedLabel
	type DomainType <: LabelDomain[VariableType]
	class DomainClass extends LabelDomain[VariableType]
	//type ValueType = String 
	def this(trueval: LabelValue) = this (trueval.entry)
  class DomainInSubclasses
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
	//def this(initval:String) = this(LabelDomain.get[Label](this/*.getClass*/).get(initval))
	def set(s: String)(implicit d: DiffList) = setByIndex(domain.index(s))
 	override def trueValue : LabelValue = domain.get(trueIndex) 
	//override def setFirstValue: Unit = setByIndex(0)(null)
	//override def hasNextValue = _index < domain.size - 1
	//override def setNextValue: Unit = if (hasNextValue) setByIndex(_index + 1)(null) else throw new Error("No next value")
	def iterableValues: Iterable[LabelValue] = domain
	def iterableOtherValues: Iterable[LabelValue] = domain.filter(_ != value)
	//type ValueType <: LabelDomain[VariableType]#Value // TODO try to get this working
	/*trait Value {
		def index : Int
		g      def domain : LabelDomain[_<:Label]
		                                def entry : String
	}*/
	override def toString = printName + "(Value=" + value.entry + "=" + _index + ")"
	def ====(other: CoordinatedLabel) = value.index == other.value.index
	def !===(other: CoordinatedLabel) = value.index != other.value.index
}

class Label(trueval: String) extends CoordinatedLabel(trueval) {
  type VariableType <: Label
  class DomainInSubclasses
	override final def set(newValue: String)(implicit d: DiffList) = super.set(newValue)(d)
	override final def set(newValue: LabelValue)(implicit d: DiffList) = super.set(newValue)(d)
	override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d) // TODO should be final, right?
}

class TrueLabelTemplate[V<:Label](implicit m:Manifest[V]) extends TrueIndexedValueTemplate[V]()(m)

/**A variable whose value is a SparseBinaryVector; immutable. */
// TODO Rename to BinaryVectorVariable
abstract class VectorVariable[T] extends IndexedVariable with TypedVariable {
	type ValueType = T
	type VariableType <: VectorVariable[T]
  class DomainInSubclasses
  //def this (es:T*) = this(es.toArray)   TODO include this again later
  protected var indxs = new ArrayBuffer[Int]()
  def indices : Seq[Int] = indxs
  def values : Seq[T] = { val d = this.domain; indxs.map(d.get(_)) }
  private var _vector: Vector = null // TODO Can we make this more memory efficient?  Avoid having both Vector and ArrayBuffer?
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
  def +(value: T) = {this += value; this} // TODO Shouldn't this method actually return a new VectorVariable, leaving old one unchanged?  Yes.
  def ++=(vals: Iterable[T]) = vals.foreach(v => this += v)
  def ++(vals: Iterable[T]) = {this ++= vals; this} // TODO this method should return a new Vector
  override def toString = {
    val s = new StringBuilder(printName + "(")
    val iter = vector.activeDomain.elements
    if (iter.hasNext) { val i:Int = iter.next ; s ++= (domain.get(i).toString + "=" + i) }
    while (iter.hasNext) {
    	val i:Int = iter.next
      s ++= ("," + domain.get(i).toString + "=" + i)
    }
    s ++= ")"
    s.toString
  }
}

/**A variable class for boolean values, defined here for convenience.  If you have several different "types" of booleans, you might want to subclass this to enable type safety checks. */
class Bool(b: Boolean) extends EnumVariable(b) {
  def this() = this(false)
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
}

/* TODO Consider adding such a thing
class IntRange(i:Int) extends IndexedVariable {
	type VariableType = IntRange
	def trueScore = 0.0
}*/

/**A variable class for string values. */
class StringVariable(str: String) extends PrimitiveVariable(str) {
	type VariableType = StringVariable
	class DomainInSubclasses
}

/**For Variables that hold their list of Factors */
trait FactorList requires Variable {
  private var factorList: List[Factor] = Nil
  def addFactor(f: Factor) = factorList = f :: factorList
  def clearFactors = factorList = Nil
  def factors: Iterable[Factor] = factorList
}


