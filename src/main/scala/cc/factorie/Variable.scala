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

/** Simple containers for fixed values can inherit from this. */
abstract trait Constant extends Variable

/** Used as a marker for Variables whose value does not change once created. */
abstract trait ConstantValue extends Variable

/**For variables whose value has a type stored in type ValueType */
abstract trait TypedVariable {
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

// TODO remove this now that we have Proposer?  No.  GibbsSampler1 is now using it.  -akm
/** A variable that can iterate over its possible configurations */
trait IterableSettings{
	this: Variable =>
  /** Return an iterator over some/all configurations of this variable, each time returning simply the same Variable object. */
  def settings: Iterator[{def set(d:DiffList):Unit}]
}

// TODO could this be put in TypedVariable?  But then we'd have to figure out how to compare VectorVariable
trait PrimitiveComparison[T] {
  this :Variable =>
  def value : T
  def ===(other: PrimitiveComparison[T]) = value == other.value
  def !==(other: PrimitiveComparison[T]) = value != other.value

}

abstract class PrimitiveObservation[T](theValue:T) extends Variable with TypedVariable with PrimitiveComparison[T] {
	type VariableType <: PrimitiveObservation[T]
  type ValueType = T
  class DomainInSubclasses
  protected val _value: T = theValue
  def value = _value
  //def ===(other: PrimitiveObservation[T]) = value == other.value
  //def !==(other: PrimitiveObservation[T]) = value != other.value
  override def toString = printName + "(" + value.toString + ")"
}

/**A variable with a single mutable (unindexed) value */
// TODO A candidate for Scala 2.8 @specialized
abstract class PrimitiveVariable[T] extends Variable with TypedVariable with PrimitiveComparison[T] {
  def this(initval:T) = { this(); set(initval)(null) } // initialize like this because subclasses may do coordination in overridden set()()
	type VariableType <: PrimitiveVariable[T]
  type ValueType = T
  class DomainInSubclasses
  protected var _value: T = _
  def value = _value
  def set(newValue: T)(implicit d: DiffList): Unit =
    if (newValue != _value) {
      if (d != null) d += new PrimitiveDiff(_value, newValue)
      _value = newValue
    }
  def :=(newValue:T) = set(newValue)(null)
  // Should we implement "equals" here to compare Variable values??
  // No, I don't think so because we might need to put multiple variables with the same values in a HashMap
  // But we can implement our own specialized equality method... 
  // (the name === overlaps with an implicit conversion in scalatest, but I don't think this matters)
  //def ===(other: PrimitiveVariable[T]) = _value == other._value
  //def !==(other: PrimitiveVariable[T]) = _value != other._value
  override def toString = printName + "(" + value.toString + ")"
	case class PrimitiveDiff(oldValue: T, newValue: T) extends Diff {
  	//        Console.println ("new PrimitiveDiff old="+oldValue+" new="+newValue)
  	def variable: PrimitiveVariable[T] = PrimitiveVariable.this
  	def redo = _value = newValue
  	def undo = _value = oldValue
  }
}

/** For variables that have a true value described by a Scala type T. */
trait PrimitiveTrueValue[T] {
	this: PrimitiveVariable[T] =>
  var trueValue: T = _
  def isUnlabeled = trueValue == _
}

/** For use with variables whose values are mapped to dense integers */
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


abstract trait SingleIndexedObservation extends IndexedVariable {
	type VariableType <: SingleIndexedObservation
 	class DomainInSubclasses
	def index : Int
	override def toString = printName + "(" + index + ")"
	override def vector = new SingletonBinaryVector(domain.allocSize, index)
	def ===(other: SingleIndexedObservation) = index == other.index
	def !==(other: SingleIndexedObservation) = index != other.index
} 

/** For variables whose values are associated with a an Int from an index. */
abstract trait SingleIndexedVariable extends SingleIndexedObservation with Proposer with MultiProposer with IterableSettings {
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
	def :=(newIndex:Int) = setByIndex(newIndex)(null)
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
	/** Tests equality of variable values, whereas == tests equality of variable objects themselves. */
	case class SingleIndexedDiff(oldIndex: Int, newIndex: Int) extends Diff {
		@scala.inline final def variable: SingleIndexedVariable = SingleIndexedVariable.this
		@scala.inline final def redo = _index = newIndex
		@scala.inline final def undo = _index = oldIndex
	}
}

/** For variables that can store counts associated with each of their possible values, for use in estimating marginal probabilities from samples. */
// TODO: I'm not sure I like the names of these methods.  Consider changes. 
trait SampleCounts {
  this : SingleIndexedVariable =>
  val sampleCounts = new Array[Double](domain.allocSize)
  var sampleTotal = 0.0
  def incrementSample(incr:Double) : Unit = { sampleCounts(index) += incr; sampleTotal += incr }
  def incrementSample : Unit = incrementSample(1.0)
  def clearSamples = { sampleTotal = 0.0; for (i <- 0 until domain.allocSize) sampleCounts(i) = 0.0 }
  def samplePr(idx:Int) : Double = sampleCounts(idx)/sampleTotal
  //def samplePr(x:VariableType#ValueType) : Double = samplePr(domain.index(x)) // TODO How can I make this typing work?
}

/** For variables put in an index, and whose value is the variable itself. */
abstract trait ItemizedVariable[This <: ItemizedVariable[This]] extends SingleIndexedVariable {
	this : This =>
  type VariableType = This
  type ValueType = This
  class DomainInSubclasses
  domain.index(this) // Put the variable in the index
}

abstract trait TypedSingleIndexedObservation[T] extends SingleIndexedObservation with TypedVariable {
	type VariableType <: TypedSingleIndexedObservation[T]
  type ValueType = T
  class DomainInSubclasses
  def value: T = domain.get(index)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + index + ")"
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
abstract trait TypedSingleIndexedVariable[T] extends SingleIndexedVariable with TypedVariable {
	type VariableType <: TypedSingleIndexedVariable[T]
  type ValueType = T
  class DomainInSubclasses
  def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
	def :=(newValue:T) = set(newValue)(null)
  def value: T = domain.get(_index)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + _index + ")"
}	

/** A Variable to hold one of an enumerated set of values of type T, and which does not change.  */
abstract class EnumObservation[T](value:T) extends TypedSingleIndexedObservation[T] with ConstantValue {
	type VariableType <: EnumObservation[T]
  class DomainInSubclasses
  val index = domain.index(value)
}

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.
// TODO Really?  Verify how much efficiency gain we could get.

/**A variable whose value is a single indexed value, initialized at construction time; mutable.
 This variable does not, however, hold a trueValue.  For that you should use a Label. */
abstract class CoordinatedEnumVariable[T](initialValue:T) extends TypedSingleIndexedVariable[T] {
	type VariableType <: CoordinatedEnumVariable[T]
  class DomainInSubclasses
	if (initialValue != null) setByIndex(domain.index(initialValue))(null)
}


/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method,  
 * ensuring no coordination is necessary for optimization of belief propagation. 
 * This variable does not hold a trueValue; for that you should use a Label. */
abstract class EnumVariable[T](initialValue:T) extends CoordinatedEnumVariable[T](initialValue) {
	type VariableType <: EnumVariable[T]
  class DomainInSubclasses
  final override def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
	final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d) // TODO uncomment final
}



/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value. */
//trait TrueIndexedValue[T] extends TypedSingleIndexedVariable[T] 
trait TrueIndexedValue {
  this : SingleIndexedVariable =>
	//type VariableType <: TrueIndexedValue // TODO Try to make this work, so that "trueValue" returns the right type
  /** The index of the true labeled value for this variable.  If unlabeled, set to -1 */
  var trueIndex: Int
  //private var _trueValue:T = domain.get(trueIndex)
  def trueValue = if (trueIndex >= 0) domain.get(trueIndex) else null
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

class TrueLabelTemplate[V<:CoordinatedLabel[_]](implicit m:Manifest[V]) extends TrueIndexedValueTemplate[V]()(m)


/** A variable with a single index and a true value. */
class CoordinatedLabel[T](trueval:T) extends CoordinatedEnumVariable[T](trueval) with TypedTrueIndexedValue[T] {
	type VariableType <: CoordinatedLabel[T]
  class DomainInSubclasses
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
}

class Label[T](trueval:T) extends CoordinatedLabel(trueval) {
  type VariableType <: Label[T]
  class DomainInSubclasses
	override final def set(newValue:T)(implicit d: DiffList) = super.set(newValue)(d)
	override final def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}

/**A variable whose value is a SparseBinaryVector; immutable. */
// I considered renaming this VectorObservation, but then I realized that methods such as += change its value. -akm
// TODO Rename to BinaryVectorVariable?
// TODO Make a constructor that takes argument of Iterable[T]
abstract class VectorVariable[T](initVals:Iterable[T]) extends IndexedVariable with TypedVariable {
	//def this(iv:T*) = this(iv:Seq[T])
	def this() = this(null)
	type ValueType = T
	type VariableType <: VectorVariable[T]
  class DomainInSubclasses
  protected var indxs = new ArrayBuffer[Int]()
  private var _vector: Vector = null // TODO Can we make this more memory efficient?  Avoid having both Vector and ArrayBuffer?;
  if (initVals ne null) this ++= initVals
  def indices : Seq[Int] = indxs // TODO project to ensure no changes, even with casting?  But this would involve allocating the Projection
  def values : Seq[T] = { val d = this.domain; indxs.map(d.get(_)) }
  override def vector = {
  	if (_vector == null || _vector.size != domain.allocSize) {
  		val indices = indxs.toArray
  		Sorting.quickSort(indices)
  		_vector = new SparseBinaryVector(domain.allocSize, indices)
  	}
  	_vector
  }
  // TODO when we have Scala 2.8, add to the method below difflist argument with default value null
  // But will a += b syntax with with default arguments?
  def +=(value: T) : Unit = {
  	val idx = domain.index(value);
  	if (idx == IndexedDomain.NULL_INDEX) throw new Error("VectorVariable += value " + value + " not found in domain " + domain)
  	indxs += idx
  	_vector = null
  }
  //def +(value: T) = {this += value; this} // TODO Shouldn't this method actually return a new VectorVariable, leaving old one unchanged?  Yes.
  def ++=(vals: Iterable[T]) : Unit = vals.foreach(v => this += v)
  //def ++(vals: Iterable[T]) = {this ++= vals; this} // TODO this method should return a new Vector
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
class Bool(b: Boolean) extends CoordinatedEnumVariable(b) {
  def this() = this(false)
	type VariableType <: Bool
	type DomainType <: BoolDomain[VariableType]
  class DomainClass extends BoolDomain
	def ^(other:Bool) = value && other.value
	def v(other:Bool) = value || other.value
	def ==>(other:Bool) = !value || other.value
}
class BoolDomain[V<:Bool] extends IndexedDomain[V] {
  this += false
  this += true
  this.freeze
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

/**A variable class for string values. */
class StringVariable(str: String) extends PrimitiveVariable(str) {
	type VariableType = StringVariable
	class DomainInSubclasses
}

class IntRangeVariable(low:Int, high:Int) extends SingleIndexedVariable {
  type VariableType = IntRangeVariable
  type ValueType = Int
  class DomainInSubclasses
  assert(low < high)
  if (domain.size == 0) { for (i <- low until high) domain.index(i) }
  assert (domain.size == high-low)
}

/** A variable who value is a pointer to an ItemizedVariable; useful for entity-attributes whose value is another variable. */
class ItemizedVariablePointer[V<:ItemizedVariable[V]] extends TypedSingleIndexedVariable[V] {
  type VariableType = ItemizedVariablePointer[V]
  class DomainInSubclasses
}

/**For Variables that hold their list of Factors */
trait FactorList {
	this : Variable =>
  private var factorList: List[Factor] = Nil
  def addFactor(f: Factor) = factorList = f :: factorList
  def clearFactors = factorList = Nil
  def factors: Iterable[Factor] = factorList
}

/** A marker for Variables that declare themselves not to automatically change other Variables' values when they are changed */
trait NoVariableCoordination 

/** A marker for Variables that declare themselves not to only to rely on their own Factors when they are changed,
    even if changing this variable involves changes to other variables as well. 
    Furthermore the section of Factors must not change depending on the variable's values. */
trait NoFactorCoordination 


