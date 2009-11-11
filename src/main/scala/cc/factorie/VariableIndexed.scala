package cc.factorie
import scala.util.Random
import scala.reflect.Manifest
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

/** For use with variables whose values are mapped to densely-packed integers from 0 and higher, using an IndexedDomain.
 * It can apply to a single index (as in EnumVariable or IndexedVariable) or a collection of indices (as in BinaryVectorVariable) */
abstract trait IndexedVariable extends Variable with TypedVariable {
	type VariableType <: IndexedVariable
	type DomainType <: IndexedDomain[VariableType]
	class DomainClass extends IndexedDomain[VariableType]
	class DomainInSubclasses
	def vector: Vector // TODO remove this method?  No perhaps not.
}

// TODO Consider making a ConstantSingleIndexedVariable, for use by MixtureComponent
// But how would it be enforced?

/** A single-indexed Variable without storage for the index.  Sub-trait SingleIndexedVariable declares mutable storage for the index.
 * This trait is a generalization of both the (mutable) SingleIndexedVariable and the (immutable) TypedSingleIndexedObservation
 * If you are looking for a concrete implementation with storage for index, consider EnumObservation or EnumVariable or CoordinatedEnumVariable. */
abstract trait SingleIndexed extends IndexedVariable {
	type VariableType <: SingleIndexed
 	class DomainInSubclasses
	def index : Int
	override def toString = printName + "(" + index + ")"
	override def vector = new SingletonBinaryVector(domain.allocSize, index)
	def ===(other: SingleIndexed) = index == other.index
	def !==(other: SingleIndexed) = index != other.index
} 

/** For variables whose values are associated with a an Int from an index. */
abstract trait SingleIndexedVariable extends SingleIndexed with Proposer /*with IterableSettings*/ with IterableSettings {
	type VariableType <: SingleIndexedVariable
 	class DomainInSubclasses
	protected var _index = -1
	def index = _index
	def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
		// TODO Note that we do not check that (newIndex < domain.size), but perhaps we should; this would slow us down, though!
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
	/*def settings = new Iterator[{def set(d:DiffList):Unit}] {
	  case class Setting(newIndex:Int) { def set(d:DiffList) : Unit = setByIndex(newIndex)(d) }
	  var d : DiffList = _
	  var i = -1
	  val max = domain.size - 1
	  def hasNext = i < max
	  def next = { i += 1; new Setting(i)}
	}*/
	def settings = new SettingIterator {
	  var i = -1
	  val max = domain.size - 1
	  def hasNext = i < max
	  def next(difflist:DiffList) = { i += 1; val d = newDiffList; setByIndex(i)(d); d }
	  def reset = i = -1
	  override def variable : SingleIndexedVariable.this.type = SingleIndexedVariable.this
	  //override def variable : SingleIndexedVariable = SingleIndexedVariable.this
	}
	def propose(model:Model, d: DiffList) = {setByIndex(Global.random.nextInt(domain.size))(d); 0.0} // TODO remove this method
	// The reason for the "toList" (now changed to "force"), see 
	// http://stackoverflow.com/questions/1332574/common-programming-mistakes-for-scala-developers-to-avoid
	// http://creativekarma.com/ee.php/weblog/comments/the_scala_for_comprehension_from_a_java_perspective/
  // TODO Look at this issue more carefully and turn on printing in Implicits.bonusIterables to look for additional efficiencies 
	/*def multiPropose(model:Model, objective:Model, difflist: DiffList) = {
	  val aps = for (i <- 0 until domain.size force) yield {
	  	//println("SingleIndexedVariable multiPropose " +i) // TODO check this for repeated evaluation
	  	val ap = new AutoProposal(model, objective, diff => setByIndex(i)(diff))
	  	//println("SingleIndexedVariable.multiPropose i="+i+" modelScore="+ap.modelScore)
	  	ap
    }
		// val d = new DiffList; setByIndex(i)(d); new CaseProposal(d.scoreAndUndo, d)
		aps
	}*/
	/** Tests equality of variable values, whereas == tests equality of variable objects themselves. */
	case class SingleIndexedDiff(oldIndex: Int, newIndex: Int) extends Diff {
		@inline final def variable: SingleIndexedVariable = SingleIndexedVariable.this
		@inline final def redo = _index = newIndex
		@inline final def undo = _index = oldIndex
	}
}

/** For variables that can store counts associated with each of their possible values, for use in estimating marginal probabilities from samples. */
// TODO: I'm not sure I like the names of these methods.  Consider changes. 
// TODO I think this should be removed.  All inference meta-data should be stored separately from the Variables in the inferencer itself,
//  because we can't know at Variable creation time which Variables we will want to do inference on and which not.
//  Likely it would be stored in a HashMap[Variable,Array[Double]], or somesuch.
/*@deprecated // Such distributions are now stored separately from variables, in Marginal objects
trait SampleCounts {
  this : SingleIndexedVariable =>
  val sampleCounts = new Array[Double](domain.allocSize)
  var sampleTotal = 0.0
  def incrementSample(incr:Double) : Unit = { sampleCounts(index) += incr; sampleTotal += incr }
  def incrementSample : Unit = incrementSample(1.0)
  def clearSamples = { sampleTotal = 0.0; for (i <- 0 until domain.allocSize) sampleCounts(i) = 0.0 }
  def samplePr(idx:Int) : Double = sampleCounts(idx)/sampleTotal
  //def samplePr(x:VariableType#ValueType) : Double = samplePr(domain.index(x)) // TODO How can I make this typing work?
}*/

/** For variables put in an index, and whose value is the variable itself. */
abstract trait ItemizedVariable[This <: ItemizedVariable[This]] extends SingleIndexed {
	this : This =>
  type VariableType = This
  type ValueType = This
  class DomainInSubclasses
  domain.index(this) // Put the variable in the IndexedDomain
  val index = domain.index(this) // Remember our own index.  We could save memory by looking it up in the Domain each time, but speed is more important
}

/** A SingledIndexed variable (which is also a TypedVariable), whose type is specified by a type argument. */
abstract trait TypedSingleIndexedObservation[T] extends SingleIndexed {
	// But doesn't have mixin 'ConstantValue' because we can't yet be guaranteed that this variable's index will not change; we can in EnumObservation, though
	type VariableType <: TypedSingleIndexedObservation[T]
  type ValueType = T
  class DomainInSubclasses
  def value: T = domain.get(index)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + index + ")"
  // NOTE that "def index" has yet to be defined
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
abstract trait TypedSingleIndexedVariable[T] extends SingleIndexedVariable with TypedVariable {
	type VariableType <: TypedSingleIndexedVariable[T]
  type ValueType = T
  class DomainInSubclasses
  final def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
	def :=(newValue:T) = set(newValue)(null)
  def value: T = domain.get(_index)
  override def toString = printName + "(" + (if (value == this) "this" else value.toString + "=") + _index + ")"
}	

/** A Variable to hold one of an enumerated set of values of type T, and which does not change.  */
abstract class EnumObservation[T](value:T) extends TypedSingleIndexedObservation[T] with ConstantValue {
	type VariableType <: EnumObservation[T]
  class DomainInSubclasses
  final val index = domain.index(value)
}

// TODO get rid of all this "Coordinated" versus non-coordinated.  Everything should just be coordinated.
// It is less efficient, but too error-prone.
// TODO Really?  Verify how much efficiency gain we could get.
// No.  We can't do this.  For example, belief propagation relies on having no coordination

/**A variable whose value is a single indexed value, initialized at construction time; mutable.
 This variable does not, however, hold a trueValue.  For that you should use a Label. */
abstract class CoordinatedEnumVariable[T](initialValue:T) extends TypedSingleIndexedVariable[T] {
	type VariableType <: CoordinatedEnumVariable[T]
  class DomainInSubclasses
	if (initialValue != null) setByIndex(domain.index(initialValue))(null)
}

/** A kind of _EnumVariable that does no variable value coordination in its 'set' method. 
    This trait abstracts over both EnumVariable and Label, and is used in belief probagation 
    and other places that cannot tolerate coordination. */
trait UncoordinatedSingleIndexedVariable extends SingleIndexedVariable with NoVariableCoordination {
  // TODO But this does not absolutely guarantee that some other trait hasn't already overriden set and setByIndex to do coordination!
  // TODO I want some way to tell the compiler that this method should be overriding the SingleIndexedVariable.set method.
	final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method,  
 * ensuring no coordination is necessary for optimization of belief propagation. 
 * This variable does not hold a trueValue; for that you should use a Label. */
abstract class EnumVariable[T](initialValue:T) extends CoordinatedEnumVariable[T](initialValue) with UncoordinatedSingleIndexedVariable {
	type VariableType <: EnumVariable[T]
  class DomainInSubclasses
  //final override def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
	//final override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value. */
//trait TrueIndexedValue[T] extends TypedSingleIndexedVariable[T] 
trait TrueIndexedValue extends TrueSetting {
  this : SingleIndexedVariable =>
  /** The index of the true labeled value for this variable.  If unlabeled, set to (-trueIndex)-1. */
  var trueIndex: Int
  def trueValue = if (trueIndex >= 0) domain.get(trueIndex) else null
  def setToTruth(implicit d:DiffList): Unit = setByIndex(trueIndex)
  def valueIsTruth: Boolean = trueIndex == index
  def isUnlabeled = trueIndex < 0
  def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex - 1 else throw new Error("Already unlabeled.")
  def relabel = if (trueIndex < 0) trueIndex = -(trueIndex+1) else throw new Error("Already labeled.")
}

// TODO consider moving TrueIndexedValue to inside with this:TrueIndexedValue => ?
abstract trait TypedTrueIndexedValue[T] extends TrueIndexedValue with TypedSingleIndexedVariable[T] {
  class DomainInSubclasses
	def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
}

abstract class TrueIndexedValueTemplate[V<:SingleIndexedVariable with TrueIndexedValue](implicit m:Manifest[V]) extends TemplateWithVectorStatistics1[V] {
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

class Label[T](trueval:T) extends CoordinatedLabel(trueval) with UncoordinatedSingleIndexedVariable {
  type VariableType <: Label[T]
  class DomainInSubclasses
	//override final def set(newValue:T)(implicit d: DiffList) = super.set(newValue)(d)
	//override final def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
}


/**A variable class for boolean values, defined here for convenience.  If you have several different "types" of booleans, you might want to subclass this to enable type safety checks. */
class CoordinatedBool(b: Boolean) extends CoordinatedEnumVariable(b) {
  def this() = this(false)
	type VariableType <: CoordinatedBool
	type DomainType <: BoolDomain[VariableType]
  class DomainClass extends BoolDomain
  def intValue = if (b) Bool.t.index else Bool.f.index
	def ^(other:Bool) = value && other.value
	def v(other:Bool) = value || other.value
	def ==>(other:Bool) = !value || other.value
	override def toString = if (index == 0) printName+"(false)" else printName+"(true)"
}
class Bool(b: Boolean) extends CoordinatedBool(b) with UncoordinatedSingleIndexedVariable {
  def this() = this(false)
	type VariableType <: Bool
	type DomainType <: BoolDomain[VariableType]
  class DomainClass extends BoolDomain
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



class IntRangeVariable(low:Int, high:Int) extends SingleIndexedVariable {
  type VariableType = IntRangeVariable
  type ValueType = Int
  class DomainInSubclasses
  assert(low < high)
  // TODO But note that this will not properly initialize the IndexedDomain until an instance is created
  if (domain.size == 0) { for (i <- low until high) domain.index(i) }
  assert (domain.size == high-low)
}

// TODO Is this really necessary?  Who added this? -akm
/** A variable who value is a pointer to an ItemizedVariable; useful for entity-attributes whose value is another variable. */
class ItemizedVariablePointer[V<:ItemizedVariable[V]] extends TypedSingleIndexedVariable[V] {
  type VariableType = ItemizedVariablePointer[V]
  class DomainInSubclasses
}
