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

/**Abstract superclass of all variables.  Don't need to know its value type to use it. */
trait Variable {
	type VariableType <: Variable
	def domain: Domain[VariableType] = Domain[VariableType](this.getClass)
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
	def factors(model:Model): Iterable[Factor] = model.factors(this)
}

/*
trait ItemizedVariable extends Variable {
	type VariableType <: ItemizedVariable
	def domain: ItemizedDomain[VariableType] = Domain[VariableType](this.getClass).asInstanceOf[ItemizedDomain[VariableType]]
	                                                                                            domain.index(this) // Put this variable in the index.
}
*/

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
abstract class IndexedVariableWithDomain[V <: IndexedVariable](implicit variableOfDomain: Manifest[V]) extends IndexedVariable {
	// put this.getClass in the Domain global hash.  // TODO but no need to do it again and again!
	Domain.set(this.getClass, Domain[V](variableOfDomain))
	override def domain: IndexedDomain[VariableType] = IndexedDomain.get[VariableType](variableOfDomain.erasure)
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
/** A variable that can iterate over its possible configurations */
trait IterableSettings {
	this: Variable =>
  /** Return an iterator over some/all configurations of this variable, each time returning simply the same Variable object. */
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

/** For variables that have a true value describable by a Scala type T. */
trait PrimitiveTrueValue[T] {
	this: PrimitiveVariable[T] =>
  var trueValue: T = _
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
	//def setFirstValue: Unit = throw new Error("Cannot set constant IndexedVariable")
	//def hasNextValue = false
	//def setNextValue: Unit = {}
}

/** For variables whose values are associated with a an Int from an index. */
trait SingleIndexedVariable extends IndexedVariable with Proposer with MultiProposer {
	type VariableType <: SingleIndexedVariable
	protected var indx = -1 //domain.index(initval) // but this provides no way to initialize with null
	// Consider changing this method name to just "set"?  But then will code readers more easily get confused?
	def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
		if (newIndex < 0) throw new Error("SingleIndexedVariable setByIndex can't be negative.")
		if (newIndex != indx) {
			if (d != null) d += new SingleIndexedDiff(indx, newIndex)
			indx = newIndex
		}
	}
	def setRandomly(implicit random:Random, d:DiffList) : Unit = setByIndex(random.nextInt(domain.size))
	def setRandomly(implicit random:Random) : Unit = setByIndex(random.nextInt(domain.size))(null)
	def setRandomly : Unit = setRandomly(cc.factorie.Global.random)
	def propose(d: DiffList)(implicit random:Random) = {setByIndex(random.nextInt(domain.size))(d); 0.0}
	// The reason for the "toList", see 
	// http://stackoverflow.com/questions/1332574/common-programming-mistakes-for-scala-developers-to-avoid
	// http://creativekarma.com/ee.php/weblog/comments/the_scala_for_comprehension_from_a_java_perspective/
  // TODO Look at this issue more carefully and turn on printing in Implicits.bonusIterables to look for additional efficiencies 
	def multiPropose(model:Model, objective:Model, difflist: DiffList) = for (i <- 0 until domain.size toList) yield {
		new AutoProposal(model, objective, diff => setByIndex(i)(diff))
		// val d = new DiffList; setByIndex(i)(d); new CaseProposal(d.scoreAndUndo, d)
	}
	def index = indx
	override def toString = printName + "(" + indx + ")"
	override def vector = new SingletonBinaryVector(domain.allocSize, indx)
	/** Tests equality of variable values, whereas == tests equality of variable objects themselves. */
	def ====(other: SingleIndexedVariable) = indx == other.indx
	def !===(other: SingleIndexedVariable) = indx != other.indx
	case class SingleIndexedDiff(oldIndex: Int, newIndex: Int) extends Diff {
		def variable: SingleIndexedVariable = SingleIndexedVariable.this
		def redo = setByIndex(newIndex)(null)
		def undo = setByIndex(oldIndex)(null)
	}
}

/** For variables put in a index, and whose value is the variable itself. */
trait ItemizedVariable[This <: ItemizedVariable[This]] extends SingleIndexedVariable {
	this : This =>
  type VariableType = This
  type ValueType = This
  domain.index(this) // Put the variable in the index
}

/** For variables holding a single indexed value, which is not the variable object itself, but a Scala value of type T. */
trait TypedSingleIndexedVariable[T] extends SingleIndexedVariable with TypedVariable {
	type VariableType <: TypedSingleIndexedVariable[T]
  type ValueType = T
  def set(newValue: T)(implicit d: DiffList) = setByIndex(domain.index(newValue))
  def value: T = domain.get(indx)
  override def toString = printName + "(" + value.toString + "=" + indx + ")"
}	


/**A variable whose value is a single indexed value; mutable */
trait CoordinatedEnumVariable[T] extends TypedSingleIndexedVariable[T] {
	type VariableType <: CoordinatedEnumVariable[T]
  override def domain: IndexedDomain[VariableType] = IndexedDomain.get[VariableType](this.getClass)
  // initialize the variable's value; using this method in case coordination in necessary
  //setByIndex(domain.index(initval))(null)
}

/**A variable of finite enumerated values that has a true "labeled" value, separate from its current value. */
//trait TrueIndexedValue[T] extends TypedSingleIndexedVariable[T] 
trait TrueIndexedValue extends SingleIndexedVariable {
  /** The index of the true labeled value for this variable.  If unlabeled, set to -1 */
  var trueIndex: Int
  //private var _trueValue:T = domain.get(trueIndex)
  def trueValue = if (trueIndex >= 0) domain.get(trueIndex) else null // _trueValue
  def isUnlabeled = trueIndex < 0
  def unlabel = if (trueIndex >= 0) trueIndex = -trueIndex else throw new Error("Already unlabeled.")
}

trait TypedTrueIndexedValue[T] extends TrueIndexedValue with TypedSingleIndexedVariable[T] {
	def trueValue_=(x: T) = if (x == null) trueIndex = -1 else trueIndex = domain.index(x)
}

class TrueIndexedValueTemplate[V<:TrueIndexedValue](implicit m:Manifest[V]) extends TemplateWithExpStatistics1[V]()(m) {
  def score(s:Stat) = if (s.s1.index == s.s1.trueIndex) 0.0 else 1.0
}

/**A variable whose value is a single indexed value that does no variable coordination in its 'set' method.  Ensuring no coordination is necessary for optimization of belief propagation. */
abstract class EnumVariable[T](trueval:T) extends CoordinatedEnumVariable[T] with TypedTrueIndexedValue[T] with IterableValues[T] {
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
	override final def set(newValue: T)(implicit d: DiffList) = super.set(newValue)(d)
	override def setByIndex(index: Int)(implicit d: DiffList) = super.setByIndex(index)(d)
	//override def setFirstValue: Unit = setByIndex(0)(null)
	//override def hasNextValue = indx < domain.size - 1
	//override def setNextValue: Unit = if (hasNextValue) setByIndex(indx + 1)(null) else throw new Error("No next value")
	override def isConstant = false
	def iterableValues: Iterable[T] = domain
	def iterableOtherValues: Iterable[T] = domain.filter(_ != value)
}

class TrueEnumTemplate[V<:EnumVariable[_]](implicit m:Manifest[V]) extends TrueIndexedValueTemplate[V]()(m)

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
class CoordinatedLabel(trueval: String) extends CoordinatedEnumVariable[LabelValue] with TypedTrueIndexedValue[LabelValue] with IterableValues[LabelValue] {
	def this(trueval: LabelValue) = this (trueval.entry)
	var trueIndex = domain.index(trueval)
	setByIndex(domain.index(trueval))(null)
	//def this(initval:String) = this(LabelDomain.get[Label](this/*.getClass*/).get(initval))
	type VariableType <: CoordinatedLabel
	override def domain: LabelDomain[VariableType] = LabelDomain.get[VariableType](this.getClass)
	override def isConstant = false
	def set(s: String)(implicit d: DiffList) = setByIndex(domain.index(s))
	//override def setFirstValue: Unit = setByIndex(0)(null)
	//override def hasNextValue = indx < domain.size - 1
	//override def setNextValue: Unit = if (hasNextValue) setByIndex(indx + 1)(null) else throw new Error("No next value")
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

class TrueLabelTemplate[V<:Label](implicit m:Manifest[V]) extends TrueIndexedValueTemplate[V]()(m)

/**A variable whose value is a SparseBinaryVector; immutable. */
// TODO Rename to BinaryVectorVariable
abstract class VectorVariable[T] extends IndexedVariable with TypedVariable {
	type ValueType = T
	type VariableType <: VectorVariable[T]
  //def this (es:T*) = this(es.toArray)   TODO include this again later
  override def isConstant = true
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
      s ++= ("," + domain.get(iter.next).toString + "=" + i)
    }
    s ++= ")"
    s.toString
  }
}

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
}

/* TODO Consider adding such a thing
class IntRange(i:Int) extends IndexedVariable {
	type VariableType = IntRange
	def trueScore = 0.0
}*/

/**A variable class for string values. */
class StringVariable(str: String) extends PrimitiveVariable(str) {
	type VariableType = StringVariable
}

/**For Variables that hold their list of Factors */
trait FactorList requires Variable {
  private var factorList: List[Factor] = Nil
  def addFactor(f: Factor) = factorList = f :: factorList
  def clearFactors = factorList = Nil
  def factors: Iterable[Factor] = factorList
}


