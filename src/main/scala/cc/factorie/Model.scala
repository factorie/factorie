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

trait Model extends Variables { 

	// Random number generator specific to the Model
	def randomSeed = 0
	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
 
	/* Replaced by sample() method in cc.factorie.util.Implicits converstion of Iterable 
 	def randomVariable[V](vars: RandomAccessSeq[V]): V = vars(random.nextInt(vars.size));
	def randomVariable[V](vars: RandomAccessSeq[V], test: V => boolean): V = {
		val filteredVars = vars.filter(test)
		filteredVars(random.nextInt(filteredVars.size))
	}*/

  // The collection of Domains of Variables within the Model	
 	val modelDomains = new HashSet[Domain[_ <: Variable]]()

 	// Management of Factor Templates within the Model
	val modelTemplates = new ArrayBuffer[Template]()
	def clearModelTemplates = modelTemplates.clear
	def modelTemplatesOf[T <: Template](implicit m: Manifest[T]): Iterable[T] = {
		for (t <- modelTemplates; if (m.erasure.isAssignableFrom(t.getClass))) yield t.asInstanceOf[T]
	}
	def modelScoreTemplates = modelTemplates.filter(!_.isInstanceOf[NoScore]) 
	def modelMarginalSamplesTemplates = modelTemplates.filter(_.isInstanceOf[MarginalSamples])
   
 /*
  // TODO AKM: All this cruft really annoys me, but it was added for speed in DiffList.modelScore.  Does it really take so much time to post-filter Templates?
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
 */

	// Helper functions for assessing the model state
	def worldTrueScore(vars: Iterable[Variable]): Double = vars.sum(_ trueScore)
	def worldAccuracy(vars: Collection[Variable]): Double = worldTrueScore(vars) / vars.size

 
 
	// Domains
 
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

 
 
	// Variables
 
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
			modelTemplates.foreach(_.unroll0(this).foreach(f => factors += f))
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
		override def propose(d: DiffList) = {setByIndex(random.nextInt(domain.size))(d); 0.0}
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
 

 
 
	// Templates
	
 	/** A single factor in a factor graph.  In other words, a factor
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

	/** Unroll a collection of Variables in a Model, and return all Factors that touch those Variables. */
	def unroll(variables: Iterable[Variable]): Iterable[Factor] = {
		var factors = new HashSet[Factor]
		// unroll all factors touching all v in variables
		for (v <- variables; t <- modelTemplates)
			t.unroll0(v).foreach(f => factors += f)
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
	trait Stat extends Iterable[Stat] {
		def template: Template
		def vector: Vector
		/**A Stat can act as as singleton Iterable[Stat].
		This makes it easier to return a single Stat from unroll* methods. */
		def elements: Iterator[Stat] = Iterator.single(this)
		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Stat) = List(that, this)
	}

   

	/** The template for many factors.  Manages its connections to neighboring variables.
  	* Stores its parameters and has methods for templating behavior */
	trait Template {
	  type TemplateType <: Template
		type S <: Stat // The case class of sufficient statistics
		//type N <: Factor // The case class of neighbors, defined in subtrait of Neighbors
    val statClasses = new ArrayBuffer[Class[IndexedVariable]] {
      var frozen :Boolean = false;
      def freeze = frozen = true;
      override def ensureSize(s:Int) = if (frozen) throw new IllegalStateException("Template already .init'ed.") else super.ensureSize(s)
    }
	  def statDomains : Seq[IndexedDomain[_]] = {
	    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
	    statClasses.map(IndexedDomain.get(_))
	  }	
    def freezeDomains : Unit = {
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.foreach(IndexedDomain.get[IndexedVariable](_).freeze)
    }	
    lazy val statsize : Int = {
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.productInts(IndexedDomain.get[IndexedVariable](_).allocSize)
    }	
 		/**A version of unroll0 that takes the Diff object instead of just the variable */
		def unroll(d: Diff): Iterable[Factor] = if (d.variable == null) Nil else unroll0(d.variable)
		def unroll0(v: Variable): Iterable[Factor]
		def template : TemplateType = this.asInstanceOf[TemplateType]
		trait Factor extends Model.this.Factor { def template : TemplateType = Template.this.asInstanceOf[TemplateType] }
    trait Stat extends Model.this.Stat { override def template : TemplateType = Template.this.asInstanceOf[TemplateType] }
		def factors(difflist: Iterable[Diff]): Iterable[Factor] = {
			var result = new LinkedHashSet[Factor]()
			difflist.foreach(diff => result ++= unroll(diff))
			result.toList
		}
    //class Factor extends Templates.Factor
    def score(s:S) : Double
		def scoreStats(ss:Iterable[_<:S]) : Double = ss.foldLeft(0.0)(_ + score(_))
		def vectorStats(ss:Iterable[_<:S]) : Vector = {
		  val iter = ss.elements
		  if (iter.hasNext) {
		    val first: Vector = iter.next.vector
        if (!iter.hasNext) // There is only one there
          first
          else {
            var vec = new SparseVector(first.size) // TODO if 'first' is SparseBinaryVector this should be Sparse also
            vec += first
            while (iter.hasNext)
              vec += iter.next.vector
            vec
          }	
		  } else { // an empty iterator over Suff's.  Just return a (sparse) vector of zero's.
		    new SparseVector(statsize)
		  }
		} 
	}	
	trait LogLinearScoring extends Template {
		type TemplateType <: LogLinearScoring
		def weights: Vector
		//def score(s:S): Double
	}
	trait DenseLogLinearScoring extends Template {
		type TemplateType <: DenseLogLinearScoring
		lazy val weights = {freezeDomains; new DenseVector(statsize)}
		def score(s:S) = weights dot s.vector
	}
	trait SparseLogLinearScoring extends Template {
		type TemplateType <: SparseLogLinearScoring
		lazy val weights = new SparseVector(statsize)
		def score(s:S) = weights dot s.vector
	}
	//trait LogLinearTemplate extends LogLinearScoring with Template
	//trait LogLinearTemplate extends Template with LogLinearScoring

 	abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template {
 	  val nc1 = nm1.erasure
    def unroll0(v:Variable): Iterable[Factor] =
      if (nc1.isAssignableFrom(v.getClass)) unroll1(v.asInstanceOf[N1])
      else Nil
		def unroll1(v:N1): Iterable[Factor] = new Factor(v)
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1)
 	  def statistic(v1:N1) : Iterable[S]
		case class Factor(n1:N1) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic1[S1<:IndexedVariable] extends Template {
    case class Stat(s1:S1) extends super.Stat with Iterable[Stat] {
      def vector : Vector = s1.vector
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1]) : this.type = { statClasses += m1.erasure.asInstanceOf[Class[IndexedVariable]]; statClasses.freeze; this }  
  }
  abstract class TemplateWithStatistic1[N1<:IndexedVariable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistic1[N1]	{
		def statistic(v1:N1): Iterable[Stat] = Stat(v1)
		init(nm1)
	}

  abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
    def unroll0(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
      if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])	
      if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])	
			ret
		}
		def unroll1(v:N1): Iterable[Factor] 
		def unroll2(v:N2): Iterable[Factor]
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1, f.n2)
 	  def statistic(v1:N1, v2:N2) : Iterable[S]
		case class Factor(n1:N1, n2:N2) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic2[S1<:IndexedVariable,S2<:IndexedVariable] extends Template {
    this : Template =>
    case class Stat(s1:S1, s2:S2) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, s2.vector)
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithStatistic2[N1<:IndexedVariable,N2<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with Statistic2[N1,N2]	{
		def statistic(v1:N1,v2:N2): Iterable[Stat] = Stat(v1,v2)
		init(nm1, nm2)
	}

  abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
 	  val nc3 = nm3.erasure
    def unroll0(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
			if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
			if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
			if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
			ret
		}
		def unroll1(v:N1): Iterable[Factor]
		def unroll2(v:N2): Iterable[Factor]
		def unroll3(v:N3): Iterable[Factor]
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1, f.n2, f.n3)
 	  def statistic(v1:N1, v2:N2, v3:N3) : Iterable[S]
		case class Factor(n1:N1, n2:N2, n3:N3) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic3[S1<:IndexedVariable,S2<:IndexedVariable,S3<:IndexedVariable] extends Template {
    case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]], m3.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithStatistic3[N1<:IndexedVariable,N2<:IndexedVariable,N3<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with Statistic3[N1,N2,N3]	{
		def statistic(v1:N1,v2:N2,v3:N3): Iterable[Stat] = Stat(v1,v2,v3)
		init(nm1, nm2, nm3)
	}

  /** Perform the outer-product of two vectors, to yield */
	private def flatOuter(vector1: Vector, vector2: Vector) : Vector = vector1 match {
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

 
	trait MarginalSamples extends Template {
		lazy val samples = {freezeDomains; new Array[Double](statsize)}
		def clearSamples = for (i <- 0 until samples.length) samples(i) = 0.0 // TODO surely there is a faster way
	}

	trait NoScore extends Template {
		override final def score(s: S) = 0
	}

 
 
	// Proposals
 
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

	/**An object (typically a variable or a world) that can propose changes to itself, and possibly also other variables through variable value coordination */
	trait Proposer {
		/** Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
		def propose(d: DiffList): Double
	}

	/**An object (typically a variable or a world) that can propose a menu of changes to itself.  Useful for considering all choices and choosing the max, or for parameter estimation in which we check the ranking of the best choice. */
	trait MultiProposer extends Proposer {
		/**Make all possible proposals.
		The argument is not implicit because we do not want it implicitly passed to other methods that change variables. Instead a new DiffList should be created for each Proposal.  The only reason to pass the DiffList argument to
		this method is so that implementations can check the DiffList for circular changes. */
		def multiPropose(d: DiffList): Seq[Proposal]
		def propose(d: DiffList): Double = {
			val proposals = multiPropose(d)
			val maxModelScore = proposals.max(_.modelScore).modelScore
			val proposal = proposals.sampleProportionally(p => Math.exp(p.modelScore - maxModelScore))
			proposal.diff.redo
			d ++= proposal.diff
			0.0 // TODO Make an API for putting ratio of q's into multiPropose
		}
	}
 
 
	// Diffs
 
 	// TODO consider adding state that throws error if users try to "undo" or "redo" twice in a row.
	/**A change record for a variable, holding its old and new values */
	trait Diff {
		def variable: Variable
		def redo: Unit
		def undo: Unit
	}

 	abstract class AutoDiff(implicit d:DiffList) extends Diff {
 	  if (d != null) d += this
    redo
    override def toString = this.getClass.toString
 	}

 	/**The default DiffList is null, and therefore calls to
	Variable.set that don't set a DiffList do not accumulate Diffs */
	//implicit val nullDiffList : DiffList = null

	/**A collection of changes to variables; the result of a "jump" in configuration */
	class DiffList extends ArrayBuffer[Diff] {
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
			val s = modelScoreTemplates.map(_ factors(this)).foldLeft(0.0)((total, fs) => total + fs.sum(_ score))
			//scoreFactors.sum(_ score)
			s
		}
		/**Return the sum of scores of all factors that touch changed variables, and whose Template's pass the test. */
		def modelScoreFilter(test: (Template) => Boolean): Double =
			modelScoreTemplates.filter(test).flatMap(_ factors (this)).sum(_ score)
		def scoreAndUndo: Double = {
			var s = modelScore
			//println("Score: " + s)
			//log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
			this.undo
			// We need to re-calculate the Factors list because the structure may have changed
			s -= modelScore
			//log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
			s
		}
	}
 
 
}