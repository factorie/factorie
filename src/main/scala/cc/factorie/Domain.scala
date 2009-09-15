package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
//import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._


/**The "domain" of a variable---also essentially serving as the variables' "type" */
class Domain[V<:Variable](implicit m:Manifest[V]) {
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
}

/*
class ItemizedDomain[V <: ItemizedVariable](implicit m:Manifest[V]) extends Domain[V] with util.Index[V] {
	def randomValue: V = get(Model.this.random.nextInt(size))
}
*/

abstract class IndexedDomain[V <: IndexedVariable](implicit m: Manifest[V]) extends Domain[V] with util.Index[V#ValueType] {
	//if (Domain.forClass(m.erasure) != null) throw new Error("IndexedDomain["+m.erasure.getName+"] already exists!")
	//Domain.set(m.erasure, this)
	def randomValue : V#ValueType = randomValue(Global.random)
	def randomValue(random:Random): V#ValueType = get(random.nextInt(size))
}

class StringDomain[V <: IndexedVariable {type ValueType = String}](implicit m: Manifest[V]) extends IndexedDomain[V] {
	/* For all member variables, if its type is String and its name is all upper case or digits,
		set its value to its name, and intern in the Domain.  Usage:
		object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value; internValues } */
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
	/* Enables syntax like Domain[Token] */
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

