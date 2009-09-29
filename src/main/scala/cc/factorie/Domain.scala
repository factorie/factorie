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
class Domain[V<:Variable] {
  /** Return the classes that have this Domain. */
	def variableClasses : Seq[Class[V]] = {
		val matchingElements : Seq[(Class[_],Domain[_])] = Domain.domains.elements.filter({case (key,value) => value == this}).toList
		matchingElements.map({case (key,value) => key})
	}
}

/*
class ItemizedDomain[V <: ItemizedVariable](implicit m:Manifest[V]) extends Domain[V] with util.Index[V] {
	def randomValue: V = get(Model.this.random.nextInt(size))
}
*/

class IndexedDomain[V<:IndexedVariable] extends Domain[V] with util.Index[V#ValueType] {
	def randomValue : V#ValueType = randomValue(Global.random)
	def randomValue(random:Random): V#ValueType = get(random.nextInt(size))
}
object IndexedDomain {
  val NULL_INDEX = -1
}

class StringDomain[V<:IndexedVariable {type ValueType = String}] extends IndexedDomain[V] {
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

// TODO We can get rid of LabelValue??
//class LabelDomain[V<:Label] extends IndexedDomain[W forSome {type W <: LabelValue}]
class LabelDomain[V<:CoordinatedLabel] extends IndexedDomain[V] {
  private val stringIndex = new util.Index[String] {} // TODO Why is Index abstract (and thus requiring the {})
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
		override def toString = "LabelValue("+entry+")"
		override def entry : String = stringIndex.get(index)
		override def equals(other: Any) = other match {
			case label: Value => this.index == label.index
			// case value : V#ValueType => this.index == index(value) // TODO consider something like this
			case _ => false
		}
		def compare(other: Value) = other.index - this.index
	}
}


/** A static map from a Variable class to its Domain. */
object Domain {
	private val _domains = new HashMap[Class[_], Domain[_]]()
	def domains : scala.collection.Map[Class[_],Domain[_]] = _domains
	/** Get the Domain for Variables of type V */
 	def apply[V<:Variable](implicit mv:Manifest[V]) = get[V](mv.erasure)
	/** Get the Domain for Variables of class vc */
	def get[V<:Variable](vc:Class[_]) = {
		//println("Domain.get "+vc+" classes.length="+vc.getDeclaredClasses.length)
		//if (_domains.isDefinedAt(vc)) println("Domain.get "+vc+" already defined"); Console.flush
		_domains.getOrElseUpdate(vc, getDomainForClass(vc)).asInstanceOf[V#DomainType]
	}
	/** Make Variables of type V1 use the same Domain as Variables of type V2. */
	def alias[V1<:Variable,V2<:Variable](implicit vm1:Manifest[V1], vm2:Manifest[V2]) = {
		if (_domains.isDefinedAt(vm1.erasure)) throw new Error("Cannot alias a Domain that has already been used.")
		_domains.put(vm1.erasure, get[V2](vm2.erasure))
	} 
	/** Register d as the domain for variables of type V. */
	def +=[V<:Variable](d:Domain[V])(implicit vm:Manifest[V]) = {
		val c = vm.erasure
		if (_domains.isDefinedAt(c)) throw new Error("Cannot add a Domain["+vm+"] when one has already been created for "+c)
		val dvc = getDomainVariableClass(c)
		//println("+= dvc="+dvc); println("+= c="+c)
		if (dvc != null && dvc != c && _domains.isDefinedAt(dvc)) throw new Error("Cannot add a Domain["+vm+"] when one has already been created for superclass "+dvc)
		if (dvc != c) throw new Error("Cannot add a Domain["+vm+"] because superclass "+dvc+" should have the same Domain; you should consider instead adding the domain to "+dvc)
		_domains.put(vm.erasure, d)
	}
	/** Return a Domain instance for Variables of class c, constructing one if necessary.  Also put it in the _domains map. */
	private def getDomainForClass(c:Class[_]) : Domain[_] = {
		if (domainInSubclasses(c)) throw new Error("Cannot get a Domain for "+c+" because it declares DomainInSubclasses, and should be considered abstract.")
		var dvc = getDomainVariableClass(c)
		//println("getDomainForClass c="+c+" dvc="+dvc)
		if (dvc == null) dvc = c
		_domains.getOrElseUpdate(dvc, newDomainFor(getDomainClass(c),dvc))
	}
	/** Construct a Domain of class dc.  (Parameter vc is currently unused.)  Domains must have only a zero-arg constructor. */
	private def newDomainFor[D](dc:Class[D],vc:Class[_]) : Domain[_] = {
		val constr = dc.getConstructors()(0)
		//println("constructor # args="+constr.getParameterTypes.length)
		dc.getConstructors()(0).newInstance().asInstanceOf[Domain[_]]
	}
	/** Find the (sub)class of Domain to use for constructing a domain for variable class c. */
	private def getDomainClass(c:Class[_]) : Class[_] = {
		// First check this class to see if it specifies the DomainClass
		val classes = c.getDeclaredClasses()
		val index = if (classes == null) -1 else classes.findIndexOf(c=>c.getName.endsWith("$DomainClass"))
		//println("getDomainClass superclass"+c.getSuperclass.getSuperclass)
		if (index >= 0) return classes(index).getSuperclass
		// Next check the interfaces/traits
		val interfaces = c.getInterfaces.elements
		while (interfaces.hasNext) {
			val dc = getDomainClass(interfaces.next)
			if (dc != null) return dc
		}
		// Next check the superclass
		val sc = c.getSuperclass
		if (sc == null || sc == classOf[java.lang.Object]) null //throw new Error("DomainClass not found")
		else getDomainClass(sc)
	}
	private def domainInSubclasses(c:Class[_]) : Boolean = c.getDeclaredClasses.findIndexOf(c=>c.getName.endsWith("$DomainInSubclasses")) != -1

  //def domainInSubclassesByAnnotation(c:Class[_]) : Boolean = c.isAnnotationPresent(classOf[DomainInSubclasses])


	/** Find a potential substitute for c as the key into _domains. */
	private def getDomainVariableClass(c:Class[_]) : Class[_] = {
		//println("getDomainVariableClass c "+c+" classes.length="+c.getDeclaredClasses.length)
		if (domainInSubclasses(c)) throw new Error("Cannot create Domain["+c+"] because it declares inner class DomainInSubclasses.")
		else if (c.getSuperclass == null || c.getSuperclass == classOf[java.lang.Object]) c 
		else if (domainInSubclasses(c.getSuperclass)) c 
		else getDomainVariableClass(c.getSuperclass)
	}
}


