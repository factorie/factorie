/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.Implicits._
import java.io.{File,PrintStream,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}


/** The "domain" of a variable---also essentially serving as the variables' "type".
    @author Andrew McCallum
    @since 0.8
*/
class Domain[V<:Variable](implicit m:Manifest[V]) {
  /** Return the classes that have this Domain. */
  def variableClasses: Seq[Class[V]] =
    Domain.domains.elements.filter({case (key,value) => value == this}).map({case (key,value) => key.asInstanceOf[Class[V]]}).toList

  // TODO Trying to create ability for domain assignment syntax like:
  // Domain[Token] := new CategoricalDomain[Token] { ... }
  // I don't think this actually works yet
  def :=(d:Domain[Variable]) = println("In Domain.:=")
  def save(dirname:String): Unit = {}
  def load(dirname:String): Unit = {}
  //made this public in order to check from outside whether the file to load from exists
  def filename:String = this.getClass.getName+"["+variableClasses.apply(0).getName+"]"
  // Automatically register ourselves.  
  // This enables pairing a Domain with its variable V by simply: "val MyDomain = new FooDomain[MyVariable]"
  Domain.+=[V](this)(m)
}

/*
class ItemizedDomain[V <: ItemizedVariable](implicit m:Manifest[V]) extends Domain[V] with util.Index[V] {
  def randomValue: V = get(Model.this.random.nextInt(size))
}
*/

// TODO Consider if it would be helpful to have a RealDomain or PositiveRealDomain

/** A Domain that has a positive integer size. */
class DiscreteDomain[V<:DiscreteValues](implicit m:Manifest[V]) extends Domain[V]()(m) {
  private val _size: Int = {
    val c = m.erasure
    val s = if (c.isAnnotationPresent(classOf[DomainSize])) m.erasure.getAnnotation(classOf[DomainSize]).value else -1
    if (s == -1 && this.getClass == classOf[DiscreteDomain[V]]) throw new Error("DiscreteDomain must have its size set with a @DomainSize annotation.")
    s
  }
  def size = _size
  //assert (size > 0)
  def freeze: Unit = {}
  def allocSize = size
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    val s = new PrintWriter(new FileWriter(f))
    s.println(size)
    s.close
  }
  override def load(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    val s = new BufferedReader(new FileReader(f))
    val line = s.readLine
    if (size != Integer.parseInt(line)) throw new Error("Saved size does not match")
  }
}

// TODO Also make a randomized-representation CategoricalDomain

class CategoricalDomain[V<:CategoricalValues](implicit m:Manifest[V]) extends DiscreteDomain[V]()(m) with util.Index[V#ValueType] with Collection[V#ValueType] /*with DomainEntryCounter[V]*/ {
  override def freeze = freeze0
  override def allocSize = allocSize0
  override def size = size0
  def randomValue : V#ValueType = randomValue(Global.random)
  def randomValue(random:Random): V#ValueType = get(random.nextInt(size))
  def +=(x:V#ValueType) : Unit = this.index(x)
  def ++=(xs:Iterable[V#ValueType]) : Unit = xs.foreach(this.index(_))
 
   override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    if (f.exists) return // Already exists, don't write it again
    val s = new PrintWriter(new FileWriter(f))
    //if (elements.next.asInstanceOf[AnyVal].getClass != classOf[String]) throw new Error("Only know how to save ValueType String.")
    if (frozen) s.println("#frozen = true") else s.println("#frozen = false")
    for (e <- elements) {
      if (e.toString.contains("\n")) throw new Error("Cannot save Domain with entry containing newline.")
      s.println(e.toString)
    }
    s.close
  }
  override def load(dirname:String): Unit = {
    if (size > 0) return // Already initialized, don't read again
    val f = new File(dirname+"/"+filename)
    val s = new BufferedReader(new FileReader(f))
    var line = s.readLine
    var willFreeze = false
    if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
    while ({line = s.readLine; line != null}) {
      //println("Domain load got "+line)
      this.index(line.asInstanceOf[V#ValueType])
    }
    if (willFreeze) freeze
    s.close
    //println("Loading Domain["+filename+"].size="+size)
  }

  // Code that used to be in DomainEntryCounter[V], but when separate was causing compiler to crash.
  /*
  type T = V#ValueType
  var gatherCounts = false
  private val _countsInitialSize = 64
  private var _counts = new Array[Int](_countsInitialSize)
  protected def ensureSize(n:Int): Unit = {
    if (_counts.length - 1 < n) {
      var newsize = _counts.length * 2
      while (newsize < n) newsize *= 2
      val new_counts = new Array[Int](newsize)
      Array.copy(_counts, 0, new_counts, 0, _counts.size)
      _counts = new_counts
    }
  }
  def count(i:Int): Int = _counts(i)
  def count(entry:T): Int = _counts(indexOnly(entry))
  def incrementCount(i:Int): Unit = { ensureSize(i); _counts(i) += 1 }
  def incrementCount(entry:T): Unit = incrementCount(indexOnly(entry))
  private def someCountsGathered: Boolean = { for (i <- 0 until _counts.size) if (_counts(i) > 0) return true; return false }
  /** Return the index associated with this entry, without incrementing its count, even if 'gatherCounts' is true. */
  def indexOnly(entry:T): Int = super.index(entry)
  /** Return the index associated with entry, and also, if 'gatherCounts' is true, increment +1 the count associated with this entry. */
  override def index(entry:T): Int = {
    val i = super.index(entry)
    if (gatherCounts) incrementCount(i)
    i
  }
  /** Returns the number of unique elements trimmed */
  def trimBelowCount(threshold:Int): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _entries
    reset // TODO Should we override reset to also set gatherCounts = true?  I don't think so.
    gatherCounts = false
    for (i <- 0 until origEntries.size)
      if (_counts(i) >= threshold) indexOnly(origEntries(i))
    _counts = null // We don't need counts any more; allow it to be garbage collected.  Note that if
    freeze
    origEntries.size - size
  }
  /** Return the number of unique entries with count equal to 'c'. */
  def sizeAtCount(c:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = Math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) == c) ret += 1
    ret
  }
  /** Return the number of unique entries with count greater than or equal to 'threshold'. 
      This returned value will be the size of the Domain after a call to trimBelowCount(threshold). */
  def sizeAtOrAboveCount(threshold:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = Math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) >= threshold) ret += 1
    ret
  }
  /** Return the number of unique entries with count below 'threshold'. */
  def sizeBelowCount(threshold:Int): Int = size - sizeAtOrAboveCount(threshold)  
  /** Returns the count threshold below which entries were discarded. */
  def trimBelowSize(target:Int): Int = {
    assert(!frozen)
    var threshold = 2
    while (sizeAtOrAboveCount(threshold) >= target) threshold += 1
    trimBelowCount(threshold)
    threshold
  }
  */
}

object CategoricalDomain {
  val NULL_INDEX = -1
}


/** Mixin for CategoricalDomain that facilitates counting occurreces of entries, and trimming the Domain size.
    WARNING: Any indices that you use and store before trimming will not be valid after trimming!
    Typical usage:
    <pre>
    Domain[Token] := new CategoricalDomain[Token] with DomainEntryCounter[Token]
    data.readAndIndex
    Domain[Token].trimBelowSize(100000) // this also automatically turns off counting
    data.readIndexAndCreateVariables // again
    </pre>
    But this typical usage was so awkward, that for now DomainEntryCounter is mixed in to CategoricalDomain by default.
    */
trait DomainEntryCounter[V<:CategoricalValues] extends util.Index[V#ValueType] {
  this: CategoricalDomain[V] =>
  type T = V#ValueType
  var gatherCounts = true
  private val _countsInitialSize = 64
  private var _counts = new Array[Int](_countsInitialSize)
  protected def ensureSize(n:Int): Unit = {
    if (_counts.length - 1 < n) {
      var newsize = _counts.length * 2
      while (newsize < n) newsize *= 2
      val new_counts = new Array[Int](newsize)
      Array.copy(_counts, 0, new_counts, 0, _counts.size)
      _counts = new_counts
    }
  }
  def count(i:Int): Int = _counts(i)
  def count(entry:T): Int = _counts(indexOnly(entry))
  def incrementCount(i:Int): Unit = { ensureSize(i); _counts(i) += 1 }
  def incrementCount(entry:T): Unit = incrementCount(indexOnly(entry))
  private def someCountsGathered: Boolean = { for (i <- 0 until _counts.size) if (_counts(i) > 0) return true; return false }
  /** Return the index associated with this entry, without incrementing its count, even if 'gatherCounts' is true. */
  def indexOnly(entry:T): Int = super.index(entry)
  /** Return the index associated with entry, and also, if 'gatherCounts' is true, increment +1 the count associated with this entry. */
  override def index(entry:T): Int = {
    val i = super.index(entry)
    if (gatherCounts) incrementCount(i)
    i
  }
  /** Returns the number of unique elements trimmed */
  def trimBelowCount(threshold:Int): Int = {
    assert(!frozen)
    if (!someCountsGathered) throw new Error("Can't trim without first gathering any counts.")
    val origEntries = _entries
    reset // TODO Should we override reset to also set gatherCounts = true?  I don't think so.
    gatherCounts = false
    for (i <- 0 until origEntries.size)
      if (_counts(i) >= threshold) indexOnly(origEntries(i))
    _counts = null // We don't need counts any more; allow it to be garbage collected.  Note that if
    freeze
    origEntries.size - size
  }
  /** Return the number of unique entries with count equal to 'c'. */
  def sizeAtCount(c:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = Math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) == c) ret += 1
    ret
  }
  /** Return the number of unique entries with count greater than or equal to 'threshold'. 
      This returned value will be the size of the Domain after a call to trimBelowCount(threshold). */
  def sizeAtOrAboveCount(threshold:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = Math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) >= threshold) ret += 1
    ret
  }
  /** Return the number of unique entries with count below 'threshold'. */
  def sizeBelowCount(threshold:Int): Int = size - sizeAtOrAboveCount(threshold)  
  /** Returns the count threshold below which entries were discarded. */
  def trimBelowSize(target:Int): Int = {
    assert(!frozen)
    var threshold = 2
    while (sizeAtOrAboveCount(threshold) >= target) threshold += 1
    trimBelowCount(threshold)
    threshold
  }
}
// An example of old real-world usage:
/*  class Token(val word:String, features:Seq[String]) extends BinaryVectorVariable(features, true) with VarInSeq {
      type VariableType <: Token
      type DomainType <: CategoricalDomain[VariableType] with DomainEntryCounter[VariableType]
    }
    Domain += new CategoricalDomain[Token] with DomainEntryCounter[Token] 
*/
// But now much easier to use class below, simply like this:
// class Token extends EnumVariable[String] with CountingCategoricalDomain[Token]
// CountingCategoricalDomain is defined in VariableCategorical.scala
  
class CategoricalDomainWithCounter[V<:CategoricalValues](implicit m:Manifest[V]) extends CategoricalDomain[V]()(m) with DomainEntryCounter[V]

/** A Categorical domain with string values.  Provides convenient intialization to known values, 
    with value members holding those known values.  For example:
    object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value } */
class StringDomain[V<:CategoricalValues {type ValueType = String}](implicit m:Manifest[V]) extends CategoricalDomain[V]()(m) {
  /* For all member variables, if its type is String and its name is all upper case or digits,
    set its value to its name, and intern in the Domain.  Usage:
    object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value } */
  private def stringFields = this.getClass.getDeclaredFields.filter(f => { /*println(f);*/ f.getType == classOf[String] }).reverse
  private var stringFieldsIterator: Iterator[java.lang.reflect.Field] = _
  def Value: String = {
    if (stringFieldsIterator == null) stringFieldsIterator = stringFields.elements
    assert(stringFieldsIterator.hasNext)
    val field = stringFieldsIterator.next
    //println("StringDomain Value got "+field.getName)
    checkFields
    index(field.getName) // Add it to the index
    field.getName
  } 
  private def checkFields: Unit = {
    for (field <- stringFields) {
      val fieldName = field.getName
      val fieldMethod = getClass.getMethod(fieldName, null)
      val fieldValue = fieldMethod.invoke(this, null).asInstanceOf[String]
      //val fieldValue = field.get(this) // Violated access protection since in Scala "val PER" creates a private final variable.
      //println("Field "+fieldName+" has value "+fieldValue)
      if (fieldValue != null && fieldValue != fieldName) throw new Error("Somehow StringDomain category "+fieldName+" got the wrong String value "+fieldValue+".")
    }
  }
}



/** A static map from a Variable class to its Domain. */
object Domain {
  private val debug = false
  private val _domains = new HashMap[Class[_], Domain[_]]()
  def domains : scala.collection.Map[Class[_],Domain[_]] = _domains
  /** Get the Domain for Variables of type V */
  def apply[V<:Variable](implicit mv:Manifest[V]) = get[V](mv.erasure)
  /** Get the Domain for Variables of class vc */
  def get[V<:Variable](vc:Class[_]): V#DomainType = {
    if (debug) {
      println("Domain.get "+vc+" classes.length="+vc.getDeclaredClasses.length)
      if (_domains.isDefinedAt(vc)) println("Domain.get "+vc+" already defined: "+_domains(vc).getClass.getName)
      Console.flush
    }
    _domains.getOrElseUpdate(vc, getDomainForClass(vc)).asInstanceOf[V#DomainType]
  }
  /** Make Variables of type V1 use the same Domain as Variables of type V2. */
  def alias[V1<:Variable,V2<:Variable](implicit vm1:Manifest[V1], vm2:Manifest[V2]) = {
    if (_domains.isDefinedAt(vm1.erasure)) {
      if (_domains(vm1.erasure) != get[V2](vm2.erasure))
        throw new Error("Cannot alias a Domain that has already been used (unless aliases match).")
    } else _domains.put(vm1.erasure, get[V2](vm2.erasure))
  } 
  /** Register d as the domain for variables of type V.  Deprecated.  Use := instead. */
  @deprecated
  def +=[V<:Variable](d:Domain[V])(implicit vm:Manifest[V]) = {
    val c = vm.erasure
    if (_domains.isDefinedAt(c) && _domains(c) != d) throw new Error("Cannot add a Domain["+vm+"]="+d+" when one has already been created for ["+c+"]="+_domains(c))
    val dvc = getDomainVariableClass(c)
    if (debug) { println("+= dvc="+dvc); println("+= c="+c) }
    if (dvc != null && dvc != c && _domains.isDefinedAt(dvc)) throw new Error("Cannot add a Domain["+vm+"] when one has already been created for superclass "+dvc)
    if (dvc != c) throw new Error("Cannot add a Domain["+vm+"] because superclass "+dvc+" should have the same Domain; you should consider instead adding the domain to "+dvc)
    _domains.put(vm.erasure, d)
  }
  /** Register d as the domain for variables of type V. */
  def :=[V<:Variable](d:Domain[V])(implicit vm:Manifest[V]) = this.+=[V](d)(vm)
  def update[V<:Variable](d:Domain[V])(implicit vm:Manifest[V]): Unit = { 
    println("In Domain.update!")
  }
  //todo: this should be more picky about the type parameter
  def update(c:Class[_],d:Domain[_]): Unit = { 
    _domains(c) = d     
  }
  /** Return a Domain instance for Variables of class c, constructing one if necessary.  Also put it in the _domains map. */
  private def getDomainForClass(c:Class[_]) : Domain[_] = {
    if (domainInSubclassesByAnnotation(c)) throw new Error("Cannot get a Domain for "+c+" because it declares DomainInSubclasses, and should be considered abstract.")
    var dvc = getDomainVariableClass(c)
    if (debug) println("getDomainForClass c="+c+" dvc="+dvc)
    if (dvc == null) dvc = c
    _domains.getOrElseUpdate(dvc, newDomainFor(getDomainClass(c),dvc))
  }
  /** Construct a Domain of class dc.  (Parameter vc is currently unused.)  Domains must have only a zero-arg constructor. */
  private def newDomainFor[D](dc:Class[D],vc:Class[_]) : Domain[_] = {
    val constructors = dc.getConstructors()
    val i = constructors.findIndexOf(c => c.getParameterTypes.length == 1)
    if (i == -1) throw new Error("Domain "+dc.getName+" does not have a one-arg constructor; all Domains must.")
    if (debug) println("newDomainFor calling "+constructors(i).getClass+" constructor # args="+constructors(i).getParameterTypes.length)
    val manifest = Manifest.classType[Variable](vc)
    val constructorArgs: Array[Object] = Array(manifest)
    constructors(i).newInstance(constructorArgs: _*).asInstanceOf[Domain[_]]
  }
  /** Find the (sub)class of Domain to use for constructing a domain for variable class c. */
  private def getDomainClass(c:Class[_]) : Class[_] = {
    if (debug) println("getDomainClass "+c)
    // First check this class to see if it specifies the DomainClass
    val classes = c.getDeclaredClasses()
    val index = if (classes == null) -1 else classes.findIndexOf(c=>c.getName.endsWith("$DomainClass"))
    //if (debug) println("  $DomainClass index="+index+"  classes "+classes.toList)
    if (index >= 0) {
      if (debug) println("getDomainClass   returning "+classes(index).getSuperclass)
      return classes(index).getSuperclass
    }
    // Next check the superclass and interfaces/traits; choose the most specific (subclass of) Domain
    val candidateDomainClasses = new ListBuffer[Class[_]]
    val sc = c.getSuperclass
    if (sc != null && sc != classOf[java.lang.Object]) 
      candidateDomainClasses += getDomainClass(sc)
    val interfaces = c.getInterfaces.elements
    while (interfaces.hasNext) {
      val dc = getDomainClass(interfaces.next)
      if (dc != null) candidateDomainClasses += dc
    }
    if (candidateDomainClasses.size > 0) {
      // Find the most specific subclass of the first domain class found
      var dc = candidateDomainClasses.first
      candidateDomainClasses.foreach(dc2 => if (dc.isAssignableFrom(dc2)) dc = dc2)
      if (debug) println("getDomainClass "+c+" specific="+dc)
      return dc
    } else
      null
  }
  private def domainInSubclasses(c:Class[_]) : Boolean = c.getDeclaredClasses.findIndexOf(c=>c.getName.endsWith("$DomainInSubclasses")) != -1

  def domainInSubclassesByAnnotation(c:Class[_]) : Boolean = c.isAnnotationPresent(classOf[DomainInSubclasses])


  /** Find a potential substitute for c as the key into _domains. */
  private def getDomainVariableClass(c:Class[_]) : Class[_] = {
    if (debug) println("getDomainVariableClass c "+c+" classes.length="+c.getDeclaredClasses.length)
    if (domainInSubclassesByAnnotation(c)) throw new Error("Cannot create Domain["+c+"] because it is annotated with DomainInSubclasses.")
    else if (c.getSuperclass == null || c.getSuperclass == classOf[java.lang.Object]) c 
    else if (domainInSubclassesByAnnotation(c.getSuperclass)) c 
    else getDomainVariableClass(c.getSuperclass)
  }
}


