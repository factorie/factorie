/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie
import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import scala.util.Random
import cc.factorie.la._
import scala.reflect.Manifest
import java.io._
import util.ClassPathUtils

/** The "domain" of a variable---also essentially serving as the Variable's "type".
    This most generic superclass of all Domains does not provide much functionality.
    Key functionality of subclasses: 
    DiscreteDomain provides a size.  
    VectorDomain provides the maximum dimensionality of its vectors.
    CategoricalDomain provides a densely-packed mapping between category values and integers.
    @author Andrew McCallum
    @since 0.8 */
class Domain[V<:Variable](implicit m:Manifest[V]) {
  /** Return the collection of all classes that have this Domain. */
  def variableClasses: Seq[Class[V]] =
    Domain.domains.iterator.filter({case (key,value) => value == this}).map({case (key,value) => key.asInstanceOf[Class[V]]}).toList.sortBy(_.getName)
  /** Serialize this domain to disk in the given directory. */
  def save(dirname:String): Unit = {}
  /** Deserialize this domain from disk in the given directory. */
  def load(dirname:String): Unit = {}
  /** The name of the file (in directory specified in "save" and "load") to which this Domain is saved. */
  def filename:String = this.getClass.getName+"["+variableClasses.head.getName+"]"
  // Automatically register ourselves.  
  // This enables pairing a Domain with its variable V by simply: "val MyDomain = new FooDomain[MyVariable]"
  // It also ensures that we throw an Error if someone tries to create two Domains for the same Variable class.
  Domain.+=[V](this)(m)
}

// TODO Consider if it would be helpful to have a RealDomain or PositiveRealDomain

/** A Domain for variables that respond to the "vector" method */
class VectorDomain[V<:VectorVar](implicit m:Manifest[V]) extends Domain[V] {
  def maxVectorSize: Int = 1 // TODO We should somehow force subclasses to override this, but VectorVar needs VectorDomain to be non-abstract
  def vectorDimensionName(i:Int): String = i.toString
  def freeze: Unit = {}
}

class RealDomain[V<:RealVar](implicit m:Manifest[V]) extends VectorDomain[V] {
  override def maxVectorSize = 1
}

//trait RealVectorDomain[V<:RealVars] extends Domain[V] with VectorDomain[V]
// TODO Still needs a definition for "maxVectorSize"

/** A Domain that has a positive integer size.  
    Set its size by Domain[MyDiscrete].size = 9; or Domain[MyDiscrete].size = Domain[MyOther].size. 
    @author Andrew McCallum */
class DiscreteDomain[V<:DiscreteVars](implicit m:Manifest[V]) extends VectorDomain[V] {
  private var _frozen = false
  private var _size: Int = -1
  private var _sizeFunction: ()=>Int = null
  def size_=(size:Int): Unit = size_=(() => size)
  def size_=(sizeFunction: ()=>Int): Unit = {
    if (_size == -1) _sizeFunction = sizeFunction
    else throw new Error("DiscreteDomain["+m.erasure.getName+"].size already accessed; cannot re-set size.")
  }
  /** Actually call the sizeFunction to set the size. */
  private def setSize(): Unit = 
    if (_sizeFunction != null) { val s = _sizeFunction.apply; require(s > 0); require(s >= _size); _size = s; _frozen = true }
    else throw new Error(getClass.getName+": DiscreteDomain size must be set; e.g. Domain[MyDiscrete].size = 10")
  def setSize(s:Int): Unit = if (!_frozen) _size = s else throw new Error(getClass.getName+": DiscreteDomain size is already frozen and cannot be set.")
  def size: Int = { if (_size == -1) setSize(); _size }
  override def freeze: Unit = { setSize(); _frozen = true }
  def allocSize = size
  override def maxVectorSize = allocSize
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    val s = new PrintWriter(new FileWriter(f))
    s.println(size)
    s.close
  }
  override def load(dirname:String): Unit = {
    val reader = new InputStreamReader(ClassPathUtils.getStreamFromClassPathOrFile(dirname+"/"+filename))
    val s = new BufferedReader(reader)
    val line = s.readLine
    val readSize = Integer.parseInt(line)
    this.size_=(()=>readSize)
  }
}

// TODO Also make a randomized-representation CategoricalDomain, with hashes.

/** A domain for categorical variables.  It stores not only a size,
    but also the mapping from category values (of type V#CategoryType)
    to densely packed integers suitable for indices into parameter
    vectors.

    Furthermore if domain.gatherCounts = true, this domain will count
    the number of calls to 'index'.  Then you can reduce the size of
    the Domain by calling 'trimBelowCount' or 'trimBelowSize', which
    will recreate the new mapping from categories to densely-packed
    non-negative integers.  In typical usage you would (1) read in the
    data, (2) trim the domain, (3) re-read the data with the new
    mapping, creating variables.

    @author Andrew McCallum
    @author Sebastian Riedel (domain loading from classpath)
    */
class CategoricalDomain[V<:AbstractCategoricalVars](implicit m:Manifest[V]) extends DiscreteDomain[V]()(m) with IndexedSeqEqualsEq[V#CategoryType] {
  import scala.collection.mutable.Map
  type T = V#CategoryType

  override def freeze = _frozen = true
  /** The size others might want to allocate to hold data relevant to this Index.  
      If maxSize is set can be bigger than size. 
      @see maxSize */
  override def allocSize = if (maxSize < 0) size else maxSize
  override def size = _indices.size
  override def size_=(sizeFunction: ()=>Int): Unit = throw new Error("CategoricalDomain.size cannot be set directly; only DiscreteDomains' can.")
  def length = _indices.size
  /** If true, do not allow this Index to change. */
  private var _frozen = false
  /** Can new category values be added to this Domain? */
  def frozen = _frozen

  /**Forward map from int to object */
  private var _objects = new ArrayBuffer[T]
  /**Map from object back to int index */
  private var _indices = Map[T, Int]()
  /** Wipe the Index clean */
  def reset: Unit = {
    _frozen = false
    _objects = new ArrayBuffer[T]
    _indices = Map[T,Int]()
  }
  /** Allow subclasses to access the list objects.  Useful for subclasses calling reset and then re-entering a filtered subset of the old objects. */
  protected def _entries: IndexedSeq[T] = _objects
  /** If positive, throw error if size tries to grow larger than it.  Use for growable multi-dim Factor weights;
      override this method with the largest you think your growable domain will get. */
  var maxSize = -1
  // TODO consider putting the following method back in later -akm
  //override def maxSize_=(s:Int) : Unit = if (maxSize >= size) maxSize = s else throw new Error("Trying to set maxSize smaller than size.")

  override def iterator = _objects.iterator
  override def contains(entry: Any) = _indices.contains(entry.asInstanceOf[T])
  /* entry match { case e:T => _indices.contains(e); case _ => false } */

  def apply(index:Int) = get(index)
  def unapply(entry:T): Option[Int] = if (_indices.contains(entry)) Some(_indices(entry)) else None

  /** Return an object at the given position or throws an exception if it's not found. */
  def get(pos: Int): T = _objects(pos)

  /** Return a densely-packed positive integer index for the given object.  
      By default, allocate a new index (at the end) if the object was not found, 
      but if immutable may return -1 */
  def indexOnly(entry: T): Int = {
    def nextMax = {
      val m = _objects.size
      if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
      _objects += entry;
      m
    }
    if (_frozen) _indices.getOrElse(entry, -1) else _indices.getOrElseUpdate(entry, nextMax)
  }
  def index(entry: T): Int = {
    val i = indexOnly(entry)
    if (gatherCounts && i != -1) incrementCount(i)
    i
  }

 
  /** Like index, but throw an exception if the entry is not already there. */
  def getIndex(entry:T) : Int = _indices.getOrElse(entry, throw new Error("Entry not present; use index() to cause a lookup."))

  /** Override indexOf's slow, deprecated behavior. */
  override def indexOf[B >: T](elem: B): Int = index(elem.asInstanceOf[T]);

  /** Clears the index. */
  def clear() = { _indices.clear(); _objects.clear() }

  // Separate argument types preserves return collection type
  def indexAll(c: Iterator[T]) = c map index;
  def indexAll(c: List[T]) = c map index;
  def indexAll(c: Array[T]) = c map index;
  def indexAll(c: Set[T]) = c map index;
  def getAll(c: Iterator[Int]) = c map get;
  def getAll(c: List[Int]) = c map get;
  def getAll(c: Array[Int]) = c map get;
  def getAll(c: Set[Int]) = c map get;
  def indexKeys[V](c: scala.collection.Map[T, V]) = Map[T, V]() ++ c.map {case (a, b) => (index(a), b)}
  def indexValues[K](c: scala.collection.Map[K, T]) = Map[K, T]() ++ c.map {case (a, b) => (a, index(b))}

  def randomValue : V#CategoryType = randomValue(cc.factorie.random)
  def randomValue(random:Random): V#CategoryType = get(random.nextInt(size))
  def +=(x:V#CategoryType) : Unit = this.index(x)
  def ++=(xs:Traversable[V#CategoryType]) : Unit = xs.foreach(this.index(_))
 
  override def toString = "CategoricalDomain["+m.erasure+"]("+size+")"
  override def vectorDimensionName(i:Int): String = get(i).toString
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    if (f.exists) return // Already exists, don't write it again.  // TODO Careful about trying to re-write to the same location, though.
    val s = new PrintWriter(new FileWriter(f))
    //if (elements.next.asInstanceOf[AnyVal].getClass != classOf[String]) throw new Error("Only know how to save CategoryType String.")
    if (frozen) s.println("#frozen = true") else s.println("#frozen = false")
    for (e <- iterator) {
      if (e.toString.contains("\n")) throw new Error("Cannot save Domain with entry containing newline.")
      s.println(e.toString)
    }
    s.close
  }
  override def load(dirname:String): Unit = {
    if (size > 0) return // Already initialized, don't read again
    val reader = new InputStreamReader(ClassPathUtils.getStreamFromClassPathOrFile(dirname+"/"+filename))
    val s = new BufferedReader(reader)
    var line = s.readLine
    var willFreeze = false
    if (line.split("\\s+").apply(2) == "true") willFreeze = true // Parse '#frozen = true'
    while ({line = s.readLine; line != null}) {
      //println("Domain load got "+line)
      this.index(line.asInstanceOf[V#CategoryType]) // TODO What if V#CategoryType isn't a String?  Fix this.
    }
    if (willFreeze) freeze
    s.close
    //println("Loading Domain["+filename+"].size="+size)
  }
  
  // Code for managing occurrence counts
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
    val min = math.min(size, _counts.size)
    for (i <- 0 until min) if (_counts(i) == c) ret += 1
    ret
  }
  /** Return the number of unique entries with count greater than or equal to 'threshold'. 
      This returned value will be the size of the Domain after a call to trimBelowCount(threshold). */
  def sizeAtOrAboveCount(threshold:Int): Int = {
    if (!someCountsGathered) throw new Error("No counts gathered.")
    var ret = 0
    val min = math.min(size, _counts.size)
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

object CategoricalDomain {
  val NULL_INDEX = -1
}


/* CategoricalDomain also facilitates counting occurences of entries, and trimming the Domain size.
   WARNING: Any indices that you use and store before trimming will not be valid after trimming!
   Typical usage:
   <pre>
   class Token(s:String) extends CategoricalVariable(s)
   data.readAndIndex
   Domain[Token].trimBelowSize(100000) // this also automatically turns off counting
   data.readIndexAndCreateVariables // again
   </pre>
   */

/** To be used to avoid re-reading the data, as described in above comment, 
    but not yet implemented. */
trait CategoricalRemapping {
  def remapCategories(fn:(Int)=>Int)
}



/** A Categorical domain with string values.  Provides convenient intialization to known values, 
    with value members holding those known values.  For example:
    object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value }
    Each of the defined val will have Int type.  Their corresponding String category values 
    will be the name of the variable (obtained through reflection). */
class StringDomain[V<:CategoricalVars[_] {type CategoryType = String}](implicit m:Manifest[V]) extends CategoricalDomain[V]()(m) {
  /* For all member variables, if its type is Int, set its value to its name, 
     and intern in the Domain.  Usage: 
     object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value } */
  private def stringFields = this.getClass.getDeclaredFields.filter(f => { /* println("stringFields "+f); */  f.getType == classOf[Int] })
  private var stringFieldsIterator: Iterator[java.lang.reflect.Field] = _
  def Value: Int = {
    if (stringFieldsIterator == null) stringFieldsIterator = stringFields.iterator
    assert(stringFieldsIterator.hasNext)
    val field = stringFieldsIterator.next
    //println("StringDomain Value got "+field.getName)
    //checkFields // TODO Re-add this and make sure example/DirichletDemo works
    index(field.getName) // Add it to the index
  } 
  private def checkFields: Unit = {
    for (field <- stringFields) {
      val fieldName = field.getName
      println("StringDomain.checkFields "+field+" fieldName="+fieldName)
      //getClass.getMethods.foreach(m => println(m.toString))
      val fieldMethod = getClass.getMethod(fieldName) // was with ,null)
      val fieldValue = fieldMethod.invoke(this).asInstanceOf[Int]
      //println("Field "+fieldName+" has value "+fieldValue)
      if (fieldValue != 0 && this.get(fieldValue) != fieldName) throw new Error("Somehow StringDomain category "+fieldName+" got the wrong String value "+fieldValue+" ("+this.get(fieldValue)+").")
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
      System.console.flush
    }
    _domains.getOrElseUpdate(vc, getDomainForClass(vc)).asInstanceOf[V#DomainType]
  }
  /** Make Variables of type V1 use the same Domain as Variables of type V2. */
  def alias[V1<:Variable,V2<:Variable](implicit vm1:Manifest[V1], vm2:Manifest[V2]) = {
    // TODO make this simple call the 'alias' implementation below.
    if (_domains.isDefinedAt(vm1.erasure)) {
      if (_domains(vm1.erasure) != get[V2](vm2.erasure))
        throw new Error("Cannot alias a Domain that has already been used (unless aliases match).")
    } else _domains.put(vm1.erasure, get[V2](vm2.erasure))
  }
  def alias(class1:Class[_<:Variable], class2:Class[_<:Variable]): Unit = {
    if (_domains.isDefinedAt(class1)) {
      if (_domains(class1) != getDomainForClass(class2))
        throw new Error("Cannot alias a Domain that has already been used (unless aliases match).")
    } else _domains.put(class1, getDomainForClass(class2))
  }
  /** Register d as the domain for variables of type V.  Deprecated?  Use := instead? */
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
  //def update[V<:Variable](d:Domain[V])(implicit vm:Manifest[V]): Unit = { println("In Domain.update!") }
  //todo: this should be more picky about the type parameter
  def update(c:Class[_],d:Domain[_]): Unit = { 
    _domains(c) = d     
  }
  /** Return a Domain instance for Variables of class c, constructing one if necessary.  Also put it in the _domains map. */
  private def getDomainForClass(c:Class[_]) : Domain[_] = {
    if (domainInSubclasses(c)) throw new Error("Cannot get a Domain for "+c+" because it declares DomainInSubclasses, and should be considered abstract.  You should subclass "+c)
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
    if (sc != null && sc != classOf[java.lang.Object]) {
      val dc = getDomainClass(sc)
      if (dc != null) candidateDomainClasses += dc // Before Aug 21 did not have this check
    }
    val interfaces = c.getInterfaces.iterator
    while (interfaces.hasNext) {
      val dc = getDomainClass(interfaces.next)
      if (dc != null) candidateDomainClasses += dc
    }
    if (candidateDomainClasses.size > 0) {
      // Find the most specific subclass of the first domain class found
      var dc = candidateDomainClasses.head
      assert(dc ne null)
      candidateDomainClasses.foreach(dc2 => if (dc.isAssignableFrom(dc2)) dc = dc2)
      if (debug) println("getDomainClass "+c+" specific="+dc)
      return dc
    } else
      null // TODO In what cases will this happen?
  }

  def domainInSubclasses(c:Class[_]) : Boolean = 
    //c.getDeclaredClasses.findIndexOf(c=>c.getName.endsWith("$DomainInSubclasses")) != -1 // by searching for a member class whose sole purpose is to flag this class
    c.isAnnotationPresent(classOf[DomainInSubclasses]) // by annotation

  /** Find a potential substitute for c as the key into _domains. */
  private def getDomainVariableClass(c:Class[_]) : Class[_] = {
    if (debug) { println("getDomainVariableClass c "+c+" classes.length="+c.getDeclaredClasses.length); /*new Exception().printStackTrace()*/ }
    if (domainInSubclasses(c)) throw new Error("Cannot create Domain["+c+"] because it is annotated with DomainInSubclasses.")
    else if (c.getSuperclass == null || c.getSuperclass == classOf[java.lang.Object]) c 
    else if (domainInSubclasses(c.getSuperclass)) c 
    else getDomainVariableClass(c.getSuperclass)
  }
}


