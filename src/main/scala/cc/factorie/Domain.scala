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
import java.io.{File,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}
import scala.reflect.Manifest

/** The "domain" of a variable---also essentially serving as the Variable's "type".
    This most generic superclass of all Domains does not provide much functionality.
    Key functionality of subclasses: 
    DiscreteDomain provides a size.  
    VectorDomain provides the maximum dimensionality of its vectors.
    CategoricalDomain provides a densely-packed mapping between category values and integers.
    @author Andrew McCallum
    @since 0.8 */
class Domain[V<:Variable](implicit m:Manifest[V]) {
  type VariableType = V
  /** Return the collection of all classes that have this Domain. */
  def variableClasses: Seq[Class[V]] =
    Domain.domains.iterator.filter({case (key,value) => value == this}).map({case (key,value) => key.asInstanceOf[Class[V]]}).toList
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


