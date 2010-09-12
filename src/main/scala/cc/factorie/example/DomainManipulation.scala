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



package cc.factorie.example
import scala.reflect.Manifest
import cc.factorie._

object DomainManipulation {

  class MyIndexedVariable extends CategoricalVariable //{ def vector = null }
  println("MyIndexedVariable  "+Domain[MyIndexedVariable])
  
  println("Inner classes of CategoricalValues:")
  classOf[CategoricalVars[_]].getClasses.foreach(c => println(c))
  println("Inner declared classes of IndexedVariable:")
  classOf[CategoricalVars[_]].getDeclaredClasses.foreach(c => println(c))
  println("Inner declared classes of IndexedVariable:")
  println(classOf[CategoricalVars[_]].getSuperclass)
  //println("method "+classOf[IndexedVariable].getDeclaredMethod("domainInSubclasses", null))

  class LabelDomain[V<:Variable](implicit m:Manifest[V]) extends Domain[V]()(m) {
    println("Creating LabelDomain "+this.getClass.getName)
    def numLabels = 2
    def +=(x:AnyRef) = println("Adding "+x.toString)
  }

  // Here we are declaring a new Variable class that has a special Domain subclass
  // DomainType and DomainClass should always be set in tandem
  class Label1 extends Variable {
    type VariableType <: Label1
    override type DomainType <: LabelDomain[VariableType]
    class DomainClass extends LabelDomain[VariableType]()(null)
  }
  
  // Here we declare that the Domain of Label2 should be the same object as the Domain for Label1
  class Label2 extends Label1 {
    type VariableType <: Label2
    class DomainVariableClass extends Label1
    //override def domain = Domain[Label]
  }
  
  // But actually in this case, all Variable classes that do not declare
  // class DomainInSubclasses
  // will automatically inherit their Domain from their superclass
  class Label3 extends Label1 
  // Since Label can have its own Domain, we also inherit it here
  
  // If we don't especially care about knowing the type of the new Domain,
  // we can also set the Domain for a Variable class with
  // Domain += new FooDomain[VarClass]
  // This method of setting the Domain is useful when the
  // Domain type or the Variable class that keys the Domain choice
  // is not known statically at the time of coding the Variable class.
  // For example, this is the case in LDA with MixtureComponent and MixtureChoice
  // which must share a Domain, but in the library implementation, it is actually
  // the subclasses of these Variable classes that must share Domains---we don't
  // known them statically yet; so we must ask the FACTORIE user to type this += syntax.
  class Label4 extends Variable // Error should be thrown if extends Label
  println("About to Domain += new LabelDomain[Label4]"); Console.flush
  Domain += new LabelDomain[Label4] {
    override def numLabels = 88
  }
  
  // Here we define yet another new type of Domain,
  // and declare that Label5 uses it.
  // The Domain will automatically be constructed.
  class Label5 extends Variable {
    type VariableType <: Label5
    override type DomainType <: Label5Domain[VariableType]
    class DomainClass extends Label5Domain[VariableType]()(null)
  }
  class Label5Domain[V<:Label5](implicit m:Manifest[V]) extends LabelDomain[V]()(m) {
    println("In Label5Domain constructor")
    this += ("label5one")
    this += ("label5two")
    Console.flush
    def numFives = 55
  }
  
  // Here is yet another way to give a Variable a new Domain subclass, but it would be
  // better to use the DomainClass declaration in Label5
  // because if someone asks for Domain[Label6] before Domain+= is called, then we get error.
  class Label6 extends Variable {
    // following two lines are optional
    //type VariableType <: Label6
    override type DomainType <: Label6Domain[VariableType]
  }
  class Label6Domain[V<:Variable](implicit m:Manifest[V]) extends LabelDomain[V]()(m) {
    def numSixes = 66
  }
  Domain += new Label6Domain[Label6]

  
  
  def main (args:Array[String]) : Unit = {
    println("Hello")

    //println(Domain.get[Label](classOf[Label]))
    val l = new Label1{}
    println
    //val vd = Domain[Variable]; println
    val ld2 = Domain[Label1]
    println
    if (l.domain == ld2) println("l == ld2")
    //val m = man[Variable#DomainClass]; println ("erasure constructor "+m.erasure.getSuperclass.getConstructors()(0))
    println (l.domain)
    if (Domain[Label1] == Domain[Label2]) println ("Label and Label2 domains match")
    println("Setting Domain[Label3] := Domain[Label2]")
    Domain.alias[Label3,Label2]
    println
    println (Domain[Label1]); println
    println (Domain[Label2]); println
    println (Domain[Label3]); println
    println (Domain[Label4]); println
    println (Domain[Label5]); println
    println (Domain[Label6]); println
    println
    //println (Domain[Label4].numLabels)
    println (Domain[Label5].numFives)
    println (Domain[Label6].numSixes)
    0 
  }

}
