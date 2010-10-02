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
import cc.factorie._
import scala.collection.mutable.{ArrayStack,HashSet,HashMap,ListBuffer}

/** Classes and functions supporting Entity-Relationship languages for creating Templates. 
    @author Andrew McCallum
    @since 0.8
*/
package object er {

// TODO There are a disturbing number of casts in this package.  
// It could benefit from a serious sweep to get rid of as many of these as possible. 
// See also last message from Geoffrey Washburn about an idea for cleaning up parts of this.

// Getter support:

/** Fill in abstract type Getter.A with parameterized type.  Necessary for Scala type-inferencer. */
//trait GetterHead[A1,C1/*<:GetterType[C1]*/] extends Getter[C1] { type A = A1 }
type GetterHead[A1,C1] = Getter[C1] { type A = A1 }

// TODO? Consider avoiding the need to mix this into Entity by using instead duck typing: type WithGetterType = { type GetterClass <: Getter[_,_] }
//trait GetterType[D] { type GetterClass <: Getter[D] }
/** Construct a new Getter with tail type A. */
def newGetter[V<:{type GetterType <: Getter[V]}](/*implicit*/ m:Manifest[V]): V#GetterType = {
  newGetter[V](m.erasure)
}
def newGetter[V<:{type GetterType <: Getter[V]}](variableClass:Class[_]): V#GetterType = {
  val getterClass = getGetterClass(variableClass)
  assert(getterClass ne null)
  val constructors = getterClass.getConstructors
  if (constructors.size != 1) throw new Error("Getters must have only one constructor; class="+getterClass+" has "+constructors.size+".  You can get this error if you failed to correctly set 'type GetterClass'.")
  val constructor = constructors.apply(0)
  val numArgs = constructor.getParameterTypes.length
  if (numArgs == 0) {
    constructor.newInstance().asInstanceOf[V#GetterType]
  } /*else if (numArgs == 1) {
  val args = new Array[Object](1)
  args(0) = null
  constructor.newInstance(args).asInstanceOf[A#GetterClass] // TODO Yipes, passing null here, when expecting a pointer to the outer instance!  OK as long as the user isn't relying on the outer instance.  Can we get it somehow?
  } */ else {
    val msg = new StringBuffer
    msg.append("Getter constructors must not take any arguments.\nInstead "+getterClass.getName+" takes "+numArgs+" argument(s): ")
    constructor.getParameterTypes.foreach(t => msg.append(t.getName+" "))
    msg.append("\nYou will get an unexpected single argument when you declare a Getter as an inner class;\nthe solution is to declare all Getters in packages, not inner classes.")
    throw new Error(msg.toString)
  }
}
/** Find the (sub)class of Getter to use for constructing a getter for variable class c. */
protected[er] def getGetterClass(c:Class[_]) : Class[_] = {
  val debug = false
  if (debug) println("getGetterClass "+c)
  // First check this class to see if it specifies the GetterClass
  val classes = c.getDeclaredClasses()
  val index = if (classes == null) -1 else classes.findIndexOf(c=>c.getName.endsWith("$GetterClass"))
  //if (debug) println("  $GetterClass index="+index+"  classes "+classes.toList)
  if (index >= 0) {
    if (debug) println("getGetterClass   returning "+classes(index).getSuperclass)
    return classes(index).getSuperclass
  }
  // Next check the superclass and interfaces/traits; choose the most specific (subclass of) Domain
  val candidateGetterClasses = new ListBuffer[Class[_]]
  val sc = c.getSuperclass
  if (sc != null && sc != classOf[java.lang.Object]) 
    candidateGetterClasses += getGetterClass(sc)
  val interfaces = c.getInterfaces.iterator
  while (interfaces.hasNext) {
    val dc = getGetterClass(interfaces.next)
    if (dc != null) candidateGetterClasses += dc
  }
  if (candidateGetterClasses.size > 0) {
    // Find the most specific subclass of the first domain class found
    var dc = candidateGetterClasses.head
    candidateGetterClasses.foreach(dc2 => if (dc.isAssignableFrom(dc2)) dc = dc2)
    if (debug) println("getGetterClass "+c+" specific="+dc)
    return dc
  } else
    null
}

/** Construct a new Getter representing the beginning of an getter chain, taking input A. */
def newGetterUnit[X<:{type GetterType <: Getter[X]}](implicit m:Manifest[X]): X#GetterType { type A = X /* ; type B = X */ } = {
  //println("GetterUnit m="+m)
  newGetter[X](m).asInstanceOf[X#GetterType { type A = X }];
}




// Formula support:

/** The values of leaves of the formula tree.  
 For the common case of a BooleanTerm it is a BooleanValue (which inherits from DiscreteVar). */
type FormulaArg = DiscreteVar //with GetterType[BooleanValue]
//type FormulaValue[A] = BooleanValue //with GetterType[A];
/** The collection of arguments to the boolean expression; the variables neighboring the factor.  
 Using DiscreteValue instead of BooleanValue enables mixed use of BooleanValue and other DiscreteVars, as in IntExpression. */
type FormulaArgs = ArrayStack[FormulaArg]
implicit def getter2formula[X<:Variable,Y<:BooleanVar/* with GetterType[A]*/](g:Getter[Y] {type A = X})(implicit ma:Manifest[Y]): Formula[X] = new BooleanTerm(g)(ma)
implicit def getter2IntTerm[X<:Variable,A<:FormulaArg](g:GetterHead[X,A])(implicit ma:Manifest[A]): IntExpression[X] = new IntTerm(g)(ma)



// Score support:

type ScorableValues[X] = DiscreteVars // CategoricalValues //with GetterType[X]
type ScorableValues0 = DiscreteVars // CategoricalValues //with GetterType[CategoricalValues]

implicit def getter2scoreneighbor[X<:Variable,Y<:ScorableValues[Y]](a:Getter[Y] { type A = X})(implicit ma:Manifest[Y]): ScoreNeighbor0[X] = { 
  //println("getter2scoreneighbor ma="+ma+" a="+a)
  new ScoreNeighbor[X,Y](a)(ma)
}



}
