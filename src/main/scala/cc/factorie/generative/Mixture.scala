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

package cc.factorie.generative
import cc.factorie._
import scala.collection.mutable.{ArrayBuffer,Stack}

abstract class Gate(initial:Int) extends Discrete(initial) with Parameter {
  //def prChoosing(value:Int): Double = parentFactor.template.prChoosing(parentFactor.statistics, index)
}

// For factors between a Mixture and the child generated from that Mixture
trait MixtureFamily extends GenerativeFamily {
  type FamilyType <: MixtureFamily
  //type ChildType <: MixtureGeneratedVar
  def gate(f:FactorType): Gate
  def gate(f:cc.factorie.Factor): Gate = gate(f.asInstanceOf[FactorType])
  def prChoosing(s:StatisticsType, mixtureIndex:Int): Double
  def prChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): Double = prChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
  def logprChoosing(s:StatisticsType, mixtureIndex:Int): Double = math.log(prChoosing(s, mixtureIndex))
  def logprChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): Double = logprChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
  def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): ChildType#Value
  def sampledValueChoosing(s:cc.factorie.Statistics, mixtureIndex:Int): ChildType#Value = sampledValueChoosing(s.asInstanceOf[StatisticsType], mixtureIndex)
}

//trait ParameterVars[P<:Parameter] extends Vars[P] with Parameter
//class SeqParameterVars[P<:Parameter](vs:Seq[P]) extends SeqVars(vs) with ParameterVars[P]

/** The factor family between a Mixture (child) and one of its components (parent) */
/*object MixtureComponent extends GenerativeFamilyWithStatistics2[Mixture[Parameter],ParameterVars[Parameter]] {
  def pr(s:StatisticsType) = 1.0
  def sampledValue(s:StatisticsType): ChildType#ValueType = throw new Error("Cannot sample a Mixture")
  def updateCollapsedParents(f:Factor): Boolean =   
}*/
object MixtureComponent extends GenerativeFamilyWithStatistics2[Mixture[Parameter],Parameter] {
  def pr(s:StatisticsType) = 1.0
  def sampledValue(s:StatisticsType): ChildType#ValueType = throw new Error("Cannot sample a Mixture")
  override def updateCollapsedParents(f:Factor, weight:Double): Boolean = {
    // TODO this is inefficient because it will loop through all children of the Mixture for each Mixture component
    for (f2 <- f._1.childFactorsOf(f._2)) f2.family.updateCollapsedParents(f2, weight) // TODO Consider collapsed Gate
    true
  }
}

class Mixture[+P<:Parameter](val components:Seq[P]) extends Seq[P] with GeneratedVar with Parameter 
with VarAndValueGenericDomain[Mixture[P],scala.collection.Seq[P#Value]] 
//with CollapsibleParameter with VarWithCollapsedType[Mixture[P]]
{
  def apply(i:Int) = components(i)
  def length = components.length
  def iterator = components.iterator
  def value = this.map(_.value)
  // TODO Note that if the Mixture grows, new component Parameters will not be properly get this childFactor 
  val parentFactors = components.map(c => { val f = new MixtureComponent.Factor(this, c); c.addChildFactor(f); f })
  //parentFactor = new MixtureComponent.Factor(this, new SeqParameterVars(components)) // TODO Look at this again carefully
  //components.foreach(p => p.addChildFactor(parentFactor)) 
  override def isDeterministic = true
  def childFactorsOf[P2>:P](p:P2): Seq[MixtureFamily#Factor] = {
    val index = this.indexOf(p)
    //children.filter(_.isInstanceOf[MixtureGeneratedVar]).asInstanceOf[Iterable[MixtureGeneratedVar]].filter(_.choice.intValue == index)
    // If this cast fails, it means that some of the children are not a MixtureGeneratedVar; 
    // for example, they might be a MixtureSeqGeneratedVar, which is allowed, but not supported by this.childrenOf method.
    val result = new scala.collection.mutable.ArrayBuffer[MixtureFamily#Factor]
    for (factor <- childFactors) factor match {
      case f:MixtureFamily#Factor => if (f.family.gate(f).intValue == index) result += f
    }
    result
  }
  /*def newCollapsed: Mixture[P] = {
    //parentFactor.family.resetCollapsedChild(parentFactor)
    parentFactors.foreach(f => f.resetCollapsedChild(f))
    // TODO Check to make sure that both "updates" below return true indicating success
    val b1 = parentFactor.family.updateCollapsedChild(parentFactor)
    val b2 = childFactors.forall(f => f.family.updateCollapsedParents(f, 1.0))
    require(b1)
    require(b2)
    //for (factor <- childFactors) factor match { case f:Discrete.Factor => increment(f._1.intValue, 1.0)(null) }
    this
  }*/
}
object Mixture {
  def apply[P<:Parameter](n:Int)(constructor: =>P): Mixture[P] = new Mixture[P](for (i <- 1 to n) yield constructor)
}





/*
trait CollapsedMixtureComponents[+P<:CollapsedParameter] extends MixtureComponents[P]

class FiniteMixture[+P<:Parameter](theComponents:Seq[P]) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = theComponents.toIndexedSeq
  components.foreach(_.addChild(this)(null)) // The components have this mixture as children
}

class CollapsibleFiniteMixture[+P<:CollapsibleParameter](theComponents:Seq[P]) extends FiniteMixture(theComponents) with CollapsibleParameter with VarWithCollapsedType[CollapsedFiniteMixture[P#CollapsedType]] {
  def newCollapsed = new CollapsedFiniteMixture(components.map(_.newCollapsed))
  def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
    case cfm:CollapsedFiniteMixture[_] => forIndex(components.length)(i => components(i).setFrom(cfm(i)))
  }
}
object CollapsibleFiniteMixture {
  def apply[P<:CollapsibleParameter](n:Int)(constructor: =>P): CollapsibleFiniteMixture[P] = new CollapsibleFiniteMixture[P](for (i <- 1 to n) yield constructor)
}
class CollapsedFiniteMixture[+P<:CollapsedParameter](theComponents:Seq[P]) extends FiniteMixture(theComponents) with CollapsedParameter {
  def clearChildStats: Unit = components.foreach(_.clearChildStats)
  def updateChildStats(child:Variable, weight:Double): Unit = {
    child match {
      case mgv:MixtureGeneratedVar => {
        //println("CollapsedFiniteMixture.updateChildStats "+mgv+" choice="+mgv.choice)
        components(mgv.choice.intValue).updateChildStats(mgv, weight)
      }
      case pmgv:PlatedMixtureGeneratedVar => throw new Error("MixtureSeqGeneratedVar not yet handled.")
      //case _ => {} // TODO Should we really not throw an error here?
    }
  }
}


// TODO Not yet finished
class InfiniteMixture[P<:Parameter:Manifest](constructor: =>P) extends MixtureComponents[P] {
  val generativeTemplate = new MixtureComponentsTemplate
  //def generativeFactor = new generativeTemplate.Factor(this, Vars.fromSeq(components))
  def generativeFactor = new generativeTemplate.Factor(this)
  val components = new ArrayBuffer[P] // TODO Perhaps we should project this onto a seq without holes?
  private val _emptySlots = new Stack[Int]
  def addComponent: Int = 
    if (_emptySlots.isEmpty) { components += constructor; components.length - 1 }
    else { val i = _emptySlots.pop; components(i) = constructor; i }
}
//object InfiniteMixture { def apply[P<:Parameter](constructor: =>P) = new InfiniteMixture[P](constructor) }
*/

