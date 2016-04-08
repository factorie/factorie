/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.directed

import cc.factorie.variable._

//trait GateVar extends MutableDiscreteVar
//abstract class GateVariable(initial:Int) extends DiscreteVariable(initial) with GateVar {}

//abstract class PlatedGate(initial:Seq[Int]) extends PlatedDiscrete(initial) with Parameter { }

// For factors between a Mixture and the child generated from that Mixture
trait MixtureFactor extends DirectedFactor {
  //type ChildType <: MixtureGeneratedVar
  def gate: DiscreteVariable
  //def prChoosing(s:StatisticsType, mixtureIndex:Int): Double
  //def prChoosing(mixtureIndex:Int): Double = prChoosing(statistics, mixtureIndex)
  //def logprChoosing(s:StatisticsType, mixtureIndex:Int): Double = math.log(prChoosing(s, mixtureIndex))
  //def logprChoosing(mixtureIndex:Int): Double = logprChoosing(statistics, mixtureIndex)
  //def sampledValueChoosing(s:StatisticsType, mixtureIndex:Int): ChildType#Value
  //def sampledValueChoosing(mixtureIndex:Int): ChildType#Value = sampledValueChoosing(statistics, mixtureIndex)
}


// Graphical model structure of a mixture:
// Proportions =Mixture.Factor=> Mixture[Proportions] =CategoricalMixture.Factor=> Discrete
// Proportions =/                                Gate =/

// TODO I think perhaps this should be a GeneratedVars[] ContainerVariable -akm
class MixtureDomain[+V] extends Domain {
  type Value <: scala.collection.Seq[V]
}
object MixtureDomain extends MixtureDomain[Any]
// NOTE Was Mixture[+P...]
class Mixture[P<:Var](val components:Seq[P])(implicit val model: MutableDirectedModel, implicit val random: scala.util.Random) extends scala.collection.Seq[P] with VarWithDeterministicValue
{
  type Value = scala.collection.Seq[P#Value]
  /* A Mixture is a deterministic function of its parents.  
     This fact is examined in DirectedModel.factors, causing factors(mixtureComponent)
     to return not only this Mixture but also all the children of this Mixture. */
  //type Value <: scala.collection.Seq[P#Value]
  this ~ Mixture() // This will make this a child of each of the mixture components.
  def domain = MixtureDomain.asInstanceOf[MixtureDomain[P#Value]]
  def apply(i:Int) = components(i)
  def length = components.length
  def iterator = components.iterator
  def value = this.map(_.value)
  // TODO Note that if the Mixture grows, new component Parameters will not be properly get this childFactor 
  //val parentFactors = components.map(c => { val f = new MixtureComponent.Factor(this, c); c.addChildFactor(f); f })
  //parentFactor = new MixtureComponent.Factor(this, new SeqParameterVars(components)) // TODO Look at this again carefully
  //components.foreach(p => p.addChildFactor(parentFactor)) 
  //override def isDeterministic = true
  /*def childFactorsOf[P2>:P](p:P2): Seq[MixtureFactor] = {
    val index = this.indexOf(p)
    //children.filter(_.isInstanceOf[MixtureGeneratedVar]).asInstanceOf[Iterable[MixtureGeneratedVar]].filter(_.choice.intValue == index)
    // If this cast fails, it means that some of the children are not a MixtureGeneratedVar; 
    // for example, they might be a MixtureSeqGeneratedVar, which is allowed, but not supported by this.childrenOf method.
    val result = new scala.collection.mutable.ArrayBuffer[MixtureFactor]
    for (factor <- childFactors) factor match {
      case f:MixtureFactor => if (f.gate.intValue == index) result += f
      case f:PlatedMixtureFactor => throw new Error("Not yet implemented")
    }
    result
  }*/
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

object Mixture extends DirectedFamily1[Mixture[Var]] {
  def apply[P<:Var](n:Int)(constructor: =>P)(implicit model: MutableDirectedModel, random: scala.util.Random): Mixture[P] = new Mixture[P](for (i <- 1 to n) yield constructor) // TODO Consider Seq.fill instead
  case class Factor(override val _1:Mixture[Var]) extends super.Factor(_1) {
    /** Even though they are the contents of the child, the parents are each of the mixture components. */
    override def parents: Seq[Var] = _1.components
    def pr(v:C#Value) = 1.0
    def sampledValue(implicit random: scala.util.Random): ChildType#Value = throw new Error("Cannot sample a Mixture")
    override def updateCollapsedParents(weight:Double): Boolean = {
      throw new Error("Not yet implemented.")
      //  TODO this is inefficient because it will loop through all children of the Mixture for each Mixture component
      //for (f2 <- _1.childFactorsOf(_2)) f2.updateCollapsedParents(weight) // TODO Consider collapsed Gate
      true
    }
  }
  def newFactor(a:Mixture[Var]) = Factor(a)
}



/*
// TODO Not yet finished
class InfiniteMixture[P<:Parameter:ClassTag](constructor: =>P) extends MixtureComponents[P] {
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



