/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random
import cc.factorie.util.Implicits._


/** Trait for any distribution that might be selected as as part of a mixture.
    Since it inherits from ItemizedObservation, the variables themselves are entered into the Domain,
    and this.index is a densely-packed numbering of the distribution instances themselves.
    The number of components in the mixture is the size of the domain.  
    Values of the domain are these MixtureComponent objects.
    Note that this is not a GeneratedCategoricalValue, it is the *categorical value* of a GeneratedCategoricalValue.
    (This is one of those times that the "Value" name is confusing, in that "Value" really means variable.)
    A GeneratedCategoricalValue that has this value is a MixtureChoice. 
    @author Andrew McCallum
*/
trait MixtureComponent extends Catalog {
	//this: GenerativeDistribution[GeneratedVariable[_]] with Catalog =>
  this: GenerativeDistribution[_] with Catalog =>
  type O = OutcomeType
  def mixtureIndex = Catalog.indexOf(this)
  /** Gather together the generated samples from all the MixtureComponents, returning each with its weight associated with this MixtureChoice's MixtureComponent. */
  /*override def weightedGeneratedSamples: Iterator[(O,Double)] = new Iterator[(O,Double)] {
    val components = Catalog.get[GenerativeDistribution[GeneratedVariable[_]] with MixtureComponent](this.getClass).iterator
    var currentComponent = components.next // TODO perhaps check to make sure that components hasNext
    var elts = currentComponent.generatedSamples.iterator
    var nextElt: O = if (elts.hasNext) elts.next else null.asInstanceOf[O]
    def hasNext = nextElt != null
    def next = { 
      val result = nextElt
      if (elts.hasNext) nextElt = elts.next 
      else if (components.hasNext) {
        currentComponent = components.next
        elts = currentComponent.generatedSamples.iterator
        if (elts.hasNext) nextElt = elts.next else nextElt = null.asInstanceOf[O]
      } else nextElt = null.asInstanceOf[O]
      (result,weight(result))
    }
    private def weight(o:O) = throw new Error 
//    {
//      o.asInstanceOf[GeneratedValue[_]].generativeSource.asInstanceOf[MixtureComponentRef[_,O]].gate match {
//        case mc:MultinomialDiscrete[_] => mc.pr(mixtureIndex)
//        case mc:DiscreteValue => if (mixtureIndex == mc.intValue) 1.0 else 0.0
//        case _ => throw new Error("MixtureComponent generatedSample does not have a MixtureChoice generativeSource; it was instead "+o.asInstanceOf[GeneratedValue[_]].generativeSource)
//      }
//    }
  }*/
}

trait AbstractMixtureComponentRef extends AbstractGatedRefVariable {
  def outcomePr(index:Int): Double
}

/** The generativeSource of a mixture outcome. */
class MixtureComponentRef[A<:GenerativeDistribution[O] with MixtureComponent,O<:Variable](o:O)(implicit m:Manifest[A]) extends SourceRefVariable[A,O](o) with AbstractMixtureComponentRef with GatedRefVariable[A] {
  Catalog.alias(m.erasure, this.getClass) // So that we don't have to use memory to remember 'm'
  def value(index:Int): A = Catalog.get[A](this.getClass, index)
  def domainSize = Catalog.size(this.getClass)
  /** The probability of this.outcome being generated from the 'index'th mixture component. */
  def outcomePr(index:Int) = value(index).pr(outcome)
}



/** A multinomial outcome that is an indicator of which mixture component in a MixtureChoice is chosen.
    Its categorical value is a MixtureComponent. 
    The "Z" in Latent Dirichlet Allocation is an example. 
    @author Andrew McCallum
 */  
// Example usage, in LDA: 
// class Topic extends Multinomial[Word] with MixtureComponent
// class Z extends MixtureChoice[Topic,Z]; Domain.alias[Z,Topic]
// class Theta extends Multinomial[Z];
// TODO Can I make this:
// MixtureChoice[M<:MixtureComponent[M,_],This<:MixtureChoice[M,M#O,This]]
@DomainInSubclasses
class MixtureChoice[This<:MixtureChoice[This]] extends DiscreteVariable with GeneratedDiscreteVariable[This] with Gate[AbstractMixtureComponentRef] {
  this: This =>
  type VariableType = This
  //type ContentType <: AbstractMixtureComponentRef //[GenerativeDistribution[Variable],Variable] // forSome {A<:Variable}
  // TODO The value of this gate needs to be initialized some time, but not yet?
  super.setByIndex(Global.random.nextInt(domain.size))(null) // TODO is this how _index should be initialized?
  def temperature = 1.0
  override def sample(implicit d:DiffList): Unit = {
    val src = generativeSource.value
    // Unregister the mixture outcomes whose generativeSource this controls, to remove their sufficient statistics from the model
    // Each 'ref' here is a MixtureComponentRef, inheriting from SourceRefVariable, which calls GenerativeDistribution._unregisterSample as necessary.
    src.preChange(this)
    for (ref <- contents) ref.setToNull()
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0
    while (i < size) { 
      distribution(i) = src.pr(i)
      for (ref <- contents) distribution(i) *= ref.asInstanceOf[MixtureComponentRef[_,_]].outcomePr(i)
      if (temperature != 1.0) distribution(i) = Math.pow(distribution(i), 1.0/temperature)
      sum += distribution(i)
      i += 1
    }
    assert(sum > 0.0) // This can happen is the temperature is too low
    // If we are actually a MultinomialDiscrete distribution (integrating out our discrete value) then save the distribution
    this match { case md:MultinomialDiscrete[This] => md.multinomial.set(distribution); case _ => {} }
    // Sample a value from this distribution
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    this.setByIndex(i) // change the value of choice, this will also correspondingly set each of the gate contents values
    src.postChange(this) // tell the GenerativeDistribution responsible for this about our new value
  }
}

class MarginalizedMixtureChoice[This<:MarginalizedMixtureChoice[This]] extends MixtureChoice[This] with MultinomialDiscrete[This] {
  this: This =>
}

/*
class MarginalizedMixtureChoice[M<:MixtureComponent[M,O],O<:GeneratedVariable[O],This<:MarginalizedMixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends MixtureChoice[M,O,This] with MultinomialCategorical[This] {
  this : This =>
  // Next two methods removed because we will actually register and unregister samples, even if it means we will most of the time simply remove/add to the same MixtureChoice.generatedSample HashSet
  // This way is will be safe to move a sample from a MixtureChoice to a different source. 
//  def disabled_registerSample(o:O)(implicit d:DiffList): Unit = {
//    if (!(o.generativeSource == null || o.generativeSource == this)) throw new Error("Trying to move a sample to a MixtureChoice not part of its original Domain")
//    // If 'o' is already registered assume that it has been registered in all mixture components
//    if (choice.generatedSamples.contains(o)) return // TODO remove this so that we throw error if we try to register twice.
//    // Register this 'o' with this mixture component's choice
//    super._registerSample(o)
//  }
//  def disabled_unregisterSample(o:O)(implicit d:DiffList): Unit = {
//    if (o.generativeSource != this) throw new Error("Trying to remove a sample from a MixtureComponent of which is was not a part.")
//    // Do nothing.  The degree of membership of an 'o' in each mixture component is determined by this.multinomial.
//    // TODO Note that this does not allow us to change the generativeSource to a GenerativeDistribution outside the mixture once it has been set in the mixture!
//  }
  // TODO !!! I'm not sure the next method is correct.
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (outcome == null) throw new Error("No outcome yet set.")
    val src = generativeSource
    choice._unregisterSample(outcome)
    src.preChange(this)
    super._setByIndex(newIndex) // this changes the value of 'choice'
    src.postChange(this)
    choice._registerSample(outcome)
  }
  override def setRandomly(random:Random, d:DiffList): Unit = {
    super.setRandomly(random, d)
    this match { case md:MultinomialDiscrete[This] => md.multinomial.setDirac(this.index); case _ => {} }
  }
  //override def setRandomly:Unit = super.setRandomly
  // TODO Are these next two methods correct?
  //override def preChange(o:O)(implicit d:DiffList): Unit = Domain[M](mm).foreach(mc => mc.preChange(o))
  //override def postChange(o:O)(implicit d:DiffList): Unit = Domain[M](mm).foreach(mc => mc.postChange(o))
  //override def pr(o:O): Double = { val d = Domain[M](mm); var result = 0.0; for (i <- 0 until d.size) result += multinomial(i) * d.get(i).pr(o); result } 
  //override def logpr(o:O): Double = Math.log(pr(o))
  def unused_sample(implicit d:DiffList): Unit = {
    val src = generativeSource
    // Remove this variable and its sufficient statistics from the model
    choice.unregisterSample(outcome)
    //choice.preChange(outcome)
    src.preChange(this)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0; while (i < size) { distribution(i) = src.pr(i) * dom.get(i).pr(outcome); sum += distribution(i); i += 1 }
    // If we are actually a MultinomialDiscrete distribution (integrating out our discrete value) then save the distribution
    this match { case md:MultinomialDiscrete[This] => md.multinomial.set(distribution); case _ => {} }
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    this._setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    //src.postChange(this) // Could be postChange(this) instead of registerSample(this)
    choice.registerSample(outcome)
    choice.postChange(outcome)
  }
}
*/

//@DomainInSubclasses class MultinomialMixtureChoice[M<:MixtureComponent[Multinomial[O],O],O<:DiscreteOutcome[O],This<:MixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends MixtureChoice[M,O,This] { this: This => }
//class DirichletMixtureChoice
//class GaussianMixtureChoice


  
/** A Template for scoring changes to a MixtureChoice. */ 
object MixtureChoiceTemplate extends TemplateWithStatistics1[GenericMixtureChoice] {
  def score(s:Stat) = { throw new Error; val mc = s.s1; mc.logpr /* + mc.contents.reduceLeft((sum,ref) => sum + mc.value.logpr(outcome))*/ }
}
abstract class GenericGeneratedDiscreteValue extends GeneratedDiscreteValue[GenericGeneratedDiscreteValue] { def intValue = -1 }
// The "2" below is arbitrary, but since this constructor is never called, it shouldn't make any difference
abstract class GenericMixtureComponent extends DenseCountsMultinomial[GenericGeneratedDiscreteValue](2) with MixtureComponent //[GenericMixtureComponent,GenericGeneratedDiscreteValue]
abstract class GenericMixtureChoice extends MixtureChoice[GenericMixtureChoice]
