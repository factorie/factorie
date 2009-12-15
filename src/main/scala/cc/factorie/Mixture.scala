/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._


  
/** Trait for any distribution that might be selected as as part of a Multinomial mixture.
    Since it inherits from ItemizedObservation, the variables themselves are entered into the Domain,
    and this.index is a densely-packed numbering of the distribution instances themselves.
    The number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponent objects.
    Note that this is not a CategoricalOutcome, it is the *categorical value* of a CategoricalOutcome. 
    A CategoricalOutcome that has this value is a MixtureChoice. 
    @author Andrew McCallum
*/
trait MixtureComponent[This<:MixtureComponent[This,O] with GenerativeDistribution[O] with ItemizedObservation[This],O<:Variable] extends GenerativeDistribution[O] with ItemizedObservation[This] {
  this : This =>
}


/** A multinomial outcome that is an indicator of which mixture component in a MixtureChoice is chosen.
    Its categorical value is a MixtureComponent. 
    The "Z" in Latent Dirichlet Allocation is an example. 
    @author Andrew McCallum
 */  
// Example usage, in LDA: 
// class Topic extends Multinomial[Word] with MixtureComponent[Topic]
// class Z extends MixtureChoice[Topic,Z]; Domain.alias[Z,Topic]
// class Theta extends Multinomial[Z];
@DomainInSubclasses
class MixtureChoice[M<:MixtureComponent[M,O],O<:Variable,This<:MixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends CategoricalOutcomeVariable[This] with GenerativeDistributionProxy[M,O] {
  this : This =>
  type VariableType = This
  type ValueType = M
  //def asOutcome = this
  override def asGenerativeSource = choice
  def choice: M = domain.get(index)
  // We do the following line in super because "_outcome" will not be set at initialization time.
  super.setByIndex(Global.random.nextInt(domain.size))(null) // TODO is this how _index should be initialized?
  // Make sure the defaultModel is prepared to handle this
  if (!Global.defaultModel.contains(MixtureChoiceTemplate)) Global.defaultModel += MixtureChoiceTemplate
  Domain.alias[This,M](mt,mm) // We must share the same domain as M; this aliasing might have been done already, but just make sure its done.
  private var _outcome : O = _
  @inline final def outcome : O = _outcome // The particular outcome that was generated from this choice of mixture component
  override def _registerSample(o:O)(implicit d:DiffList): Unit = {
    setOutcome(o)
    super._registerSample(o)
  }
  private def setOutcome(o:O) = if (_outcome == null) _outcome = o else if (o != _outcome) throw new Error("Outcome already set")
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (_outcome == null) throw new Error("No outcome yet set.")
    choice.unregisterSample(outcome)
    super.setByIndex(newIndex) // this changes the value of 'choice'
    choice.registerSample(outcome)
  }
  // example.LDA on 127 documents with 185129 tokens and 17032 types (Users/mccallum/research/data/text/nipstxt/nips05)
  // 9 iterations, printing topics 4 times: 
  // this.sample with this.setByIndex = 118.4 seconds (old version)
  // this.sample with local super.setByIndex = 114.5 seconds
  // this.sample with above and "val dom" = 109.0 seconds
  // this.sample with above and "DirichletMultinomial.pre/postChange" = 108.0 seconds // TODO Didn't help much; consider removing pre/postChange?
  // NOT DEFAULT this.sample with above and keepGeneratedSamples = false = 103.3 seconds // in DirichletMultinomial?
  // NOT DEFAULT this.sample with above and noDiffList = 503.0 seconds.  Fishy!!!!  // TODO Why?????  Investigate!
  // this.sample with above, after Generative infrastructure overhaul = 115.4 seconds
  // this.sample with above, and _setByIndex instead of setByIndex = 111.4 seconds
  // this.sample with above, after Indexed=>Categorical naming overhaul = 451.5 seconds.  Yipes!  What happened?
  // this.sample with above, after caching results of Manifest <:< in GenericSampler = 34 seconds.  Wow!  Much better!! :-)
  // GibbsSampler = 368.3 seconds
  override def sample(implicit d:DiffList): Unit = {
    //println("MixtureChoice.sample "+index+" diff "+d)
    //|**("MixtureChoice.sample.prep")
    val src = generativeSource
    // Remove this variable and its sufficient statistics from the model
    choice.unregisterSample(outcome)
    generativeSource.preChange(this) // TODO this could be preChange(this) instead of unregisterSample(this)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    //**|
    //|**("MixtureChoice.sample.sample")
    //val distribution = Array.fromFunction[Double]((i:Int) => src.pr(i) * dom.get(i).unsafePr(outcome))(dom.size)
    //val i = Maths.nextDiscrete(distribution, distribution.foldLeft(0.0)(_+_))(Global.random) // TODO Yipes, I'm seeing BoxedDoubleArray here!
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0; while (i < size) { distribution(i) = src.asGenerativeSource.pr(i) * dom.get(i).pr(outcome); sum += distribution(i); i += 1 }
    //// If we are actually a MultinomialDiscrete distribution (integrating out our discrete value )
    ////this match { case md:MultinomialDiscrete[This] => md.multinomial.set(distribution) }
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    //choice.unsafeGenerate(outcome) // Put outcome back, although, inefficiently, the next line moves it again
    //setByIndex(i - 1) // So, instead we do it ourselves.  But then subclassers cannot meaningfully override setByIndex!! // TODO Consider alternatives
    //**|
    //|**("MixtureChoice.sample.post")
    this._setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    src.postChange(this) // Could be postChange(this) instead of registerSample(this)
    choice.registerSample(outcome)
    //**|
  }
}

/*
// I don't think this is the right way to do it
class MarginalizedMixtureChoice[M<:MixtureComponent[M],This<:MarginalizedMixtureChoice[M,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends CategoricalOutcomeVariable[This] with MultinomialDiscrete[This] {
  this : This =>
  type VariableType = This
  type ValueType = M
  class DomainInSubclasses
  def choice: M = domain.get(index)
  _index = Global.random.nextInt(domain.size) // TODO is this how _index should be initialized?
  // Make sure the defaultModel is prepared to handle this
  if (!Global.defaultModel.contains(MixtureChoiceTemplate)) { Global.defaultModel += MixtureChoiceTemplate; throw new Error("Fix this") }
  Domain.alias[This,M](mt,mm) // We must share the same domain as M; this aliasing might have been done already, but just make sure its done.
  private var _outcome : M#OutcomeType = _
  @inline final def outcome : M#OutcomeType = _outcome // The particular outcome that was generated from this choice of mixture component
  def setOutcome(o:M#OutcomeType) = if (_outcome == null) _outcome = o else throw new Error("Outcome already set")
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (_outcome == null) throw new Error("No outcome yet set.")
    choice.unsafeUnregisterSample(outcome)
    super.setByIndex(newIndex) // this changes the value of 'choice'
    choice.unsafeRegisterSample(outcome)
  }
  override def sample(implicit d:DiffList): Unit = {
    val src = source
    // Remove this variable and its sufficient statistics from the model
    choice.unsafeUnregisterSample(outcome)
    source.preChange(this) // TODO this could be preChange(this) instead of unregisterSample(this)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0; while (i < size) { distribution(i) = src.pr(i) * dom.get(i).unsafePr(outcome); sum += distribution(i); i += 1 }
    multinomial.set(distribution)
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    this._setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    src.postChange(this) // Could be postChange(this) instead of registerSample(this)
    choice.unsafeRegisterSample(outcome)
  }
}
*/

/** A Template for scoring changes to a MixtureChoice. */ 
object MixtureChoiceTemplate extends TemplateWithStatistics1[GenericMixtureChoice] {
  def score(s:Stat) = { val mc = s.s1; mc.logpr + mc.choice.logpr(mc.outcome) } 
}
abstract class GenericDiscreteOutcome extends DiscreteOutcome[GenericDiscreteOutcome] { def intValue = -1 }
// The "2" below is arbitrary, but since this constructor is never called, it shouldn't make any difference
abstract class GenericMixtureComponent extends DenseCountsMultinomial[GenericDiscreteOutcome](2) with MixtureComponent[GenericMixtureComponent,GenericDiscreteOutcome]
abstract class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericDiscreteOutcome,GenericMixtureChoice]
