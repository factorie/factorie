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

// A collection of abstract Variables (and a generic Sampler) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).

// TODO Remove these next two traits.  I don't think they are necessary.
/** A stand-in for a GeneratedValue.  Note that GeneratedValue itself inherits from this.
    This allows a MarginalizedMixtureChoice to be ... */
trait GeneratedValueLike[O<:GeneratedValue[O]] {
  def asGeneratedValue: O
}
trait GeneratedValueProxy[O<:GeneratedValue[O]] extends GeneratedValueLike[O] {
  def asGeneratedValue: O
}

                                                                                  
/** An Variable that has a 'generativeSource' GenerativeDistribution and a 'pr' probability of being generated.
    This trait does not indicate whether the Variable is mutable or immutable.
    For the corresponding mutable-valued Variable, see GenerativeVariable. */  
trait GeneratedValue[This<:GeneratedValue[This]] extends Variable with GeneratedValueLike[This] {
  this: This => 
  type SourceType <: GenerativeDistributionLike[GenerativeDistribution[This],This]
  private var _generativeSource: SourceType = _
  //def generativeSource: SourceType#GenerativeDistributionType = _generativeSource //.asGenerativeDistribution
  def generativeSource: SourceType = _generativeSource
  @inline final def asGeneratedValue: This = this
  final def setSource(s:SourceType)(implicit d:DiffList) : Unit = {
    if (generativeSource ne null) generativeSource._unregisterSample(asGeneratedValue)
    _setSource(s)
    if (generativeSource ne null) generativeSource._registerSample(asGeneratedValue)
  }
  /** Set 'source' directly without coordinating via source.(un)registerSample.  Don't call this yourself; call 'setSource' instead. */
  override def _setSource(s:AnyRef)(implicit d:DiffList) : Unit = {
    val ss = s.asInstanceOf[SourceType] // TODO Arg.  I want s:SourceType, but Scala type system doesn't like M#OutcomeType vs M.OutcomeType
    if (d ne null) d += SetSourceDiff(_generativeSource, ss)
    _generativeSource = ss
  }
  /** Register this variable as having been generated from source 's'. */
  def ~(s:SourceType): this.type = { setSource(s)(null); this }
  /** Register this variable as having been generated from the source indicated by the MixtureChoice 'mmc'. */
  /*def ~[M<:SourceType](mc:MixtureChoice[M,This,_]) : this.type = {
    mc.setOutcome(asOutcome); 
    this.~(mc.choice) // either here or in mmc.setOutcome; not sure which is more natural
  }*/
  /** Probability of this variable given its 'source' parents. */
  def pr: Double = generativeSource.pr(asGeneratedValue)
  /** log-probability of this variable given its 'source' parents. */
  def logpr: Double = Math.log(pr)
  case class SetSourceDiff(oldSource:SourceType, newSource:SourceType) extends Diff {
    def variable = GeneratedValue.this
    def redo = _generativeSource = newSource
    def undo = _generativeSource = oldSource
  }
}

//trait GenerativeVariable[This<:GenerativeVariable[This] with Variable] extends GenerativeObservation[This] with AbstractGenerativeVariable {
trait GeneratedVariable[This<:GeneratedVariable[This]] extends GeneratedValue[This] with AbstractGeneratedVariable {
  this: This =>
  /** Sample a new value for this variable from the distribution defined by 's'.  
      Note, this differs from the method 'sample' (defined in AbstractGeneratedVariable), which accounts for both the parent and children 
      when this is a GenerativeDistribution as well as a GeneratedVariable */
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit // = throw new Error("Must be defined in subclasses.")
  def :==(s:SourceType): Unit = sampleFrom(s)(null)
  /** Set this variable to a new value, sampled according to the distribution indicated by its 'generativeSource' parents, but not its children. 
      By contrast, see @see sample. */
  def regenerate(implicit d:DiffList): Unit = sampleFrom(generativeSource)
  /** Register this variable as having been generated from source 's', and furthermore set this variable's value to a sample from 's'. */
  def :~(s:SourceType): this.type = { this.sampleFrom(s)(null); this.~(s); this } 
  // Better to sampleFrom first then :~, because :~ may look at this.value and because :~ may add this' currentValue to s' parameter estimate
  // Note however that you can't do "MixtureChoice :~ Multinomial" because of outcome-setting-ordering.
  // We could try to change the design to allow this if we decide it is important.
  /** Register this variable as having been generated from the MixtureComponent source indicated by the given MixtureChoice, 
      and furthermore set this variable's value to a sample from it. */
  /*def :~[M<:SourceType](mmc:MixtureChoice[M,This,_]) : this.type = { this.sampleFrom(mmc.choice)(null); this.~(mmc); this }*/
  //def maximize(implicit d:DiffList): Unit // TODO Consider adding this also; how interact with 'estimate'?  // Set value to that with highest probability
  // 'maximize' would differ from 'estimate' in that maximum likelihood estimation is just one way to 'estimate' a parameter      
}

/** A stand-in for GeneratedVariable that does not take type parameters, which we use as the type argument to GeneratedVariableSampler. */
trait AbstractGeneratedVariable extends Variable {
  /** Set this variable to a new value, sampled according to the distribution 
      indicated by both its 'source' parents and also its 'generatedSamples' children if available. */
  def sample(implicit d:DiffList): Unit
}


// Some specific cases of GeneratedObservation types

// TODO Consider changing DiscreteValue to DiscreteValues
// It would help Inferencer.IndexedMarginal
// But it is a little odd because
// * It could be a BinaryVectorVariable, 
// * It doesn't have "def index".  It might be a little painful to match/case all these places.  
// but we could certainly define pr(BinaryVectorVariable)
// Hmm... Requires further thought.
                                    


trait GeneratedRealValue[This<:GeneratedRealValue[This] with RealValue with GeneratedValue[This]] extends RealValue with GeneratedValue[This] {
  this: This =>
  type SourceType = GenerativeDistributionLike[RealDistribution[This],This]
  //def pr: Double = generativeSource.asGenerativeDistribution.pr(this)
  //def logpr: Double = generativeSource.asGenerativeDistribution.logpr(this)
}

trait GeneratedRealVariable[This<:GeneratedRealVariable[This]] extends RealVariable with GeneratedRealValue[This] with GeneratedVariable[This] {
  this: This =>
  def sampleFrom(source:SourceType)(implicit d:DiffList) = set(source.asGenerativeDistribution.sampleDouble)
  def sample(implicit d:DiffList) = sampleFrom(generativeSource)
}
                                    
/** A GeneratedObservation with densely-packed integer values, for example the outcome of a Multinomial or Poisson.  
    This trait is used for Variables whose value is observed and does not change; 
    for Variables with changing value, you should use DiscreteOutcomeVariable. */
@DomainInSubclasses
trait GeneratedDiscreteValue[This<:GeneratedDiscreteValue[This] with DiscreteValue with GeneratedValue[This]] extends DiscreteValue with GeneratedValue[This] {
  // DiscreteOutcome should not be merged with DiscreteOutcomeVariable because not everything we want to generate has a "setByIndex" to override
  this: This =>  
  // "This" types are a bit verbose.  Could Scala be changed to replace them with this#type ??? 
  // Geoffrey Washburn says yes it is technically possible, but that Martin is simply against adding this feature to the language.
  type SourceType = GenerativeDistributionLike[DiscreteDistribution[This],This]
  //@inline final def asOutcome = this
}

/** A DiscreteOutcome that is also a DiscreteVariable whose value can be changed. */
trait GeneratedDiscreteVariable[This<:GeneratedDiscreteVariable[This] with DiscreteVariable] extends DiscreteVariable with GeneratedDiscreteValue[This] with GeneratedVariable[This] {
  this : This =>
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (generativeSource ne null) generativeSource.preChange(this)
    super.setByIndex(newIndex)
    if (generativeSource ne null) generativeSource.postChange(this)
  }
  /** Alternative setByIndex that avoids coordination with source, for use when you *really* know what you are doing, 
      and you are doing the source coordination yourself. */
  def _setByIndex(newIndex:Int)(implicit d:DiffList) = super.setByIndex(newIndex) 
  // TODO The above method is a bit scary because we may loose opportunities to fruitfully override setByIndex in subclasses
  // However it saved us (115-111) seconds in the LDA timing run described in Generative.scala. 
  def sampleFrom(source:SourceType)(implicit d:DiffList) = setByIndex(source.asGenerativeDistribution.sampleIndex)
  def distribution: Array[Double] = { // TODO Remove or rename?  Rename proportion?
    val buffer = new Array[Double](domain.size);
    for (i <- 0 until buffer.length) buffer(i) = generativeSource.asGenerativeDistribution.pr(i); 
    buffer
  }
  def sample(implicit d:DiffList): Unit = {
    // TODO Thursday Check!!!  Just removed this because I think it is redundant with preChange/postChange in setByIndex 
    //val isDM = classOf[DirichletMultinomial[This]].isAssignableFrom(getClass)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, -1)
    setByIndex(generativeSource.asGenerativeDistribution.sampleIndex)
    //if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, 1)
  }
  def maximize(implicit d:DiffList): Unit = {
    setByIndex(generativeSource.asGenerativeDistribution.maxPrIndex)
  }
}


// TODO Delete these next two.  DiscreteGenerating is sufficient to generate a CategoricalValue
trait GeneratedCategoricalValue[This<:GeneratedCategoricalValue[This] with CategoricalValue] extends CategoricalValue with GeneratedDiscreteValue[This] {
  this : This =>
  //type SourceType = CategoricalGenerating[This]
}
trait GeneratedCategoricalVariable[This<:GeneratedCategoricalVariable[This] with CategoricalVariable with GeneratedCategoricalValue[This]] extends CategoricalVariable with GeneratedDiscreteVariable[This] with GeneratedCategoricalValue[This] {
  this : This =>
}


/** A GeneratedObservation that consists of a vector of Doubles that sum to one, for example the parameters of a Multinomial. */
trait GeneratedProportionValue[O<:DiscreteValue] extends RandomAccessSeq[Double] with GeneratedVariable[GeneratedProportionValue[O]] {
  type SourceType <: GenerativeDistributionLike[ProportionDistribution[O],GeneratedProportionValue[O]]
  def localPr(index:Int): Double // TODO Needed in DirichletMomentMatchingEstimator, but general enough?
  def proportion: RandomAccessSeq[Double]
}





/** A generic Sampler for any AbstractGeneratedVariable, relying on its 'sample' method. */
class GeneratedVariableSampler extends Sampler[AbstractGeneratedVariable] {
  def process1(v:AbstractGeneratedVariable): DiffList = { 
    val d = newDiffList
    v.sample(d)
    //println("GeneratedVariableSampler "+d)
    d
  }
}
