/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet

// A collection of abstract Variables (and a generic Template) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).


/** A mutable reference to a generative distribution, used in GeneratedValue to hold a pointer to the variable's generative source distribution. */
class SourceRefVariable[A<:Distribution[O],O<:Variable](val outcome:O) extends RefVariable[A] {
  override def set(newValue:A)(implicit d:DiffList): Unit = if (newValue != value) {
    if (value != null) value._unregisterSample(outcome)
    super.set(newValue)
    if (value != null) value._registerSample(outcome)
  }
  // TODO Because the same diff.variable may be in the difflist multiple times, we should unique them before creating Factors, rather than after!
}
                                                                                  
/** An Variable that has a 'generativeSource' Distribution and a 'pr' probability of being generated.
    This trait does not indicate whether the Variable is mutable or immutable.
    For the corresponding mutable-valued Variable, see GenerativeVariable. */  
trait GeneratedValue[This<:GeneratedValue[This]] extends Variable {
  this: This =>  // TODO Try to make this less strict so that we could have a Gaussian generating "generic" Real values.
  type SourceType <: Distribution[This]
  var generativeSource: SourceRefVariable[SourceType,This] = null
  /** Register this variable as having been generated from source 's'. */
  def ~(s:SourceType): this.type = { 
    assert(generativeSource eq null)
    generativeSource = new SourceRefVariable[SourceType,This](this)
    generativeSource := s
    this
  }
  /** Register this variable as having been generated from mixture 'M' with gate 'g'. */
  //def ~[M<:SourceType with MixtureComponent](gate:MixtureChoice[_])(implicit m:Manifest[M]): this.type =
  def ~[M<:SourceType with MixtureComponent](gate:MixtureChoice[_])(implicit m:Manifest[M]): this.type = {
    assert(generativeSource eq null)
    val gs = new MixtureComponentRef[M,This](this)(m)
    generativeSource = gs.asInstanceOf[SourceRefVariable[SourceType,This]] //.asInstanceOf[SourceRefVariable[SourceType,This]] // TODO Yipes, make this cast unnecessary
    gs := Catalog.get[SourceType with MixtureComponent](m.erasure, gate.intValue).asInstanceOf[M]
    // TODO Get rid of the following cast
    //throw new Error // the next line needs uncommenting
    gate += gs //.asInstanceOf[SourceRefVariable[SourceType,This]] // TODO If this.generativeSource gets re-set later, do we g -= gs?  Yes, I think so.  Needs doing.
    this
  }
  /** Probability of this variable given its 'source' parents. */
  def pr: Double = generativeSource.value.pr(this)
  /** log-probability of this variable given its 'source' parents. */
  def logpr: Double = Math.log(pr)
}

// TODO Is this needed any more?  Consider removing.
/** A stand-in for GeneratedVariable that does not take type parameters, which we use as the type argument to GeneratedVariableSampler. */
trait AbstractGeneratedVariable extends Variable {
  type SourceType <: Variable
  /** Sample a new value for this variable from the distribution defined by generative source 's'. */
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit
}

trait GeneratedVariable[This<:GeneratedVariable[This]] extends GeneratedValue[This] with AbstractGeneratedVariable {
  this: This =>
  /** Sample a new value for this variable from the distribution defined by generative source 's'.  
      Note, this differs from the method 'sample' (defined in AbstractGeneratedVariable), which accounts for both the parent and children 
      when this is a Distribution as well as a GeneratedVariable */
  def sampleFrom(s:SourceType)(implicit d:DiffList): Unit // = throw new Error("Must be defined in subclasses.")
  /** sampleFrom this variable's current generativeSource. */
  def sampleFromSource(implicit d:DiffList): Unit = sampleFrom(generativeSource.value)(d)
  /** An alias for sampleFrom. */
  def :==(s:SourceType): Unit = sampleFrom(s)(null)
  /** Register this variable as having been generated from source 's', and furthermore set this variable's value to a sample from 's'. */
  def :~(s:SourceType): this.type = { this.sampleFrom(s)(null); this.~(s); this } 
  def :~[M<:SourceType with MixtureComponent](g:Gate[_])(implicit m:Manifest[M]) = throw new Error("Not yet implemented")
  // Better to sampleFrom first then :~, because :~ may look at this.value and because :~ may add this' currentValue to s' sufficient statistics
  // Note however that you can't do "MixtureChoice :~ Multinomial" because of outcome-setting-ordering.
  // We could try to change the design to allow this if we decide it is important.
  /** Set the value of this variable to that which has highest probability from distribution 's'. */
  def maximizeFrom(s:SourceType)(implicit d:DiffList): Unit
  /** Maximize from this variable's current generativeSource. */
  def maximizeFromSource(implicit d:DiffList): Unit = maximizeFrom(generativeSource.value)(d)
  // 'maximize' differs from 'estimate' in Distribution in that maximum likelihood estimation is just one way to estimate a parameter.
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
  type SourceType = RealDistribution[This]
}

trait GeneratedRealVariable[This<:RealVariable with GeneratedRealVariable[This]] extends RealVariable with GeneratedRealValue[This] with GeneratedVariable[This] {
  this: This =>
  def sampleFrom(source:SourceType)(implicit d:DiffList) = set(source.sampleDouble)
}

// TODO Flesh this out.  But I think we need to use "This" generic type.
//trait GeneratedIntValue extends IntValue with GeneratedVariable[IntValue]

trait GeneratedOrdinalValue[This<:GeneratedOrdinalValue[This]] extends OrdinalValue with GeneratedVariable[This] {
  this: This =>
}

trait GeneratedOrdinalVariable[This<:GeneratedOrdinalValue[This]] extends OrdinalVariable with GeneratedVariable[This] {
  this: This =>
}
                                    
/** A GeneratedObservation with densely-packed integer values, for example the outcome of a Multinomial or Poisson.  */
@DomainInSubclasses
trait GeneratedDiscreteValue[This<:GeneratedDiscreteValue[This]] extends DiscreteValue with GeneratedOrdinalValue[This] {
  this: This =>  
  // "This" types are a bit verbose.  Could Scala be changed to replace them with this#type ??? 
  // Geoffrey Washburn says yes it is technically possible, but that Martin is simply against adding this feature to the language.
  type SourceType = DiscreteDistribution[This]
  def setByIndex(newIndex:Int)(implicit d:DiffList): Unit
}

/** A DiscreteOutcome that is also a DiscreteVariable whose value can be changed. */
class GeneratedDiscreteVariable[This<:GeneratedDiscreteVariable[This]] extends DiscreteVariable with GeneratedDiscreteValue[This] with GeneratedOrdinalVariable[This] /*with QDistribution with Marginal*/ {
  this : This =>
  def this(initialValue:Int) = { this(); assert(initialValue >= 0 && initialValue < domain.size); setByInt(initialValue)(null) }
  /*override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (generativeSource.value ne null) generativeSource.value.preChange(this) // TODO remove?  YES!!
    super.setByIndex(newIndex)
    if (generativeSource.value ne null) generativeSource.value.postChange(this)
  }*/
  // TODO The above method is a bit scary because we may loose opportunities to fruitfully override setByIndex in subclasses
  // However it saved us (115-111 = 4) seconds in the LDA timing run described in Generative.scala. 
  def sampleFrom(source:SourceType)(implicit d:DiffList) = setByIndex(source.sampleIndex)
  /*def distribution: Array[Double] = { // TODO Remove or rename?  Rename proportion?
    val buffer = new Array[Double](domain.size);
    for (i <- 0 until buffer.length) buffer(i) = generativeSource.value.pr(i); 
    buffer
  }*/
  def maximizeFrom(s:SourceType)(implicit d:DiffList): Unit = setByIndex(s.maxPrIndex)
  //override def newQ: DenseMultinomial[This] = new DenseMultinomial[This](domain.size)
  //override def newMarginal: MultinomialDiscrete[This] = new MultinomialDiscrete[This] {}
}


// TODO Delete these next two.  DiscreteGenerating is sufficient to generate a CategoricalValue
// trait GeneratedCategoricalValue[T,This<:GeneratedCategoricalValue[T,This] with CategoricalValue[T]] extends CategoricalValue[T] with GeneratedDiscreteValue[This] {
//   this : This =>
// }
// trait GeneratedCategoricalVariable[This<:CategoricalVariable[_] with GeneratedCategoricalVariable[This]] extends GeneratedDiscreteVariable[This] with GeneratedCategoricalValue[T,This] {
//   this : This =>
//   def this(initialValue:T) = { this(); set(initialValue)(null) }
// }


/** A GeneratedObservation that consists of a vector of Doubles that sum to one, for example the parameters of a Multinomial. */
trait GeneratedProportionValue[O<:DiscreteValue] extends IndexedSeq[Double] with GeneratedVariable[GeneratedProportionValue[O]] {
  type SourceType <: ProportionDistribution[O]
  def localPr(index:Int): Double // TODO Needed in DirichletMomentMatchingEstimator, but general enough?
  def proportion: Seq[Double]
}




object GeneratedVariableTemplate extends TemplateWithStatistics1[GeneratedVariable[GeneratedVariable]] {
  def score(s:Stat) = s.s1.logpr
}


