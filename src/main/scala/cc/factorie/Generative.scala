/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection abstract Variables for the distributions of generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).



// TODO  Consider something like this.  A trait for factory objects
//trait GenerativeFactory[Source<:AbstractGenerativeDistribution] { def apply(Source#OutcomeType#ValueType): Source#OutcomeType }

/** A Variable representing a probability distribution that generates other variable values with type OutcomeType
    It provides methods returning the probability of given values, and for (re-)sampling a new value into an existing variable. 
    Optionally, it keeps track of all the variables generated, for use in estimating the parameters of this distribution,
    and for use in finding neighbors for factor Templates. 
    @author Andrew McCallum */
// Change this to O<:TypedVariable so that we can add def sampleValue:O#ValueType
trait GenerativeDistribution[O<:Variable] extends AbstractGenerativeDistribution {
  // Note that 'O' is not *required* to be a GenerativeVariable.  This allows us to put any DiscreteVariable into Multinomial, for example.
  type OutcomeType = O // TODO Consider insisting the OutcomeType = GenerativeObservation[O]; For now I've simly added a noop 'setSource' method to Variable
}

/** A stand-in for GenerativeDistribution that does not take type parameters.
    @author Andrew McCallum */
trait AbstractGenerativeDistribution extends Variable {
  type OutcomeType <: Variable 
  // Perhaps we really want OutcomeType to be 'either ConstantVariable or GenerativeObservation', 
  //  because changing Variables should be able to find their source.
  // How to do this?  
  // Perhaps define a trait VariableWithSource[This] { type SourceType <: GenerativeDistribution[This]; def _setSource...}
  // and then make ConstantVariable and GenerativeObservation both inherit from it?
  // No, I don't think this is necessary.  Putting Real into Gaussian would be useful, 
  //  and it is useful even when we are not trying to track score changes due to changes in Real.value
  type VariableType <: AbstractGenerativeDistribution
  def estimate: Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?  No, but change its name to 'maximize'!  This will apply to both variables and distributions
  lazy val generatedSamples = new HashSet[OutcomeType];
  var keepGeneratedSamples = true
  def _registerSample(o:OutcomeType)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
    generatedSamples += o
    if (d != null) d += GenerativeDistributionRegisterDiff(o)
  }
  def _unregisterSample(o:OutcomeType)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    generatedSamples -= o
    if (d != null) d += GenerativeDistributionUnregisterDiff(o)
  }
  /* Consider something like registerWeightedSample(o,w), for EM.  
   * But then what is o.source?  A 'WeightedSource' collection of sources?  
   * Should this be done by some more general variational approach? 
  lazy val weightedGeneratedSamples = new HashMap[OutcomeType,Double];
  // TODO Candidate for Scala 2.8 @specialized
  def _registerWeightedSample(o:OutcomeType, weight:Double)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    if (weightedGeneratedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
    weightedGeneratedSamples(o) = weight
    if (d != null) d += GenerativeDistributionRegisterWeightedDiff(o, weight)
  }
  def _unregisterWeightedSample(o:OutcomeType, weight:Double)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    weightedGeneratedSamples -= o
    if (d != null) d += GenerativeDistributionUnregisterWeightedDiff(o, weight)
  }*/
  /** Notify this GenerativeDistribution that it is now associated with an additional sampled outcome, and set o's source to this. */
  final def registerSample(o:OutcomeType)(implicit d:DiffList): Unit = {
    _registerSample(o)
    //o._setSource(this)//.asInstanceOf[OutcomeType#SourceType]
    o._setSource(this)
  }
  /** Notify this GenerativeDistribution that it is no longer associated with a sampled outcome, and set o's source to null. */
  final def unregisterSample(o:OutcomeType)(implicit d:DiffList): Unit = {
    _unregisterSample(o)
    //if (o.isInstanceOf[GenerativeObservation[_]]) o.asInstanceOf[GenerativeObservation[_]]._setSource(null.asInstanceOf[OutcomeType#SourceType])
    o._setSource(null)
  }
  // TODO consider removing preChange/postChange, because it requires extra infrastructure/Diffs from implementers?  Can just use (un)registerSample
  /** Notify this GenerativeDistribution that the value of its associated outcome 'o' is about to change.  
      Calls to this method are always paired with (followed by) a call to postChange. */
  def preChange(o:OutcomeType)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  def postChange(o:OutcomeType)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  /** Return the probability that this GenerativeDistribution would generate outcome 'o' */
  def pr(o:OutcomeType): Double
  /** Return the log-probability that this GenerativeDistribution would generate outcome 'o' */
  def logpr(o:OutcomeType): Double = Math.log(pr(o))
  /** Change the value of outcome 'o' to a value sampled from this GenerativeDistribution */
  //@deprecated def sampleInto(o:OutcomeType): Unit // TODO Perhaps this should take a DiffList; then GenerativeVariable.sample could be implemented using it
  // Fighting with the Scala type system, see:
  // http://www.nabble.com/Path-dependent-type-question-td16767728.html
  // http://www.nabble.com/Fwd:--lift--Lift-with-Scala-2.6.1--td14571698.html 
  def unsafeRegisterSample(o:Variable)(implicit d:DiffList) = registerSample(o.asInstanceOf[OutcomeType])
  def unsafeUnregisterSample(o:Variable)(implicit d:DiffList) = unregisterSample(o.asInstanceOf[OutcomeType])
  def unsafePr(o:Variable) = pr(o.asInstanceOf[OutcomeType])
  def unsafeLogpr(o:Variable) = logpr(o.asInstanceOf[OutcomeType])
  case class GenerativeDistributionRegisterDiff(m:OutcomeType) extends Diff {
    def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
    def redo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
    def undo = { generatedSamples -= m }
  }
  case class GenerativeDistributionUnregisterDiff(m:OutcomeType) extends Diff {
    def variable = AbstractGenerativeDistribution.this.asInstanceOf[VariableType]
    def redo = { generatedSamples -= m }
    def undo = { if (generatedSamples.contains(m)) throw new Error else generatedSamples += m}
  }
}

// TODO Consider something like this?  Would be needed in "def sample"
//trait RealParameter { val children ... }




// Some specific cases of GenerativeDistribution types
// TODO Rename OrdinalDistribution, DiscreteDistribution, CategoricalDistribution, ProportionDistribution, RealDistribution, etc.

/** A GenerativeDistribution that generates discrete (Int) outcomes (perhaps even a DiscreteOutcome), for example a Poisson. */
trait OrdinalGenerating[O<:OrdinalValue] extends GenerativeDistribution[O] {
  def sampleIndex: Int // TODO Rename sampleInt?  And likewise below?
  def pr(index:Int): Double
  def logpr(index:Int): Double
}

/** A GenerativeDistribution that generates DiscreteValue outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
trait DiscreteGenerating[O<:DiscreteValue] extends GenerativeDistribution[O] with OrdinalGenerating[O] {
  def sampleIndex: Int
  def maxPrIndex: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
  def proportion: RandomAccessSeq[Double]
}

/** A GenerativeDistribution that generates categorical outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
// TODO This should simply inherit from DiscreteGenerating
trait CategoricalGenerating[O<:CategoricalValue] extends GenerativeDistribution[O] with DiscreteGenerating[O] {
  def sampleIndex: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
}


/** A GenerativeDistribution that generates proportions (vectors with values summing to 1.0), for example a Dirichlet*/
trait ProportionGenerating[O<:DiscreteValue] extends GenerativeDistribution[ProportionOutcome[O]] {
  def size: Int
  //def sampleProportions: Seq[Double]
  def sampleProportionsWithCounts(counts:{def apply(i:Int):Double; def size:Int}): Seq[Double]
  def pr(proportions:ProportionOutcome[O]): Double
}

/** A GenerativeDistribution that generates real values as Double (specifically a cc.factorie.Real), for example a Gaussian distribution */
// TODO Consider changing "Real" to "RealValue" and "RealVariable"
trait RealGenerating[O<:Real] extends GenerativeDistribution[O] {
  def sampleDouble: Double
}

/** A GenerativeDistribution that generates positive real values as Double (represented by a cc.factorie.Real), for example a Gamma distribution */
trait PositiveRealGenerating[O<:Real] extends RealGenerating[O]






