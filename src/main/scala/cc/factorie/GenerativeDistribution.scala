/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection abstract Variables for the distributions of generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).
                                                                               
// TODO Create factors for all generative distributions, so that BP will be supported.
// Make sure there is a factor between a Gaussian, its Real mean parameter, and the data generated from the Gaussian.
// So GeneratedVariables should have children also.
// Then factors could have gate numbers, and we could try harder to implement Minka and Winn's "Gates".

// TODO Consider also making a GenerativeFunction for deterministic relations between parent and child.



/** A stand-in for a GenerativeDistribution.  Note that GenerativeDistribution itself inherits from this.
    This allows a MixtureChoice to be a the generativeSource of a GeneratedVariable, 
    because although MixtureChoice does not inherit from GenerativeDistribution it does inherit from
    GenerativeDistributionProxy, which in turn inherits from GenerativeDistributionLike. */
trait GenerativeDistributionLike[+S<:GenerativeDistribution[O],O<:Variable] extends Variable {
  //type GenerativeDistributionType = S
  def asGenerativeDistribution: S //= this.asInstanceOf[S]
  def registerSample(o:O)(implicit d:DiffList): Unit
  def unregisterSample(o:O)(implicit d:DiffList): Unit
  def _registerSample(o:O)(implicit d:DiffList): Unit
  def _unregisterSample(o:O)(implicit d:DiffList): Unit
  def preChange(o:O)(implicit d:DiffList): Unit
  def postChange(o:O)(implicit d:DiffList): Unit
  def pr(o:O): Double
  def logpr(o:O): Double
}

/** A GenerativeDistributionLike that forwards all its requests to a proxy, returned by method asGenerativeDistribution. */
trait GenerativeDistributionProxy[+S<:GenerativeDistribution[O],O<:Variable] extends GenerativeDistributionLike[S,O] {
  def registerSample(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution.registerSample(o)
  def unregisterSample(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution.unregisterSample(o)
  def _registerSample(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution._registerSample(o)
  def _unregisterSample(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution._unregisterSample(o)
  def preChange(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution.preChange(o)
  def postChange(o:O)(implicit d:DiffList): Unit = asGenerativeDistribution.postChange(o)
  def pr(o:O): Double = asGenerativeDistribution.pr(o)
  def logpr(o:O): Double = asGenerativeDistribution.logpr(o)
}

// TODO  Consider something like this.  A trait for factory objects
//trait GenerativeFactory[Source<:AbstractGenerativeDistribution] { def apply(Source#OutcomeType#ValueType): Source#OutcomeType }

/** A Variable representing a probability distribution that generates other variable values with type O.
    It provides methods returning the probability of given values, and for (re-)sampling a new value into an existing variable. 
    Optionally, it keeps track of all the variables generated, for use in estimating the parameters of this distribution,
    and for use in finding neighbors for factor Templates. 
    @author Andrew McCallum */
// TODO Change this to O<:TypedVariable so that we can add def sampleValue:O#ValueType
//   but then make sure this still works for IntValue and RealValue.
trait GenerativeDistribution[O<:Variable] extends GenerativeDistributionLike[GenerativeDistribution[O],O] {
  // Note that 'O' is not *required* to be a GenerativeVariable.  This allows us to put any DiscreteVariable into Multinomial, for example.
  type VariableType <: GenerativeDistribution[O];
  type OutcomeType = O // TODO Consider insisting the OutcomeType = GenerativeObservation[O]; For now I've simly added a noop 'setSource' method to Variable
  def asGenerativeDistribution = this
  def estimate: Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?  No, but change its name to 'maximize'!  This will apply to both variables and distributions
  private lazy val _generatedSamples: HashSet[O] = new HashSet[O];
  def generatedSamples: scala.collection.Set[O] = _generatedSamples // TODO I want this to be .readOnly, but how in Scala 2.8?
  def weightedGeneratedSamples: Iterator[(O,Double)] = new Iterator[(O,Double)] {
    val elts = _generatedSamples.iterator
    def hasNext = elts.hasNext
    def next = (elts.next,1.0)
  }
  def keepGeneratedSamples = true
  override def _registerSample(o:O)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
    _generatedSamples += o
    if (d != null) d += GenerativeDistributionRegisterDiff(o)
  }
  override def _unregisterSample(o:O)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    _generatedSamples -= o
    if (d != null) d += GenerativeDistributionUnregisterDiff(o)
  }
  /** Notify this GenerativeDistribution that it is now associated with an additional sampled outcome, and set o's source to this. */
  final override def registerSample(o:O)(implicit d:DiffList): Unit = {
    _registerSample(o)
    o._setSource(this)
  }
  /** Notify this GenerativeDistribution that it is no longer associated with a sampled outcome, and set o's source to null. */
  final override def unregisterSample(o:O)(implicit d:DiffList): Unit = {
    _unregisterSample(o)
    o._setSource(null)
  }
  // TODO consider removing preChange/postChange, because it requires extra infrastructure/Diffs from implementers?  Can just use (un)registerSample
  /** Notify this GenerativeDistribution that the value of its associated outcome 'o' is about to change.  
      Calls to this method are always paired with (followed by) a call to postChange. */
  override def preChange(o:O)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  override def postChange(o:O)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  /** Return the probability that this GenerativeDistribution would generate outcome 'o' */
  def pr(o:O): Double
  /** Return the log-probability that this GenerativeDistribution would generate outcome 'o' */
  override def logpr(o:O): Double = Math.log(pr(o))
  case class GenerativeDistributionRegisterDiff(m:O) extends Diff {
    def variable: GenerativeDistribution[O] = GenerativeDistribution.this//.asInstanceOf[VariableType]
    def redo = { if (_generatedSamples.contains(m)) throw new Error else _generatedSamples += m}
    def undo = { _generatedSamples -= m }
  }
  case class GenerativeDistributionUnregisterDiff(m:O) extends Diff {
    def variable: GenerativeDistribution[O] = GenerativeDistribution.this //.asInstanceOf[VariableType]
    def redo = { _generatedSamples -= m }
    def undo = { if (_generatedSamples.contains(m)) throw new Error else _generatedSamples += m}
  }
}

// TODO Consider something like this?  Would be needed in "def sample"
//trait RealParameter { val children ... }




// Some specific cases of GenerativeDistribution types
// TODO Rename OrdinalDistribution, DiscreteDistribution, CategoricalDistribution, ProportionDistribution, RealDistribution, etc.

/** A GenerativeDistribution that generates discrete (Int) outcomes (perhaps even a OrdinalOutcome), for example a Poisson. */
trait OrdinalDistributionLike[O<:OrdinalValue] extends GenerativeDistributionLike[OrdinalDistribution[O],O] {
  //def sampleIndex: Int // TODO Rename sampleInt or sampleIntValue?  And likewise below?
  //def pr(index:Int): Double
  //def logpr(index:Int): Double
}

/** A GenerativeDistribution that generates discrete (Int) outcomes (perhaps even a OrdinalOutcome), for example a Poisson. */
trait OrdinalDistribution[O<:OrdinalValue] extends /*GenerativeDistributionLike[OrdinalDistribution[O],O]*/ OrdinalDistributionLike[O] with GenerativeDistribution[O] {
  def sampleIndex: Int // TODO Rename sampleInt or sampleIntValue?  And likewise below?
  def pr(index:Int): Double
  def logpr(index:Int): Double
}


/** A GenerativeDistribution that generates DiscreteValue outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
trait DiscreteDistributionLike[O<:DiscreteValue] extends OrdinalDistributionLike[O] with GenerativeDistributionLike[DiscreteDistribution[O],O] {
  //def maxPrIndex: Int
  //def proportion: RandomAccessSeq[Double]
}

/** A GenerativeDistribution that generates DiscreteValue outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
trait DiscreteDistribution[O<:DiscreteValue] extends GenerativeDistributionLike[DiscreteDistribution[O],O] with OrdinalDistribution[O] {
  def maxPrIndex: Int
  def proportion: RandomAccessSeq[Double]
}

/** A GenerativeDistribution that generates categorical outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
// TODO This should simply inherit from DiscreteGenerating
/*trait CategoricalGenerating[O<:CategoricalValue] extends GenerativeDistribution[O] with DiscreteGenerating[O] {
  def sampleIndex: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
}*/


  /** A GenerativeDistribution that generates proportions (vectors with values summing to 1.0), for example a Dirichlet*/
trait ProportionDistributionLike[O<:DiscreteValue] extends GenerativeDistributionLike[ProportionDistribution[O],GeneratedProportionValue[O]] {
  //def size: Int
  ////def sampleProportions: Seq[Double]
  //def sampleProportionsWithCounts(counts:{def apply(i:Int):Double; def size:Int}): Seq[Double]
  //def pr(proportions:ProportionOutcome[O]): Double
}

/** A GenerativeDistribution that generates proportions (vectors with values summing to 1.0), for example a Dirichlet*/
trait ProportionDistribution[O<:DiscreteValue] extends ProportionDistributionLike[O] with GenerativeDistribution[GeneratedProportionValue[O]] {
  def size: Int
  //def sampleProportions: Seq[Double]
  def sampleProportionsWithCounts(counts:{def apply(i:Int):Double; def size:Int}): Seq[Double]
  def pr(proportions:GeneratedProportionValue[O]): Double
}


/** A GenerativeDistribution that generates real values as Double (specifically a cc.factorie.Real), for example a Gaussian distribution */
// TODO Consider changing "Real" to "RealValue" and "RealVariable"
trait RealDistributionLike[O<:RealValue] extends GenerativeDistributionLike[RealDistribution[O],O] {
  //def sampleDouble: Double
}

trait RealDistribution[O<:RealValue] extends RealDistributionLike[O] with GenerativeDistribution[O] {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}

/** A GenerativeDistribution that generates positive real values as Double (represented by a cc.factorie.Real), for example a Gamma distribution */
//trait PositiveRealGenerating[O<:Real] extends RealGenerating[O]






