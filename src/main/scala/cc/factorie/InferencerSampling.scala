/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie.util.Implicits._

// Note that putting a [V], as in DenseCountsMultinomial[V], doesn't work here because CategoricalValues not <: MultinomialOutcome[V].  
// But as long as we don't use any methods that require [V], I think we are OK.
class DiscreteMarginal[V<:DiscreteValues](val variable:V) extends DenseCountsMultinomial(variable.domain.size) with Marginal {
  override def keepGeneratedSamples = false
  def incrementCurrentValue : Unit = variable match {
    case v:DiscreteValue => increment(v.index, 1.0)(null)
    case v:BinaryVectorVariable[_] => { for (index <- v.indices) increment(index, 1.0)(null) } // throw new Error // TODO Put this code back in: v.incrementInto(this)
  }
}

// TODO This is over variables.  We want something over Factors... and perhaps also something separate over Variables
class SamplingLattice[V<:CategoricalValues](variables:Collection[V]) extends Lattice {
  val map = new HashMap[V,DiscreteMarginal[V]]
  variables.foreach(v => map(v) = new DiscreteMarginal(v))
  def marginal(v:V) = map(v)
  def apply(v:V) = map(v)
  def marginals: Iterator[DiscreteMarginal[V]] = map.valuesIterator
}

// A simple special case, to be generalized later
// TODO Could be "DiscreteVariable" instead of "CategoricalVariable"?
class SamplingInferencer[V<:CategoricalVariable,C](val sampler:Sampler[C]) extends Inferencer[V,C] {
  type LatticeType = SamplingLattice[V]
  var burnIn = 100 // I really want these to be  the default-values for parameters to infer, in Scala 2.8.
  var thinning = 20
  var iterations = 500
  def infer(targets:Collection[V], contexts:Collection[C]): SamplingLattice[V] = {
    val lat = new SamplingLattice(targets)
    sampler.process(contexts, burnIn)
    for (i <- 0 until iterations/thinning) {
      sampler.process(contexts, thinning)
      targets.foreach(v => lat.marginal(v).incrementCurrentValue)
    }
    lat
  }
}

class VariableSamplingInferencer[V<:CategoricalVariable](sampler:Sampler[V]) extends SamplingInferencer[V,V](sampler) with VariableInferencer[V] {
  def this() = this(new GibbsSampler1[V])
  def this(model:Model) = this(new GibbsSampler1[V](model))
}


// Max-Product inference, finding the best scoring configuration

/** The result of inference by a SamplingMaximizer.  
    'diff' is the list the changes from the initial configuration.  It may be null if no DiffList is provided to 'infer'.
    'diffScore' is the relative change in model score from the initial configuration. */
class SamplingMaximizerLattice(val diff:DiffList, val diffScore:Double) extends Lattice

/** Provide 'infer' method that uses the 'sampler' to search for the best-scoring configuration.
 
    @author Andrew McCallum
    @since 0.8
 */
// TODO Update this for the new separated "modelScore" and "acceptScore" in Proposal.
class SamplingMaximizer[V<:Variable with IterableSettings](val sampler:ProposalSampler[V]) extends Maximizer[V] with VariableInferencer[V] {
  def this(model:Model) = this(new GibbsSampler1[V](model))
  type LatticeType = SamplingMaximizerLattice
  var iterations = 50 // TODO What should these be by default?
  var rounds = 3
  var initialTemperature = 1.0
  var finalTemperature = 0.01
  def infer(variables:Collection[V], numIterations:Int): LatticeType = {
    val origIterations = iterations; iterations = numIterations
    val result = inferd(variables, variables)(null)
    iterations = origIterations
    result
  }
  def infer(variables:Collection[V], varying:Collection[V]): LatticeType = inferd(variables, varying)(null)
  // TODO I really want Scala 2.8 default parameters: (implicit diff:DiffList = null)  !!!
  def inferd(variables:Collection[V], varying:Collection[V])(implicit diff:DiffList): LatticeType = {
    var currentScore = 0.0
    var maxScore = currentScore
    val maxdiff = new DiffList
    val origSamplerTemperature = sampler.temperature
    sampler.temperature = initialTemperature
    def updateMaxScore(p:Proposal): Unit = {
      currentScore += p.modelScore // TODO Check proper handling of fbRatio
      //println("SamplingMaximizer modelScore="+p.modelScore+" currentScore="+currentScore)
      if (diff != null) diff appendAll p.diff
      if (currentScore > maxScore) {
        maxScore = currentScore
        maxdiff.clear
        //println("SamplingMaximizer maxScore="+maxScore)
      } else if (p.diff.size > 0) {
        maxdiff appendAll p.diff
        //println("SamplingMaximizer diff.size="+diff.size)
      }
    }
    val updateHook: Proposal=>Unit = updateMaxScore _ 
    sampler.proposalHooks += updateHook 
    //sampler.proposalsHooks += { (props:Seq[Proposal]) => { props.foreach(p => println(p.modelScore)) }}
    val iterationsPerRound = if (iterations < rounds) 1 else iterations/rounds
    var iterationsRemaining = iterations
    if (iterationsRemaining == 1) sampler.temperature = finalTemperature
    while (iterationsRemaining > 0) {
      val iterationsNow = Math.min(iterationsPerRound, iterationsRemaining)
      sampler.process(varying, iterationsNow)
      iterationsRemaining -= iterationsNow
      sampler.temperature += (finalTemperature-initialTemperature)/rounds // Adding a negative number
      //println("Reducing temperature to "+sampler.temperature)
    }
    maxdiff.undo // Go back to maximum scoring configuration so we return having changed the config to the best
    sampler.proposalHooks -= updateHook // Remove our temporary hook
    sampler.temperature = origSamplerTemperature // Put back the sampler's temperature where we found it
    new SamplingMaximizerLattice(diff, maxScore)
  }
}
