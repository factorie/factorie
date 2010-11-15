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
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}

// TODO This is over variables.  We want something over Factors... and perhaps also something separate over Variables
class SamplingLattice[V<:DiscreteVars](variables:Iterable[V]) extends Lattice[V] {
  type VariableMarginalType = DiscreteMarginal[V]
  val map = new HashMap[V,DiscreteMarginal[V]]
  variables.foreach(v => map(v) = new DiscreteMarginal(v))
  override def marginal(v:V) = map.get(v)
  override def marginal(f:Factor) = throw new Error("Not yet implemented") // TODO
  def apply(v:V) = map(v)
  def marginals: Iterator[DiscreteMarginal[V]] = map.valuesIterator
}

// A simple special case, to be generalized later
class SamplingInferencer[V<:DiscreteVar,C](val sampler:Sampler[C]) extends Inferencer[V,C] {
  type LatticeType = SamplingLattice[V]
  var burnIn = 100 // I really want these to be  the default-values for parameters to infer, in Scala 2.8.
  var thinning = 20
  var iterations = 500
  def infer(targets:Iterable[V], contexts:Iterable[C]): SamplingLattice[V] = {
    val lat = new SamplingLattice(targets)
    sampler.processAll(contexts, burnIn)
    for (i <- 0 until iterations/thinning) {
      sampler.processAll(contexts, thinning)
      targets.foreach(v => lat.marginal(v).get.incrementCurrentValue)
      //postSample(targets)
    }
    lat
  }
}

class VariableSamplingInferencer[V<:DiscreteVariable](sampler:Sampler[V]) extends SamplingInferencer[V,V](sampler) with VariableInferencer[V] {
  //def this() = this(new GibbsSampler)
  //def this(model:Model) = this(new GibbsSampler(model))
}


// Max-Product inference, finding the best scoring configuration

/** The result of inference by a SamplingMaximizer.  
    'diff' is the list the changes from the initial configuration.  It may be null if no DiffList is provided to 'infer'.
    'diffScore' is the relative change in model score from the initial configuration. */
class SamplingMaximizerLattice[V<:Variable](val diff:DiffList, val diffScore:Double) extends Lattice[V] {
  type VariableMarginalType = Marginal
  type FactorMarginalType = Marginal
  override def marginal(v:V): Option[Marginal] = throw new Error // TODO
  override def marginal(f:Factor): Option[Marginal] = throw new Error // TODO
}

/** Provide 'infer' method that uses the 'sampler' to search for the best-scoring configuration.
     @author Andrew McCallum
    @since 0.8
 */
// TODO Update this for the new separated "modelScore" and "acceptScore" in Proposal.
class SamplingMaximizer[V<:Variable with IterableSettings](val sampler:ProposalSampler[V]) extends VariableInferencer[V] {
  def this(model:Model) = this(new VariableSettingsSampler[V](model))
  type LatticeType = SamplingMaximizerLattice[V]
  var iterations = 50 // TODO What should these be by default?
  var rounds = 3
  var initialTemperature = 1.0
  var finalTemperature = 0.01
  def infer(variables:Iterable[V], numIterations:Int): LatticeType = {
    val origIterations = iterations; iterations = numIterations
    val result = inferd(variables, variables)(null)
    iterations = origIterations
    result
  }
  def infer(variables:Iterable[V], varying:Iterable[V]): LatticeType = inferd(variables, varying)(null)
  // TODO I really want Scala 2.8 default parameters: (implicit diff:DiffList = null)  !!!
  def inferd(variables:Iterable[V], varying:Iterable[V])(implicit diff:DiffList): LatticeType = {
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
      val iterationsNow = math.min(iterationsPerRound, iterationsRemaining)
      sampler.processAll(varying, iterationsNow)
      iterationsRemaining -= iterationsNow
      sampler.temperature += (finalTemperature-initialTemperature)/rounds // Adding a negative number
      //println("Reducing temperature to "+sampler.temperature)
    }
    maxdiff.undo // Go back to maximum scoring configuration so we return having changed the config to the best
    sampler.proposalHooks -= updateHook // Remove our temporary hook
    sampler.temperature = origSamplerTemperature // Put back the sampler's temperature where we found it
    new SamplingMaximizerLattice[V](diff, maxScore)
  }
}
