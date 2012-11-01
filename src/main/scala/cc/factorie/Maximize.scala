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
import cc.factorie.generative._
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}

/** An inference engine that finds score-maximizing values.  
    The "infer" method returns a summary holding the maximizing assignment, but does not change the current variable values.
    By convention, subclass-implemented "apply" methods should change the current variable values to those that maximize;
    this convention differs from other Infer instances, which do not typically change variable values.  */
trait Maximize extends Infer {
  def maximize(vs:Iterable[Variable], model:Model, summary:Summary[Marginal] = null) = infer(vs, model, summary).get.setToMaximize(null)
  // TODO Consider adding the following
  //def twoBest(vs:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): (Summary[Marginal], Summary[Marginal])
}



/* A suite containing various recipes to maximize the value of variables to maximize some objective, 
   usually maximum likelihood. 
   @author Andrew McCallum */
class MaximizeSuite extends Maximize {
  def defaultSuite = Seq(
    MaximizeGeneratedDiscrete, MaximizeGate, MaximizeProportions,
    cc.factorie.generative.MaximizeGaussianMean, cc.factorie.generative.MaximizeGaussianVariance, MaximizeByBPChain
//    ,    cc.factorie.generative.MaximizeMultivariateGaussianMean,
//    cc.factorie.generative.MaximizeMultivariateGaussianCovariance
  )
  val suite = new scala.collection.mutable.ArrayBuffer[Maximize]
  suite ++= defaultSuite
  //def infer(variables:Iterable[Variable], model:Model): Option[Summary[Marginal]] = None
  override def infer(varying:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = {
    // The handlers can be assured that the Seq[Factor] will be sorted alphabetically by class name
    // This next line does the maximization
    var option: Option[Summary[Marginal]] = None
    val iterator = suite.iterator
    while (option == None && iterator.hasNext) {
      option = iterator.next().infer(varying, model, summary)
    }
    option
  }
  def apply(varying:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Summary[Marginal] = {
    val option = infer(varying, model, summary)
    if (option == None) throw new Error("No maximizer found for factors "+model.factors(varying).take(10).map(_ match { case f:Family#Factor => f.family.getClass.getName; case f:Factor => f.getClass.getName }).mkString(" "))
    option.get.setToMaximize(null)
    option.get
  }
}
object Maximize extends MaximizeSuite // A default instance of this class


//trait Maximizer[C] {
//  def maximize(c:C)
//}

object SamplingMaximizer {
  def apply[V <: Variable with IterableSettings](model: Model) = new SamplingMaximizer[V](new VariableSettingsSampler[V](model))
}

class SamplingMaximizer[C](val sampler:ProposalSampler[C]) {
  def maximize(varying:Iterable[C], iterations:Int): Iterable[Variable] = {
    var currentScore = 0.0
    var maxScore = currentScore
    val maxdiff = new DiffList
    val origSamplerTemperature = sampler.temperature
    val variablesTouched = new HashSet[Variable]
    def updateMaxScore(p:Proposal): Unit = {
      currentScore += p.modelScore // TODO Check proper handling of fbRatio
      //println("SamplingMaximizer modelScore="+p.modelScore+" currentScore="+currentScore)
      variablesTouched ++= p.diff.map(_.variable)
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
    sampler.proposalHooks += updateHook // Add temporary hook
    sampler.processAll(varying, iterations)
    sampler.proposalHooks -= updateHook // Remove our temporary hook
    sampler.temperature = origSamplerTemperature // Put back the sampler's temperature where we found it
    maxdiff.undo // Go back to maximum scoring configuration so we return having changed the config to the best
    variablesTouched
  }
  def maximize(varying:Iterable[C], iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5): Iterable[Variable] = {
    //sampler.proposalsHooks += { (props:Seq[Proposal]) => { props.foreach(p => println(p.modelScore)) }}
    val iterationsPerRound = if (iterations < rounds) 1 else iterations/rounds
    var iterationsRemaining = iterations
    if (iterationsRemaining == 1) sampler.temperature = finalTemperature
    val variablesTouched = new HashSet[Variable]
    sampler.temperature = initialTemperature
    while (iterationsRemaining > 0) {
      val iterationsNow = math.min(iterationsPerRound, iterationsRemaining)
      variablesTouched ++= maximize(varying, iterationsNow)
      iterationsRemaining -= iterationsNow
      sampler.temperature += (finalTemperature-initialTemperature)/rounds // Adding a negative number
      //println("Reducing temperature to "+sampler.temperature)
    }
    variablesTouched
    //new SamplingMaximizerLattice[V](diff, maxScore)
  }
  def apply(varying:Iterable[C], iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5): AssignmentSummary = {
    new AssignmentSummary(new HashMapAssignment(maximize(varying, iterations, initialTemperature, finalTemperature, rounds)))
  }
}
