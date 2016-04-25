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

package cc.factorie.infer

import cc.factorie.directed.{MaximizeGate, MaximizeGaussianMean, MaximizeGaussianVariance, MaximizeGeneratedDiscrete, _}
import cc.factorie.model.Model
import cc.factorie.variable._

import scala.collection.mutable.HashSet

/** An inference engine that finds score-maximizing values.  
    The "infer" method returns a summary holding the maximizing assignment, but does not change the current variable values.
    By convention, subclass-implemented "apply" methods should change the current variable values to those that maximize;
    this convention differs from other Infer instances, which do not typically change variable values.  */
trait Maximize[-A<:Iterable[Var],-B<:Model] extends Infer[A,B] {
  def maximize(vs:A, model:B, marginalizing:Summary = null): Unit = infer(vs, model, marginalizing).setToMaximize(null)
  //def maximize(vs:A, model:B): Unit = maximize(vs, model, null)
  // TODO Consider adding the following
  //def twoBest(vs:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): (Summary[Marginal], Summary[Marginal])
}

//trait MaximizeMarginalizing[-A<:Iterable[Var],-B<:Model,-C<:Summary] extends InferMarginalizing[A,B,C] {
//  def maximize(vs:A, model:B, marginalizing:C) = infer(vs, model, marginalizing).setToMaximize(null)
//  // TODO Consider adding the following
//  //def twoBest(vs:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): (Summary[Marginal], Summary[Marginal])
//}



/* A suite containing various recipes to maximize the value of variables to maximize some objective, 
   usually maximum likelihood.  The suite tries each recipe in turn until it finds one that succeeds.
   @author Andrew McCallum */
class MaximizeSuite extends Maximize[Iterable[Var],Model] {
  def defaultSuite: Seq[Maximize[Iterable[Var],Model]] =
    Seq(MaximizeGeneratedDiscrete.asInstanceOf[Maximize[Iterable[Var],Model]],
      MaximizeGate.asInstanceOf[Maximize[Iterable[Var],Model]],
      MaximizeProportions.asInstanceOf[Maximize[Iterable[Var],Model]],
      MaximizeGaussianMean.asInstanceOf[Maximize[Iterable[Var],Model]],
      //MaximizeGaussianMeansNoSummary,
      MaximizeGaussianVariance.asInstanceOf[Maximize[Iterable[Var],Model]],
      MaximizeByBPChain.asInstanceOf[Maximize[Iterable[Var],Model]])
  val suite = new scala.collection.mutable.ArrayBuffer[Maximize[Iterable[Var],Model]]

  suite ++= defaultSuite
  //def infer(variables:Iterable[Variable], model:Model): Option[Summary[Marginal]] = None
  def infer(varying:Iterable[Var], model:Model, marginalizing:Summary): Summary = {
    // The handlers can be assured that the Seq[Factor] will be sorted alphabetically by class name
    // This next line does the maximization
    var summary = null.asInstanceOf[Summary]
    val iterator = suite.iterator
    while ((summary eq null) && iterator.hasNext) {
      try {
        summary = iterator.next().infer(varying, model)
      } catch {
        case e: ClassCastException => ()
      }
    }
    summary
  }
  def apply(varying:Iterable[Var], model:Model): Summary = {
    val summary = infer(varying, model)
    if (summary eq null) throw new Error("No maximizing method found.")
    summary.setToMaximize(null)
    summary
  }
  // A convenient pretty interface, especially for tutorials and beginner users
  def apply(varying:Var)(implicit model:DirectedModel): Summary = apply(Seq(varying), model)
  //def apply(varying:Any*)(implicit model:DirectedModel): Summary = apply(varying, model)
}
object Maximize extends MaximizeSuite // A default instance of this class


//trait Maximizer[C] {
//  def maximize(c:C)
//}

object SamplingMaximizer {
  def apply[V <: Var with IterableSettings](model: Model)(implicit random: scala.util.Random) = new SamplingMaximizer[V](new VariableSettingsSampler[V](model))
}

class SamplingMaximizer[C](val sampler:ProposalSampler[C]) {
  def maximize(varying:Iterable[C], iterations:Int): Iterable[Var] = {
    var currentScore = 0.0
    var maxScore = currentScore
    val maxdiff = new DiffList
    val origSamplerTemperature = sampler.temperature
    val variablesTouched = new HashSet[Var]
    def updateMaxScore(p:Proposal[C]): Unit = {
      currentScore += p.modelScore // TODO Check proper handling of fbRatio
      //println("SamplingMaximizer modelScore="+p.modelScore+" currentScore="+currentScore)
      variablesTouched ++= p.diff.map(_.variable)
      if (currentScore > maxScore) {
        maxScore = currentScore
        maxdiff.clear()
        //println("SamplingMaximizer maxScore="+maxScore)
      } else if (p.diff.size > 0) {
        maxdiff appendAll p.diff
        //println("SamplingMaximizer diff.size="+diff.size)
      }
    }
    val updateHook: Proposal[C]=>Unit = updateMaxScore _
    sampler.proposalHooks += updateHook // Add temporary hook
    sampler.processAll(varying, iterations)
    sampler.proposalHooks -= updateHook // Remove our temporary hook
    sampler.temperature = origSamplerTemperature // Put back the sampler's temperature where we found it
    maxdiff.undo() // Go back to maximum scoring configuration so we return having changed the config to the best
    variablesTouched
  }
  def maximize(varying:Iterable[C], iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5): Iterable[Var] = {
    //sampler.proposalsHooks += { (props:Seq[Proposal]) => { props.foreach(p => println(p.modelScore)) }}
    val iterationsPerRound = if (iterations < rounds) 1 else iterations/rounds
    var iterationsRemaining = iterations
    if (iterationsRemaining == 1) sampler.temperature = finalTemperature
    val variablesTouched = new HashSet[Var]
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
