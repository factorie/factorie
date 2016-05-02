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

import cc.factorie.la.{SparseIndexedTensor1, Tensor}
import cc.factorie.model.{CombinedModel, Factor1, ItemizedModel, Model}
import cc.factorie.variable.{DiffList, DiscreteVar, HashMapAssignment}

/**
 * User: apassos
 * Date: 6/15/13
 * Time: 7:38 AM
 */

trait WarmStartWeightedSummary {
  def summary: Summary
  def infer(): Unit
  def incrementWeights(v: DiscreteVar, t: Tensor, d: Double)
}

case class ModelWithInference[M,V](vars: V, model: M)(implicit val infer: (V,M) => WarmStartWeightedSummary) {
  def summary = infer(vars, model)
}

class DualDecomposition(stepSize: (Int,Int) => Double = DualDecomposition.LearningRates.expDualIncrease(1.0, 0.9)) extends /*Infer[Seq[WarmStartWeightedSummary], Seq[(Int,DiscreteVar,Int,DiscreteVar)]] with*/ cc.factorie.util.GlobalLogging {
  def infer(summaries: Seq[WarmStartWeightedSummary], constraints: Seq[(Int, DiscreteVar, Int, DiscreteVar)]): MAPSummary = {
    var dual = summaries.map(_.summary.logZ).sum
    var updated = true
    var dualIncreases = 1
    var iterations = 1
    while (updated) {
      summaries.map(_.infer()) // summaries are responsible for caching inference results if they did not change
      val newDual = summaries.map(_.summary.logZ).sum
      if (newDual > dual) dualIncreases += 1
      dual = newDual
      logger.info(s"Dual: $dual}")
      updated = false
      for ((i1, v1, i2, v2) <- constraints) {
        val discreteMarginal1 = summaries(i1).summary.marginal(v1).asInstanceOf[DiscreteMarginal]
        val discreteMarginal2 = summaries(i2).summary.marginal(v2).asInstanceOf[DiscreteMarginal]
        if (discreteMarginal1.proportions.maxIndex != discreteMarginal2.proportions.maxIndex) {
          updated = true
          val t = new SparseIndexedTensor1(discreteMarginal1.proportions.length)
          val rate = 1.0/(1 + dualIncreases)
          t += (discreteMarginal1.proportions.maxIndex, -rate)
          t += (discreteMarginal2.proportions.maxIndex, rate)
          val step = stepSize(iterations, dualIncreases)
          summaries(i1).incrementWeights(v1, t, step)
          summaries(i2).incrementWeights(v2, t, -step)
        }
      }
      iterations += 1
    }
    val as = new HashMapAssignment(ignoreNonPresent=false)
    for (summary <- summaries) {
      implicit val d = new DiffList
      summary.summary.setToMaximize(d)
      for (diff <- d; v = diff.variable)
        as.update(v, v.value)
      d.undo()
    }
    new MAPSummary(as, summaries.flatMap(_.summary.factorMarginals.map(_.factor)).distinct)
  }
}

object DualDecomposition {
  class WeightedSummaryWithBP(vars: Iterable[DiscreteVar], model: Model, baseInfer: MaximizeByBP) extends WarmStartWeightedSummary {
    val weightedModel = new ItemizedModel
    val combinedModel = new CombinedModel(model, weightedModel)
    var summary = baseInfer.infer(vars, combinedModel)
    def infer() { summary = baseInfer.infer(vars, combinedModel)}
    case class WeightedFactor(var v: DiscreteVar, var t: Tensor, var d: Double) extends Factor1[DiscreteVar](v) {
      def score(v1: DiscreteVar#Value) = d*(v1 dot t)
      override def valuesScore(v1: Tensor) = d*(v1 dot t)
      override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    }
    val factors = collection.mutable.ArrayBuffer[WeightedFactor]()
    def incrementWeights(v: DiscreteVar, t: Tensor, d: Double) {
      factors += new WeightedFactor(v, t, d)
      weightedModel += factors.last
    }
  }
  def getBPInferChain(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPChain)
  def getBPInferTree(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPTree)
  def getBPInferLoopy(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPLoopy)

  object LearningRates {
    def expDualIncrease(eta: Double, c: Double) = (iteration: Int, dualIncreases: Int) => eta*math.pow(c, -dualIncreases)
    def expT(eta: Double, c: Double) = (iteration: Int, dualIncreases: Int) => eta*math.pow(c, -iteration)

    def invDualIncrease(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/dualIncreases
    def invT(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/iteration

    def invSqrtDualIncrease(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/math.sqrt(dualIncreases)
    def invSqrtT(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/math.sqrt(iteration)
  }
}

