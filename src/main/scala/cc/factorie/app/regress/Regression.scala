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
package cc.factorie.app.regress

import cc.factorie._
import cc.factorie.la._
import cc.factorie.model.{Parameters, Weights1, Weights2, WeightsSet}
import cc.factorie.optimize._
import cc.factorie.variable.TensorVar

// Infrastructure for regression.  The architecture is somewhat parallel to app.classify. 

// Explanatory variables are not listed here because we don't know how many there are, or what their types are.  These may be provided in subclasses.
/** Container for the result of running a Regressor. */
trait Regression[A<:TensorVar] {
  def dependant: A
  def dependantValue: A#Value
  def regressor: Regressor[A]
}

/** Perform regression, with output of type A.  Input type is not specified here.
 0   @author Andrew McCallum */
trait Regressor[A<:TensorVar] {
  //def regressionValue(x:A#Value): Tensor // May not be possible if a Model.factors(x) is needed. 

  def regression(x:A): Regression[A]
  def regressions(xs:Iterable[A]): Seq[Regression[A]] = xs.toSeq.map(label => regression(label))
  //def regressionValues(xs:Iterable[A#Value]): Seq[Tensor] = xs.toSeq.map(x => regressionValue(x))
  /** Set the RealVariable to regressor-predicted value and return a Regression object summarizing the outcome. */

  def regress(x:A): Regression[A] = { val r = regression(x); /*r.globalize(null);*/ r }

 def regress(xs:Iterable[A]): Seq[Regression[A]] = { val r = regressions(xs); /*r.foreach(_.globalize(null));*/ r }
}

class LinearRegressor[E<:TensorVar,A<:TensorVar](val dependant2Explanatory: A=>E, val weights: Tensor2) extends Regressor[A]{
  linearRegressor =>

  def regression(x: A) = {
    val result = weights.leftMultiply(dependant2Explanatory(x).value.asInstanceOf[Tensor1]).reshape(x.value.dimensions)
    new Regression[A] {
      def dependant = x
      def dependantValue = result.asInstanceOf[A#Value]
      def regressor = linearRegressor
    }
  }
}

trait MultivariateModel extends Parameters with app.classify.backend.OptimizablePredictor[Tensor1,Tensor1] {
  val weights: Weights2
  def predict(feats: Tensor1): Tensor1 = weights.value.leftMultiply(feats)
  def accumulateObjectiveGradient(accumulator: WeightsMapAccumulator, features: Tensor1, gradient: Tensor1, weight: Double) = accumulator.accumulate(weights, features outer gradient, weight)
}

trait UnivariateModel extends Parameters {
  val weights: Weights1
  def predict(feats: Tensor1): Double = weights.value dot feats
}

class LinearRegressionModel(nFeatures: Int, nLabel: Int) extends MultivariateModel {
  val weights = Weights(new DenseTensor2(nFeatures, nLabel))
}

object LinearRegressionTrainer {
  // Assumes variables are set to their target values
  def train[E <: TensorVar, A <: TensorVar](
    examples: Iterable[A], dependant2Explanatory: A => E, l2: Double,
    objective: MultivariateOptimizableObjective[Tensor1] = OptimizableObjectives.squaredMultivariate): LinearRegressor[E, A] = {
    val optimizer = new cc.factorie.optimize.LBFGS() with cc.factorie.optimize.L2Regularization
    optimizer.variance = 1.0/l2
    val trainer: WeightsSet => Trainer = m => new BatchTrainer(m, optimizer)
    trainCustom(examples, dependant2Explanatory, trainer, objective)
  }

  def trainCustom[E <: TensorVar, A <: TensorVar](
    examples: Iterable[A], dependant2Explanatory: A => E,
    trainerMaker: WeightsSet => Trainer,
    objective: MultivariateOptimizableObjective[Tensor1] = OptimizableObjectives.squaredMultivariate): LinearRegressor[E, A] = {
    val exampleDependent = examples.head
    val exampleExplanatory = dependant2Explanatory(exampleDependent)
    val dependentSize = exampleDependent.value.dimensions.product
    val explanatorySize = exampleExplanatory.value.dimensions.product
    val model = new LinearRegressionModel(explanatorySize, dependentSize)
    val trainer = trainerMaker(model.parameters)
    val trainingExamples = examples.map(e => new PredictorExample(model, dependant2Explanatory(e).value.asInstanceOf[Tensor1], e.value.asInstanceOf[Tensor1], objective))
    while (!trainer.isConverged) {
      trainer.processExamples(trainingExamples)
    }
    new LinearRegressor(dependant2Explanatory, model.weights.value)
  }
}

