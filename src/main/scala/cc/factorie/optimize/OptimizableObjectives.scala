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
package cc.factorie.optimize
import cc.factorie.la._

/**
 * Abstract trait for any (sub)differentiable objective function used to train predictors.
 * More accurately, this defines a family of objective functions indexed by each possible label.
 * Note that these are concave objective functions, not convex loss functions.
 * @tparam Prediction The type of the prediction: is it a tensor or a scalar?
 * @tparam Output The type of the output/label: is it integer or real-valued or tensor-valued?
 */
trait OptimizableObjective[Prediction, Output] {
  def valueAndGradient(prediction: Prediction, label: Output): (Double, Prediction)
}

trait UnivariateOptimizableObjective[Output] extends OptimizableObjective[Double, Output]
trait MultivariateOptimizableObjective[Output] extends OptimizableObjective[Tensor1, Output]

object OptimizableObjectives {
  /**
   * General type for multivariate linear objective functions for clasification
   */
  type Multiclass = MultivariateOptimizableObjective[Int]
  /**
   * General type for objective functions for binary classification
   */
  type Binary = UnivariateOptimizableObjective[Int]

  class HingeMulticlass extends MultivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val lossAugmented = { val c = prediction.copy; c -= (label, 1.0); c }
      val maxLabel = lossAugmented.maxIndex
      if (maxLabel == label)
        (0.0, new SparseIndexedTensor1(prediction.size))
      else {
        val grad = new SparseIndexedTensor1(prediction.size)
        grad(label) += 1.0
        grad(maxLabel) -= 1.0
        val value = lossAugmented(label) - lossAugmented(maxLabel)
        (value, grad)
      }
    }
  }

  class HingeSqMulticlass extends MultivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val lossAugmented = { val c = prediction.copy; c -= (label, 1.0); c }
      val maxLabel = lossAugmented.maxIndex
      if (maxLabel == label)
        (0.0, new SparseIndexedTensor1(prediction.size))
      else {
        val violation = lossAugmented(label) - lossAugmented(maxLabel)
        val grad = new SparseIndexedTensor1(prediction.size)
        grad(label) += 2 * -violation
        grad(maxLabel) += 2 * violation
        (-violation * violation, grad)
      }
    }
  }

  class LogMulticlass extends MultivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val normed = prediction.expNormalized.asInstanceOf[Tensor1]
      val loss = math.log(normed(label))
      normed *= -1
      normed(label) += 1.0
      (loss, normed)
    }
  }

  class SparseLogMulticlass extends MultivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Tensor1, label: Int): (Double, Tensor1) = {
      val normed = prediction.expNormalized.asInstanceOf[Tensor1]
      val loss = math.log(normed(label))
      normed *= -1
      normed(label) += 1.0
      val sparse = new SparseIndexedTensor1(normed.dim1)
      var i = 0
      while (i < normed.dim1) {
        if (math.abs(normed(i)) > 0.01)
          sparse += (i,normed(i))
        i += 1
      }
      (loss, sparse)
    }
  }

  class SquaredMultivariate extends MultivariateOptimizableObjective[Tensor1] {
    def valueAndGradient(prediction: Tensor1, label: Tensor1): (Double, Tensor1) = {
      for (i <- prediction.activeDomain)
        prediction(i) -= label(i)
      val value = -(prediction dot prediction)
      prediction *= -2
      (value, prediction)
    }
  }

  class EpsilonInsensitiveSqMultivariate(epsilon: Double) extends MultivariateOptimizableObjective[Tensor1] {
    def valueAndGradient(prediction: Tensor1, label: Tensor1): (Double, Tensor1) = {
      var objective = 0.0
      val gradient = new SparseIndexedTensor1(prediction.size)
      for (i <- prediction.activeDomain) {
        val diff = label(i) - prediction(i)
        val value = -math.max(0, math.abs(diff) - epsilon)
        objective -= value * value
        gradient += (i, if (value == 0.0) 0.0 else math.signum(diff) * -value)
      }
      (objective, gradient)
    }
  }

  class EpsilonInsensitiveAbsMultivariate(epsilon: Double) extends MultivariateOptimizableObjective[Tensor1] {
    def valueAndGradient(prediction: Tensor1, label: Tensor1): (Double, Tensor1) = {
      var objective = 0.0
      val gradient = new SparseBinaryTensor1(prediction.size)
      for (i <- prediction.activeDomain) {
        val diff = label(i) - prediction(i)
        val value = -math.max(0, math.abs(diff) - epsilon)
        objective -= value
        gradient += (i, if (value == 0.0) 0.0 else math.signum(diff))
      }
      (objective, gradient)
    }
  }

  class LogBinary extends UnivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) = {
      val probCorrect = 1.0 / (1 + math.exp(-label * prediction))
      (math.log(probCorrect), (1 - probCorrect) * label)
    }
  }

  class HingeBinary extends UnivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) =
      if (prediction * label < 1.0)
        (prediction * label - 1.0, label)
      else
        (0.0, 0.0)
  }

  class HingeScaledBinary(val posCost: Double = 1.0, val negCost: Double = 1.0) extends UnivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) = {
      val cost = if (label == 1.0) negCost else posCost
      if (prediction * label < 1.0)
        (prediction * label * cost - cost, label * cost)
      else
        (0.0, 0.0)
    }
  }
  
  class SmoothHingeBinary(val gamma: Double = 1.0, val margin: Double = 1.0, val posCost: Double = 1.0, val negCost: Double = 1.0)
    extends UnivariateOptimizableObjective[Int] {
    def valueAndGradient(score: Double, label: Int): (Double, Double) = {
      val prediction = score * label
      val cost = if (label == 1.0) negCost else posCost
      if (prediction >= margin)
        (0.0, 0.0)
      else if (prediction < margin - gamma) {
        val value = -cost * (margin - prediction - gamma / 2)
        val grad = label * cost
        (value, grad)
      } else {
        val value = -cost * (prediction - margin) * (prediction - margin) / (2 * gamma)
        val grad = -label * cost * (prediction - margin) / gamma
        (value, grad)
      }
    }
  }

  class SquaredUnivariate extends UnivariateOptimizableObjective[Double] {
    def valueAndGradient(prediction: Double, label: Double): (Double, Double) =
      (-0.5 * (prediction - label) * (prediction - label), label - prediction)
  }

  class AbsoluteUnivariate extends UnivariateOptimizableObjective[Double] {
    def valueAndGradient(prediction: Double, label: Double): (Double, Double) =
      (-math.abs(label - prediction), math.signum(label - prediction))
  }

  class EpsilonInsensitiveAbsUnivariate(epsilon: Double = 1.0) extends UnivariateOptimizableObjective[Double] {
    def valueAndGradient(prediction: Double, label: Double): (Double, Double) = {
      val value = -math.max(math.abs(label - prediction) - epsilon, 0)
      (value, if (value != 0.0) math.signum(label - prediction) else 0.0)
    }
  }

  class EpsilonInsensitiveSqUnivariate(epsilon: Double = 1.0) extends UnivariateOptimizableObjective[Double] {
    def valueAndGradient(prediction: Double, label: Double): (Double, Double) = {
      val value = -0.5 * math.pow(math.max(math.abs(label - prediction) - epsilon, 0), 2)
      (value, if (value != 0.0) label - prediction else 0.0)
    }
  }

  /**
   * Squared objective for multivariate regression
   */
  val squaredMultivariate = new SquaredMultivariate

  /**
   * Hinge objective for multiclass classification
   */
  val hingeMulticlass = new HingeMulticlass

  /**
   * Squared hinge objective for multiclass classification
   */
  val hingeSqMulticlass = new HingeSqMulticlass

  /**
   * Log objective for multiclass classification. Inefficient.
   */
  val logMulticlass = new LogMulticlass

  /**
   * Sparse Log objective for multiclass classification; very efficient.
   */
  val sparseLogMulticlass = new SparseLogMulticlass

  /**
   * Epsilon-insensitive squared objective for multivariate regression
   * @param epsilon The tolerance of the objective function
   * @return An objective function
   */
  def epsilonInsensitiveSqMultivariate(epsilon: Double) = new EpsilonInsensitiveSqMultivariate(epsilon)

  /**
   * Epsilon-insensitive squared objective for univariate regression
   * @param epsilon The tolerance of the objective function
   * @return An objective function
   */
  def epsilonInsensitiveSqUnivariate(epsilon: Double) = new EpsilonInsensitiveSqUnivariate(epsilon)

  /**
   * Epsilon-insensitive absolute objective for multivariate regression
   * @param epsilon The tolerance of the objective function
   * @return An objective function
   */
  def epsilonInsensitiveAbsMultivariate(epsilon: Double) = new EpsilonInsensitiveAbsMultivariate(epsilon)

  /**
   * Epsilon-insensitive absolute objective for univariate regression
   * @param epsilon The tolerance of the objective function
   * @return An objective function
   */
  def epsilonInsensitiveAbsUnivariate(epsilon: Double) = new EpsilonInsensitiveAbsUnivariate(epsilon)

  /**
   * A variant of the hinge objective for binary classification which can have different costs for type 1 and type 2 errors.
   * @param posCost The cost of predicting positive when the label is negative
   * @param negCost The cost of predicting negative when the label is positive
   * @return An objective function
   */
  def hingeScaledBinary(posCost: Double = 1.0, negCost: Double = 1.0) = new HingeScaledBinary(posCost, negCost)

  /**
   * A smoothed (Lipschitz gradient) variant of the hinge objective for binary classification which can have different costs for type 1 and type 2 errors and adjustable margin.
   * @param gamma Adjusts how smoothly the hinge drops down to zero. Higher is more smooth, zero gives unsmoothed hinge.
   * @param margin The number that you need to predict above to achieve the maximum objective score.
   * @param posCost The cost of predicting positive when the label is negative.
   * @param negCost The cost of predicting negative when the label is positive.
   * @return An objective function
   */
  def smoothHingeBinary(gamma: Double = 1.0, margin: Double = 1.0, posCost: Double = 1.0, negCost: Double = 1.0) = new SmoothHingeBinary(gamma, margin, posCost, negCost)
  
  /**
   * Log objective for binary classification
   */
  val logBinary = new LogBinary

  /**
   * Hinge objective for binary classification
   */
  val hingeBinary = new HingeBinary

  /**
   * Squared objective for univariate regression
   */
  val squaredUnivariate = new SquaredUnivariate

  /**
   * Absolute objective for univariate regression
   */
  val absoluteUnivariate = new AbsoluteUnivariate

  type UnivariateLinkFunction = Double => Double

  val squaredLossLinkFunction: UnivariateLinkFunction = prediction => prediction

  /**
   * The logistic sigmoid function.
   */
  val logisticLinkFunction: UnivariateLinkFunction = prediction => 1.0 / (1 + math.exp(-prediction))
}