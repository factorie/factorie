package cc.factorie.optimize
import cc.factorie.la._

/**
 * Abstract trait for any (sub)differentiable objective function used to train predictors.
 * More accurately, this defines a family of objective functions indexed by each possible output.
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

  class HingeScaledBinary(posSlackRescale: Double = 1.0, negSlackRescale: Double = 1.0) extends UnivariateOptimizableObjective[Int] {
    def valueAndGradient(prediction: Double, label: Int): (Double, Double) = {
      val slackRescale = if (label == 1.0) negSlackRescale else posSlackRescale
      if (prediction * label < 1.0)
        (prediction * label * slackRescale - 1.0, label * slackRescale)
      else
        (0.0, 0.0)
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
   * @param posSlackRescale The cost of predicting positive when the label is negative
   * @param negSlackRescale The cost of predicting negative when the label is positive
   * @return An objective function
   */
  def hingeScaledBinary(posSlackRescale: Double = 1.0, negSlackRescale: Double = 1.0) = new HingeScaledBinary(posSlackRescale, negSlackRescale)

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