package cc.factorie.optimize

import cc.factorie.la.{Tensor, SparseIndexedTensor, DenseTensor}
import cc.factorie.model.{WeightsMap, WeightsSet}

// TODO this should really store weights in log-space, and have an unnormalized version

/**
 * This implements the Exponentiated Gradient algorithm of Kivinen and Warmuth
 * - also known as Entropic Mirror Descent (Beck and Teboulle)
 * @param rate The base learning rate
 */
class ExponentiatedGradient(rate: Double = 1.0) extends GradientOptimizer {
  private var initialized = false

  def initializeWeights(weights: WeightsSet) = MutableScalableWeights.initializeWeights(weights)
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) {
      initializeWeights(weights)
      val len = weights.length * 1.0
      weights.tensors.foreach(t => t += Array.fill(t.length)(1 / len)) // need to initialize it to lie on the simplex
      initialized = true
    }
    var newWeightsSum = 1.0
    for (template <- gradient.keys)
      (weights(template), gradient(template)) match {
        case (weights: Tensor, gradient: DenseTensor) =>
          val gArr = gradient.asArray
          val len = gradient.length
          var i = 0
          while (i < len) {
            val oldWeight = weights(i)
            newWeightsSum -= oldWeight
            val expGrad = math.exp(rate * gArr(i))
            val newWeight = oldWeight * expGrad
            weights(i) = newWeight
            newWeightsSum += newWeight
            i += 1
          }
        case (weights: Tensor, gradient: SparseIndexedTensor) =>
          val len = gradient.activeDomainSize
          val indices = gradient._indices
          val values = gradient._values
          var i = 0
          while (i < len) {
            val idx = indices(i)
            val oldWeight = weights(idx)
            newWeightsSum -= oldWeight
            val expGrad = math.exp(rate * values(i))
            val newWeight = oldWeight * expGrad
            weights(idx) = newWeight
            newWeightsSum += newWeight
            i += 1
          }
      }
    weights *= (1 / newWeightsSum)
    println(weights.oneNorm)
  }
  def isConverged: Boolean = false
  def reset(): Unit = {
    initialized = false
  }
}
