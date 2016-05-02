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
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.util.{DenseDoubleSeq, DoubleSeq, RangeIntSeq}

/**
 * The FTRL-Proximal algorithm for regularized adaptive online gradient descent from McMahan, "A Unified View of Regularized Dual Averaging
 * and Mirror Descent with Implicit Updates", and McMahan et al., "Ad Click Prediction: a View From The Trenches"
 *
 * This works much like AdaGradRDA, but applies the l1 and l2 regularization to the dual, and the adaptive learning rate to the primal iterates.
 * This provides better accuracy in practice while preserving the sparsifying properties of dual-averaging based l1 regularization.
 *
 * @param beta beta is a ridge that is added to the "covariance matrix" of gradients that keeps learning rates from being too big initiall
 * @param alpha The initial learning rate
 * @param l1 The strength of l1 regularization
 * @param l2 The strength of l2 regularization
 * @param numExamples The number of examples for online training, used to scale regularizers
 */
class FTRLProximal(val beta: Double = 0.1, val alpha: Double = 0.1, val l1: Double = 0.0, val l2: Double = 0.0, val numExamples: Int = 1) extends GradientOptimizer {
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double) {
    if (!initialized) initializeWeights(weights)
    weights.+=(gradient)
  }
  def initializeWeights(weights: WeightsSet): Unit = {
    if (initialized) return
    for (key <- weights.keys) {
      key.value match {
        case t: FTRLProximalTensor => println("Warning: creating two FTRLProximal optimizers on the same tensors. Reusing old one...")
        case t: Tensor1 => weights(key) = new FTRLProximalTensor1(t, alpha, beta, l1 / numExamples, l2 / numExamples)
        case t: Tensor2 => weights(key) = new FTRLProximalTensor2(t, alpha, beta, l1 / numExamples, l2 / numExamples)
        case t: Tensor3 => weights(key) = new FTRLProximalTensor3(t, alpha, beta, l1 / numExamples, l2 / numExamples)
        case t: Tensor4 => weights(key) = new FTRLProximalTensor4(t, alpha, beta, l1 / numExamples, l2 / numExamples)
      }
    }
    initialized = true
  }

  def finalizeWeights(weights: WeightsSet): Unit =
    for (key <- weights.keys) {
      val scaledTensor = key.value
      weights(key) = key.newBlankTensor
      scaledTensor.foreachElement((i, v) => key.value(i) = v)
    }

  def reset() {}
  def isConverged = false

  private trait FTRLProximalTensor extends Tensor with DenseDoubleSeq {
    def activeDomain = new RangeIntSeq(0, length)
    val zs: Array[Double]
    val gradSquares: Array[Double]
    var t = 0
    val alpha: Double
    val beta: Double
    val l1: Double
    val l2: Double

    // can we use += here?
    def copyToDense[D <: DenseTensor](d: D): D = {
      var i = 0
      assert(length == d.length)
      while (i < length) {
        d(i) = apply(i)
        i += 1
      }
      d
    }

    override def update(i: Int, v: Double) { throw new Error("DualAveragingTensors can't be updated") }
    override def apply(i: Int): Double = {
      if (gradSquares(i) == 0.0) zs(i)
      else {
        val h = (1.0 / alpha) * (math.sqrt(gradSquares(i)) + beta) + t * l2
        val t1 = 1.0 / h
        t1 * ISTAHelper.truncate(zs(i), t * l1)
      }
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      t += 1
      ds match {
        case o: SparseTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          val values = o._valuesSeq
          var i = 0
          while (i < len) {
            val idx = indices(i)
            val gsqr = values(i) * values(i) * factor * factor
            val oldsumgsqr = gradSquares(idx)
            val sigma_i = (1 / alpha) * (math.sqrt(oldsumgsqr + gsqr) - math.sqrt(oldsumgsqr))
            val w_i = apply(idx)
            zs(idx) += values(i)*factor
            zs(idx) += (-sigma_i * w_i)
            gradSquares(idx) += gsqr
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            val gsqr = arr(i) * arr(i) * factor * factor
            val oldsumgsqr = gradSquares(i)
            val sigma_i = (1 / alpha) * (math.sqrt(oldsumgsqr + gsqr) - math.sqrt(oldsumgsqr))
            val w_i = apply(i)
            zs(i) += arr(i)*factor
            zs(i) += (-sigma_i * apply(i))
            gradSquares(i) += gsqr
            i += 1
          }
        case o: Outer1Tensor2 =>
          o.foreachActiveElement((i, v) => {
            val gsqr = v*v*factor*factor
            val oldsumgsqr = gradSquares(i)
            val sigma_i = (1 / alpha) * (math.sqrt(oldsumgsqr + gsqr) - math.sqrt(oldsumgsqr))
            val w_i = apply(i)
            zs(i) += v*factor
            zs(i) += (-sigma_i * w_i)
            gradSquares(i) += gsqr
          })
        case _ => throw new Error("no match statement for " + ds.getClass.getName)
      }
    }

    override def dot(ds: DoubleSeq) = {
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += apply(i)*x)
      res
    }
    def activeDomainSize = length
    def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
    def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
    def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
    def +=(i: Int, v: Double): Unit = throw new Error("You should add tensors all at once to the AdaGradRDATensor")
    def zero(): Unit = for (i <- 0 until length) { zs(i) = 0; gradSquares(i) = 0 }
  }

  private class FTRLProximalTensor1(baseTensor: Tensor1, val alpha: Double, val beta: Double, val l1: Double, val l2: Double) extends FTRLProximalTensor with Tensor1 {
    val zs = baseTensor.asArray
    val gradSquares = Array.fill(zs.length)(0.0)
    val dim1 = baseTensor.dim1
    def isDense = false
    override def copy = copyToDense(new DenseTensor1(dim1))
  }
  private class FTRLProximalTensor2(baseTensor: Tensor2, val alpha: Double, val beta: Double, val l1: Double, val l2: Double) extends FTRLProximalTensor with Tensor2 {
    val zs = baseTensor.asArray
    val gradSquares = Array.fill(zs.length)(0.0)
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = copyToDense(new DenseTensor2(dim1, dim2))
  }
  private class FTRLProximalTensor3(baseTensor: Tensor3, val alpha: Double, val beta: Double, val l1: Double, val l2: Double) extends FTRLProximalTensor with Tensor3 {
    val zs = baseTensor.asArray
    val gradSquares = Array.fill(zs.length)(0.0)
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    val dim3 = baseTensor.dim3
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = copyToDense(new DenseTensor3(dim1, dim2, dim3))
  }
  private class FTRLProximalTensor4(baseTensor: Tensor4, val alpha: Double, val beta: Double, val l1: Double, val l2: Double) extends FTRLProximalTensor with Tensor4 {
    val zs = baseTensor.asArray
    val gradSquares = Array.fill(zs.length)(0.0)
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    val dim3 = baseTensor.dim3
    val dim4 = baseTensor.dim4
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = copyToDense(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
}