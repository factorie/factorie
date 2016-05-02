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
 * This implements an efficient version of the Pegasos SGD algorithm for l2-regularized hinge loss
 * it won't necessarily work with other losses because of the aggressive projection steps
 * note that adding a learning rate here is nontrivial since the update relies on baseRate / step < 1.0 to avoid zeroing the weights
 * but if I don't add a rate <1 here this optimizer does terribly in my tests -luke
 * @param baseRate The base learning rate
 * @param l2 The l2 regularization constant
 */
class Pegasos(baseRate: Double = 0.1, l2: Double = 0.01) extends GradientOptimizer {
  private var step = 1
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    if (!initialized) { initializeWeights(weights); initialized = true }
    if (step == 1) {
      // make sure weights start off with ||w|| <= 1 / sqrt(l2)
      if (weights.twoNorm > 1.0 / math.sqrt(l2))
        weights *= 1.0 / (weights.twoNorm * math.sqrt(l2))
    }
    val eta_t = baseRate / (l2 * step)
    weights *= (1.0 - eta_t * l2)
    weights += (gradient, eta_t)
    val projCoeff = math.min(1.0, (1.0 / math.sqrt(l2)) / weights.twoNorm)
    weights *= projCoeff
    step += 1
  }

  def initializeWeights(weights: WeightsSet) = if (!initialized) MutableScalableWeights.initializeWeights(weights, cacheTwoNormSq = true)
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  // can we get a good convergence criterion here? since it's not regular sgd, I feel like yes?
  def isConverged = false
  def reset(): Unit = {
    step = 1
  }
}

object MutableScalableWeights {
  def initializeWeights(weights: WeightsSet, cacheTwoNormSq: Boolean = false): Unit = {
    for (key <- weights.keys) {
      key.value match {
        case t: Tensor1 => weights(key) = new MutableScaledTensor1(t.length, cacheTwoNormSq)
        case t: Tensor2 => weights(key) = new MutableScaledTensor2(t.dim1, t.dim2, cacheTwoNormSq)
        case t: Tensor3 => weights(key) = new MutableScaledTensor3(t.dim1, t.dim2, t.dim3, cacheTwoNormSq)
        case t: Tensor4 => weights(key) = new MutableScaledTensor4(t.dim1, t.dim2, t.dim3, t.dim4, cacheTwoNormSq)
      }
    }
  }
  def finalizeWeights(weights: WeightsSet): Unit =
    for (key <- weights.keys) {
      val scaledTensor = key.value
      weights(key) = key.newBlankTensor
      scaledTensor.foreachElement((i, v) => key.value(i) = v)
    }

  abstract class MutableScaledTensor(cacheTwoNormSq: Boolean) extends Tensor with DenseDoubleSeq {
    private var cachedTwoNormSq: Double = 0.0
    def activeDomain = new RangeIntSeq(0, length)
    def activeDomainSize = activeDomain.length
    def forallActiveElements(f: (Int,Double) => Boolean) = forallElements(f)
    protected val _values = Array.fill(length)(0.0)
    var multiplier = 1.0
    var tolerance = 0.00001
    override def twoNormSquared: Double = {
      if (cacheTwoNormSq) return cachedTwoNormSq
      val myValues = _values
      val myMultiplier = multiplier
      var normSq = 0.0
      var i = 0
      while (i < myValues.length) {
        normSq += myValues(i) * myValues(i)
        i += 1
      }
      normSq * myMultiplier * myMultiplier
    }
    override def update(i: Int, v: Double): Unit = {
      val myValues = _values
      val myMultiplier = multiplier
      if (cacheTwoNormSq) {
        val oldValue = myValues(i) * myMultiplier
        cachedTwoNormSq -= oldValue * oldValue
        cachedTwoNormSq += v * v
      }
      myValues(i) = v / myMultiplier
    }
    override def apply(i: Int): Double = _values(i) * multiplier
    override def *=(f: Double): Unit = {
      if (f == 0.0) zero()
      else multiplier *= f
      if (math.abs(multiplier) < tolerance) applyMultiplier()
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      val myValues = _values
      val myMultiplier = multiplier
      ds match {
        case o: SparseIndexedTensor =>
          val len = o._unsafeActiveDomainSize
          val indices = o._indices
          val values = o._values
          var i = 0
          while (i < len) {
            val idx = indices(i)
            val oldValue = myValues(idx) * myMultiplier
            val newValue = oldValue + values(i) * factor
            if (cacheTwoNormSq) {
              cachedTwoNormSq -= oldValue * oldValue
              cachedTwoNormSq += newValue * newValue
            }
            myValues(idx) = newValue / myMultiplier
            i += 1
          }
        case o: SparseBinaryTensor =>
          val len = o._unsafeActiveDomainSize
          val indices = o._indices
          var i = 0
          while (i < len) {
            val idx = indices(i)
            val oldValue = myValues(idx) * myMultiplier
            val newValue = oldValue + factor
            if (cacheTwoNormSq) {
              cachedTwoNormSq -= oldValue * oldValue
              cachedTwoNormSq += newValue * newValue
            }
            myValues(idx) = newValue / myMultiplier
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            val oldValue = myValues(i) * myMultiplier
            val newValue = oldValue + arr(i) * factor
            if (cacheTwoNormSq) {
              cachedTwoNormSq -= oldValue * oldValue
              cachedTwoNormSq += newValue * newValue
            }
            myValues(i) = newValue / myMultiplier
            i += 1
          }
        case _ => throw new Error("ScaledTensor can't yet handle += from" + ds.getClass.getName)
      }
    }
    override def dot(ds: DoubleSeq) = {
      val myValues = _values
      val myMultiplier = multiplier
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += myValues(i) * myMultiplier * x)
      res
    }
    def copy: Tensor = throw new Error("Method copy not defined on MutableScaledTensor")
    def blankCopy: Tensor = throw new Error("Method blankCopy not defined on MutableScaledTensor")
    def +=(i: Int, v: Double): Unit =  update(i, v + apply(i))
    def zero(): Unit = {
      for (i <- 0 until length) { _values(i) = 0 }
      multiplier = 1.0
    }

    protected def copyTo[D <: DenseTensor](c: D): D = {
      val cArr = c.asArray
      val myValues = _values
      val myMultiplier = multiplier
      var i = 0
      while (i < myValues.length) {
        cArr(i) = myValues(i) * myMultiplier
        i += 1
      }
      c
    }

    private def applyMultiplier(): Unit = {
      var i = 0
      val myValues = _values
      val myMultiplier = multiplier
      while (i < myValues.length) {
        myValues(i) *= myMultiplier
        i += 1
      }
      multiplier = 1.0
    }
  }

  private class MutableScaledTensor1(val dim1: Int, val cacheTwoNormSq: Boolean) extends MutableScaledTensor(cacheTwoNormSq) with Tensor1 {
    def isDense = false
    override def copy = copyTo(new DenseTensor1(dim1))
  }
  private class MutableScaledTensor2(val dim1: Int, val dim2: Int, val cacheTwoNormSq: Boolean) extends MutableScaledTensor(cacheTwoNormSq) with Tensor2 {
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = copyTo(new DenseTensor2(dim1, dim2))
    override def leftMultiply(t: Tensor1): Tensor1 = {
      val myValues = _values
      val myMultiplier = multiplier
      assert(dim1 == t.dim1, "Dimensions don't match: " + dim1 + " " + t.dim1)
      val myDim2 = dim2
      val newT = new DenseTensor1(dim2)
      val newArray = newT.asArray
      t match {
        case t: DenseTensor =>
          val tArr = t.asArray
          var row = 0
          while (row < tArr.length) {
            val v = tArr(row)
            val offset = row * myDim2
            var col = 0
            while (col < myDim2) {
              newArray(col) += (myValues(offset + col) * myMultiplier * v)
              col += 1
            }
            row += 1
          }
        case t: SparseBinaryTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          var ti = 0
          while (ti < tActiveDomainSize) {
            val row = tIndices(ti)
            val offset = row * myDim2
            var col = 0
            while (col < myDim2) {
              newArray(col) += myValues(offset + col) * myMultiplier
              col += 1
            }
            ti += 1
          }
        case t: SparseIndexedTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          val tValues = t._values
          var ti = 0
          while (ti < tActiveDomainSize) {
            val row = tIndices(ti)
            val offset = row * myDim2
            val v = tValues(ti)
            var col = 0
            while (col < myDim2) {
              newArray(col) += (myValues(offset + col) * v * myMultiplier)
              col += 1
            }
            ti += 1
          }
        case _ =>
          throw new Error("tensor type neither dense nor sparse: " + t.getClass.getName)
      }
      newT
    }

    override def *(t: Tensor1): Tensor1 = {
      assert(dim2 == t.dim1, "Dimensions don't match: " + dim2 + " " + t.dim1)
      val myValues = _values
      val myMultiplier = multiplier
      val newT = new DenseTensor1(dim1)
      val newArray = newT.asArray
      t match {
        case t: DenseTensor =>
          val tArr = t.asArray
          var col = 0
          while (col < tArr.length) {
            val v = tArr(col)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (myValues(offset + col) * myMultiplier * v)
              row += 1
            }
            col += 1
          }
        case t: SparseTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          val tValues = t._valuesSeq
          var ti = 0
          while (ti < tActiveDomainSize) {
            val col = tIndices(ti)
            val v = tValues(ti)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (myValues(offset + col) * myMultiplier * v)
              row += 1
            }
            ti += 1
          }
        case _ =>
          throw new Error("tensor type neither dense nor sparse: " + t.getClass.getName)
      }
      newT
    }
  }
  private class MutableScaledTensor3(val dim1: Int, val dim2: Int, val dim3: Int, val cacheTwoNormSq: Boolean) extends MutableScaledTensor(cacheTwoNormSq) with Tensor3 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = copyTo(new DenseTensor3(dim1, dim2, dim3))
  }
  private class MutableScaledTensor4(val dim1: Int, val dim2: Int, val dim3: Int, val dim4: Int, val cacheTwoNormSq: Boolean) extends MutableScaledTensor(cacheTwoNormSq) with Tensor4 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = copyTo(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
}