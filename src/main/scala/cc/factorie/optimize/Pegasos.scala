package cc.factorie.optimize

import cc.factorie.la._
import cc.factorie.util.{DenseDoubleSeq, TruncatedArrayIntSeq, DoubleSeq, RangeIntSeq}
import cc.factorie.model.{WeightsMap, WeightsSet}


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

  def initializeWeights(weights: WeightsSet) = if (!initialized) MutableScalableWeights.initializeWeights(weights)
  def finalizeWeights(weights: WeightsSet) = MutableScalableWeights.finalizeWeights(weights)

  // can we get a good convergence criterion here? since it's not regular sgd, I feel like yes?
  def isConverged = false
  def reset(): Unit = {
    step = 1
  }
}

object MutableScalableWeights {
  def initializeWeights(weights: WeightsSet): Unit = {
    for (key <- weights.keys) {
      key.value match {
        case t: Tensor1 => weights(key) = new MutableScaledTensor1(t.length)
        case t: Tensor2 => weights(key) = new MutableScaledTensor2(t.dim1, t.dim2)
        case t: Tensor3 => weights(key) = new MutableScaledTensor3(t.dim1, t.dim2, t.dim3)
        case t: Tensor4 => weights(key) = new MutableScaledTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
      }
    }
  }
  def finalizeWeights(weights: WeightsSet): Unit =
    for (key <- weights.keys) {
      val scaledTensor = key.value
      weights(key) = key.newBlankTensor
      scaledTensor.foreachElement((i, v) => key.value(i) = v)
    }

  // NOTE use the scaled tensor to build Pegasos and l2 sgd, and then see if we can
  // make the l2 regularized thing work with the averaged perceptron -
  // it can certainly work with the MIRA, different LR's etc
  private trait MutableScaledTensor extends Tensor with DenseDoubleSeq {
    def activeDomain = new RangeIntSeq(0, length)
    def activeDomainSize = activeDomain.length
    def forallActiveElements(f: (Int,Double) => Boolean) = forallElements(f)
    protected val _values = Array.fill(length)(0.0)
    var multiplier = 1.0
    var tolerance = 0.00001
    override def twoNormSquared: Double = {
      var normSq = 0.0
      var i = 0
      while (i < _values.length) {
        normSq += _values(i) * _values(i)
        i += 1
      }
      normSq
    }
    override def update(i: Int, v: Double): Unit = _values(i) = v / multiplier
    override def apply(i: Int): Double = _values(i) * multiplier
    override def *=(f: Double): Unit = {
      if (math.abs(multiplier) < tolerance) applyMultiplier()
      if (f == 0.0) zero()
      else multiplier *= f
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      ds match {
        case o: SparseIndexedTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          val values = o._values
          var i = 0
          while (i < len) {
            val idx = indices(i)
            _values(idx) = (_values(idx) * multiplier + values(i) * factor) / multiplier
            i += 1
          }
        case o: SparseBinaryTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          var i = 0
          while (i < len) {
            val idx = indices(i)
            _values(idx) = (_values(idx) * multiplier + factor) / multiplier
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            _values(i) = (_values(i) * multiplier + arr(i) * factor) / multiplier
            i += 1
          }
        case _ => throw new Error("ScaledTensor can't yet handle += from" + ds.getClass.getName)
      }
    }
    override def dot(ds: DoubleSeq) = {
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += apply(i) * x)
      res
    }
    def copy: Tensor = throw new Error("Method copy not defined on ScaledTensor")
    def blankCopy: Tensor = throw new Error("Method blankCopy not defined on ScaledTensor")
    def +=(i: Int, v: Double): Unit =  update(i, v + apply(i))
    def zero(): Unit = {
      for (i <- 0 until length) { _values(i) = 0 }
      multiplier = 1.0
    }

    protected def copyTo[D <: DenseTensor](c: D): D = {
      val cArr = c.asArray
      val valArr = _values
      var i = 0
      while (i < _values.length) {
        cArr(i) = valArr(i) * multiplier
        i += 1
      }
      c
    }

    private def applyMultiplier(): Unit = {
      var i = 0
      while (i < _values.length) {
        _values(i) *= multiplier
        i += 1
      }
      multiplier  = 1.0
    }
  }

  private class MutableScaledTensor1(val dim1: Int) extends MutableScaledTensor with Tensor1 {
    def isDense = false
    override def copy = copyTo(new DenseTensor1(dim1))
  }
  private class MutableScaledTensor2(val dim1: Int, val dim2: Int) extends MutableScaledTensor with Tensor2 {
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = copyTo(new DenseTensor2(dim1, dim2))
    override def *(t: Tensor1): Tensor1 = {
      val newT = new DenseTensor1(dim1)
      val newArray = newT.asArray
      t match {
        case t: DenseTensor1 =>
          val tArr = t.asArray
          var col = 0
          while (col < tArr.length) {
            val v = tArr(col)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
            col += 1
          }
        case t: SparseIndexedTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          val tValues = t._values
          var ti = 0
          while (ti < tActiveDomainSize) {
            val col = tIndices(ti)
            val v = tValues(ti)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
            ti += 1
          }
        case t: SparseBinaryTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          var ti = 0
          while (ti < tActiveDomainSize) {
            val col = tIndices(ti)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += apply(offset + col)
              row += 1
            }
            ti += 1
          }
        case _ =>
          t.foreachActiveElement((col, v) => {
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
          })
      }
      newT
    }
  }
  private class MutableScaledTensor3(val dim1: Int, val dim2: Int, val dim3: Int) extends MutableScaledTensor with Tensor3 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = copyTo(new DenseTensor3(dim1, dim2, dim3))
  }
  private class MutableScaledTensor4(val dim1: Int, val dim2: Int, val dim3: Int, val dim4: Int) extends MutableScaledTensor with Tensor4 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = copyTo(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
}