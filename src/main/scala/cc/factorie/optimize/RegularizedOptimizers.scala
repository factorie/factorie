package cc.factorie.optimize

import cc.factorie.la.{Tensor, SparseIndexedTensor, DenseTensor, Tensors}

/**
 * User: apassos
 * Date: 5/2/13
 * Time: 9:47 AM
 */
class LazyL2ProjectedGD(var l2: Double = 0.0, rate: Double = 1.0) extends GradientOptimizer {
  var lastUpdate: Tensors = null
  var t = 0
  @inline final def learningRate(t: Double): Double = {
    rate / math.sqrt(t)
  }
  var printed = false

  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
    t += 1
    val eta = rate
    if (lastUpdate == null) { lastUpdate = weights.blankDenseCopy }
    for (template <- gradient.keys)
      (weights(template), gradient(template), lastUpdate(template)) match {
        case (w: DenseTensor, g: DenseTensor, lastUpdate: DenseTensor) =>
          val wArr = w.asArray
          val gArr = g.asArray
          val lastArr = lastUpdate.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            lastArr(i) += 1
            wArr(i) *= (1 - l2*learningRate(lastArr(i)))
            val t2 = wArr(i) + learningRate(lastArr(i)) * gArr(i)
            wArr(i) = t2
            i += 1
          }
        case (w: DenseTensor, g: SparseIndexedTensor,  lastUpdate: DenseTensor) =>
          val wArr = w.asArray
          val lastArr = lastUpdate.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            lastArr(idx) += 1
            wArr(idx) *= (1 - l2*learningRate(lastArr(idx)))
            val t2 = wArr(idx) + learningRate(lastArr(idx)) * g
            wArr(idx) = t2
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor,  lastUpdated: Tensor) =>
          if (!printed) {
            printed = true
            println("No implementations for: " + weights(template).getClass.getName + " " +
              gradient(template).getClass.getName +" " + lastUpdate(template).getClass.getName)
          }
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            val idx = indices(i)
            lastUpdated(idx) += 1
            w(idx) *= (1 - l2*learningRate(lastUpdated(idx)))
            val t2 = w(idx) + learningRate(lastUpdated(idx)) * g
            w(idx) = t2
            i += 1
          }
      }
  }
  def reset(): Unit = {
    lastUpdate = null
  }
  def isConverged: Boolean = false
}

object DualAveraging {
  @inline final def truncate(x0: Double, l1: Double): Double = {
    if (x0 > l1)
      x0-l1
    else if (x0 < -l1)
      x0+l1
    else 0.0
  }
}

// This implements the AdaGrad algorithm with primal-dual updates and support for l1 regularization
class AdaGradDualAveraging(var l1: Double = 0.0, var l2: Double = 0.0, var rate: Double = 1.0, var delta: Double = 0.1) extends GradientOptimizer {
  var HSq: Tensors = null
  var sumGs: Tensors = null
  var t = 0

  import DualAveraging.truncate
  var printed = false

  def step(weights: Tensors, gradient: Tensors, value: Double): Unit = {
    val eta = rate
    t += 1
    if (HSq == null) { HSq = weights.blankDenseCopy }
    if (sumGs == null) { sumGs = weights.blankDenseCopy }
    for (template <- gradient.keys)
      (weights(template), gradient(template), HSq(template), sumGs(template)) match {
        case (w: DenseTensor, g: DenseTensor, hSq: DenseTensor, sGs: DenseTensor) =>
          val wArr = w.asArray
          val gArr = g.asArray
          val hArr = hSq.asArray
          val sgArr = sGs.asArray
          var i = 0
          val len = wArr.length
          while (i < len) {
            if (gArr(i) != 0) {
              hArr(i) += gArr(i) * gArr(i)
              sgArr(i) += gArr(i)
              val h = (1.0/eta) *(math.sqrt(hArr(i)) + delta) + t*l2
              val t1 = 1.0/h
              val t2 = t1 * truncate(sgArr(i), t*l1)
              wArr(i) = t2
            }
            i += 1
          }
        case (w: DenseTensor, g: SparseIndexedTensor, hSq: DenseTensor, sGs: DenseTensor) =>
          val wArr = w.asArray
          val hArr = hSq.asArray
          val sgArr = sGs.asArray
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            if (g != 0) {
              val idx = indices(i)
              hArr(idx) += g*g
              sgArr(idx) += g
              val h = (1.0/eta)*(math.sqrt(hArr(idx)) + delta) + t*l2
              val t1 = 1.0 / h
              val t2 = t1 * truncate(sgArr(idx), t*l1)
              wArr(idx) = t2
            }
            i += 1
          }
        case (w: Tensor, g: SparseIndexedTensor, hSq: Tensor, sGs: Tensor) =>
          if (!printed) {
            printed = true
            println("AdaradDualAveaging: no implementations for: " + weights(template).getClass.getName + " " +
              gradient(template).getClass.getName +" " + HSq(template).getClass.getName)
          }
          var i = 0
          val len = g.activeDomainSize
          val indices = g._indices
          val values = g._values
          while (i < len) {
            val g = values(i)
            if (g != 0) {
              val idx = indices(i)
              hSq(idx) += g*g
              sGs(idx) += g
              val h = (1.0/eta)*(math.sqrt(hSq(idx)) + delta) + t*l2
              val t1 = 1.0 / h
              val t2 = t1 * truncate(sGs(idx), t*l1)
              w(idx) = t2
            }
            i += 1
          }
      }
  }
  def reset(): Unit = {
    HSq = null
    sumGs = null
  }
  def isConverged: Boolean = false
}
