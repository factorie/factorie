package cc.factorie.optimize

import cc.factorie._
import la._
import util._

/**
 * User: apassos
 * Date: 4/8/13
 * Time: 5:54 PM
 */
// TODO Alex: please comment this. -akm
// TODO probably should rename this to AdaGradDualAveraging since it specifically does the AdaGrad proximal step -luke

class DualAveragingOptimizer(val delta: Double, val eta: Double, val l1: Double, val l2: Double) extends GradientOptimizer {
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double) {
    if (!initialized) setUpTensors(weights)
    weights.+=(gradient)
  }
  def setUpTensors(weights: WeightsSet): Unit = {
    for (key <- weights.keys) {
      key.value match {
        case t: Tensor1 => weights(key) = new DualAveragingTensor1(t.length, delta, eta, l1, l2)
        case t: Tensor2 => weights(key) = new DualAveragingTensor2(t.dim1, t.dim2, delta, eta, l1, l2)
        case t: Tensor3 => weights(key) = new DualAveragingTensor3(t.dim1, t.dim2, t.dim3, delta, eta, l1, l2)
        case t: Tensor4 => weights(key) = new DualAveragingTensor4(t.dim1, t.dim2, t.dim3, t.dim4, delta, eta, l1, l2)
      }
    }
    initialized = true
  }

  // TODO can we use the tensorsetkey's "newBlank" thing here?
  // I guess either way we still need to figure out how to combine with locking -luke
  def setToDense(weights: WeightsSet): Unit = {
    for (key <- weights.keys) {
      val daTensor = key.value.asInstanceOf[DualAveragingTensor]
      key.value match {
        case t: Tensor1 => weights(key) = new DenseTensor1(t.dim1)
        case t: Tensor2 => weights(key) = new DenseTensor2(t.dim1, t.dim2)
        case t: Tensor3 => weights(key) = new DenseTensor3(t.dim1, t.dim2, t.dim3)
        case t: Tensor4 => weights(key) = new DenseTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
      }
      daTensor.toDenseTensor(key.value.asInstanceOf[DenseTensor])
    }
  }
  def reset() {}
  def isConverged = false

  private trait DualAveragingTensor extends Tensor  {
    def activeDomain = new RangeIntSeq(0, length)
    val gradients = Array.fill(length)(0.0)
    val gradSquares = Array.fill(length)(0.0)
    var t = 0
    val delta: Double
    val eta: Double
    val l1: Double
    val l2: Double

    // can we use += here?
    def toDenseTensor[D <: DenseTensor](d: D): D = {
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
      if (gradSquares(i) == 0.0) 0.0
      else {
        val h = (1.0/eta) *(math.sqrt(gradSquares(i)) + delta) + t*l2
        val t1 = 1.0/h
        t1 * DualAveraging.truncate(gradients(i), t*l1)
      }
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      t += 1
      ds match {
        case o: SparseIndexedTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          val values = o._values
          var i = 0
          while (i < len) {
            gradients(indices(i)) += values(i)*factor
            gradSquares(indices(i)) += values(i)*values(i)*factor*factor
            i += 1
          }
        case o: SparseBinaryTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          var i = 0
          while (i < len) {
            gradients(indices(i)) += factor
            gradSquares(indices(i)) += factor*factor
            i += 1
          }
        case o: SparseBinaryTensorLike1 =>
          val len = o.activeDomainSize
          val indices = o._indices
          var i = 0
          while (i < len) {
            gradients(indices(i)) += factor
            gradSquares(indices(i)) += factor*factor
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            gradients(i) += arr(i)*factor
            gradSquares(i) += arr(i)*arr(i)*factor*factor
            i += 1
          }
        case _ => throw new Error("no match statement for " + ds.getClass.getName)
      }
    }

    override def dot(ds: DoubleSeq) = {
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += apply(i)*x)
      res
    }
    def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
    def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
    def +=(i: Int, v: Double): Unit = throw new Error("You should add tensors all at once to the DualAveragingTensor")
    def zero(): Unit = for (i <- 0 until length) { gradients(i) = 0; gradSquares(i) = 0 }
  }

  private class DualAveragingTensor1(val dim1: Int, val eta: Double, val delta: Double, val l1: Double, val l2: Double) extends DualAveragingTensor with Tensor1 {
    def isDense = false
    override def copy = toDenseTensor(new DenseTensor1(dim1))
  }
  private class DualAveragingTensor2(val dim1: Int, val dim2: Int, val eta: Double, val delta: Double, val l1: Double, val l2: Double) extends DualAveragingTensor with Tensor2 {

    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = toDenseTensor(new DenseTensor2(dim1, dim2))

    override def *(t: Tensor1): Tensor1 = {
  //    assert(dim2 == t.dimensions.reduce(_ * _), "Dimensions don't match: " + dim2 + " " + t.dimensions)
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
        case t: SparseBinaryTensorLike1 =>
          val tIndexSeq = t.activeDomain.asInstanceOf[TruncatedArrayIntSeq]
          val tIndices = tIndexSeq.array
          var row = 0
          while (row < dim1) {
            val offset = row * dim2
            var ti = 0
            var dot = 0.0
            while (ti < tIndexSeq.size) {
              val col = tIndices(ti)
              dot += apply(offset + col)
              ti += 1
            }
            newArray(row) = dot
            row += 1
          }
        case _ =>
          val vecIter = t.activeElements
          while (vecIter.hasNext) {
            val (col, v) = vecIter.next()
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
          }
      }
      newT
    }
  }
  private class DualAveragingTensor3(val dim1: Int, val dim2: Int, val dim3: Int, val eta: Double, val delta: Double, val l1: Double, val l2: Double) extends DualAveragingTensor with Tensor3 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = toDenseTensor(new DenseTensor3(dim1, dim2, dim3))
  }
  private class DualAveragingTensor4(val dim1: Int, val dim2: Int, val dim3: Int, val dim4: Int, val eta: Double, val delta: Double, val l1: Double, val l2: Double) extends DualAveragingTensor with Tensor4 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = toDenseTensor(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
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