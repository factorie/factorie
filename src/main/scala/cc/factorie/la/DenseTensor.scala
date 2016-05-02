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

package cc.factorie.la
import cc.factorie.maths
import cc.factorie.util.{DenseDoubleSeq, IntSeq, RangeIntSeq, DoubleSeq}

trait DenseTensor extends Tensor with TensorWithMutableDefaultValue with DenseDoubleSeq with Serializable {
  protected def _initialArray: Array[Double] = new Array[Double](length)
  private var __values = _initialArray
  if (__default != 0.0) java.util.Arrays.fill(__values, __default)
  private var __default: Double = 0.0
  override def defaultValue: Double = __default
  def defaultValue_=(v:Double): Unit = __default = v
  protected def _values = __values
  protected def _valuesSize: Int = __values.size
  protected def _resetValues(s:Int): Unit = __values = new Array[Double](s)
  protected def _setArray(a:Array[Double]): Unit = { assert(a.length == length); __values = a }
  def isDense = true
  def activeDomain: IntSeq = new RangeIntSeq(0, length)
  def apply(i:Int): Double = __values(i)
  def activeDomainSize = length
  override def update(i:Int, v:Double): Unit = __values(i) = v
  override def zero(): Unit = java.util.Arrays.fill(__values, 0.0)
  override def asArray = __values
  override def *=(d: Double) = {
    val myValues = __values
    val len = myValues.length; var i = 0
    while (i < len) { myValues(i) *= d; i += 1 }
  }
  override def +=(i:Int, incr:Double): Unit = __values(i) += incr
  override def :=(ds:DoubleSeq): Unit = ds match {
    case ds:DenseTensor => System.arraycopy(ds.__values, 0, __values, 0, length)
    case ds:DoubleSeq => super.:=(ds)
  }
  def fill(f: ()=>Double): this.type = { var i = 0; val len = length; while (i < len) { __values(i) = f(); i += 1 }; this }
  @deprecated("Use fill() instead.", "Before 2014-11-17")
  def initializeRandomly(mean: Double = 0.0, variance: Double = 1.0)(implicit rng: scala.util.Random): Unit = { (0 until length).foreach(i => _values(i) = rng.nextGaussian()*variance + mean ) }
  def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = forallElements(f)
  override def :=(a:Array[Double]): Unit = { require(a.length == length, "Expected length="+length+" but got "+a.length); System.arraycopy(a, 0, _values, 0, a.length) }
  override def :=(a:Array[Double], offset:Int): Unit = System.arraycopy(a, offset, __values, 0, length)
  var hasLogged = false
  override def dot(t2:DoubleSeq): Double = t2 match {
    case t2:SingletonBinaryTensor => apply(t2.singleIndex)
    case t2:SingletonTensor => apply(t2.singleIndex) * t2.singleValue
    case t2:DenseTensor => {
      val myValues = __values
      val otherValues = t2.__values
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += myValues(i) * otherValues(i); i += 1 }
      result
    }
//    case t2:SparseBinaryTensor => {
//      var s = 0.0; t2.foreachActiveElement((i,v) => s += __values(i)); s
//    }
//    case t:UniformTensor => sum * t.uniformValue
    // TODO Any other special cases here?
//    case t2:SparseIndexedTensor => {var s = 0.0;t2.foreachActiveElement((i,v) => s += __values(i)*v);s}
    case t: Tensor =>
      // can we just do this? since dense things are easy for other tensors to dot against cause they can grab the array -luke
      t dot this
    case t2:DoubleSeq => { // TODO Consider removing this to catch inefficiency
      if (!hasLogged) {
        hasLogged = true
        println("Warning: DenseTensor slow dot for type " + t2.getClass.getName)
      }
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
  }

  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t: SingletonBinaryLayeredTensor2 => {
      val i0 = t.singleIndex1
      t.inner match {
        case inner: SingletonTensor => {
          val i = inner.singleIndex
          this(t.singleIndex(i0, i)) += inner.singleValue * f
        }
        case inner: SparseBinaryTensorLike1 => {
          var i = 0
          val indices = inner.activeDomain
          while (i < indices.length) {
            this(t.singleIndex(i0, indices(i))) += f
            i += 1
          }
        }
        case inner: SparseIndexedTensor => {
          inner.foreachActiveElement((i, v) => {
            this(t.singleIndex(i0, i)) += v * f
          })
        }
        case _ => sys.error(t.inner.getClass.getName + " doesn't match")
      }
    }
    case t:SingletonBinaryTensor => __values(t.singleIndex) += f
    case t:SingletonTensor => __values(t.singleIndex) += f * t.singleValue
    case t:SparseBinaryTensor => t.=+(__values, f)
    case t:DenseTensor => {
      val myValues = __values
      val otherValues = t.__values
      val len = t.length; var i = 0
      while (i < len) { myValues(i) += f * otherValues(i); i += 1 }
    }
    case t:UniformTensor => {
      val myValues = __values
      val len = length; val u = t.uniformValue * f; var i = 0
      while (i < len) { myValues(i) += u; i += 1 }
    }
    case t:TensorTimesScalar => this.+=(t.tensor, t.scalar) //{ t.tensor.activeDomain.foreach(i => this(i) += t(i)*t.scalar) } 
    case t:Outer1Tensor2 => {
      val ff = f*t.scale
      require(this.isInstanceOf[Tensor2]) // Makes sure rank matches!
      t =+ (__values, ff)
    }
    case t:Tensor => t.=+(this.asArray, f)
  }
  
  /** Increment into this DenseTensor at an offset. */
  def +=(t:DoubleSeq, offset:Int, f:Double): Unit = t match {
    case t:SingletonBinaryTensor => __values(offset + t.singleIndex) += f
    case t:SingletonTensor => __values(offset + t.singleIndex) += f * t.singleValue
    case t:SparseBinaryTensorLike1 => t.=+(__values, offset, f)
    case t:DenseTensor => {
      val myValues = __values
      val otherValues = t.__values
      val len = t.length; var i = 0
      while (i < len) { myValues(i+offset) += f * otherValues(i); i += 1 }
    }
  }

  // A little faster than the MutableDoubleSeq implementation because it can access the __values array directly
  override def expNormalize(): Double = {
    var sum = maths.sumLogProbs(this)
    this -= sum
    var i = 0
    while (i < length) {
      this(i) = math.exp(this(i))
      i += 1
    }
    sum
  }

  def euclideanDistance(t:DenseTensor): Double = {
    var d = 0.0
    val len = length; var i = 0; while (i < len) {
      val diff = __values(i) - t.__values(i)
      d += diff * diff
      i += 1
    }
    math.sqrt(d)
  }
  
  
}

