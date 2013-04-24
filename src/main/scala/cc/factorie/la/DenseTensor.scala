/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
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
import cc.factorie._
import cc.factorie.util._

trait DenseTensor extends Tensor with TensorWithMutableDefaultValue {
  private var __values = new Array[Double](length)
  if (__default != 0.0) java.util.Arrays.fill(__values, __default)
  private var __default: Double = 0.0
  override def defaultValue: Double = __default
  def defaultValue_=(v:Double): Unit = __default = v
  protected def _values = __values
  protected def _valuesSize: Int = __values.size
  protected def _resetValues(s:Int): Unit = __values = new Array[Double](s)
  // Used by subclass GrowableDenseTensor1
  private def ensureCapacity(size:Int): Unit = if (__values.size < size) { // TODO Delete this method
    if (__values.size == 0) { __values = new Array[Double](size); return }
    val newSize = math.max(__values.size * 2, size)
    val newValues = new Array[Double](newSize)
    Array.copy(_values, 0, newValues, 0, __values.size)
    if (__default != 0.0) java.util.Arrays.fill(newValues, __values.size, newSize, __default)
    __values = newValues
  }
  protected def _setArray(a:Array[Double]): Unit = { assert(a.length == length); __values = a }
  def isDense = true
  def activeDomain: IntSeq = new RangeIntSeq(0, length)
  def apply(i:Int): Double = __values(i)
  override def update(i:Int, v:Double): Unit = __values(i) = v
  override def zero(): Unit = java.util.Arrays.fill(__values, 0.0)
  override def asArray = __values
  override def *=(d: Double) = { var i = 0; while (i < __values.length) { __values(i) *= d; i += 1 } }
  override def +=(i:Int, incr:Double): Unit = __values(i) += incr
  override def :=(ds:DoubleSeq): Unit = ds match {
    case ds:DenseTensor => System.arraycopy(ds.__values, 0, __values, 0, length)
    case ds:DoubleSeq => super.:=(ds)
  }
  override def :=(a:Array[Double]): Unit = { require(a.length == length, "Expected length="+length+" but got "+a.length); System.arraycopy(a, 0, _values, 0, a.length) }
  override def :=(a:Array[Double], offset:Int): Unit = System.arraycopy(a, offset, __values, 0, length)
  var hasLogged = false
  override def dot(t2:DoubleSeq): Double = t2 match {
    case t2:SingletonBinaryTensor => apply(t2.singleIndex)
    case t2:SingletonTensor => apply(t2.singleIndex) * t2.singleValue
    case t2:DenseTensor => {
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += __values(i) * t2.__values(i); i += 1 }; result
    }
    case t2:SparseBinaryTensor => {
      var s = 0.0; t2.foreachActiveElement((i,v) => s += __values(i)); s
    }
    case t:UniformTensor => sum * t.uniformValue
    case t2:SparseIndexedTensor => {var s = 0.0;t2.foreachActiveElement((i,v) => s += __values(i)*v);s}
    // TODO Any other special cases here?
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
    case t:SingletonBinaryLayeredTensor2 => {
      val i0 = t.singleIndex1
      t.inner match {
        case inner:SparseBinaryTensorLike1 => {
          var i = 0
          val indices = inner.activeDomain
          while (i < indices.length) {
            this(t.singleIndex(i0, indices(i))) += f
            i += 1
          }
        }
        case inner:SparseIndexedTensor => {
          for ((i,v) <- inner.activeElements) {
            this(t.singleIndex(i0, i)) += v * f
          }
        }
        case inner: SingletonTensor => {
          val i = inner.singleIndex
          this(t.singleIndex(i0, i)) += inner.singleValue * f
        }
        case _ => assert(false, t.inner.getClass.getName + " doesn't match")
      }
    }
    case t:SingletonBinaryTensor => __values(t.singleIndex) += f
    case t:SingletonTensor => __values(t.singleIndex) += f * t.singleValue
    case t:SparseBinaryTensor => t.=+(__values, f)
    case t:SparseBinaryTensorLike1 => t.=+(__values, f)
    case t:DenseTensor => { val len = length; var i = 0; while (i < len) { __values(i) += f * t.__values(i); i += 1 }}
    case t:UniformTensor => { val len = length; val u = t.uniformValue * f; var i = 0; while (i < len) { __values(i) += u; i += 1 }}
    case t:TensorTimesScalar => this.+=(t.tensor, t.scalar) //{ t.tensor.activeDomain.foreach(i => this(i) += t(i)*t.scalar) } 
    case t:Outer1Tensor2 => {
      require(this.isInstanceOf[Tensor2]) // Makes sure rank matches!
      t =+ (__values, f)
    }
    case t:Tensor => t.=+(this.asArray, f)
  }
  
  /** Increment into this DenseTensor at an offset. */
  def +=(t:DoubleSeq, offset:Int, f:Double): Unit = t match {
    case t:SingletonBinaryTensor => __values(offset + t.singleIndex) += f
    case t:SingletonTensor => __values(offset + t.singleIndex) += f * t.singleValue
    case t:SparseBinaryTensorLike1 => t.=+(__values, offset, f)
    case t:DenseTensor => { val len = t.length; var i = 0; while (i < len) { __values(i+offset) += f * t.__values(i); i += 1 }}
  }

  // A little faster than the MutableDoubleSeq implementation because it can access the __values array directly
  override def expNormalize(): Double = {
    var max = Double.MinValue
    var i = 0; val l = length
    while (i < l) { if (max < __values(i)) max = __values(i); i += 1 }
    var sum = 0.0
    i = 0
    while (i < l) {
      val e = math.exp(__values(i) - max)  //update(i, math.exp(apply(i) - max))
      __values(i) = e
      sum += e
      i += 1
    }
    i = 0; while (i < l) {
      __values(i) = __values(i) / sum
      i += 1
    }
    //println("DenseTensor.expNormalize "+__values.mkString(" "))
    sum
  }

}

