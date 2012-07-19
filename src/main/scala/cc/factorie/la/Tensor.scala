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


// Note: Many Tensor-like methods are actually implemented in DoubleSeq

/** An N-dimensional collection of Doubles. */
trait Tensor extends MutableDoubleSeq {
  def numDimensions: Int
  def dimensions: Array[Int]
  // For handling sparsity
  def activeDomain: IntSeq
  def activeDomains: Array[IntSeq]
  def isDense: Boolean
  def dimensionsMatch(t:Tensor): Boolean = dimensions.toSeq equals t.dimensions.toSeq
  def activeDomainSize: Int = activeDomain.length // Should be overridden for efficiency in many subclasses
  /** The default value at indices not covered by activeDomain.  Subclasses may override this  */
  def defaultValue: Double = 0.0 // TODO This is not actually yet properly used by subclasses
  //def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  def activeElements: Iterator[(Int,Double)] = (for (i <- activeDomain.toArray) yield (i, apply(i))).iterator
  def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = forallElements(f) // To be override for efficiency in subclasses
  def exists(f:(Double)=>Boolean): Boolean = !forallActiveElements((i,v) => !f(v))
  def outer(t:Tensor): Tensor = Tensor.outer(this, t)
  override def dot(ds:DoubleSeq): Double = throw new Error("Subclasses should override dot with specialized efficient versions. t1="+this.getClass.getName+" t2="+ds.getClass.getName)
  // Methods for mutability not implemented in all Tensors
  def +=(i:Int, incr:Double): Unit = throw new Error("Method +=(Int,Double) not defined on class "+getClass.getName)
  def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName)
  def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
  def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
  def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
  def *(v:Double): Tensor = new TensorTimesScalar(this.copy, v) // TODO Should I use this.copy here?
  def /(v:Double): Tensor = new TensorTimesScalar(this.copy, 1.0/v) // TODO Should I use this.copy here?
  def +(that:Tensor): Tensor = { val t = this.copy; t += that; t }
  def -(that:Tensor): Tensor = { val t = this.copy; t -= that; t }
  def *(that:Tensor): Tensor = throw new Error("Not yet implemented")
  def /(that:Tensor): Tensor = throw new Error("Not yet implemented")
  def normalized: Tensor = { val t = copy; t.normalize; t }
  def expNormalized: Tensor = { val t = copy; t.expNormalize; t }
  def isUniform = false
  def stringPrefix = getClass.getName // "Tensor"
  def printLength = 50
  override def toString = { val suffix = if (length > printLength) "...)" else ")"; this.asSeq.take(printLength).mkString(stringPrefix+"(", ",", suffix) }
}

object Tensor {
  
  // Support for creating new empty Tensors with dimensions matching an argument
  def newDense(t:Tensor): Tensor = t match {
    case t:Tensor1 => new DenseTensor1(t.dim1)
    case t:Tensor2 => new DenseTensor2(t.dim1, t.dim2)
    case t:Tensor3 => new DenseTensor3(t.dim1, t.dim2, t.dim3)
    case t:Tensor4 => new DenseTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
  }
  def newDense(dims:Int*): Tensor = dims match {
    case Seq(d1) => new DenseTensor1(d1)
    case Seq(d1, d2) => new DenseTensor2(d1, d2)
    case Seq(d1, d2, d3) => new DenseTensor3(d1, d2, d3)
    case Seq(d1, d2, d3, d4) => new DenseTensor4(d1, d2, d3, d4)
  }
  def newDense(dims:Array[Int]): Tensor = dims.length match {
    case 1 => new DenseTensor1(dims(0))
    case 2 => new DenseTensor2(dims(0), dims(1))
    case 3 => new DenseTensor3(dims(0), dims(1), dims(2))
    case 4 => new DenseTensor4(dims(0), dims(1), dims(2), dims(3))
  }
  def newSparse(t:Tensor): Tensor = t match {
    case t:Tensor1 => new SparseTensor1(t.dim1)
    case t:Tensor2 => new DenseLayeredTensor2(t.dim1, t.dim2, (dim1:Int) => new SparseTensor1(dim1))
    case t:Tensor3 => new Dense2LayeredTensor3(t.dim1, t.dim2, t.dim3, new SparseTensor1(_))
    case t:Tensor4 => new Dense3LayeredTensor4(t.dim1, t.dim2, t.dim3, t.dim4, new SparseTensor1(_))
  }
  def newSparse(dims:Int*): Tensor = dims match {
    case Seq(d1) => new SparseTensor1(d1)
    case Seq(d1, d2) => new DenseLayeredTensor2(d1, d2, new SparseTensor1(_))
    case Seq(d1, d2, d3) => new Dense2LayeredTensor3(d1, d2, d3, new SparseTensor1(_))
    case Seq(d1, d2, d3, d4) => new Dense3LayeredTensor4(d1, d2, d3, d4, new SparseTensor1(_))
  }
  def newSparse(dims:Array[Int]): Tensor = dims.length match {
    case 1 => new SparseTensor1(dims(0))
    case 2 => new DenseLayeredTensor2(dims(0), dims(1), new SparseTensor1(_))
    case 3 => new Dense2LayeredTensor3(dims(0), dims(1), dims(2), new SparseTensor1(_))
    case 4 => new Dense3LayeredTensor4(dims(0), dims(1), dims(2), dims(3), new SparseTensor1(_))
  }

  // Support for dot inner products with dense tensors
  def dot(t1:DenseTensor, t2:DenseTensor): Double = {
    val len = t1.length; assert(len == t2.length); var result = 0.0; var i = 0
    while (i < len) { result += t1(i) * t2(i); i += 1 }; result
  }
  
  // Support for outer products between tensors
  def outer(t1:Tensor, t2:Tensor): Tensor = t1 match {
    case t1:Tensor1 => t2 match {
      case t2:Tensor1 => outer(t1, t2)
    }
  }
  def outer(t1:Tensor, t2:Tensor, t3:Tensor): Tensor = t1 match {
    case t1:Tensor1 => t2 match {
      case t2:Tensor1 => t3 match {
        case t3:Tensor1 => outer(t1, t2, t3)
      }
    }
  }
  def outer(t1:Tensor, t2:Tensor, t3:Tensor, t4:Tensor): Tensor = t1 match {
    case t1:Tensor1 => t2 match {
      case t2:Tensor1 => t3 match {
        case t3:Tensor1 => t4 match {
          case t4:Tensor1 => outer(t1, t2, t3, t4)
        }
      }
    }
  }
  def outer(t1:Tensor1, t2:Tensor1): Tensor2 = t1 match {
    case t1:SingletonBinaryTensorLike1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => new SingletonBinaryTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex)
      case t2:SingletonTensor1 => new SingletonTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex, t2.singleValue)
      case t2:Tensor1 => new SingletonBinaryLayeredTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2)
    }
    case t1:SingletonTensor1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => new SingletonTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue)
      case t2:Tensor1 => new SingletonLayeredTensor2(t1.dim1, t2.dim1, t1.singleIndex, t1.singleValue, t2)
    }
    case t1:SparseBinaryTensorLike1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => { val t = new SparseBinaryTensor2(t1.dim1, t2.dim1); val a1 = t1.asIntArray; val t2si = t2.singleIndex; var i = 0; while (i < a1.length) { t(t2si, a1(i)) = 1.0; i += 1 }; t }
      case t2:SingletonTensor1 => throw new Error("SparseTensor2 not yet implemented.") //{ val t = new SparseTensor2(t1.dim1, t2.dim1); val a1 = t1.asIntArray; var i = 0; while (i < a1.length) { t(t2.singleIndex, a1(i)) = 1.0; i += 1 }; t }
    }
    case t1:DenseTensor1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); val t2si = t2.singleIndex; for (i <- 0 until t1.dim1) t(i,t2si) = t1(i); t }
      case t2:SingletonTensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1) t(i,t2.singleIndex) = t1(i) * t2.singleValue; t }
      case t2:SparseBinaryTensorLike1 => throw new Error("Not yet implemented; needs SparseTensor2")
      case t2:DenseTensorLike1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1) t(i,j) = t1(i) * t2(j); t } // TODO Make a version of this that creates a GrowableDenseTensor2
      case t2:Tensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1; j <- t2.activeDomain.asSeq) t(i,j) = t1(i) * t2(j); t }
      //case t2:DenseTensor2 => { val t = new DenseTensor3(t1.dim1, t2.dim1, t2.dim2); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1; k <- 0 until t2.dim2) t(i,j,k) = t1(i) * t2(j,k); t }
    }
    case t1:Tensor1 => t2 match {
      //case t2:Tensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- t1.activeDomain.asSeq; j <- t2.activeDomain.asSeq) t(i,j) = t1(i) * t2(j); t }
      case t2:Tensor1 => {
        val t = new DenseTensor2(t1.dim1, t2.dim1); val ad1 = t1.activeDomain; val ad2 = t2.activeDomain; var ii = 0; var ji = 0
        while (ii < ad1.length) {
          ji = 0
          while (ji < ad2.length) {
            val i = ad1(ii); val j = ad2(ji)
            t(i,j) = t1(i) * t2(j)
            ji += 1
          }
          ii += 1
        }
        t
      } 
    }
  }
  def outer(t1:Tensor1, t2:Tensor2): Tensor3 = t1 match {
    case t1:DenseTensorLike1 => t2 match {
      case t2:DenseTensorLike2 => { val t = new DenseTensor3(t1.dim1, t2.dim1, t2.dim2); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1; k <- 0 until t2.dim2) t(i,j,k) = t1(i) * t2(j,k); t }
    }
  }
  def outer(t1:Tensor1, t2:Tensor1, t3:Tensor1): Tensor3 = t1 match {
    case t1:SingletonBinaryTensorLike1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => t3 match {
        case t3:SingletonBinaryTensorLike1 => new SingletonBinaryTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex)
        case t3:SingletonTensor1 => new SingletonTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex, t3.singleValue)
        case t3:Tensor1 => new Singleton2BinaryLayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3)
      }
      case t2:SingletonTensor1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, 1.0, t2.singleValue, t3)
      }
      case t2:SparseBinaryTensorLike1 => t3 match {
        case t3:SparseBinaryTensorLike1 => {
          //println("Tensor.outer3 dim1="+t1.dim1+" dim2="+t2.dim1+" dim3="+t3.dim1+"  t2="+t2)
          val t = new SparseBinaryTensor3(t1.dim1, t2.dim1, t3.dim1)
          // TODO This next line is inefficient
          for (j <- t2.activeDomain1.asSeq; k <- t3.activeDomain1.asSeq) t.update(t1.singleIndex, j, k, 1.0)
          t
        }
        case _ => throw new Error("Not yet implemented.")// { val t = new SparseBinaryTensor3...} 
      }
    }
    case t1:SingletonTensor1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue, 1.0, t3)
      }
      case t2:SingletonTensor1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue, t2.singleValue, t3)
      }
    }
  }
  def outer(t1:Tensor1, t2:Tensor1, t3:Tensor1, t4:Tensor1): Tensor4 = t1 match {
    case t1:SingletonBinaryTensorLike1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => t3 match {
        case t3:SingletonBinaryTensorLike1 => t4 match {
          case t4:SingletonBinaryTensorLike1 => new SingletonBinaryTensor4(t1.dim1, t2.dim1, t3.dim1, t4.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex, t4.singleIndex)
          case t4:Tensor1 => new Singleton3LayeredTensor4(t1.dim1, t2.dim1, t3.dim1, t4.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex, t4)
        }
      }
    }
  }
}

trait TensorWithMutableDefaultValue extends Tensor {
  def defaultValue_=(v:Double): Unit
  def defaultValue_+=(v:Double): Unit = defaultValue_=(defaultValue + v)
  def defaultValue_*=(v:Double): Unit = defaultValue_=(defaultValue * v)
}


/** An lazy product of a Vector and a scalar.
    Note that changes in the underlying Tensor will also show up here. 
    @author Andrew McCallum */
class TensorTimesScalar(val tensor:Tensor, val scalar:Double) extends Tensor {
  def numDimensions: Int = tensor numDimensions
  def dimensions: Array[Int] = tensor.dimensions
  // For handling sparsity
  def activeDomain: IntSeq = tensor.activeDomain
  def activeDomains: Array[IntSeq] = tensor.activeDomains
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = tensor.activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  def isDense: Boolean = tensor.isDense
  def length = tensor.length
  override def activeDomainSize: Int = tensor.activeDomainSize
  override def dot(t:DoubleSeq): Double = tensor.dot(t) * scalar
  override def *(scalar:Double) = new TensorTimesScalar(tensor, scalar*this.scalar)
  //override def update(i:Int, v:Double): Unit = tensor.update(idx, value/scalar)
  //override def +=(v: Vector) { vector += v*(1.0/scalar) }
  def apply(index:Int) = tensor.apply(index) * scalar
}

trait DenseTensor extends Tensor with TensorWithMutableDefaultValue {
  private var __values = new Array[Double](length)
  if (__default != 0.0) java.util.Arrays.fill(__values, __default)
  private var __default: Double = 0.0
  override def defaultValue: Double = __default
  def defaultValue_=(v:Double): Unit = __default = v
  protected def _values = __values
  protected def _valuesSize: Int = _values.size
  // Used by subclass GrowableDenseTensor1
  protected def ensureCapacity(size:Int): Unit = if (__values.size < size) {
    val newSize = math.max(__values.size * 2, size)
    val newValues = new Array[Double](newSize)
    Array.copy(_values, 0, newValues, 0, __values.size)
    if (__default != 0.0) java.util.Arrays.fill(newValues, __values.size, newSize, __default)
    __values = newValues
  }
  protected def _setArray(a:Array[Double]): Unit = { assert(a.length == length); __values = a }
  def isDense = true
  def activeDomain = new RangeIntSeq(0, length)
  def apply(i:Int): Double = __values(i)
  override def update(i:Int, v:Double): Unit = __values(i) = v
  override def zero(): Unit = java.util.Arrays.fill(__values, 0.0)
  override def asArray = __values
  override def +=(i:Int, incr:Double): Unit = __values(i) += incr
  override def :=(ds:DoubleSeq): Unit = ds match {
    case ds:DenseTensor => System.arraycopy(ds.__values, 0, __values, 0, length)
    case ds:DoubleSeq => super.:=(ds)
  }
  override def :=(a:Array[Double]): Unit = { require(a.length == length, "Expected length="+length+" but got "+a.length); System.arraycopy(a, 0, _values, 0, a.length) }
  override def :=(a:Array[Double], offset:Int): Unit = System.arraycopy(a, offset, __values, 0, length)
  override def dot(t2:DoubleSeq): Double = t2 match {
    case t2:SingletonBinaryTensor => apply(t2.singleIndex)
    case t2:SingletonTensor => apply(t2.singleIndex) * t2.singleValue
    case t2:DenseTensor => {
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += __values(i) * t2.__values(i); i += 1 }; result
    }
    case t2:SparseBinaryTensor => {
      var s = 0.0; t2.foreachElement((i,v) => s += __values(i)); s
    }
    case t:UniformTensor => sum * t.uniformValue
    // TODO Any other special cases here?
    case t2:DoubleSeq => { // TODO Consider removing this to catch inefficiency
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
  }

  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensor => __values(t.singleIndex) += f
    case t:SingletonTensor => __values(t.singleIndex) += f * t.singleValue
    case t:SparseBinaryTensor => t.=+(__values, f)
    case t:DenseTensor => { val len = length; var i = 0; while (i < len) { __values(i) += f * t.__values(i); i += 1 }}
    case t:UniformTensor => { val len = length; val u = t.uniformValue * f; var i = 0; while (i < len) { __values(i) += u; i += 1 }}
  }
}

trait SingletonTensor extends Tensor with SparseDoubleSeq {
  def singleIndex: Int
  def singleValue: Double
  //def activeDomain: IntSeq = new SingletonIntSeq(singleIndex) // Can't be here and in Tensor1
  def isDense = false
  def apply(i:Int) = if (i == singleIndex) singleValue else 0.0
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = f(singleIndex, singleValue)
  override def activeElements: Iterator[(Int,Double)] = Iterator.single((singleIndex, singleValue))
  override def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = f(singleIndex, singleValue)
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = a(offset+singleIndex) += f * singleValue
  override def sum: Double = singleValue
  override def max: Double = if (singleValue > 0.0) singleValue else 0.0 
  override def min: Double = if (singleValue < 0.0) singleValue else 0.0 
  override def maxIndex: Int = if (singleValue >= 0.0) singleIndex else if (singleIndex != 0) 0 else 1
  override def containsNaN: Boolean = false
  //override def dot(v:DoubleSeq): Double = v(singleIndex) * singleValue
  override def copy: SingletonTensor = this // immutable, but careful in the future we might make a mutable version
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor => if (singleIndex == t.singleIndex) singleValue else 0.0
    case t:SingletonTensor => if (singleIndex == t.singleIndex) singleValue * t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex) * singleValue
  }
}

trait SingletonBinaryTensor extends SingletonTensor {
  def singleValue: Double = 1.0
  override def apply(i:Int) = if (i == singleIndex) 1.0 else 0.0
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = f(singleIndex, 1.0)
  override def activeElements: Iterator[(Int,Double)] = Iterator.single((singleIndex, 1.0))
  override def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = f(singleIndex, 1.0)
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = a(offset+singleIndex) += f
  override def sum: Double = 1.0
  override def max: Double = 1.0
  override def min: Double = 0.0
  override def containsNaN: Boolean = false
  override def maxIndex: Int = singleIndex
  override def dot(t:DoubleSeq): Double = t match {
    case t:SingletonBinaryTensor => if (singleIndex == t.singleIndex) 1.0 else 0.0
    case t:SingletonTensor => if (singleIndex == t.singleIndex) t.singleValue else 0.0
    case t:DoubleSeq => t(singleIndex)
  }
  override def copy: SingletonBinaryTensor = this // immutable, but careful in the future we might make a mutable version
}
// TODO Make a mutable version of this to be used in BP with DotFamily.score(Tensor)

trait UniformTensor extends Tensor {
  def uniformValue: Double
  def apply(i:Int) = uniformValue
  def isDense = true
  //def activeDomain: IntSeq = new RangeIntSeq(0, length) // Can't be both here an Tensor1
  override def isUniform = true
  override def sum: Double = length * uniformValue
  override def max: Double = uniformValue
  override def min: Double = uniformValue
  override def maxIndex: Int = 0
  override def containsNaN: Boolean = false
  override def dot(v:DoubleSeq): Double = v.sum * uniformValue
  override def copy = this // safe because it is immutable
}

trait SparseBinaryTensor extends Tensor with cc.factorie.util.ProtectedIntArrayBuffer with SparseDoubleSeq {
  def isDense = false
  def activeDomain = new ArrayIntSeq(_array)
  @inline final def apply(index:Int): Double = if (_indexOfSorted(index) >= 0) 1.0 else 0.0
  @inline final def contains(index:Int): Boolean = _containsSorted(index)
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val len = _length; var i = 0; while (i < len) { f(_array(i), 1.0); i += 1 }}
  override def sum: Double = _length.toDouble
  override def max: Double = if (_length > 0) 1.0 else 0.0
  override def min: Double = if (_length == 0) 0.0 else 1.0
  override def indexOf(d:Double): Int = if (d != 0.0 && d != 1.0) -1 else if (d == 1.0) { if (_length == 0) -1 else _apply(0) } else { if (_length == 0) 0 else throw new Error("Not yet implemented") }
  override def maxIndex: Int = if (_length == 0) 0 else _apply(0)
  override def containsNaN: Boolean = false
  //def =+(a:Array[Double]): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)) += 1.0; i += 1 } }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { val len = _length; var i = 0; while (i < len) { a(_array(i)+offset) += f; i += 1 } }
  def +=(i:Int): Unit = _insertSortedNoDuplicates(i)
  def -=(i:Int): Unit = { val index = _indexOfSorted(i); if (index >= 0) _remove(index) else throw new Error("Int value not found: "+i)}
  def ++=(is:Array[Int]): Unit = { _ensureCapacity(_length + is.length); var j = 0; while (j < is.length) { _insertSortedNoDuplicates(is(j)); j += 1} }
  def ++=(is:Iterable[Int]): Unit = { _ensureCapacity(_length + is.size); is.foreach(_insertSortedNoDuplicates(_)) }
  override def update(i:Int, v:Double): Unit = {
    if (i < 0 || i >= length) throw new Error("Tensor index out of range: "+i)
    if (v == 1.0) this += i else if (v == 0.0) this -= i else throw new Error(getClass.getName+" cannot update with values other than 0.0 or 1.0.")
  }
  /** In SparseBinary, this is equivalent to update(i,v) */
  override def +=(i:Int, v:Double): Unit = update(i, v)
  override def zero(): Unit = _clear() // TODO I think _clear should be renamed _zero -akm
  override def dot(v:DoubleSeq): Double = v match {
    case t:SingletonBinaryTensor1 => if (contains(t.singleIndex)) 1.0 else 0.0
    case t:SingletonTensor1 => if (contains(t.singleIndex)) t.singleValue else 0.0
    // TODO Any other special cases here?
    case ds:DoubleSeq => { var result = 0.0; var i = 0; while (i < _length) { result += ds(_apply(i)); i += 1 }; result }
  }
}

trait    SparseIndexedTensor extends Tensor {
  def isDense = false
  // In subclasses either _length should be set > 0 or _sizeProxy should be set non-null, but not both.
  private var _length: Int = 0
  private var _sizeProxy: Iterable[Any] = null
  private var _values: Array[Double] = new Array[Double](4)
  private var _indexs: Array[Int] = new Array[Int](4) // the indices, in order corresponding to _values
  private var _positions: Array[Int] = null // a dense array containing the index into _indices and _values; not yet implemented
  private var _npos = 0 // the number of positions in _values and _indices that are actually being used
  private var _sorted = 0 // The number of positions in _values & _indices where indices are sorted; if _sorted == _npos then ready for use
  private def setCapacity(cap:Int): Unit = {
    assert(cap >= _npos)
    val newInd = new Array[Int](cap)
    val newVal = new Array[Double](cap)
    System.arraycopy(_indexs, 0, newInd, 0, _npos)
    System.arraycopy(_values, 0, newVal, 0, _npos)
    _indexs = newInd; _values = newVal
  }
  private def ensureCapacity(cap:Int): Unit = if (_indexs.length < cap) setCapacity(math.max(cap, _indexs.length + _indexs.length/2))
  def trim: Unit = setCapacity(_npos)
  
  def length: Int = if (_length < 0) _sizeProxy.size else _length
  override def activeDomainSize: Int = { makeReadable; _npos }
  def activeDomain: IntSeq = { makeReadable ; new TruncatedArrayIntSeq(_indexs, _npos) } // TODO Consider making more efficient
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { var i = 0; while (i < _npos) { f(_indexs(i), _values(i)); i += 1 } }
  override def activeElements: Iterator[(Int,Double)] = {
    makeReadable
    new Iterator[(Int,Double)] { // Must not change _indexs and _values during iteration!
      var i = 0
      def hasNext = i < _npos
      def next = { i += 1 ; (_indexs(i-1), _values(i-1)) }
    }
  }
  override def zero(): Unit = _npos = 0
  override def sum: Double = { var s = 0.0; var i = 0; while (i < _npos) { s += _values(i); i += 1 }; s }

  /** Return the position at which index occurs, or -1 if index does not occur. */
  def position(index:Int): Int = {
    makeReadable
    var i = 0; var ii = 0
    while (i < _npos) { ii = _indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    //while (i < _npos) { if (_indexs(i) == index) return i; i += 1 }
    -1
  }
  def position(index:Int, start:Int): Int = { // Just linear search for now; consider binary search with memory of last position
    makeReadable
    var i = start; var ii = 0
    while (i < _npos) { ii = _indexs(i); if (ii == index) return i else if (ii > index) return -1; i += 1 }
    -1
  }

  def apply(index:Int): Double = {
    // makeReadable is called in this.position
    val pos = position(index)
    if (pos < 0) 0.0 else _values(pos)
  }

  override def dot(v:DoubleSeq): Double = {
    makeReadable
    v match {
      case v:SingletonBinaryTensor1 => apply(v.singleIndex)
      case v:SingletonTensor1 => apply(v.singleIndex) * v.singleValue
      case v:SparseIndexedTensor => {
        val v1 = if (this._npos < v._npos) this else v
        val v2 = if (v._npos< this._npos) v else this
        var i = 0; var j = -1; var j2 = 0
        var result = 0.0
        while (i < v1._npos) {
          j2 = v2.position(v1._indexs(i), j+1)
          if (j2 >= 0) { result += v1._values(i) * v2._values(j2); j = j2 }
          i += 1
        }
        result
      }
      case v:DoubleSeq => { var result = 0.0; var p = 0; while (p < _npos) { result += v(_indexs(p)) * _values(p); p += 1 }; result }
    }
  }
  
  // Consider using bit shifting and only one array for this!
  // How many bits are in the mantissa of a Double?  Enough to also keep the index?
  
  // Sort _indexs & _values between start and end; does not modify positions outside that range.
  // Return the number of duplicate indices.  
  @inline private def sort(start:Int, end:Int): Int = {
    throw new Error("Not yet implemented")
    var cp = start
    while (cp < end) {
      val ci = _indexs(cp)
      val cv = _values(cp)
      var i = cp - 1
      while (i >= 0 && _indexs(i) >= ci) {
        val tmpi = 
        i -= 1
      }
    }
    0
  }
  
  override def toString = "SparseIndexedTensor npos="+_npos+" sorted="+_sorted+" ind="+_indexs.mkString(",")+" val="+_values.mkString(",")
  
  @inline private def makeReadable: Unit = {
    var cp = _sorted // "current position", the position next to be placed into sorted order
    while (cp < _npos) {
      //println("cp="+cp)
      val ci = _indexs(cp) // "current index", the index next to be placed into sorted order.
      val cv = _values(cp) // "current value"
      var i = _sorted - 1
      //println("i="+i)
      // Find the position at which the current index/value belongs
      while (i >= 0 && _indexs(i) >= ci) i -= 1
      i += 1
      // Put it there, shifting to make room if necessary
      //println("Placing at position "+i)
      if (_indexs(i) == ci) { if (i != cp) _values(i) += cv else _sorted += 1 }
      else insert(i, ci, cv, incrementNpos=false, incrementSorted=true)
      //println("sorted="+_sorted)
      cp += 1
    }
    _npos = _sorted
    if (_npos * 1.5 > _values.length) trim
  }
  
  // Caller is responsible for making sure there is enough capacity
  @inline private def insert(position:Int, index:Int, value:Double, incrementNpos:Boolean, incrementSorted:Boolean): Unit = {
    if (_npos - position > 0) {
      System.arraycopy(_values, position, _values, position+1, _sorted-position)
      System.arraycopy(_indexs, position, _indexs, position+1, _sorted-position)
    }
    _indexs(position) = index
    _values(position) = value
    if (incrementNpos) _npos += 1
    if (incrementSorted) _sorted += 1
  }

  override def update(index:Int, value:Double): Unit = {
    val p = position(index)
    if (p >= 0) _values(p) = value
    else +=(index, value) 
  }
  // Efficiently support multiple sequential additions
  override def +=(index:Int, incr:Double): Unit = {
    ensureCapacity(_npos+1)
    _indexs(_npos) = index
    _values(_npos) = incr
    _npos += 1
  }
  
  override def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:SingletonBinaryTensorLike1 => +=(t.singleIndex, f)
    case t:SingletonTensor1 => +=(t.singleIndex, f * t.singleValue)
    case t:SparseBinaryTensorLike1 => { val a = t.asIntArray; val len = a.length; var i = 0; while (i < len) { +=(a(i), f); i += 1 }}
    case t:SparseIndexedTensor => { val len = t._npos; var i = 0; while (i < len) { +=(t._indexs(i), f * t._values(i)); i += 1 }}
  }
  override def =+(a:Array[Double], offset:Int, f:Double): Unit = { var i = 0; while (i < _npos) { a(_indexs(i)+offset) += f * _values(i); i += 1 }}
  
  def cloneFrom(t:SparseIndexedTensor): Unit = {
    makeReadable
    t._length = _length
    t._sizeProxy = _sizeProxy
    t._npos = _npos
    t._sorted = _sorted
    t._values = _values.clone
    t._indexs = _indexs.clone
    // TODO Deal with _positions, once is it implemented
  }
}


// TODO Finish this implementation
class ConcatenatedTensor(theTensors:Seq[Tensor]) extends Tensor1 {
  def tensors: Array[Tensor] = theTensors.toArray
  def foreachTensor(f:Tensor=>Unit): Unit = { var i = 0; while (i < tensors.length) { f(tensors(i)); i += 1 }}
  def isDense = throw new Error("Not yet implemented")
  lazy val lengths: Array[Int] = { val a = new Array[Int](tensors.length); var i = 0; while (i < a.length) { a(i) = tensors(i).length; i += 1 }; a }
  lazy val lengthsSums: Array[Int] = { val a = new Array[Int](lengths.length); a(0) = lengths(0); var i = 1; while (i < a.length) { a(i) = a(i-1) + lengths(i); i += 1 }; a }
  lazy val offsets: Array[Int] = { val a = new Array[Int](lengths.length); a(0) = 0; var i = 1; while (i < a.length) { a(i) = a(i-1) + lengths(i); i += 1 }; a }
  lazy val dim1 = lengths.sum
  def activeDomain1: IntSeq = throw new Error("Not yet implemented")
  // Careful!  This will be very slow.  You should really try to use the more specific methods, such as += 
  def apply(index:Int): Double = {
    throw new Error("This is very slow.  I'm throwing an error here to find situations where this would be called, and then we should try to find a faster alternative.")
    var i = 0
    var sum = 0
    while (i < lengths.length) {
      if (index < sum) return tensors(i-1).apply(index-lengthsSums(i-1))
      sum += lengths(i)
      i += 1
    }
    throw new Error("Index out of bounds: "+index)
  }
  override def copy: ConcatenatedTensor = new ConcatenatedTensor(tensors.map(_.copy))
  override def :=(a:Array[Double]): Unit = { var i = 0; while (i < tensors.length) { tensors(i).:=(a, offsets(i)); i += 1 } } 
  override def toArray: Array[Double] = { val a = new Array[Double](length); var i = 0; while (i < tensors.length) { System.arraycopy(tensors(i).asArray, 0, a, offsets(i), tensors(i).length); i +=1 }; a }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:ConcatenatedTensor => for (pair <- tensors.zip(t.tensors)) pair._1.+=(pair._2, f)
  }
  override def +=(d:Double): Unit = foreachTensor(_ += d)
  override def *=(d:Double): Unit = foreachTensor(_ *= d)
  protected def sumOverTensors(f:Tensor=>Double): Double = { val len = tensors.length; var s = 0.0; var i = 0; while (i < len) { s += f(tensors(i)); i += 1 }; s }
  override def oneNorm: Double = sumOverTensors(_.twoNorm)
  override def twoNormSquared: Double = sumOverTensors(_.twoNormSquared)
  override def dot(t:DoubleSeq): Double = t match {
    case t:ConcatenatedTensor => { var s = 0.0; for (pair <- tensors.zip(t.tensors)) s += pair._1 dot pair._2; s }
  }
}

//* A Tensor to represent the weights in a collection of DotFamilies as the keys in a HashMap from DotFamily to Tensor. */
class WeightsTensor(val newTensor:DotFamily=>Tensor = _.newSparseTensor) extends Tensor1 {
  // This functionality moved to TemplateModel
  //def this(model:TemplateModel, newTensor:DotFamily=>Tensor) = { this(newTensor); val wt = this; model.familiesOfClass[DotFamily].foreach(f => wt(f) = newTensor(f)) } 
  private val _map = new scala.collection.mutable.LinkedHashMap[DotFamily,Tensor] {
    override def default(f:DotFamily) = { val t = newTensor(f); this(f) = t; t }
  }
  def families = _map.keys
  def dim1: Int = _map.values.map(_.length).sum
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:WeightsTensor => _map.keys.toSeq equals t._map.keys.toSeq
    case _ => false
  }
  // Careful!  This will be very slow.  You should really try to use the more specific methods, such as += 
  def apply(index:Int): Double = {
    throw new Error("This is very slow.  I'm throwing an error here to find situations where this would be called, and then we should try to find a faster alternative.")
    var i = 0
    var sum = 0
    for (t <- _map.values) {
      val len = t.length
      if (index < sum + len) return t.apply(index-sum)
      sum += len
    }
    throw new Error("Index out of bounds: "+index)
  }
  def activeDomain1: IntSeq = throw new Error("Method activeDomain1 not defined for WeightTensors.")
  def isDense = false
  override def stringPrefix = "WeightsTensor"
  def apply(f:DotFamily): Tensor = _map.apply(f)
  def update(f:DotFamily, t:Tensor) = _map(f) = t
  override def *=(d:Double): Unit = _map.values.foreach(_.*=(d))
  protected def sumOverTensors(f:Tensor=>Double): Double = { var s = 0.0; _map.values.foreach(s += f(_)); s }
  override def oneNorm: Double = sumOverTensors(_.twoNorm)
  override def twoNormSquared: Double = sumOverTensors(_.twoNormSquared)
  //override def toArray: Array[Double] = throw new Error("Cannot be implemented.")
  override def toArray: Array[Double] = { val a = new Array[Double](dim1); var offset = 0; _map.values.foreach(t => { System.arraycopy(t.asArray, 0, a, offset, t.length); offset += t.length }); a }
  override def copy: WeightsTensor = { val t = new WeightsTensor(newTensor); _map.keys.foreach(k => t(k) = apply(k).copy); t }
  override def different(t:DoubleSeq, threshold:Double): Boolean = t match {
    case t:WeightsTensor => _map.keys.exists(k => _map(k).different(t._map(k), threshold))
  }
  override def containsNaN: Boolean = _map.values.exists(_.containsNaN)
  override def :=(t:DoubleSeq): Unit = t match {
    case t:WeightsTensor => _map.keys.foreach(k => if (t(k) eq null) apply(k).zero() else apply(k) := t(k))
  }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:WeightsTensor => t._map.keys.foreach(k => apply(k).+=(t.apply(k), f))
  }
  override def +=(ds:DoubleSeq, factor:DoubleSeq): Unit = ds match {
    case t:WeightsTensor => {
      factor match {
        case t2:WeightsTensor => {
          t._map.keys.foreach(k => apply(k).+=(t.apply(k), t2.apply(k)))
        }
      }
    }
  }
  override def +=(ds:DoubleSeq, factor:DoubleSeq,scalar:Double): Unit = ds match {
    case t:WeightsTensor => {
      factor match {
        case t2:WeightsTensor => {
          t._map.keys.foreach(k => apply(k).+=(t.apply(k), t2.apply(k),scalar))
        }
      }
    }
  }
  override def dot(t:DoubleSeq): Double = t match {
    case t:WeightsTensor => { var s = 0.0; for (k <- t._map.keys) { val t2 = apply(k); if (t2 ne null) s += t2 dot t.apply(k) }; s }
  }
  override def toString: String = _map.keys.map((f:DotFamily) => {
    val tensor = this(f)
    f.defaultFactorName+" "+tensor.getClass+" dims="+tensor.dimensions.mkString(",")+" contents:"+tensor.toString
  }).mkString("\n")
}
