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
  def *(v:Double): Tensor = new TensorTimesScalar(this, v)
  def /(v:Double): Tensor = new TensorTimesScalar(this, 1.0/v)
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
  def newSparse(t:Tensor): Tensor = t match {
    case t:Tensor1 => new SparseTensor1(t.dim1)
    case t:Tensor2 => new DenseLayeredTensor2(t.dim1, t.dim2, (dim1:Int) => new SparseTensor1(dim1))
    case t:Tensor3 => new Dense2LayeredTensor3(t.dim1, t.dim2, t.dim3, new SparseTensor1(_)) // with InnerSparseTensor1
    case t:Tensor4 => new Dense3LayeredTensor4(t.dim1, t.dim2, t.dim3, t.dim4, new SparseTensor1(_)) // with InnerSparseTensor1
  }
  def newSparse(dims:Int*): Tensor = dims match {
    case Seq(d1) => new SparseTensor1(d1)
    case Seq(d1, d2) => new DenseLayeredTensor2(d1, d2, new SparseTensor1(_)) // with InnerSparseTensor1
    case Seq(d1, d2, d3) => new Dense2LayeredTensor3(d1, d2, d3, new SparseTensor1(_)) // with InnerSparseTensor1
    case Seq(d1, d2, d3, d4) => new Dense3LayeredTensor4(d1, d2, d3, d4, new SparseTensor1(_)) // with InnerSparseTensor1
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
      case t2:SingletonBinaryTensorLike1 => { val t = new SparseBinaryTensor2(t1.dim1, t2.dim1); val a1 = t1.asIntArray; var i = 0; while (i < a1.length) { t(t2.singleIndex, a1(i)) = 1.0; i += 1 }; t }
      case t2:SingletonTensor1 => throw new Error("SparseTensor2 not yet implemented.") //{ val t = new SparseTensor2(t1.dim1, t2.dim1); val a1 = t1.asIntArray; var i = 0; while (i < a1.length) { t(t2.singleIndex, a1(i)) = 1.0; i += 1 }; t }
    }
    case t1:DenseTensor1 => t2 match {
      case t2:SingletonBinaryTensorLike1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1) t(i,t2.singleIndex) = t1(i); t }
      case t2:SingletonTensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1) t(i,t2.singleIndex) = t1(i) * t2.singleValue; t }
      case t2:SparseBinaryTensorLike1 => throw new Error("Not yet implemented; needs SparseTensor2")
      case t2:DenseTensorLike1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1) t(i,j) = t1(i) * t2(j); t }
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

@deprecated("Not yet used, and Tensor subclasses not set up to leverage this anyway.")
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

trait DenseTensor extends Tensor {
  protected def _values: Array[Double]
  override def zero(): Unit = java.util.Arrays.fill(_values, 0.0)
  override def dot(t2:DoubleSeq): Double = t2 match {
    case t2:SingletonBinaryTensor => apply(t2.singleIndex)
    case t2:SingletonTensor => apply(t2.singleIndex) * t2.singleValue
    case t2:DenseTensor => {
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
    case t2:SparseBinaryTensor => {
      var s = 0.0; t2.foreachElement((i,v) => s += _values(i)); s
    }
    // TODO Any other special cases here?
    case t2:DoubleSeq => { // TODO Consider removing this to catch inefficiency
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
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
  override def sum: Double = singleValue
  override def max: Double = if (singleValue > 0.0) singleValue else 0.0 
  override def min: Double = if (singleValue < 0.0) singleValue else 0.0 
  override def maxIndex: Int = if (singleValue >= 0.0) singleIndex else if (singleIndex != 0) 0 else 1
  override def containsNaN: Boolean = false
  override def dot(v:DoubleSeq): Double = v(singleIndex) * singleValue
  override def copy: SingletonTensor = this // immutable, but careful in the future we might make a mutable version
}

trait SingletonBinaryTensor extends SingletonTensor {
  def singleValue: Double = 1.0
  override def apply(i:Int) = if (i == singleIndex) 1.0 else 0.0
  override def foreachActiveElement(f:(Int,Double)=>Unit): Unit = f(singleIndex, 1.0)
  override def activeElements: Iterator[(Int,Double)] = Iterator.single((singleIndex, 1.0))
  override def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = f(singleIndex, 1.0)
  override def sum: Double = 1.0
  override def max: Double = 1.0
  override def min: Double = 0.0
  override def containsNaN: Boolean = false
  override def maxIndex: Int = singleIndex
  override def dot(v:DoubleSeq): Double = v(singleIndex)
  override def copy: SingletonBinaryTensor = this // immutable, but careful in the future we might make a mutable version
}

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

// To be used eventually for getting a TemplateModel into cc.factorie.optimize
class DenseConcatenatedTensor(ts:DenseTensor*) extends Tensor {
  val tensors = ts.toIndexedSeq
  val length = tensors.map(_.length).sum
  def apply(i:Int): Double = throw new Error
  val numDimensions: Int = tensors.map(_.numDimensions).sum
  val dimensions: Array[Int] = { val a = new Array[Int](numDimensions); throw new Error }
  // For handling sparsity
  def activeDomain: IntSeq = throw new Error("Not yet implemented.")
  def activeDomains: Array[IntSeq] = throw new Error("Not yet implemented.")
  def isDense: Boolean = false
}

abstract class SparseConcatenatedTensor(ts:DenseTensor*) extends Tensor {
  // Unfinished
}
