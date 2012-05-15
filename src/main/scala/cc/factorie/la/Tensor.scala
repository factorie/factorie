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
  def foreachActiveElement(f:(Int,Double)=>Unit): Unit = { val d = activeDomain; var i = 0; while (i < d.length) { f(d(i), apply(d(i))); i += 1 } }
  def activeElements: Iterator[(Int,Double)] = (for (i <- activeDomain.toArray) yield (i, apply(i))).iterator
  def forallActiveElements(f:(Int,Double)=>Boolean): Boolean = forallElements(f) // To be override for efficiency in subclasses
  def outer(t:Tensor): Tensor = Tensor.outer(this, t)
  override def dot(ds:DoubleSeq): Double = throw new Error("Subclasses should override dot with specialized efficient versions.")
  // Methods for mutability not implemented in all Tensors
  def +=(i:Int, incr:Double): Unit = throw new Error("Method +=(Int,Double) not defined on class "+getClass.getName)
  def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName)
  def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
  def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
  // TODO Consider methods like +, -, *, /
  def +(that:Tensor): Tensor = throw new Error
  def -(that:Tensor): Tensor = throw new Error
  def *(that:Tensor): Tensor = throw new Error
  def /(that:Tensor): Tensor = throw new Error
  def normalized: Tensor = throw new Error
  def expNormalized: Tensor = throw new Error
  def expNormalizer: Double = throw new Error
  def isUniform = false // TODO Fix this for the Uniform Tensors!!
  def stringPrefix = "Tensor"
  override def toString = this.asSeq.mkString(stringPrefix+"(", ",", ")")
}

object Tensor {
  
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
    case t1:SingletonBinaryTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => new SingletonBinaryTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex)
      case t2:SingletonTensor1 => new SingletonTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex, t2.singleValue)
      case t2:Tensor1 => new SingletonBinaryLayeredTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2)
    }
    case t1:SingletonTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => new SingletonTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue)
      case t2:Tensor1 => new SingletonLayeredTensor2(t1.dim1, t2.dim1, t1.singleIndex, t1.singleValue, t2)
    }
    case t1:DenseTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1) t(i,t2.singleIndex) = t1(i); t }
      case t2:SingletonTensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1) t(i,t2.singleIndex) = t1(i) * t2.singleValue; t }
      case t2:SparseBinaryTensor1 => throw new Error("Not yet implemented; needs SparseTensor2")
      case t2:DenseTensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1) t(i,j) = t1(i) * t2(j); t }
      case t2:Tensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- 0 until t1.dim1; j <- t2.activeDomain.asSeq) t(i,j) = t1(i) * t2(j); t }
      //case t2:DenseTensor2 => { val t = new DenseTensor3(t1.dim1, t2.dim1, t2.dim2); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1; k <- 0 until t2.dim2) t(i,j,k) = t1(i) * t2(j,k); t }
    }
    case t1:Tensor1 => t2 match {
      case t2:Tensor1 => { val t = new DenseTensor2(t1.dim1, t2.dim1); for (i <- t1.activeDomain.asSeq; j <- t2.activeDomain.asSeq) t(i,j) = t1(i) * t2(j); t }
    }
  }
  def outer(t1:Tensor1, t2:Tensor2): Tensor3 = t1 match {
    case t1:DenseTensor1 => t2 match {
      case t2:DenseTensor2 => { val t = new DenseTensor3(t1.dim1, t2.dim1, t2.dim2); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1; k <- 0 until t2.dim2) t(i,j,k) = t1(i) * t2(j,k); t }
    }
  }
  def outer(t1:Tensor1, t2:Tensor1, t3:Tensor1): Tensor3 = t1 match {
    case t1:SingletonBinaryTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => t3 match {
        case t3:SingletonBinaryTensor1 => new SingletonBinaryTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex)
        case t3:SingletonTensor1 => new SingletonTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex, t3.singleValue)
        case t3:Tensor1 => new Singleton2BinaryLayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t3)
      }
      case t2:SingletonTensor1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, 1.0, t2.singleValue, t3)
      }
    }
    case t1:SingletonTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue, 1.0, t3)
      }
      case t2:SingletonTensor1 => t3 match {
        case t3:Tensor1 => new Singleton2LayeredTensor3(t1.dim1, t2.dim1, t3.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue, t2.singleValue, t3)
      }
    }
  }
  def outer(t1:Tensor1, t2:Tensor1, t3:Tensor1, t4:Tensor1): Tensor4 = t1 match {
    case t1:SingletonBinaryTensor1 => t2 match {
      case t2:SingletonBinaryTensor1 => t3 match {
        case t3:SingletonBinaryTensor1 => t4 match {
          case t4:SingletonBinaryTensor1 => new SingletonBinaryTensor4(t1.dim1, t2.dim1, t3.dim1, t4.dim1, t1.singleIndex, t2.singleIndex, t3.singleIndex, t4.singleIndex)
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


/** A lazy product of a Vector and a scalar.
    @author Andrew McCallum */
class TensorTimesScalar(val tensor:Tensor, val scalar:Double) extends Tensor {
  def numDimensions: Int = tensor numDimensions
  def dimensions: Array[Int] = tensor.dimensions
  // For handling sparsity
  def activeDomain: IntSeq = tensor.activeDomain
  def activeDomains: Array[IntSeq] = tensor.activeDomains
  def isDense: Boolean = tensor.isDense
  def length = tensor.length
  //def activeDomainSize: Int = vector.activeDomainSize
  def dot(t:Tensor): Double = tensor.dot(t) * scalar
  def *(scalar:Double) = new TensorTimesScalar(tensor, scalar*this.scalar)
  //override def update(i:Int, v:Double): Unit = tensor.update(idx, value/scalar)
  //override def +=(v: Vector) { vector += v*(1.0/scalar) }
  def apply(index:Int) = tensor.apply(index) * scalar
}

trait DenseTensor extends Tensor {
  protected def _values: Array[Double]
  override def zero(): Unit = java.util.Arrays.fill(_values, 0.0)
  override def dot(t2:DoubleSeq): Double = t2 match {
    case t2:SingletonTensor => apply(t2.singleIndex) * t2.singleValue
    case t2:SingletonBinaryTensor => apply(t2.singleIndex)
    // TODO Any other special cases here?
    case t2:DenseTensor => {
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
    case t2:DoubleSeq => { // TODO Consider removing this to catch inefficiency
      val len = length; assert(len == t2.length); var result = 0.0; var i = 0
      while (i < len) { result += apply(i) * t2(i); i += 1 }; result
    }
  }

}

trait SingletonBinaryTensor extends Tensor {
  def singleIndex: Int
  def isDense = false
  def activeDomain: IntSeq = new SingletonIntSeq(singleIndex)
  def apply(i:Int) = if (i == singleIndex) 1.0 else 0.0
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

trait SingletonTensor extends SingletonBinaryTensor {
  def singleValue: Double
  override def apply(i:Int) = if (i == singleIndex) singleValue else 0.0
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

trait UniformTensor extends Tensor {
  def uniformValue: Double
  def apply(i:Int) = uniformValue
  def isDense = true
  def activeDomain: IntSeq = new RangeIntSeq(0, length)
  override def sum: Double = length * uniformValue
  override def max: Double = uniformValue
  override def min: Double = uniformValue
  override def maxIndex: Int = 0
  override def containsNaN: Boolean = false
  override def dot(v:DoubleSeq): Double = v.sum * uniformValue
  override def copy = this // safe because it is immutable
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
