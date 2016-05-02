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
import cc.factorie.util.{IntSeq, MutableDoubleSeq, DoubleSeq, DenseDoubleSeq}

import scala.util.Random


// Note: Many Tensor-like methods are actually implemented in DoubleSeq
// Tensor adds capabilities for copying, and more explicit sparsity.

// TODO Consider having all Tensor += methods return this.type.  Question: will this reduce efficiency? -akm

/** An N-dimensional collection of Doubles. */
trait Tensor extends MutableDoubleSeq with Serializable {
  def numDimensions: Int
  def dimensions: Array[Int]
  // For handling sparsity
  def activeDomain: IntSeq
  def activeDomains: Array[IntSeq]
  def isDense: Boolean
  def dimensionsMatch(t:Tensor): Boolean = dimensions.toSeq equals t.dimensions.toSeq // Inefficient; override in subclasses
  def ensureDimensionsMatch(t:Tensor): Unit = require(dimensions.toSeq == t.dimensions.toSeq) // Inefficient; override in subclasses
  def activeDomainSize: Int
  /** The default value at indices not covered by activeDomain.  Subclasses may override this  */
  def defaultValue: Double = 0.0 // TODO This is not actually yet properly used by subclasses
  // this method lets us aggregate without boxing -luke
  // (idx, value, acc)
  def foldActiveElements(seed: Double, f: (Int, Double, Double) => Double): Double = {
    var acc = seed
    foreachActiveElement((i, v) => acc = f(i, v, acc))
    acc
  }
  // TODO!! Change this to use TensorElementIterator instead
  def activeElements: Iterator[(Int,Double)] = (for (i <- activeDomain.toArray) yield (i, apply(i))).iterator
  def forallActiveElements(f:(Int,Double)=>Boolean): Boolean // = forallElements(f) // To be override for efficiency in subclasses
  def exists(f:(Double)=>Boolean): Boolean = !forallActiveElements((i,v) => !f(v))
  def outer(t:Tensor): Tensor = Tensor.outer(this, t)
  def dot(ds:DoubleSeq): Double
  def cosineSimilarity(t:DoubleSeq): Double = {
    val numerator:Double = this dot t
    val denominator:Double = this.twoNorm * t.twoNorm
    if (denominator == 0.0 || denominator != denominator) 0.0 else numerator/denominator
  }

  // TODO: consider removing these because we could be copying an immutable Tensor or an inefficient representation
  def *(v:Double): Tensor = {val c = this.copy; c *= v; c}// TODO Should I use this.copy here?
  def /(v:Double): Tensor = {val c = this.copy; c /= v; c} // TODO Should I use this.copy here?
  def +(that:Tensor): Tensor = { val t = this.copy; t += that; t }
  def -(that:Tensor): Tensor = { val t = this.copy; t -= that; t }
  
  def normalized: Tensor = { val t = copy; t.normalize(); t } // TODO Make this return Proportions, then fix BP
  def projected(maxNorm:Double): Tensor = { val t = copy; t.project(maxNorm); t }
  def expNormalized: Tensor = { val t = copy; t.expNormalize(); t } // TODO Make this return Proportions, then fix BP
  def isUniform = false
  def stringPrefix = getClass.getName // "Tensor"
  def printLength = 50
  override def toString = { val suffix = if (length > printLength) "...)" else ")"; this.asSeq.take(printLength).mkString(stringPrefix+"(", ",", suffix) }
  def ++=(tensors:Iterable[Tensor]): this.type = { tensors.foreach(t => this += t); this }
  // Methods for mutability not implemented in all Tensors
  def +=(i:Int, incr:Double): Unit // also defined in MutableDoubleSeq
  def zero(): Unit
  def update(i:Int, v:Double): Unit
  def copy: Tensor
  def blankCopy: Tensor
}

/** A class for arbitrary tensors to become Masses.
    @author Dirk Weissenborn */
//TODO: as of scala 2.11 a macro can be used here (@delegate)
trait WrappedTensor[A <: Tensor] extends Tensor {
  def tensor:A

  def activeDomain = tensor.activeDomain
  def isDense = tensor.isDense
  def activeDomainSize = tensor.activeDomainSize
  def forallActiveElements(f: (Int, Double) => Boolean) = tensor.forallActiveElements(f)
  def dot(ds: DoubleSeq) = tensor.dot(ds: DoubleSeq)
  def copy = tensor.copy
  def blankCopy = tensor.blankCopy
  def apply(i: Int) = tensor.apply(i)
  def foreachActiveElement(f: (Int, Double) => Unit) = tensor.foreachActiveElement(f)
  def =+(a: Array[Double], offset: Int, f: Double) = tensor.=+(a,offset,f)
  def max = tensor.max
  def min = tensor.min
  def indexOf(d: Double) = tensor.indexOf(d)
  def contains(d: Double) = tensor.contains(d)
  def oneNorm = tensor.oneNorm
  def twoNormSquared = tensor.twoNormSquared
  def infinityNorm = tensor.infinityNorm
  def maxIndex = tensor.maxIndex
  def maxIndex2 = tensor.maxIndex2
  def toArray = tensor.toArray
  def containsNaN = tensor.containsNaN
  def sampleIndex(normalizer: Double)(implicit r: Random) = tensor.sampleIndex(normalizer)(r)
  def update(i: Int, v: Double) = tensor.update(i,v)
  def sum = tensor.sum
  def zero(): Unit = tensor.zero()
  def +=(i:Int, v:Double): Unit = tensor.+=(i:Int, v:Double)

}

trait WrappedTensor1[A <: Tensor1] extends WrappedTensor[A] with Tensor1 {
  def dim1 = tensor.dim1
}
trait WrappedTensor2[A <: Tensor2] extends WrappedTensor[A] with Tensor2 {
  def dim1 = tensor.dim1
  def dim2 = tensor.dim2
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
}
trait WrappedTensor3[A <: Tensor3] extends WrappedTensor[A] with Tensor3 {
  def dim1 = tensor.dim1
  def dim2 = tensor.dim2
  def dim3 = tensor.dim3
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
  def activeDomain3 = tensor.activeDomain3
}
trait WrappedTensor4[A <: Tensor4] extends WrappedTensor[A] with Tensor4 {
  def dim1 = tensor.dim1
  def dim2 = tensor.dim2
  def dim3 = tensor.dim3
  def dim4 = tensor.dim4
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
  def activeDomain3 = tensor.activeDomain3
  def activeDomain4 = tensor.activeDomain4
}


trait ReadOnlyTensor extends Tensor {
  // Methods for mutability not implemented in all Tensors
  final override def +=(i:Int, incr:Double): Unit = throw new Error("Method +=(Int,Double) not defined on class "+getClass.getName)
  final override def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName) // TODO Rename this setZero, to avoid conflict with scala.math.Numeric so that RealValue can inherit from it.
  final override def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
}

object Tensor {
  
  def tabulate(dim1:Int)(f:Int=>Double): DenseTensor1 = {
    val t = new DenseTensor1(dim1)
    var i = 0
    while (i < dim1) { t(i) = f(i); i += 1 }
    t
  }
  
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
  def newGrowableDense(dims:Array[Int]): Tensor = dims.length match {
    case 1 => new GrowableDenseTensor1(dims(0))
    case 2 => new GrowableDenseTensor2(dims(0), dims(1))
    case 3 => new GrowableDenseTensor3(dims(0), dims(1), dims(2))
  }
  def newSparse(t:Tensor): Tensor = {
    t match {
      case t:DenseLayeredTensor2 if t.inner(0).isInstanceOf[DenseDoubleSeq] => new DenseLayeredTensor2(t.dim1, t.dim2, i => new SparseIndexedTensor1(i))
      case t:Tensor1 => new SparseTensor1(t.dim1)
      case t:Tensor2 => new SparseIndexedTensor2(t.dim1, t.dim2)
      case t:Tensor3 => new SparseIndexedTensor3(t.dim1, t.dim2, t.dim3)
      case t:Tensor4 => new SparseIndexedTensor4(t.dim1, t.dim2, t.dim3, t.dim4)
    }
  }
  def newSparse(dims:Int*): Tensor = {
    dims match {
      case Seq(d1) => new SparseTensor1(d1)
      case Seq(d1, d2) => new SparseIndexedTensor2(d1, d2)
      case Seq(d1, d2, d3) => new SparseIndexedTensor3(d1, d2, d3)
      case Seq(d1, d2, d3, d4) => new SparseIndexedTensor4(d1, d2, d3, d4)
    }
  }
  def newSparse(dims:Array[Int]): Tensor = {
    dims.length match {
      case 1 => new SparseTensor1(dims(0))
      case 2 => new SparseIndexedTensor2(dims(0), dims(1))
      case 3 => new SparseIndexedTensor3(dims(0), dims(1), dims(2))
      case 4 => new SparseIndexedTensor4(dims(0), dims(1), dims(2), dims(3))
    }
  }
  
//  def sum(tensors:Iterable[Tensor]): Tensor = tensors.size match {
//    case 1 => tensors.head.copy // Because some callers may rely on being able to do mutate-in-place operations on the results
//    case 2 => tensors.head + tensors.last
//    case _ => {
//      // Was: 
//      tensors.head + sum(tensors.tail)  //Implementation below avoids lots of copies.
////      var result: Tensor = null
////      for (t <- tensors) if (result eq null) result = t.copy else result += t
////      assert(!result.isInstanceOf[UniformTensor]) // Temporary test to catch a bug.
////      result
//    }
//  }

  // Support for dot inner products with dense tensors
  def dot(t1:DenseTensor, t2:DenseTensor): Double = {
    val len = t1.length; assert(len == t2.length); var result = 0.0; var i = 0
    while (i < len) { result += t1(i) * t2(i); i += 1 }; result
  }
  
  // Support for outer products between tensors
  def outer(t1:Tensor, t2:Tensor): Tensor = t1 match {
    case t1:Tensor1 => t2 match {
      case t2:Tensor1 => outer(t1, t2)
      case t2:Tensor2 => outer(t1, t2)
    }
    case t1:Tensor2 => t2 match {
      case t2:Tensor2 => outer(t1, t2)
      case t2:Tensor1 => new Outer2Tensor3(t1, t2)
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
  def outer(t1:Tensor1, t2:Tensor1): Tensor2 = {
    (t1,t2) match {
      case (t1: SingletonBinaryTensorLike1, t2: SingletonBinaryTensorLike1) =>
        new SingletonBinaryTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex)
      case (t1: SingletonTensor1, t2: SingletonTensor1) =>
        new SingletonTensor2(t1.dim1, t2.dim1, t1.singleIndex, t2.singleIndex, t1.singleValue*t2.singleValue)
      case _ =>
        new Outer1Tensor2(t1, t2)
    }
  }
  def outer(t1:Tensor1, t2:Tensor2): Tensor3 = t1 match {
    case t1:DenseTensorLike1 => t2 match {
      case t2:DenseTensorLike2 => { val t = new DenseTensor3(t1.dim1, t2.dim1, t2.dim2); for (i <- 0 until t1.dim1; j <- 0 until t2.dim1; k <- 0 until t2.dim2) t(i,j,k) = t1(i) * t2(j,k); t }
    }
    case _ => new Outer1Tensor3(t1, t2)
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
          val t2Arr = t2.activeDomain1.asArray; val t3Arr = t3.activeDomain1.asArray
          val t2Len = t2.activeDomain1.length; val t3Len = t3.activeDomain1.length
          var j = 0
          while (j < t2Len) {
            var k = 0
            while (k < t3Len) {
              t.update(t1.singleIndex, t2Arr(j), t3Arr(k), 1.0)
              k += 1
            }
            j += 1
          }
          t
        }
        // TODO: see Diego's email on factorie-discuss, 9/12/2012
        case t3:SingletonBinaryTensorLike1 => throw new Error("Not yet implemented: you might want to try to reorder your template so that singleton variables come first.")
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

// Not used, and sub-classes don't current support anyway
trait TensorWithMutableDefaultValue extends Tensor {
  def defaultValue_=(v:Double): Unit
  def defaultValue_+=(v:Double): Unit = defaultValue_=(defaultValue + v)
  def defaultValue_*=(v:Double): Unit = defaultValue_=(defaultValue * v)
}




