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
  def dimensionsMatch(t:Tensor): Boolean = dimensions.toSeq equals t.dimensions.toSeq // Inefficient; override in subclasses
  def ensureDimensionsMatch(t:Tensor): Unit = require(dimensions.toSeq == t.dimensions.toSeq) // Inefficient; override in subclasses
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
  def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName) // TODO Rename this setZero, to avoid conflict with scala.math.Numeric so that RealValue can inherit from it.
  def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
  def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
  def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
  def *(v:Double): Tensor = new TensorTimesScalar(this.copy, v) // TODO Should I use this.copy here?
  def /(v:Double): Tensor = new TensorTimesScalar(this.copy, 1.0/v) // TODO Should I use this.copy here?
  def +(that:Tensor): Tensor = { val t = this.copy; t += that; t }
  def -(that:Tensor): Tensor = { val t = this.copy; t -= that; t }
  def *(that:Tensor): Tensor = throw new Error("Not yet implemented")
  def /(that:Tensor): Tensor = throw new Error("Not yet implemented")
  def normalized: Tensor = { val t = copy; t.normalize; t } // TODO Make this return Proportions, then fix BP
  def expNormalized: Tensor = { val t = copy; t.expNormalize; t } // TODO Make this return Proportions, then fix BP
  def isUniform = false
  def stringPrefix = getClass.getName // "Tensor"
  def printLength = 50
  override def toString = { val suffix = if (length > printLength) "...)" else ")"; this.asSeq.take(printLength).mkString(stringPrefix+"(", ",", suffix) }
}

object Tensor {
  
  def tabulate(dim1:Int)(f:Int=>Double): DenseTensor1 = {
    val t = new DenseTensor1(dim1)
    var i = 0;
    while (i < dim1) { t(i) = f(i); i += 1 }
    t
  }
  
  // Support for creating new empty Tensors with dimensions matching an argument
  def newDense(t:Tensor): Tensor = t match {
    case t:WeightsTensor => new WeightsTensor(dotFamily => la.Tensor.newDense(dotFamily.weights))
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
  def newSparse(t:Tensor): Tensor = t match {
    case t:WeightsTensor => new WeightsTensor(dotFamily => la.Tensor.newSparse(dotFamily.weights))
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
  
  def sum(tensors:Iterable[Tensor]): Tensor = tensors.size match {
    case 1 => tensors.head.copy // Because some callers may rely on being able to do mutate-in-place operations on the results
    case 2 => tensors.head + tensors.last
    case _ => tensors.head + sum(tensors.tail)
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
    case t1:Tensor2 => t2 match {
      case t2:Tensor2 => outer(t1, t2)
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
          val t2Arr = t2.activeDomain1.array; val t3Arr = t3.activeDomain1.array
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




