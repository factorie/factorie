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

package cc.factorie.variable

import cc.factorie.la._
import cc.factorie.util.{DoubleSeq, SparseDoubleSeq}

import scala.util.Random

/** A Tensor containing only non-negative entries.  These are also the basis for Proportions.
    @author Andrew McCallum */
trait Masses extends Tensor {
  def massTotal: Double
  override def sum = massTotal
  /** Get a normalized entry in this Masses, which can be interpreted as a probability. */
  def pr(index:Int): Double = {
    val mt = massTotal
    if (mt == 0.0) 1.0 / length else apply(index) / mt
  }
  def logpr(index:Int) = math.log(pr(index))
  override def sampleIndex(implicit r:Random): Int = sampleIndex(massTotal)(r)
  override def stringPrefix = "Masses"
  def maxToStringLength = 10
  override def toString = this.asSeq.take(maxToStringLength).mkString(stringPrefix+"(", ",", if (length > 10) "...)" else ")")
}

// TODO Should we get rid of all these combinations and make users extend the combinations themselves? -akm
trait Masses1 extends Tensor1 with Masses
trait Masses2 extends Tensor2 with Masses
trait Masses3 extends Tensor3 with Masses
trait Masses4 extends Tensor4 with Masses

/** A Masses that provides a protected var for holding the massTotal.
    @author Andrew McCallum */
trait MassesWithTotal extends Masses {
  protected var _massTotal: Double = 0.0
  def massTotal = _massTotal
  final override def sum = _massTotal
  override def update(i:Int, v:Double): Unit = throw new Error("Masses cannot be modified by update; use += instead.")
}

/** A class for arbitrary tensors to become Masses. E.g.: GrowableSparseHashMasses1
    @author Dirk Weissenborn */
trait WrappedTensorMasses[A <: Tensor] extends WrappedTensor[A] with MassesWithTotal {
  //initialize massTotal
  require(tensor.forallActiveElements { case (_:Int,v:Double) => v >= 0 } )
  _massTotal = tensor.sum

  final override def zero(): Unit = { tensor.zero(); _massTotal = 0.0 }              //this might be a little slow
  final override def +=(i:Int, v:Double): Unit = { _massTotal += v; tensor.+=(i,v)/*; assert(_massTotal >= 0.0); assert(tensor(i) >= 0.0)*/ }
  final override def update(i: Int, v: Double): Unit = {this += (i,v - this(i))}
  final override def *=(d:Double): Unit = { _massTotal *= d;  tensor*=d}
  final override def *=(ds:DoubleSeq): Unit = { tensor*=ds;_massTotal=tensor.sum}
  final override def /=(ds:DoubleSeq): Unit = { tensor/=ds;_massTotal=tensor.sum}
  final override def +=(ds:DoubleSeq, f:Double): Unit = { tensor.+=(ds,f); _massTotal += ds.sum }
  final override def :=(v:Double): Unit = { tensor.:=(v); _massTotal = v * tensor.activeDomainSize }
  final override def :=(ds:DoubleSeq): Unit = { tensor.:=(ds); _massTotal += tensor.sum}
  final override def :=(ds: Array[Double]) : Unit = { this := new DenseTensor1(ds) }
}

class WrappedTensorMasses1[A <: Tensor1](val tensor:A) extends WrappedTensorMasses[A] with WrappedTensor1[A] with Masses1
class WrappedTensorMasses2[A <: Tensor2](val tensor:A) extends WrappedTensorMasses[A] with WrappedTensor2[A] with Masses2
class WrappedTensorMasses3[A <: Tensor3](val tensor:A) extends WrappedTensorMasses[A] with WrappedTensor3[A] with Masses3
class WrappedTensorMasses4[A <: Tensor4](val tensor:A) extends WrappedTensorMasses[A] with WrappedTensor4[A] with Masses4


/** A DenseTensor Masses that provides a protected var for holding the massTotal.
    @author Andrew McCallum */
trait DenseMassesWithTotal extends DenseTensor with MassesWithTotal {
  final override def zero(): Unit = { super.zero(); _massTotal = 0.0 }
  final override def +=(i:Int, v:Double): Unit = { _massTotal += v; _values(i) += v; assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
  final override def update(i: Int, v: Double): Unit = {this += (i,v - this(i))}
  final override def *=(d:Double): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)*d; _massTotal += v; _values(i) = v; i += 1 }}
  final override def *=(ds:DoubleSeq): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)*ds(i); _massTotal += v; _values(i) = v; i += 1 }}
  final override def /=(ds:DoubleSeq): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)/ds(i); _massTotal += v; _values(i) = v; i += 1 }}
  final override def +=(ds:DoubleSeq, f:Double): Unit = ds.foreachElement((i,v) => { _values(i) += v*f; _massTotal += v*f })
  final override def :=(v:Double): Unit = { java.util.Arrays.fill(_values, v); _massTotal = v * length }
  final override def :=(ds:DoubleSeq): Unit = { _massTotal = 0.0; val l = length; var v = 0.0; var i = 0; while (i < l) { v = ds(i); assert(v >= 0.0); _values(i) = v; _massTotal += v; i += 1 } }
  final override def :=(ds: Array[Double]) : Unit = { this := new DenseTensor1(ds) }
}

//trait DenseMasses extends ... (gather += in here, but we need a DenseTensor class also)
class DenseMasses1(val dim1:Int) extends DenseTensorLike1 with Masses1 with DenseMassesWithTotal {
  def this(dim1:Int, uniformValue:Double) = { this(dim1); this := uniformValue }
  override def copy: DenseMasses1 = { val c = new DenseMasses1(dim1); c := this; c }
}
class DenseMasses2(val dim1:Int, val dim2:Int) extends DenseTensorLike2 with Masses2 with DenseMassesWithTotal {
  override def +=(i:Int, j:Int, v:Double): Unit = { _massTotal += v; val index = singleIndex(i, j); _values(index) += v; assert(_massTotal >= 0.0); assert(_values(index) >= 0.0) }
  override def copy: DenseMasses2 = { val c = new DenseMasses2(dim1, dim2); c := this; c }
}
class DenseMasses3(val dim1:Int, val dim2:Int, val dim3:Int) extends DenseTensorLike3 with Masses3 with DenseMassesWithTotal {
  override def +=(i:Int, j:Int, k:Int, v:Double): Unit = { _massTotal += v; val index = singleIndex(i, j, k); _values(index) += v; assert(_massTotal >= 0.0); assert(_values(index) >= 0.0) }
  override def copy: DenseMasses3 = { val c = new DenseMasses3(dim1, dim2, dim3); c := this; c }
}
class DenseMasses4(val dim1:Int, val dim2:Int, val dim3:Int, val dim4:Int) extends DenseTensorLike4 with Masses4 with DenseMassesWithTotal {
  override def +=(i:Int, j:Int, k:Int, l:Int, v:Double): Unit = { _massTotal += v; val index = singleIndex(i, j, k, l); _values(index) += v; assert(_massTotal >= 0.0); assert(_values(index) >= 0.0) }
  override def copy: DenseMasses4 = { val c = new DenseMasses4(dim1, dim2, dim3, dim4); c := this; c }
}

class UniformMasses1(dim1:Int, uniformValue:Double) extends UniformTensor1(dim1, uniformValue) with Masses1 with UniformTensor {
  override val massTotal = dim1 * uniformValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

class SingletonMasses1(dim1:Int, singleIndex:Int, singleValue:Double) extends SingletonTensor1(dim1, singleIndex, singleValue) with Masses1 {
  def massTotal = singleValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}

class SingletonMasses2(dim1:Int, dim2: Int, singleIndex1:Int, singleIndex2: Int, singleValue:Double) extends SingletonTensor2(dim1, dim2, singleIndex1, singleIndex2, singleValue) with Masses2 {
  def massTotal = singleValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}

class GrowableDenseMasses1(sizeProxy:Iterable[Any]) extends ProxyGrowableDenseTensor1(sizeProxy) with Masses1 with MassesWithTotal {
  override def +=(i:Int, v:Double): Unit = { _massTotal += v; super.+=(i, v); assert(_massTotal >= 0.0, "_masstotal is negative: "+_massTotal); assert(_values(i) >= 0.0, "Negative value " + i + " " + _values(i)) }
}

class GrowableUniformMasses1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends Masses1 with UniformTensor /*Like1 with Masses1*/ {
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, dim1)
  def dim1 = sizeProxy.size
  def massTotal = sizeProxy.size * uniformValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

class SortedSparseCountsMasses1(val dim1:Int) extends SparseDoubleSeq with Masses1 {
  val sparseCounts = new cc.factorie.util.SortedSparseCounts(dim1, 4, false)
  def activeDomainSize = sparseCounts.activeIndices.length

  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  def isDense = false
  override def foreachActiveElement(f: (Int, Double) => Unit) { sparseCounts.activeIndices.foreach(i => f(i, this(i))) }
  def activeDomain = sparseCounts.activeIndices
  //def activeDomain = activeDomain1
  def apply(index:Int): Double = {
    if (sparseCounts.countsTotal == 0) 0.0
    else sparseCounts.countOfIndex(index).toDouble
  }
  override def +=(index:Int, incr:Double): Unit = {
    assert(incr.floor == incr)
    sparseCounts.incrementCountAtIndex(index, incr.toInt)
  }
  def update(i: Int, v: Double) = this += (i, v - this(i))
  override def zero(): Unit = sparseCounts.clear()
  def massTotal = sparseCounts.countsTotal.toDouble
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = {
    if (sparseCounts.countsTotal == 0) r.nextInt(dim1) // If there are no counts, use a uniform distribution
    else {
      val sampledMass = r.nextInt(sparseCounts.countsTotal)
      var i = 0; var sum = sparseCounts.countAtPosition(0)
      while (sum < sampledMass) {
        i += 1
        sum += sparseCounts.countAtPosition(i)
      }
      sparseCounts.indexAtPosition(i)
    }
  }

}


// Masses Variables 

///** The domain type for MassesVar.  No real functionality, just used as a marker.
//    @author Andrew McCallum */
//trait MassesDomain extends TensorDomain with Domain[Masses]
//
///** The domain for MassesVar.  No real functionality, just used as a marker.
//    @author Andrew McCallum */
//object MassesDomain extends MassesDomain

/** An abstract variable with value Masses.
    @author Andrew McCallum */
trait MassesVar extends TensorVar {
  type Value <: Masses
  def value: Value
  //def domain: MassesDomain
}

/** An abstract variable with value Masses.
    @author Andrew McCallum */
trait MutableMassesVar extends MutableTensorVar with MassesVar {
  type Value <: Masses
}

/** A variable with value Masses.
    @author Andrew McCallum */
class MassesVariable extends MutableMassesVar {
  //def domain = MassesDomain
  type Value = Masses
  def this(initialValue:Masses) = { this(); set(initialValue)(null) }
}

/** Convenience methods for constructing MassesVariables with a Masses1 of various types.
    @author Andrew McCallum */
object MassesVariable {
  def dense(dim1:Int) = new MassesVariable(new DenseMasses1(dim1))
  def dense(dim1:Int, uniformValue:Double) = new MassesVariable(new DenseMasses1(dim1, uniformValue))
  def growableDense(sizeProxy:Iterable[Any]) = new MassesVariable(new GrowableDenseMasses1(sizeProxy))
  def growableUniform(sizeProxy:Iterable[Any], uniformValue:Double) = new MassesVariable(new GrowableUniformMasses1(sizeProxy, uniformValue))
  def sortedSparseCounts(dim1:Int) = new MassesVariable(new SortedSparseCountsMasses1(dim1))

  implicit def toMasses1(tensor:Tensor1) = new WrappedTensorMasses1(tensor)
  implicit def toMasses2(tensor:Tensor2) = new WrappedTensorMasses2(tensor)
  implicit def toMasses3(tensor:Tensor3) = new WrappedTensorMasses3(tensor)
  implicit def toMasses4(tensor:Tensor4) = new WrappedTensorMasses4(tensor)
}
