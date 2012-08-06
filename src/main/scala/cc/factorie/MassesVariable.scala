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


package cc.factorie
import cc.factorie._
import cc.factorie.la._
import cc.factorie.util.{DoubleSeq,IntSeq}
import scala.util.Random

/** A Tensor containing only non-negative entries.  These are also the basis for Proportions. */
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
  override def toString = this.asSeq.take(10).mkString(stringPrefix+"(", ",", if (length > 10) "...)" else ")")
}

// TODO Should we get rid of all these combinations and make users extend the combinations themselves? -akm
trait Masses1 extends Tensor1 with Masses
trait Masses2 extends Tensor2 with Masses
trait Masses3 extends Tensor3 with Masses
trait Masses4 extends Tensor4 with Masses

/** Provides a protected var for holding the massTotal */
trait MassesWithTotal extends Masses {
  protected var _massTotal: Double = 0.0
  //def resetMassTotal: Unit = _massTotal = super.sum
  def massTotal = _massTotal
  final override def sum = _massTotal
  override def update(i:Int, v:Double): Unit = throw new Error("Masses cannot be modified by udpate; use += instead.")
}

trait DenseMassesWithTotal extends DenseTensor with MassesWithTotal {
  final override def zero(): Unit = { super.zero(); _massTotal = 0.0 }
  final override def +=(i:Int, v:Double): Unit = { _massTotal += v; _values(i) += v; assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
  final override def *=(d:Double): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)*d; _massTotal += v; _values(i) = v; i += 1 }}
  final override def *=(ds:DoubleSeq): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)*ds(i); _massTotal += v; _values(i) = v; i += 1 }}
  final override def /=(ds:DoubleSeq): Unit = { _massTotal = 0.0; val l = length; var i = 0; var v = 0.0; while (i < l) { v = _values(i)/ds(i); _massTotal += v; _values(i) = v; i += 1 }}
  final override def +=(ds:DoubleSeq, f:Double): Unit = ds.foreachElement((i,v) => { _values(i) += v*f; _massTotal += v*f })
  final override def :=(v:Double): Unit = { java.util.Arrays.fill(_values, v); _massTotal = v * length }
  final override def :=(ds:DoubleSeq): Unit = { _massTotal = 0.0; var l = length; var v = 0.0; var i = 0; while (i < l) { v = ds(i); assert(v >= 0.0); _values(i) = v; _massTotal += v; i += 1 } }
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
  def massTotal = dim1 * uniformValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

class SingletonMasses1(dim1:Int, singleIndex:Int, singleValue:Double) extends SingletonTensor1(dim1, singleIndex, singleValue) with Masses1 {
  def massTotal = singleValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}

class GrowableDenseMasses1(val sizeProxy:Iterable[Any]) extends GrowableDenseTensorLike1 with Masses1 with MassesWithTotal {
  override def +=(i:Int, v:Double): Unit = { _massTotal += v; super.+=(i, v); assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
}

class GrowableUniformMasses1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends Masses1 with UniformTensor /*Like1 with Masses1*/ {
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def dim1 = sizeProxy.size
  def massTotal = sizeProxy.size * uniformValue
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

class SortedSparseCountsMasses1(val dim1:Int) extends cc.factorie.util.SortedSparseCounts(dim1, 4, false) with Masses1 {
  def isDense = false
  def activeDomain1 = activeIndices
  //def activeDomain = activeDomain1
  def apply(index:Int): Double = {
    if (countsTotal == 0) 0.0
    else countOfIndex(index).toDouble
  }
  override def +=(index:Int, incr:Double): Unit = {
    assert(incr.floor == incr)
    incrementCountAtIndex(index, incr.toInt) 
  }
  override def zero(): Unit = clear()
  def massTotal = countsTotal.toDouble
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = {
    if (countsTotal == 0) r.nextInt(dim1) // If there are no counts, use a uniform distribution
    else {
      val sampledMass = r.nextInt(countsTotal)
      var i = 0; var sum = countAtPosition(0)
      while (sum < sampledMass) {
        i += 1
        sum += countAtPosition(i)
      }
      indexAtPosition(i)
    }
  }
}

// Masses Variables 

//trait MassesVar extends TensorVar[Masses] with VarAndValueType[MassesVar,Masses] 
//class MassesVariable extends TensorVariable[Masses] with MassesVar {

trait MassesVar extends TensorVar with VarAndValueType[MassesVar,Masses] 
class MassesVariable extends TensorVariable with MassesVar {
  def this(initialValue:Masses) = { this(); _set(initialValue) }
  def domain = TensorDomain
}

object MassesVariable {
  def dense(dim1:Int) = new MassesVariable(new DenseMasses1(dim1))
  def dense(dim1:Int, uniformValue:Double) = new MassesVariable(new DenseMasses1(dim1, uniformValue))
  def growableDense(sizeProxy:Iterable[Any]) = new MassesVariable(new GrowableDenseMasses1(sizeProxy))
  def growableUniform(sizeProxy:Iterable[Any], uniformValue:Double) = new MassesVariable(new GrowableUniformMasses1(sizeProxy, uniformValue))
  def sortedSparseCounts(dim1:Int) = new MassesVariable(new SortedSparseCountsMasses1(dim1))
}