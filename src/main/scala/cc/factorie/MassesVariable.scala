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

trait Masses extends Tensor {
  def massTotal: Double
  override def sampleIndex(implicit r:Random): Int = sampleIndex(massTotal)(r) //cc.factorie.maths.nextDiscrete(this.asArray, massTotal)(r)
}
trait IncrementableMasses extends IncrementableTensor with Masses 
/** Provides a protected var for holding the massTotal */
trait IncrementableMassesWithTotal extends IncrementableMasses {
  protected var _massTotal: Double = 0.0
  //def resetMassTotal: Unit = _massTotal = super.sum
  def massTotal = _massTotal
  final override def sum = _massTotal
}

// TODO Should we get rid of all these combinations and make users extend the combinations themselves? -akm
trait Masses1 extends Tensor1 with Masses
trait Masses2 extends Tensor2 with Masses
trait IncrementableMasses1 extends IncrementableTensor1 with IncrementableMasses with Masses1
trait IncrementableMasses2 extends IncrementableTensor2 with IncrementableMasses with Masses2

//trait DenseMasses extends ... (gather += in here, but we need a DenseTensor class also)
class DenseMasses1(val dim1:Int) extends IncrementableDenseTensorLike1 with IncrementableMasses1 with IncrementableMassesWithTotal {
  override def +=(i:Int, v:Double): Unit = { _massTotal += v; _values(i) += v; assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
}
class DenseMasses2(val dim1:Int, val dim2:Int) extends IncrementableDenseTensorLike2 with IncrementableMasses2 with IncrementableMassesWithTotal {
  override def +=(i:Int, v:Double): Unit = { _massTotal += v; _values(i) += v; assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
}
class UniformMasses1(dim1:Int, uniformValue:Double) extends UniformTensor1(dim1, uniformValue) with Masses1 {
  def massTotal = dim1 * uniformValue
}
class SingletonMasses1(dim1:Int, singleIndex:Int, singleValue:Double) extends SingletonTensor1(dim1, singleIndex, singleValue) with Masses1 {
  def massTotal = singleValue
}

class GrowableDenseMasses1(val sizeProxy:Iterable[Any]) extends GrowableDenseTensorLike1 with IncrementableMasses1 with IncrementableMassesWithTotal {
  override def +=(i:Int, v:Double): Unit = { _massTotal += v; _values(i) += v; assert(_massTotal >= 0.0); assert(_values(i) >= 0.0) }
}
class GrowableUniformMasses1(val sizeProxy:Iterable[Any], val uniformValue:Double) extends UniformTensorLike1 with Masses1 {
  def dim1 = sizeProxy.size
  def massTotal = sizeProxy.size * uniformValue
}

class SortedSparseCountsMasses1(val dim1:Int) extends cc.factorie.util.SortedSparseCounts(dim1, 4, false) with IncrementableMasses1 {
  def isDense = false
  def activeDomain1 = throw new Error("Not implemented")
  def apply(index:Int): Double = {
    if (countsTotal == 0) 0.0
    else countOfIndex(index).toDouble
  }
  def +=(index:Int, incr:Double): Unit = {
    assert(incr.floor == incr)
    incrementCountAtIndex(index, incr.toInt) 
  }
  def massTotal = countsTotal.toDouble
}

// Masses Variables 

trait MassesVar extends TensorVar with VarAndValueType[MassesVar,Masses] 
class MassesVariable extends TensorVariable with MassesVar {
  def this(initialValue:Masses) = { this(); _set(initialValue) }
}

trait IncrementableMassesVar extends MassesVar with VarAndValueType[IncrementableMassesVar,IncrementableMasses] 
class IncrementableMassesVariable extends MassesVariable with IncrementableMassesVar { 
  def this(initialValue:IncrementableMasses) = { this(); _set(initialValue) }
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    if (d ne null) d += new IncrementableMassesDiff(index, incr)
    tensor.+=(index, incr)
  }
  def increment(incr:DoubleSeq)(implicit d:DiffList): Unit = {
    require(incr.length == tensor.length)
    if (d ne null) d += new IncrementableMassesDoubleSeqDiff(incr) // TODO perhaps for safety we should copy it first?
    tensor += incr
  }
  def zero(implicit d:DiffList): Unit = {
    if (d ne null) d += new IncrementableMassesZeroDiff(DoubleSeq(tensor.toArray))
    tensor.zero()
  }
  case class IncrementableMassesDiff(index:Int, incr:Double) extends Diff {
    def variable = IncrementableMassesVariable.this
    def undo = { tensor += (index, -incr); assert(tensor(index) >= 0.0); assert(tensor.massTotal >= 0.0) }
    def redo = { tensor += (index, incr);  assert(tensor(index) >= 0.0); assert(tensor.massTotal >= 0.0) }
  }
  case class IncrementableMassesDoubleSeqDiff(t: DoubleSeq) extends Diff {
    def variable = IncrementableMassesVariable.this
    def undo = tensor -= t
    def redo = tensor += t
  }
  case class IncrementableMassesZeroDiff(t: DoubleSeq) extends Diff {
    def variable = IncrementableMassesVariable.this
    def undo = tensor += t
    def redo = tensor.zero()
  }

}
