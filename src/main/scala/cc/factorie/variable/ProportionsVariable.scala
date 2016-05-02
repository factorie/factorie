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

import cc.factorie._
import cc.factorie.directed._
import cc.factorie.infer._
import cc.factorie.la._
import cc.factorie.util.{DenseDoubleSeq, DoubleSeq, SparseDoubleSeq}
import cc.factorie.variable.MassesVariable._

import scala.util.Random

// Proportions Values

/** A Tensor containing non-negative numbers summing to 1.0.  It is the parameter of a Discrete or Multinomial distribution. 
    Proportions contain Masses, which may not sum to 1.0;
    some Proportions subclasses can have their value changed by incrementing these inner masses.
    All Proportions also inherit directly from Masses, but, naturally, these Masses always sum to 1.0, and generally are not directly mutable.
    @author Andrew McCallum */
trait Proportions extends Masses with ReadOnlyTensor {
  def masses: Masses
  @inline final override def pr(i:Int): Double = apply(i)
  override def stringPrefix = "Proportions"
  override def toString = this.asSeq.take(maxToStringLength).mkString(stringPrefix+"(", ",", if (length > 10) "...)" else ")")

  override val massTotal = 1.0
  override def apply(i: Int) = {
    val mt = masses.massTotal
    if (mt == 0.0) 1.0 / length
    else masses.apply(i) / mt
  }
}

trait DirichletPrior extends Proportions {
  var prior:Masses = null

  override def apply(index:Int): Double = {
    if (prior eq null) {
      if (masses.massTotal == 0) 1.0 / length
      else masses(index) / masses.massTotal
    } else {
      if (masses.massTotal == 0) prior(index) / prior.massTotal
      else (masses(index) + prior(index)) / (masses.massTotal+prior.massTotal)
    }
  }
}

object Proportions {
  /** Return a zero-mass Proportions with the same dimensionality and sparsity as the Tensor argument. */
  def blankCopy(t:Tensor1): Proportions1 = t match {
    case t:DenseTensor1 => new DenseProportions1(t.dim1)
    case t:SparseTensor1 => throw new Error("Not yet implemeneted")
  }
  def blankCopy(t:Tensor2): Proportions2 = t match {
    case t:DenseTensor2 => new DenseProportions2(t.dim1, t.dim2)
    //case t:SparseTensor2 => throw new Error("Not yet implemeneted")
  }
  def blankCopy(t:Tensor3): Proportions3 = t match {
    case t:DenseTensor3 => new DenseProportions3(t.dim1, t.dim2, t.dim3)
    //case t:SparseTensor2 => throw new Error("Not yet implemeneted")
  }
}

trait Proportions1 extends Masses1 with Proportions { def masses: Masses1 }
trait Proportions2 extends Masses2 with Proportions { def masses: Masses2 }
trait Proportions3 extends Masses3 with Proportions { def masses: Masses3 }
trait Proportions4 extends Masses4 with Proportions { def masses: Masses4 }

trait MassesProportions[A <: Masses] extends WrappedTensor[A] with Proportions {
  def masses:A
  def tensor = masses
}

class MassesProportions1(val masses:Masses1) extends MassesProportions[Masses1] with WrappedTensor1[Masses1] with Proportions1 {
  def this(t:Tensor1) = this(toMasses1(t))
}
class MassesProportions2(val masses:Masses2) extends MassesProportions[Masses2] with WrappedTensor2[Masses2] with Proportions2 {
  def this(t:Tensor2) = this(toMasses2(t))
}
class MassesProportions3(val masses:Masses3) extends MassesProportions[Masses3] with WrappedTensor3[Masses3] with Proportions3 {
  def this(t:Tensor3) = this(toMasses3(t))
}
class MassesProportions4(val masses:Masses4) extends MassesProportions[Masses4] with WrappedTensor4[Masses4] with Proportions4 {
  def this(t:Tensor4) = this(toMasses4(t))
}



// Proportions Values of dimensionality 1

class SingletonProportions1(dim1:Int, singleIndex:Int) extends SingletonMasses1(dim1, singleIndex, 1.0) with Proportions1 {
  val masses = this
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}
class SingletonProportions2(dim1:Int, dim2: Int, singleIndex1:Int, singleIndex2: Int) extends SingletonMasses2(dim1, dim2, singleIndex1, singleIndex2, 1.0) with Proportions2 {
  val masses = this
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}
class UniformProportions1(dim1:Int) extends UniformMasses1(dim1, 1.0/dim1) with Proportions1 {
  val masses = this
  @inline final override def apply(index:Int) = {
    val result = 1.0 / length
    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
    result
  }
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}
class GrowableUniformProportions1(sizeProxy:Iterable[Any], uniformValue:Double = 1.0) extends DenseDoubleSeq with Proportions1 {
  def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
  def activeDomainSize = activeDomain.size
  val masses = new GrowableUniformMasses1(sizeProxy, uniformValue)
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  def dim1 = masses.length
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, masses.length)
  def isDense = true
  @inline final override def apply(index:Int) = {
    val result = 1.0 / masses.length
    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
    result
  }
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

/** Proportions with arbitrary probability on all its discrete outcomes.
    Extends DenseDoubleSeq.
    @author Andrew McCallum */
trait DenseProportions extends DenseDoubleSeq with Proportions  {
  def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
  def activeDomainSize = activeDomain.size
  def isDense = true
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  //this is a read only tensor, so no zero here
  //override def zero(): Unit = masses.zero()
}

class DenseProportions1(override val dim1:Int) extends Proportions1 with DenseProportions {
  def this(ds:DoubleSeq) = { this(ds.length); this.masses += ds }
  def this(a:Array[Double]) = { this(a.length); this.masses += a }
  def this(dim1:Int, uniformValue:Double) = { this(dim1); this.masses += uniformValue }
  val masses = new DenseMasses1(dim1)
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, dim1)
}
class DenseProportions2(override val dim1:Int, override val dim2:Int) extends Proportions2 with DenseProportions {
  val masses = new DenseMasses2(dim1, dim2)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
}
class DenseProportions3(override val dim1:Int, override val dim2:Int, override val dim3:Int) extends Proportions3 with DenseProportions {
  val masses = new DenseMasses3(dim1, dim2, dim3)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
  def activeDomain3 = new cc.factorie.util.RangeIntSeq(0, dim3)
}
class DenseProportions4(override val dim1:Int, override val dim2:Int, override val dim3:Int, override val dim4:Int) extends Proportions4 with DenseProportions {
  val masses = new DenseMasses4(dim1, dim2, dim3, dim4)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
  def activeDomain3 = new cc.factorie.util.RangeIntSeq(0, dim3)
  def activeDomain4 = new cc.factorie.util.RangeIntSeq(0, dim4)
}

class GrowableDenseProportions1(val sizeProxy:Iterable[Any]) extends Proportions1 with DenseProportions {
  override def dim1 = sizeProxy.size
  val masses = new GrowableDenseMasses1(sizeProxy)
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, dim1)
}

/** Proportions with arbitrary probability on all its discrete outcomes.
    Extends DenseTensor.
    @author Andrew McCallum */
trait DenseTensorProportions extends DenseTensor with Proportions {
  override def apply(i: Int) = _values(i)
  //Unecessary, already overwritten in ReadOnlyTensor (which Proportions are)
  // These overrides would not be necessary if Tensor were not a MutableDoubleSeq and we had a ImmutableDenseTensor, but I don't think it is worth it.
  //override def +=(i:Int, incr:Double): Unit = throw new Error("Method +=(Int,Double) not defined on class "+getClass.getName)
  override def +=(t:DoubleSeq, offset:Int, f:Double): Unit = throw new Error("Method +=(DoubleSeq,Int,Double) not defined on class "+getClass.getName)
  //override def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName)
  //override def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
  override def *=(d: Double): Unit = throw new Error("Method *=(Double) not defined on class "+getClass.getName)
  override def :=(ds:DoubleSeq): Unit = throw new Error("Method :=(DoubleSeq) not defined on class "+getClass.getName)
  override def :=(a:Array[Double]): Unit = throw new Error("Method :=(Array[Double]) not defined on class "+getClass.getName)
  override def :=(a:Array[Double], offset:Int): Unit = throw new Error("Method :=(Array[Double],Int) not defined on class "+getClass.getName)
  override def expNormalize(): Double = throw new Error("Method expNormalize() not defined on class "+getClass.getName)
}
// TODO Make default constructor take Array[Double], and another constructor that takes Tensor and copy
class DenseTensorProportions1(override protected val _initialArray:Array[Double], checkNormalization:Boolean = true) extends DenseTensor1(_initialArray.length) with DenseTensorProportions with Proportions1 {
  val masses = this
  def this(tensor:Tensor1) = this(tensor.toArray)
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  override def copy: DenseTensorProportions1 = this // because we should be immutable
  override def blankCopy: DenseTensorProportions1 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

class DenseTensorProportions2(override protected val _initialArray:Array[Double], dim1:Int, dim2:Int, checkNormalization:Boolean = true) extends DenseTensor2(dim1, dim2) with DenseTensorProportions with Proportions2 {
  val masses = this
  def this(tensor:Tensor2) = this(tensor.toArray, tensor.dim1, tensor.dim2)
  if (checkNormalization) require(maths.almostEquals(_initialArray.sum, 1.0, 0.0001))
  override def copy: DenseTensorProportions2 = this // because we should be immutable
  override def blankCopy: DenseTensorProportions2 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

class DenseTensorProportions3(override protected val _initialArray:Array[Double], dim1:Int, dim2:Int, dim3:Int, checkNormalization:Boolean = true) extends DenseTensor3(dim1, dim2, dim3) with DenseTensorProportions with Proportions3 {
  val masses = this
  def this(tensor:Tensor3) = this(tensor.toArray, tensor.dim1, tensor.dim2, tensor.dim3)
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  override def copy: DenseTensorProportions3 = this // because we should be immutable
  override def blankCopy: DenseTensorProportions3 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}

class DenseTensorProportions4(override protected val _initialArray:Array[Double], dim1:Int, dim2:Int, dim3:Int, dim4:Int, checkNormalization:Boolean = true) extends DenseTensor4(dim1, dim2, dim3, dim4) with DenseTensorProportions with Proportions4 {
  val masses = this
  def this(tensor:Tensor4) = this(tensor.toArray, tensor.dim1, tensor.dim2, tensor.dim3, tensor.dim4)
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  override def copy: DenseTensorProportions4 = this // because we should be immutable
  override def blankCopy: DenseTensorProportions4 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}


/** Proportions expected to have zero probability on many of its discrete outcomes.
    Extends SparseIndexedTensor.
    @author Andrew McCallum */
trait SparseTensorProportions extends SparseIndexedTensor with Proportions {
  protected def tensor: SparseIndexedTensor
  def foreachActiveElement(f:(Int,Double)=>Unit): Unit = tensor.foreachActiveElement(f)
  tensor._makeReadable()
  def _makeReadable(): Unit = {}
  def _unsafeActiveDomainSize: Int = tensor._unsafeActiveDomainSize
  def _indices: Array[Int] = tensor._indices
  def sizeHint(size: Int): Unit = {}
  def _values: Array[Double] = tensor._values
  override def activeDomainSize: Int = tensor.activeDomainSize
  override def apply(i:Int) = tensor.apply(i)
  def activeDomain = tensor.activeDomain
  def dot(t:DoubleSeq): Double = tensor.dot(t)

  // These overrides would not be necessary if Tensor were not a MutableDoubleSeq and we had a ImmutableDenseTensor, but I don't think it is worth it.
  // some overrides already happen in the ReadOnlyTensor trait
  //override def +=(i:Int, incr:Double): Unit = throw new Error("Method +=(Int,Double) not defined on class "+getClass.getName)
  //override def +=(t:DoubleSeq, factors:DoubleSeq, f:Double): Unit = throw new Error("Method +=(DoubleSeq,Int,Double) not defined on class "+getClass.getName)
  //override def zero(): Unit = throw new Error("Method zero() not defined on class "+getClass.getName)

  //override def update(i:Int, v:Double): Unit = throw new Error("Method update(Int,Double) not defined on class "+getClass.getName)
  override def *=(d: Double): Unit = throw new Error("Method *=(Double) not defined on class "+getClass.getName)
  override def :=(ds:DoubleSeq): Unit = throw new Error("Method :=(DoubleSeq) not defined on class "+getClass.getName)
  override def :=(a:Array[Double]): Unit = throw new Error("Method :=(Array[Double]) not defined on class "+getClass.getName)
  override def :=(a:Array[Double], offset:Int): Unit = throw new Error("Method :=(Array[Double],Int) not defined on class "+getClass.getName)
  override def expNormalize(): Double = throw new Error("Method expNormalize() not defined on class "+getClass.getName)
}

class SparseTensorProportions1(val tensor:SparseIndexedTensor1, checkNormalization:Boolean = true) extends Tensor1 with SparseTensorProportions with Proportions1 {
  val masses = toMasses1(tensor)
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  def dim1 = tensor.dim1
  override def copy: SparseTensorProportions1 = this // because we should be immutable
  override def blankCopy: SparseTensorProportions1 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}
class SparseTensorProportions2(val tensor:SparseIndexedTensor2, checkNormalization:Boolean = true) extends Tensor2 with SparseTensorProportions with Proportions2 {
  val masses = this
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  def dim1 = tensor.dim1
  def dim2 = tensor.dim2
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
  override def copy: SparseTensorProportions2 = this // because we should be immutable
  override def blankCopy: SparseTensorProportions2 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}
class SparseTensorProportions3(val tensor:SparseIndexedTensor3, checkNormalization:Boolean = true) extends Tensor3 with SparseTensorProportions with Proportions3 {
  val masses = this
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  def dim1 = tensor.dim1
  def dim2 = tensor.dim2
  def dim3 = tensor.dim3
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
  def activeDomain3 = tensor.activeDomain3
  override def copy: SparseTensorProportions3 = this // because we should be immutable
  override def blankCopy: SparseTensorProportions3 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}
class SparseTensorProportions4(val tensor:SparseIndexedTensor4, checkNormalization:Boolean = true) extends Tensor4 with SparseTensorProportions with Proportions4 {
  val masses = this
  if (checkNormalization) require(maths.almostEquals(this.sum, 1.0, 0.0001))
  def dim2 = tensor.dim2
  def dim1 = tensor.dim1
  def dim3 = tensor.dim3
  def dim4 = tensor.dim4
  def activeDomain1 = tensor.activeDomain1
  def activeDomain2 = tensor.activeDomain2
  def activeDomain3 = tensor.activeDomain3
  def activeDomain4 = tensor.activeDomain4
  override def copy: SparseTensorProportions4 = this // because we should be immutable
  override def blankCopy: SparseTensorProportions4 = throw new Error("Method blankCopy not defined on class "+getClass.getName)
}



///** An immutable Proportions from a pre-normalized Tensor. */
//abstract class NormalizedTensorProportions(val tensor:Tensor, checkNormalization:Boolean = true) extends Proportions {
//  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
//  if (checkNormalization) require(maths.almostEquals(tensor.sum, 1.0, 0.0001))
//  @inline final def apply(i:Int) = tensor.apply(i)
//  def massTotal = 1.0
//  def isDense = tensor.isDense
//}
//class NormalizedTensorProportions1(override val tensor:Tensor1, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions1 {
//  def dim1 = tensor.dim1
//  def activeDomain = tensor.activeDomain
//  val masses = this
//}
//class NormalizedTensorProportions2(override val tensor:Tensor2, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions2 {
//  def dim1 = tensor.dim1
//  def dim2 = tensor.dim2
//  def activeDomain = tensor.activeDomain
//  def activeDomain1 = tensor.activeDomain1
//  def activeDomain2 = tensor.activeDomain2
//  val masses = this
//}
//class NormalizedTensorProportions3(override val tensor:Tensor3, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions3 {
//  def dim1 = tensor.dim1
//  def dim2 = tensor.dim2
//  def dim3 = tensor.dim3
//  def activeDomain = tensor.activeDomain
//  def activeDomain1 = tensor.activeDomain1
//  def activeDomain2 = tensor.activeDomain2
//  def activeDomain3 = tensor.activeDomain3
//  val masses = this
//}
//class NormalizedTensorProportions4(override val tensor:Tensor4, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions4 {
//  def dim1 = tensor.dim1
//  def dim2 = tensor.dim2
//  def dim3 = tensor.dim3
//  def dim4 = tensor.dim4
//  def activeDomain = tensor.activeDomain
//  def activeDomain1 = tensor.activeDomain1
//  def activeDomain2 = tensor.activeDomain2
//  def activeDomain3 = tensor.activeDomain3
//  def activeDomain4 = tensor.activeDomain4
//  val masses = this
//}

/** Proportions expected to have zero probability on many of its discrete outcomes,
    and which is stored with high-probability outcomes first, for efficient sampling.
    Extends SparseDoubleSeq.
    @author Andrew McCallum */
class SortedSparseCountsProportions1(val dim1:Int) extends SparseDoubleSeq with Proportions1 with DirichletPrior  {
  val masses = new SortedSparseCountsMasses1(dim1)
  def activeDomainSize = masses.activeDomainSize
  override def foreachActiveElement(f: (Int, Double) => Unit) { masses.foreachActiveElement((i, v) => f(i, v/massTotal)) }
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  def activeDomain = masses.activeDomain  // throw new Error("Not implemented")
  def isDense = false

  //remove this method, because proportions are read-only tensors
  //override def zero(): Unit = masses.zero()
  // Note that "def zero()" defined in SortedSparseCountsMasses1 does not zero this.prior
  override def top(n:Int): cc.factorie.util.TopN[String] = {
    val len = math.min(n, masses.sparseCounts.numPositions)
    val result = new cc.factorie.util.TopN[String](len)
    var i = 0
    while (i < len) {
      result += (masses.sparseCounts.indexAtPosition(i), masses.sparseCounts.countAtPosition(i).toDouble / masses.sparseCounts.countsTotal)
      i += 1
    }
    result
  }
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = masses.sampleIndex(massTotal)(r)
}



// Proportions Variable

//trait ProportionsDomain extends MassesDomain with Domain[Proportions]
//object ProportionsDomain extends ProportionsDomain

/** An abstract Var whose value is a Proportions.
    @author Andrew McCallum */
trait ProportionsVar extends MassesVar {
  type Value <: Proportions
  override def value: Value
}
trait MutableProportionsVar extends MutableMassesVar with ProportionsVar

/** A Variable whose value is a Proportions.
    @author Andrew McCallum */
class ProportionsVariable extends MutableProportionsVar {
  type Value = Proportions
  def this(initialValue:Proportions) = { this(); set(initialValue)(null) }
  
  // Methods that track modifications on a DiffList
  def updateMasses(index:Int, newValue:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    value.masses.update(index, newValue)
  }
  def incrementMasses(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    if (d ne null) d += IncrementProportionsMassesIndexDiff(index, incr)
    value.masses.+=(index, incr)
  }
  def incrementMasses(incr:Tensor)(implicit d:DiffList): Unit = {
    require(incr.length == value.length)
    if (d ne null) d += IncrementProportionsMassesDiff(incr)
    value.masses += incr
  }
  def zeroMasses(implicit d:DiffList): Unit = {
    if (d ne null) d += ZeroProportionsMassesDiff(value.toArray)
    value.masses.zero()
  }
  
  case class IncrementProportionsMassesIndexDiff(index:Int, incr:Double) extends Diff {
    def variable = ProportionsVariable.this
    def undo() = value.masses.+=(index, -incr)
    def redo() = value.masses.+=(index, incr)
  }
  case class IncrementProportionsMassesDiff(t: Tensor) extends Diff {
    def variable = ProportionsVariable.this
    def undo() = value.masses -= t // Note this relies on Tensor t not having changed.
    def redo() = value.masses += t
  }
  case class ZeroProportionsMassesDiff(prev: Array[Double]) extends Diff {
    def variable = ProportionsVariable.this
    def undo() = value.masses += prev
    def redo() = value.masses.zero()
  }

}
// In the future, we might also need a ProportionsVar1, ProportionsVar2, etc. -akm

/** A factory for creating ProportionsVariables.
    @author Andrew McCallum */
object ProportionsVariable {
  def uniform(dim:Int) = new ProportionsVariable(new UniformProportions1(dim))
  def dense(dim:Int) = new ProportionsVariable(new DenseProportions1(dim))
  def growableDense(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableDenseProportions1(sizeProxy))
  def growableUniform(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableUniformProportions1(sizeProxy, 1.0))
  def sortedSparseCounts(dim:Int) = new ProportionsVariable(new SortedSparseCountsProportions1(dim))
  //def growableSparseCounts(sizeProxy:Iterable[Any]) = new ProportionsVariable(new SortedSparseCountsProportions1(sizeProxy)) // Not yet implemented
}

/** A distribution over proportions, with mean and variance, e.g. as the mean and concentration parameters of a Dirichlet distribution.
    @author Andrew McCallum */
trait ProportionsMarginal extends Marginal {
  //def variables = Seq(_1)
  def _1: ProportionsVar
  def mean: Proportions
  def variance: Double
  //def setToMaximize(implicit d:DiffList): Unit = _1.asInstanceOf[ProportionsVariable].set(mean)
}

/** A value assignment to a single ProportionsVariable.
    @author Andrew McCallum */
class ProportionsAssignment(p:ProportionsVariable, v:Proportions) extends Assignment1[ProportionsVariable](p, v.asInstanceOf[p.Value]) with Marginal1 with ProportionsMarginal {
  //final def _1 = p // TODO Consider renaming Assignment1.var1 back to _1
  override def variables = Seq(p)
  def mean = throw new Error // TODO!!! Should be this instead: value1
  def variance = Double.PositiveInfinity // TODO Is this the right value?
  def setToMaximize(implicit d:DiffList): Unit = setVariables(d)
}

/** For inferring the value of a ProportionsVariable that maximizes the score of its surrounding factors in a DirectedModel.
    @author Andrew McCallum */
object MaximizeProportions extends Maximize[Iterable[ProportionsVariable],DirectedModel] {
  def infer(variables:Iterable[ProportionsVariable], model:DirectedModel, marginalizing:Summary): Summary = {
    def maxProp(v:ProportionsVariable, model:DirectedModel, marginalizing:Summary): Proportions = maxProportions(v, model, marginalizing match { case m:DiscreteSummary1[DiscreteVar @unchecked] => m; case null => null })
    if (variables.size == 1) new SingletonSummary(new ProportionsAssignment(variables.head, maxProp(variables.head, model, marginalizing)))
    else throw new Error("Multivariate case yet implemented.")
  }
  def apply(p:ProportionsVariable, model:DirectedModel): Unit = apply(p, model, null)
  def apply(p:ProportionsVariable, model:DirectedModel, summary:DiscreteSummary1[DiscreteVar]): Unit = p.set(maxProportions(p, model, summary))(null)
  def maxProportions[A](p:ProportionsVariable, model:DirectedModel, summary:DiscreteSummary1[DiscreteVar]): Proportions = {
    // Zero an accumulator
    val e: DenseProportions1 = new DenseProportions1(p.value.length) // TODO Consider instead passing this in as an argument, so that SparseProportions could be used
    // Initialize with prior; find the factor that is the parent of "p", and use its Dirichlet masses for initialization
    model.parentFactor(p) match {
	  case f:Dirichlet.Factor => e.masses := f._2.value
	  case null => {}
      case _ => throw new Error // TODO Do something more reasonable here?
    }
    // Incorporate children
    @inline def incrementForDiscreteVar(dv:DiscreteVar, incr:Double): Unit = {
      val marginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(dv) 
      if (marginal eq null) e.masses.+=(dv.intValue, incr) else e.masses.+=(marginal.proportions, incr)
    }
    for (factor <- model.extendedChildFactors(p)) factor match {
      //case parent:DirectedFactor if (parent.child == p) => {} // Parent factor of the Proportions we are estimating already incorporated above
      // The array holding the mixture components; the individual components (CategoricalMixture.Factors) will also be among the extendedChildFactors
      case m:Mixture.Factor => {}
      // A simple DiscreteVar child of the Proportions
      case d:Discrete.Factor => incrementForDiscreteVar(d._1, 1.0)
      // A DiscreteVar child of a Mixture[Proportions]
      case dm:CategoricalMixture[A @unchecked]#Factor => {
        val gate = dm._3
        val gateMarginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(gate) 
        //val qFactors = if (qModel eq null) Nil else qModel.factors(Seq(gate))
        val mixtureIndex = dm._2.indexOf(p) // Yipes!  Linear search.
        if (gateMarginal eq null) {
          if (dm._3.intValue == mixtureIndex) incrementForDiscreteVar(dm._1, 1.0)
        } else {
          incrementForDiscreteVar(dm._1, gateMarginal.proportions(mixtureIndex))
        }
      }
      case pdm:PlatedDiscreteMixture.Factor => {
        // We don't yet handle variational summary of PlatedDiscreteMixture.Factor
        val mixtureIndex = pdm._2.indexOf(e)
        var i = 0; val len = pdm._1.length
        while (i < len) { if (pdm._3.intValue(i) == mixtureIndex) e.masses.+=(pdm._3.intValue(i), 1.0); i += 1 }
      }
    }
    e
  }
}
