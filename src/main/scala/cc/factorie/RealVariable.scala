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
import cc.factorie.la._


/** A DiscreteDomain for holding a singleton Tensor holding a single real (Double) value. */
object RealDiscreteDomain extends DiscreteDomain(1)
trait RealDomain extends DiscreteTensorDomain with Domain[RealValue] {
  def dimensionDomain = RealDiscreteDomain
}
object RealDomain extends RealDomain

/** A Tensor holding a single real (Double) value. */
// In Scala 2.10 make this an implicit class.
final class RealValue(val singleValue:Double) extends Tensor1 with SingletonTensor /*with scala.math.Numeric[RealValue]*/ {
  def domain = RealDomain
  @inline final def dim1 = 1
  @inline final def singleIndex = 0
  def activeDomain1 = new cc.factorie.util.SingletonIntSeq(singleIndex)
  @inline final def doubleValue: Double = singleValue // TODO Consider swapping singleIndex <-> intValue
  @inline final def intValue: Int = singleValue.toInt // TODO Consider swapping singleIndex <-> intValue
  override def toString: String = singleValue.toString
  def +(r:RealValue) = new RealValue(r.doubleValue + singleValue)
  def -(r:RealValue) = new RealValue(r.doubleValue - singleValue)
  def *(r:RealValue) = new RealValue(r.doubleValue * singleValue)
  def /(r:RealValue) = new RealValue(r.doubleValue / singleValue)
  type T = RealValue
  def plus(x: T, y: T): T = x + y
  def minus(x: T, y: T): T = x - y
  def times(x: T, y: T): T = x * y
  def negate(x: T): T = new RealValue(- x.doubleValue)
  def fromInt(x: Int): T = new RealValue(x.toDouble)
  def toInt(x: T): Int = singleValue.toInt
  def toLong(x: T): Long = singleValue.toLong
  def toFloat(x: T): Float = singleValue.toFloat
  def toDouble(x: T): Double = singleValue
}

/** A variable with Tensor value which holds a single real (Double) value.
    Unlike a DoubleValue, these can be used in DotFamilyWithStatistics because its value is a Tensor. */
trait RealVar extends DiscreteTensorVar with ScalarVar with Var[RealValue] {
  def doubleValue: Double
  def domain = RealDomain
  @inline final def value: RealValue = new RealValue(doubleValue)
  @inline final def tensor: RealValue = value
  def intValue: Int = doubleValue.toInt
  override def toString = printName + "(" + doubleValue.toString + ")"
}

trait MutableRealVar extends RealVar with MutableDoubleScalarVar with MutableIntScalarVar with MutableVar[RealValue]

/** A Variable with a mutable real (double) value. */
class RealVariable(initialValue: Double) extends MutableRealVar {
  def this() = this(0.0)
  def this(rv:RealValue) = this(rv.singleValue)
  private var _value: Double = initialValue
  @inline final def doubleValue = _value
  def :=(x:Double) = _value = x
  def +=(x:Double) = _value += x
  def -=(x:Double) = _value -= x
  def *=(x:Double) = _value *= x
  def /=(x:Double) = _value /= x
  def set(newValue: Double)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RealDiff(_value, newValue)
    _value = newValue
  }
  final def set(newValue: RealValue)(implicit d: DiffList): Unit = set(newValue.doubleValue)
  final def set(newValue:Int)(implicit d:DiffList): Unit = set(newValue.toDouble)
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}



//// TODO Create an implicit conversion from Double to RealSingletonVector
//// So that we can use them as sufficient statistics in a VectorTemplate
//trait RealSingletonTensorDomain extends DiscreteTensorDomain with Domain[SingletonTensor1] {
//  def dimensionDomain = RealSingletonDiscreteDomain
//  def size = 1
//}
//object RealSingletonDiscreteDomain extends DiscreteDomain(1) 
//object RealSingletonTensorDomain extends RealSingletonTensorDomain
//
///** A variable holding a single real (Double) value, but the value encased in a DiscreteVector, 
//    so that it can be among the Statistics of a VectorTemplate. */
//trait RealSingletonTensorVar extends VarWithNumericValue with DiscreteTensorVar with Var[SingletonTensor1] {
//  thisVariable =>
//  def domain = RealSingletonTensorDomain
//  /** A Vector representation of this Variable's value. */
//  @inline final def value = new SingletonTensor1(1, 0, doubleValue)
//  def tensor = value
//  // TODO Consider rewriting above line to avoid constructing new object
//  def doubleValue: Double
//  def intValue: Int = doubleValue.toInt
//  override def ===(other: Variable) = other match { case other:RealSingletonTensorVar => doubleValue == other.doubleValue; case _ => false } 
//  override def !==(other: Variable) = other match { case other:RealSingletonTensorVar => doubleValue != other.doubleValue; case _ => false } 
//  override def toString = printName + "(" + doubleValue.toString + ")"
//}
//
//class RealSingletonVectorVariable(initialValue:Double) extends RealSingletonTensorVar {
//  private var _value = initialValue
//  def doubleValue: Double = _value
//}

// TODO Consider making an implicit conversion from RealVar to RealSingletonVectorVar 
