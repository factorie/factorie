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


/** A DiscreteDomain that provides the dimensionDomain for RealDomain,
    which is a DiscreteDomain for
    holding a singleton Tensor holding a single real (Double) value.
    @author Andrew McCallum */
object RealDiscreteDomain extends DiscreteDomain(1)

/** Domain type for RealVar, which holds a single real (double) value in a SingletonTensor. */
trait RealDomain extends VectorDomain {
  type Value = RealValue
  def dimensionDomain = RealDiscreteDomain
}
/** The domain object used by RealVar. */
object RealDomain extends RealDomain

// TODO Why was math.Numeric commented out?  I think it should be re-added. -akm
/** A Tensor holding a single real (Double) value. 
    @author Andrew McCallum */
final class RealValue(val singleValue:Double) extends Tensor1 with SingletonIndexedTensor /*with scala.math.Numeric[RealValue]*/ {
  def domain = RealDomain
  @inline def dim1 = 1
  @inline def singleIndex = 0
  def activeDomain = new cc.factorie.util.SingletonIntSeq(singleIndex)
  @inline def doubleValue: Double = singleValue // TODO Consider swapping singleIndex <-> intValue
  @inline def intValue: Int = singleValue.toInt // TODO Consider swapping singleIndex <-> intValue
  override def toString: String = singleValue.toString
  def +(r:RealValue) = new RealValue(r.doubleValue + singleValue)
  def -(r:RealValue) = new RealValue(r.doubleValue - singleValue)
  def *(r:RealValue) = new RealValue(r.doubleValue * singleValue)
  def /(r:RealValue) = new RealValue(r.doubleValue / singleValue)
  def unary_- = new RealValue(-singleValue)
}

/** An abstract variable with Tensor value which holds a single real (Double) value.
    Unlike a DoubleValue, these can be used in DotFamilyWithStatistics because its value is a Tensor. 
    @author Andrew McCallum */
trait RealVar extends VectorVar with ScalarVar {
  type Value = RealValue
  def doubleValue: Double
  def domain = RealDomain
  @inline final def value: RealValue = new RealValue(doubleValue)
  def intValue: Int = doubleValue.toInt
  override def toString = printName + "(" + doubleValue.toString + ")"
}

/** An abstract mutable variable with Tensor value which holds a single real (Double) value.
    Unlike a DoubleValue, these can be used in DotFamilyWithStatistics because its value is a Tensor. 
    @author Andrew McCallum */
trait MutableRealVar extends RealVar with MutableDoubleScalarVar with MutableIntScalarVar with MutableVar

/** A mutable variable with Tensor value which holds a single real (Double) value.
    Unlike a DoubleValue, these can be used in DotFamilyWithStatistics because its value is a Tensor. 
    @author Andrew McCallum */
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
  // NOTE have to get rid of this method since RealValue is a value class and erases to Double
  def set(newValue: Double)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RealDiff(_value, newValue)
    _value = newValue
  }
  final def set(newValue: RealValue)(implicit d: DiffList): Unit = set(newValue.doubleValue)
  final def set(newValue:Int)(implicit d:DiffList): Unit = set(newValue.toDouble)
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo() = _value = newValue
    def undo() = _value = oldValue
  }
}
