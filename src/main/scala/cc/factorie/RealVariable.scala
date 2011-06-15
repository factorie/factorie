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
import cc.factorie.la.SingletonVector

trait RealDomain extends Domain[Double]
object RealDomain extends RealDomain

// TODO Consider instead ValueType[Double] and making a different class for SingletonVector.
/** A Variable with a real (double) value. */
trait RealVar extends Variable with VarAndValueType[RealVar,Double] with NumericValue {
  def domain = RealDomain
  @inline final def value: Value = doubleValue
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  //def ===(other: RealVar) = doubleValue == other.doubleValue
  //def !==(other: RealVar) = doubleValue != other.doubleValue
  override def toString = printName + "(" + doubleValue.toString + ")"
}

/** A Variable with a mutable real (double) value. */
class RealVariable(initialValue: Double = 0.0) extends RealVar with MutableVar {
  //type VariableType <: RealVariable
  private var _value: Double = initialValue
  @inline final def doubleValue = _value
  def +=(x:Double) = set(_value + x)(null) // Should we allow non-null DiffLists?
  def -=(x:Double) = set(_value - x)(null)
  def *=(x:Double) = set(_value * x)(null)
  def /=(x:Double) = set(_value / x)(null)
  def :=(x:Double) = set(x)(null)
  def set(newValue: Double)(implicit d: DiffList): Unit = if (newValue != _value) {
    if (d ne null) d += new RealDiff(_value, newValue)
    _value = newValue
  }
  //def set(newValue: Value)(implicit d:DiffList): Unit = set(newValue)
  case class RealDiff(oldValue: Double, newValue: Double) extends Diff {
    def variable: RealVariable = RealVariable.this
    def redo = _value = newValue
    def undo = _value = oldValue
  }
}





// TODO Create an implicit conversion from Double to RealSingletonVector
// So that we can use them as sufficient statistics in a VectorTemplate
trait RealSingletonVectorDomain extends DiscreteVectorDomain {
  //def maxVectorLength = 1
  def dimensionDomain = RealSingletonDiscreteDomain
  def size = 1
}
object RealSingletonDiscreteDomain extends DiscreteDomain { def size = 1 }
object RealSingletonVectorDomain extends RealSingletonVectorDomain

trait RealSingletonVectorVar extends Variable with NumericValue with DiscreteVectorVar with VarAndValueType[RealSingletonVectorVar,SingletonVector with DiscreteVectorValue] {
  thisVariable =>
  //type VariableType <: RealVar
  //type ValueType = SingletonVector with DiscreteVectorValue
  def domain = RealSingletonVectorDomain
  /** A Vector representation of this Variable's value. */
  @inline final def value = new SingletonVector(1, 0, doubleValue) with DiscreteVectorValue {
    def domain = thisVariable.domain
  }
  // TODO Consider rewriting above line to avoid constructing new object
  def doubleValue: Double
  def intValue: Int = doubleValue.toInt
  override def ===(other: VariableType) = doubleValue == other.doubleValue
  override def !==(other: VariableType) = doubleValue != other.doubleValue
  override def toString = printName + "(" + doubleValue.toString + ")"
}

class RealSingletonVectorVariable(initialValue:Double) extends RealSingletonVectorVar {
  private var _value = initialValue
  def doubleValue: Double = _value
}
