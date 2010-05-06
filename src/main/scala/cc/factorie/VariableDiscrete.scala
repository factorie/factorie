/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log}
import cc.factorie.util.Implicits._

/** An OrdinalValue with finite range 0...N.  
    For your own subclass MyDiscreteValue, you can set N=9 with Domain[MyDiscreteValue].size = 9 
    @author Andrew McCallum */
@DomainInSubclasses
trait DiscreteValues extends Variable with OrdinalValues {
  type VariableType <: DiscreteValues
  type DomainType <: DiscreteDomain[VariableType]
  // TODO Replace this mechanism with an Annotation
  class DomainClass extends DiscreteDomain[VariableType]()(null)
  def domainSize: Int = domain.size
  override def maxIntValue = domainSize - 1
  /** A scalala Vector representation of the value of this variable. */
  def vector: Vector
}

@DomainInSubclasses
trait DiscreteValue extends DiscreteValues with OrdinalValue {
  this: Variable =>
  type VariableType <: DiscreteValue
  @inline final def index = intValue // simply an alias for intValue 
  def vector = new SingletonBinaryVector(domainSize, intValue)
}

@DomainInSubclasses
abstract class DiscreteVariable extends OrdinalVariable with DiscreteValue with IterableSettings {
  type VariableType <: DiscreteVariable
  def this(initialValue:Int) = { this(); assert(initialValue >= 0 && initialValue < domain.size); setByIndex(initialValue)(null) }
  // TODO Consider doing a range check on "setByIndex", but it would slow us down, so do a speed/timing check.
  final override def setByInt(newValue: Int)(implicit d: DiffList): Unit = setByIndex(newValue)(d) 
  def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
    // TODO Note that we do not check that (newIndex < domain.size), but perhaps we should; this would slow us down, though!
    if (newIndex < 0) throw new Error("DiscreteVariable setByIndex can't be negative.")
    super.setByInt(newIndex)(d)
  }
  def setRandomly(random:Random, d:DiffList): Unit = setByIndex(random.nextInt(domainSize))(d)
  def setRandomly(random:Random): Unit = setRandomly(random, null)
  def setRandomly: Unit = setRandomly(cc.factorie.Global.random)
  def settings = new SettingIterator {
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; setByIndex(i)(d); d }
    def reset = i = -1
    override def variable : DiscreteVariable.this.type = DiscreteVariable.this
  }
}

@DomainInSubclasses
abstract class DiscreteObservation(theValue:Int) extends OrdinalObservation(theValue) with DiscreteValue {
  type VariableType <: DiscreteObservation
}
