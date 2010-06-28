/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
//import scalala.Scalala._
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.la._
import cc.factorie.util.{Log}

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
abstract class DiscreteVariable(initialValue:Int = 0) extends OrdinalVariable(initialValue) with DiscreteValue with IterableSettings {
  type VariableType <: DiscreteVariable
  /*def this(initialValue:Int) = {
    this()
    //assert(initialValue >= 0 && initialValue < domain.size) // TODO Consider adding this back if it isn't too slow
    setByInt(initialValue)(null)
  }*/
  // TODO Consider doing a range check on "setByIndex", but it would slow us down, so do a speed/timing check.
  final override def setByInt(newValue: Int)(implicit d: DiffList): Unit = setByIndex(newValue)(d) 
  // TODO Consider removing setByIndex and just having setByInt for Discretes and Categoricals.  YES!!!
  def setByIndex(newIndex: Int)(implicit d: DiffList): Unit = {
    // TODO Note that we do not check that (newIndex < domain.size), but perhaps we should; this would slow us down, though!
    //if (newIndex < 0) throw new Error("DiscreteVariable setByIndex can't be negative.")
    //val dom = domain; if (!dom.frozen && dom.size < newIndex+1) dom.setSize(newIndex+1) // Do this, even though it will make things slower?
    super.setByInt(newIndex)(d)
  }
  def setRandomly(random:Random = Global.random, d:DiffList = null): Unit = setByIndex(random.nextInt(domainSize))(d)
  def settings = new SettingIterator {
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; setByIndex(i)(d); d }
    def reset = i = -1
    override def variable : DiscreteVariable.this.type = DiscreteVariable.this
  }
}

/** A collection of DiscreteVariables that can iterate over the cross-product of all of their values.  Used for block-Gibbs-sampling.
    @author Andrew McCallum */
class DiscreteVariableBlock(vars:DiscreteVariable*) extends Variable with Seq[DiscreteVariable] with IterableSettings {
  private val _vars = vars.toList
  def length = _vars.length
  def apply(index:Int) = _vars.apply(index)
  def iterator = _vars.iterator
  override def foreach[U](f:(DiscreteVariable)=>U): Unit = _vars.foreach(f)
  def settings: SettingIterator = new SettingIterator {
    var i = -1
    val n = _vars.length
    val s = _vars.map(_.domain.size).toArray
    val max = s.foldLeft(1)(_*_)
    def hasNext = i < max
    def next(difflist:DiffList) = throw new Error // TODO Implement this properly { i += 1; val d = newDiffList; _vars(i%n).setByIndex(i/n)(d); d }
    def reset = i = -1
  }
}

@DomainInSubclasses
abstract class DiscreteObservation(theValue:Int) extends OrdinalObservation(theValue) with DiscreteValue {
  type VariableType <: DiscreteObservation
}
