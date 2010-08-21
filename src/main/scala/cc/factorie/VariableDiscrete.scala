/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.util.Random
import cc.factorie.la._

/** An IntegerVars with finite range 0...N.  
    For your own subclass MyDiscreteVar, you can set N=9 with Domain[MyDiscreteValue].size = 9.
    @author Andrew McCallum */
@DomainInSubclasses
trait DiscreteVars extends Variable with IntegerVars {
  type VariableType <: DiscreteVars
  type DomainType <: DiscreteDomain[VariableType]
  // TODO Replace this mechanism with an Annotation
  class DomainClass extends DiscreteDomain[VariableType]()(null)
  final def domainSize: Int = domain.size
  override def minIntValue = 0
  override def maxIntValue = domainSize - 1
  /** A cc.factorie.la.Vector representation of the value of this variable. */
  def vector: Vector
  def indices: Collection[Int]
}

@DomainInSubclasses
trait DiscreteVar extends DiscreteVars with IntegerVar {
  this: Variable =>
  type VariableType <: DiscreteVar
  def vector = new SingletonBinaryVector(domainSize, intValue)
  def indices = List(intValue)
}

@DomainInSubclasses
abstract class DiscreteVariable(initialValue:Int = 0) extends IntegerVariable(initialValue) with DiscreteVar with IterableSettings with QDistribution {
  type VariableType <: DiscreteVariable
  def setRandomly(random:Random = Global.random, d:DiffList = null): Unit = set(random.nextInt(domainSize))(d)
  def settings = new SettingIterator {
    var i = -1
    val max = domainSize - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable : DiscreteVariable.this.type = DiscreteVariable.this
  }
  type QType = MutableProportions
  def newQ = new DenseProportions(domainSize)
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
abstract class DiscreteObservation(theValue:Int) extends IntegerObservation(theValue) with DiscreteVar {
  type VariableType <: DiscreteObservation
}
