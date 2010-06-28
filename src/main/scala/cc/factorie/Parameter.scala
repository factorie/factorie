/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.HashSet

trait Parameter extends Variable {
  private lazy val _children = new HashSet[GeneratedValue]
  def keepChildren = true
  def children: Iterable[GeneratedValue] = _children
  def addChild(v:GeneratedValue)(implicit d:DiffList): Unit = if (keepChildren) {
    if (_children.contains(v)) throw new Error("Parameter "+this+" already has child "+v)
    _children += v 
    if (d ne null) d += ParameterAddChildDiff(v)
  }
  def removeChild(v:GeneratedValue)(implicit d:DiffList): Unit = if (keepChildren) {
    _children -= v
    if (d ne null) d += ParameterRemoveChildDiff(v)
  }
  //def weightedChildren: Iterable[(GeneratedValue,Double)]
  case class ParameterAddChildDiff(v:GeneratedValue) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { assert(!_children.contains(v)); _children += v }
    def undo = { _children -= v }
  }
  case class ParameterRemoveChildDiff(v:GeneratedValue) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { _children -= v }
    def undo = { assert(!_children.contains(v)); _children += v }
  }
}



trait RealValueParameter extends RealValue with Parameter
class RealVariableParameter(value:Double) extends RealVariable(value) with RealValueParameter
class RealConstantParameter(value:Double) extends RealObservation(value) with RealValueParameter

trait IntegerValueParameter extends IntegerValue with Parameter
class IntegerVariableParameter(value:Int) extends IntegerVariable(value) with IntegerValueParameter



trait Estimation[This<:Parameter] {
  this: This =>
  def estimate(model:Model = Global.defaultModel)(implicit e:Estimator[This]): Unit = e.estimate(this, model)
}

trait Estimator[P<:Parameter] {
  def estimate(parameter:P, model:Model): Unit
}


trait AbstractParameterRef extends Variable {
  def abstractValue: Parameter
  def child: GeneratedValue
}
class ParameterRef[P<:Parameter,C<:GeneratedValue](p:P, override val child:C) extends RefVariable(p) with AbstractParameterRef {
  def abstractValue = this.value
  // This 'set' method will be called in initialization of RefVariable
  override def set(newValue:P)(implicit d:DiffList): Unit = if (newValue ne value) { 
    // Above, if this is != instead of ne, then entire Proportion contents will be examined!  Slow!!!
    if (value ne null) value.removeChild(child)
    super.set(newValue)
    if (value ne null) value.addChild(child)
  }
}
class GatedParameterRef[P<:Parameter,C<:MixtureOutcome](val parameters:Seq[P], val gate:Gate, child:C) extends ParameterRef[P,C](parameters.apply(gate.intValue), child) with GatedRefVariable[P] {
  //println("GatedParameterRef child="+child)
  gate += this // xxx
  assert(parameters.length == gate.domainSize)
  def valueForIndex(index:Int) = parameters(index)
  def domainSize = parameters.length
}



trait DeterministicFunction extends Parameter
trait RealFunction extends DeterministicFunction with RealValueParameter
abstract class RealOpConstant(val real:RealValueParameter) extends RealFunction with GeneratedValue {
  real.addChild(this)(null) // But now might not garbage collect this when we want to
  def parents = List(real)
  def pr = 1.0 // Deterministic value given parent
}
class RealPlusConstant(override val real:RealValueParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue + constant
}
class RealTimesConstant(override val real:RealValueParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue * constant
}

