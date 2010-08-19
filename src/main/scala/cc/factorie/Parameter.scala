/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.{HashSet,ArrayBuffer}

trait Parameter extends Variable {
  private lazy val _children = new HashSet[GeneratedVar]
  def keepChildren = true
  /** A collection of variables whose value depends directly on this variable. */
  def children: Iterable[GeneratedVar] = _children
  /** A collection of variables whose value depends on the value of this variable, 
      either directly or via a sequence of deterministic variables.  If this variable's
      value changes, all of these extended children variables' .pr will change. */
  def extendedChildren: Iterable[GeneratedVar] = {
    val result = new ArrayBuffer[GeneratedVar]
    for (child <- children) {
      child match { 
        case dp:DeterministicParameter => { result += dp; result ++= dp.extendedChildren }
        case gv:GeneratedVar => result += gv
      }
    }
    result
  }
  // TODO Remove this?  Then implement this pulling of MixtureComponents.children in each of the parameter estimation inference routines.
  // Yes, I think this above method is better.
  def generatedChildren: Iterable[GeneratedVar] = {
    val result = new ArrayBuffer[GeneratedVar]
    for (child <- children) child match {
      case mcs:MixtureComponents[_] => result ++= mcs.children
      case _ => result += child
    }
    result
  }
  def addChild(v:GeneratedVar)(implicit d:DiffList): Unit = if (keepChildren) {
    //println("Parameter.addChild"); new Exception().printStackTrace()
    if (_children.contains(v)) throw new Error("Parameter "+this+" already has child "+v+" with hashCode="+v.hashCode)
    _children += v 
    if (d ne null) d += ParameterAddChildDiff(v)
  }
  def removeChild(v:GeneratedVar)(implicit d:DiffList): Unit = if (keepChildren) {
    _children -= v
    if (d ne null) d += ParameterRemoveChildDiff(v)
  }
  //def weightedChildren: Iterable[(GeneratedVar,Double)]
  case class ParameterAddChildDiff(v:GeneratedVar) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { assert(!_children.contains(v)); _children += v }
    def undo = { _children -= v }
  }
  case class ParameterRemoveChildDiff(v:GeneratedVar) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { _children -= v }
    def undo = { assert(!_children.contains(v)); _children += v }
  }
}

trait DeterministicParameter extends GeneratedVar with Parameter {
  override final def isDeterministic = true
}


trait RealVarParameter extends RealVar with Parameter
class RealVariableParameter(value:Double) extends RealVariable(value) with RealVarParameter
class RealConstantParameter(value:Double) extends RealObservation(value) with RealVarParameter

trait IntegerVarParameter extends IntegerVar with Parameter
class IntegerVariableParameter(value:Int) extends IntegerVariable(value) with IntegerVarParameter



trait Estimation[This<:Parameter] {
  this: This =>
  def estimate(model:Model = Global.defaultModel)(implicit e:Estimator[This]): Unit = e.estimate(this, model)
}

trait Estimator[P<:Parameter] {
  def estimate(parameter:P, model:Model): Unit
}


trait AbstractParameterRef extends Variable {
  def abstractValue: AnyRef //Parameter
  def child: GeneratedVar
}
class ParameterRef[P<:Parameter,C<:GeneratedVar](p:P, override val child:C) extends RefVariable(p) with AbstractParameterRef {
  p.addChild(child)(null)
  //println("ParameterRef.init parent="+p.getClass.getName+"@"+p.hashCode+" child="+child)
  // This 'set' method is no longer called in initialization of RefVariable, hence line above
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
trait RealFunction extends DeterministicFunction with RealVarParameter
abstract class RealOpConstant(val real:RealVarParameter) extends RealFunction with GeneratedVar {
  real.addChild(this)(null) // But now might not garbage collect this when we want to
  def parents = List(real)
  def pr = 1.0 // Deterministic value given parent
}
class RealPlusConstant(override val real:RealVarParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue + constant
}
class RealTimesConstant(override val real:RealVarParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue * constant
}

