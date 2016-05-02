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
package cc.factorie.directed

import cc.factorie.model.{Factor, Model}
import cc.factorie.variable.{DiffList, MutableVar, Var, VarWithDeterministicValue}

import scala.collection.mutable.{ArrayBuffer, HashMap}

trait DirectedModel extends Model {
  def getParentFactor(v:Var): Option[DirectedFactor]
  def getChildFactors(v:Var): Option[Iterable[DirectedFactor]]
  def parentFactor(v:Var): DirectedFactor
  def childFactors(v:Var): Iterable[DirectedFactor]
  def extendedParentFactors(v:Var): Iterable[DirectedFactor]
  def extendedChildFactors(v:Var): Iterable[DirectedFactor]
  def extendedParents(v:Var): Iterable[Var]
  def extendedChildren(v:Var): Iterable[Var]
  def parents(v:Var): Seq[Var]
  def children(v:Var): Iterable[Var]
  def sampleFromParents(v:MutableVar)(implicit d:DiffList, random: scala.util.Random): Unit
}

trait MutableDirectedModel extends DirectedModel {
  def +=(f:DirectedFactor): Unit
  def -=(f:DirectedFactor): Unit
}
object DirectedModel {
  /** Constructor for a default DirectedModel */
  def apply(): ItemizedDirectedModel = new ItemizedDirectedModel
}
class ItemizedDirectedModel extends MutableDirectedModel {
  private val _parentFactor = new HashMap[Var,DirectedFactor]
  private val _childFactors = new HashMap[Var,ArrayBuffer[DirectedFactor]]
  override def addFactors(variable:Var, result:scala.collection.mutable.Set[Factor]): Unit = {
    if (_parentFactor.contains(variable)) result += _parentFactor(variable)
    // TODO Do we need to use extendedParentFactors also?
    //if (_childFactors.contains(v)) result ++= _childFactors(v)
    if (_childFactors.contains(variable)) result ++= extendedChildFactors(variable)
    // TODO special handling of ContainerVariable[_]??
    //result
  }
//  override def addFactors[A<:Iterable[Factor] with collection.generic.Growable[Factor]](v:Variable, result:A): A = {
//    val set = new scala.collection.mutable.HashSet[Factor]
//    if (_parentFactor.contains(v)) set += _parentFactor(v)
//    // TODO Do we need to use extendedParentFactors also?
//    //if (_childFactors.contains(v)) result ++= _childFactors(v)
//    if (_childFactors.contains(v)) set ++= extendedChildFactors(v)
//    // TODO special handling of ContainerVariable[_]??
//    result ++= set
//    result
//  }
  def factors(variables:Iterable[Var]): Iterable[Factor] = { val result = new collection.mutable.HashSet[Factor]; variables.foreach(v => addFactors(v, result)); result }
  override def factors(v:Var): Iterable[Factor] = { val result = new collection.mutable.HashSet[Factor]; addFactors(v, result); result }
  def allFactors: Iterable[Factor] = _parentFactor.values ++ _childFactors.values.flatten
  def getParentFactor(v:Var): Option[DirectedFactor] = _parentFactor.get(v)
  def getChildFactors(v:Var): Option[Iterable[DirectedFactor]] = _childFactors.get(v)
  def parentFactor(v:Var): DirectedFactor = _parentFactor.getOrElse(v, null)
  def childFactors(v:Var): Iterable[DirectedFactor] = _childFactors.getOrElse(v, Nil)
  def extendedParentFactors(v:Var): Iterable[DirectedFactor] = {
    val result = new ArrayBuffer[DirectedFactor]
    result ++= getParentFactor(v)
    for (parent <- parents(v); if parent.isInstanceOf[VarWithDeterministicValue]) result ++= extendedParentFactors(parent)
    result
  }
  def extendedChildFactors(v:Var): Iterable[DirectedFactor] = {
    if (!_childFactors.contains(v)) return Nil
    val result = new ArrayBuffer[DirectedFactor]
    for (factor <- childFactors(v)) {
      result += factor
      if (factor.child.isInstanceOf[VarWithDeterministicValue]) result ++= extendedChildFactors(factor.child)
    }
    result
  }
  def extendedParents(v:Var): Iterable[Var] = extendedParentFactors(v).flatMap(_.parents)
  def extendedChildren(v:Var): Iterable[Var] = extendedChildFactors(v).map(_.child)
  def parents(v:Var): Seq[Var] =
    if (_parentFactor.contains(v)) _parentFactor(v).parents else Nil
  def children(v:Var): Iterable[Var] = childFactors(v).map(_.child)

  def sampleFromParents(v:MutableVar)(implicit d:DiffList, random: scala.util.Random): Unit = v.set(parentFactor(v).sampledValue.asInstanceOf[v.Value])

  def +=(f:DirectedFactor): Unit = {
    require(!_parentFactor.contains(f.child))
    _parentFactor(f.child) = f
    f.parents.foreach(v => _childFactors.getOrElseUpdate(v, new ArrayBuffer[DirectedFactor]) += f)
  }
  def -=(f:DirectedFactor): Unit = {
    require(_parentFactor(f.child) eq f)
    _parentFactor.remove(f.child)
    f.parents.foreach(v => _childFactors(v) -= f)
  }
  def ++=(fs:Iterable[DirectedFactor]): Unit = fs.foreach(f => this.+=(f))
  def --=(fs:Iterable[DirectedFactor]): Unit = fs.foreach(f => this.-=(f))
}