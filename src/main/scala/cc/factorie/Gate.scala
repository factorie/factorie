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
import scala.collection.mutable.{ListBuffer,HashSet,ArrayBuffer}

/** Similar to the definition of "gate" from Tom Minka and Jon Winn.
    A discrete variable that indicates which one among a finite number relations are currently in effect.
    Here can be thought of as a collection of @ref{GatedRefVariable}s. 
    Primarily used to implement a "mixture choice" in finite mixture models.
    @see MixtureChoice
    @author Andrew McCallum */
trait Gate extends DiscreteVariable {
  /** The collection of variable references controlled by the gate. */
  private var _gatedRefs: List[AbstractGatedRefVariable] = Nil
  def gatedRefs: List[AbstractGatedRefVariable] = _gatedRefs
  def +=(v:AbstractGatedRefVariable): this.type = {
    //println("Gate.+= "+v)
    assert(_gatedRefs ne null)
    assert(v.domainSize == domain.size)
    _gatedRefs = v :: _gatedRefs
    assert(v.gate == this)
    //println("Gate.+= setByIndex="+this.intValue) // xxx
    v.setByIndex(this.intValue)(null)
    this
  }
  override def set(newIndex:Int)(implicit d:DiffList): Unit = {
    super.set(newIndex)
    if (_gatedRefs ne null) for (ref <- _gatedRefs) {
      ref.setByIndex(newIndex)
    }
  }
  def setToNull(implicit d:DiffList): Unit = {
    super.set(-1)
    for (ref <- _gatedRefs) ref.setToNull
  }
}


/** Abstract stand-in for GatedRefVariable that doesn't take type parameters.  
    Among other things, this avoids impossible contravariant typing in MixtureComponentRef
    (which no longer exists, but may have an analogue in the future).
    @author Andrew McCallum */
trait AbstractGatedRefVariable {
  def gate: Gate
  //def gate_=(g:Gate): Unit
  def domainSize: Int
  def setToNull(implicit d:DiffList): Unit
  def setByIndex(newIndex:Int)(implicit d:DiffList): Unit
  def abstractValue: AnyRef
}

/** A RefVariable whose value is controled by a Gate.  This is used as a reference to the Distribution of samples generated from a Mixture.
    @author Andrew McCallum */
trait GatedRefVariable[A<:AnyRef] extends RefVariable[A] with AbstractGatedRefVariable {
  type VariableType <: GatedRefVariable[A]
  def gate: Gate // TODO Are we sure we need to know who our gate is?  Can we save memory by deleting this?
  /** Not the current value of this GatedRefVariable.
      Returns the value associated with a certain integer index value of the gate.  
      The gate uses this to call grf.set(grf.valueForIndex(this.intValue)).  */
  def valueForIndex(index:Int): A
  def setByIndex(index:Int)(implicit d:DiffList): Unit = set(valueForIndex(index))
  def setToNull(implicit d:DiffList): Unit = set(null.asInstanceOf[A])
  def domainSize: Int
}
