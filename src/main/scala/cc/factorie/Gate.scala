/* Copyright (C) 2009-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */


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
    assert(v.domainSize == domainSize)
    _gatedRefs = v :: _gatedRefs
    assert(v.gate == this)
    //println("Gate.+= setByIndex="+this.intValue) // xxx
    v.setByIndex(this.intValue)(null)
    this
  }
  override def set(newIndex:Int)(implicit d:DiffList): Unit = {
    super.set(newIndex)
    //println("Gate.setByIndex _gatedRefs="+_gatedRefs) // xxx
    //new Exception().printStackTrace()
    if (_gatedRefs ne null) for (ref <- _gatedRefs) {
      //println("Gate.setByIndex ref="+ref)
      ref.setByIndex(newIndex)
    }
  }
  def setToNull(implicit d:DiffList): Unit = {
    super.set(-1)
    for (ref <- _gatedRefs) ref.setToNull
  }
}


/** Abstract stand-in for GatedRefVariable that doesn't take type parameters.  
    Among other things, this avoids impossible contravariant typing in MixtureComponentRef. 
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
  //private var _gate: Gate = null 
  def gate: Gate // TODO Are we sure we need to know who our gate is?  Can we save memory by deleting this?
  //def gate_=(g:Gate): Unit = if (_gate == null) _gate = g else throw new Error("Gate already set.")
  // Not the current value of this GatedRefVariable.
  // Returns the value associated with a certain integer index value of the gate.  
  // The gate uses this to call grf.set(grf.value(this.intValue)). 
  def valueForIndex(index:Int): A
  def setByIndex(index:Int)(implicit d:DiffList): Unit = set(valueForIndex(index))
  def setToNull(implicit d:DiffList): Unit = set(null.asInstanceOf[A])
  def domainSize: Int
}
