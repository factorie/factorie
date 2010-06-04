/* Copyright (C) 2009-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */


package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,HashSet,ArrayBuffer}

/** Similar to the definition of "gate" from Tom Minka and Jon Winn.
    A discrete variable that indicates which one among a finite number relations are currently in effect.
    Here can be thought of as a collection of @ref{GatedRefVariable}s. 
    Primarily used to implement a "mixture choice" in finite mixture models.
    @see MixtureChoice */
trait Gate[A<:AbstractGatedRefVariable] extends DiscreteVariable {
  /** The collection of variable references controlled by the gate. */
  var gateContents: List[A] = Nil
  def +=(v:A): this.type = { 
    require(v.domainSize == domainSize)
    gateContents = v :: gateContents
    v.gate = this
    v.setByIndex(this.intValue)(null)
    this
  }
  override def setByIndex(newIndex:Int)(implicit d:DiffList): Unit = {
    super.setByIndex(newIndex)
    for (ref <- gateContents) ref.setByIndex(newIndex)
  }
  def setToNull(implicit d:DiffList): Unit = {
    super.setByIndex(-1)
    for (ref <- gateContents) ref.setToNull
  }
}

//trait Gate extends GenericGate[AbstractGatedRefVariable]

/** Abstract stand-in for GatedRefVariable that doesn't take type parameters.  
    Exists to avoid dealing with contravariant typing in MixtureComponentRef. 
    @author Andrew McCallum */
trait AbstractGatedRefVariable {
  def gate: Gate[_]
  def gate_=(g:Gate[_]): Unit
  def domainSize: Int
  def setToNull(): Unit
  def setByIndex(newIndex:Int)(implicit d:DiffList): Unit
}

/** A RefVariable whose value is controled by a Gate.  This is used as a reference to the Distribution of samples generated from a Mixture.
    @author Andrew McCallum */
trait GatedRefVariable[A<:AnyRef] extends RefVariable[A] with AbstractGatedRefVariable {
  //this : RefVariable[A] =>
  type VariableType <: GatedRefVariable[A]
  private var _gate: Gate[_] = null // TODO Are we sure we need to know who our gate is?  Can we save memory by deleting this?
  def gate = _gate
  def gate_=(g:Gate[_]): Unit = if (_gate == null) _gate = g else throw new Error("Gate already set.")
  // Not the current value of this GatedRefVariable.
  // Returns the value associated with a certain integer index value of the gate.  
  // The gate uses this to call grf.set(grf.value(this.intValue)). 
  def value(index:Int): A
  def setByIndex(index:Int)(implicit d:DiffList): Unit = set(value(index))
  def setToNull(implicit d:DiffList): Unit = set(null.asInstanceOf[A])
  def domainSize: Int
}


/*
object GateTest {
  class Word(s:String) extends GeneratedCategoricalVariable[String,Word](s)
  class Topic extends DirichletMultinomial[Word] with MixtureComponent
  class Z extends MixtureChoice[Z]; Domain[Z].size = Domain[Topic].size
  val z = new Z
  val w = new Word("foo") ~ [Topic](z)
}


class Z extends MixtureChoice[Topic] {
  Domain[Z].size = 
}



class DieRoll extends DiscreteVariable
Domain[DieRoll].size = 6

// or

class DieRoll extends DiscreteVariable(6)

// or

new DiscreteDomain[DieRoll] { def size = 6 }
*/
