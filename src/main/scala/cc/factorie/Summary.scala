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
import cc.factorie.generative._
import cc.factorie.la.{Tensor, Outer1Tensor2}
import cc.factorie

/** The result of inference: a collection of Marginal objects.
    @author Andrew McCallum */
trait Summary {
  /** The collection of all Marginals available in this Summary */
  def marginals: Iterable[Marginal]
  /** If this Summary has a univariate Marginal for variable v, return it; otherwise return null. */
  def marginal(v:Var): Marginal
  /** If this Summary has a Marginal that touches all or a subset of the neighbors of this factor
      return the Marginal with the maximally-available subset. */
  def marginal(factor:Factor): FactorMarginal
  def factorMarginals: Iterable[FactorMarginal]
  def logZ: Double
  /** If this summary has a univariate Marginal for variable v, return it in an Option; otherwise return None. */
  def getMarginal(v:Var): Option[Marginal] = { val m = marginal(v); if (m eq null) None else Some(m) }
  def setToMaximize(implicit d:DiffList): Unit = marginals.foreach(_.setToMaximize(d)) // Note that order may matter here if Marginals overlap with each other!
}

/** A Summary that can be used to gather weighted samples into its Marginals. */
// TODO Consider the relationship between this and Accumulator
// TODO Consider removing this
trait IncrementableSummary extends Summary {
  def incrementCurrentValues(weight:Double): Unit
}

/** A Summary that contains multiple Marginals of type M, each a marginal distribution over a single variable. */
class Summary1[V<:Var,M<:Marginal] extends Summary {
  protected val _marginals = new scala.collection.mutable.HashMap[V,M]
  protected val _factorMarginals = new scala.collection.mutable.HashMap[Factor,FactorMarginal]
  def marginals = _marginals.values
  def factorMarginals = _factorMarginals.values
  def logZ = throw new Error("logZ is not defined")
  def marginal(v:Var): M = _marginals(v.asInstanceOf[V]) // We don't actually care that this type check does nothing because only Vs could be added to the HashMap
  def marginal(f:Factor): FactorMarginal = _factorMarginals(f)
  def +=(marginal:M) = {
    val vars = marginal.variables
    require(vars.size == 1)
    val v = vars.head.asInstanceOf[V]
    if (_marginals.contains(v)) throw new Error("Marginal already present for variable "+v)
    _marginals(v) = marginal
  }
  def +=(marginal:FactorMarginal) = {
    if (_factorMarginals.contains(marginal.factor)) throw new Error("Marginal already present for factor "+marginal.factor)
    _factorMarginals(marginal.factor) = marginal
  }
}

/** A Summary containing only one Marginal. */
class SingletonSummary[M<:Marginal](val marginal:M) extends Summary {
  def marginals = Seq(marginal)
  def marginal(v:Var): M = if (Seq(v) == marginal.variables) marginal else null.asInstanceOf[M]
  def marginal(f:Factor) = null
  def logZ = throw new Error("logZ not definable for SingletonSummary")
  def factorMarginals = Nil
}



/** A Summary with all its probability on one variable-value Assignment. */
class AssignmentSummary(val assignment:Assignment) extends Summary {
  def marginals = assignment.variables.map(v=> new Marginal {
    def variables = Seq(v)
    def setToMaximize(implicit d: DiffList) = v match { case vv:MutableVar[Any] => vv.set(assignment(vv)) }
  })
  def marginal(v:Var): Marginal = null
  def marginal(f:Factor): FactorMarginal = null.asInstanceOf[FactorMarginal]
  override def setToMaximize(implicit d:DiffList): Unit = assignment.globalize(d)
  def logZ = throw new Error("AssignmentSummary does not define logZ")
  def factorMarginals = Nil
}

// An AssignmentSummary that can be used as a result of inference.
class MAPSummary(val mapAssignment: Assignment, factors: Seq[Factor]) extends Summary {
  /** The collection of all Marginals available in this Summary */
  class SingletonMarginal(v: Var) extends Marginal {
    def variables = Seq(v)
    def setToMaximize(implicit d: DiffList) { v match { case v: MutableVar[Any @unchecked] => v.set(mapAssignment(v)) } }
  }
  val marginals = mapAssignment.variables.map(new SingletonMarginal(_))
  def marginal(v: Var) = mapAssignment.get(v) match {
    case Some(_) => new SingletonMarginal(v)
    case None => null
  }
  class SingletonFactorMarginal(val factor: Factor) extends FactorMarginal {
    val tensorStatistics = factor.assignmentStatistics(mapAssignment).asInstanceOf[Tensor]
    def variables = factor.variables
    val score = factor.assignmentScore(mapAssignment)
  }
  def marginal(factor: Factor): SingletonFactorMarginal = new SingletonFactorMarginal(factor)
  def factorMarginals = factors.map(marginal)
  def logZ = factors.map(marginal(_).score).sum
}


/** A summary with a separate Proportions distribution for each of its DiscreteVars */
// TODO Consider renaming FullyFactorizedDiscreteSummary or IndependentDiscreteSummary or PerVariableDiscreteSummary
// TODO Consider making this inherit from Summary1
class DiscreteSummary1[V<:DiscreteVar] extends IncrementableSummary {
  def this(vs:Iterable[V]) = { this(); ++=(vs) }
  //val variableClass = m.erasure
  val _marginals1 = new scala.collection.mutable.HashMap[V,SimpleDiscreteMarginal1[V]]
  def marginals = _marginals1.values
  def variables = _marginals1.keys
  lazy val variableSet = variables.toSet
  def marginal(v1:Var): SimpleDiscreteMarginal1[V] = _marginals1(v1.asInstanceOf[V])
  def marginal2(vs:Var*): DiscreteMarginal = vs match {
    case Seq(v:V) => _marginals1(v) // Note, this doesn't actually check for a type match on V, because of erasure, but it shoudn't matter
    case Seq(v:V, w:V) => new DiscreteMarginal2[V,V](v, w, new NormalizedTensorProportions2(new Outer1Tensor2(_marginals1(v).proportions,_marginals1(w).proportions), false))
    case _ => null
  }
  def marginal(f:Factor): FactorMarginal = null
  def +=(m:SimpleDiscreteMarginal1[V]): Unit = _marginals1(m._1) = m
  def +=(v:V): Unit = this += new SimpleDiscreteMarginal1(v, null) // but not yet initialized marginal proportions
  def ++=(vs:Iterable[V]): Unit = vs.foreach(+=(_))
  //def ++=(ms:Iterable[DiscreteMarginal1[V]]): Unit = ms.foreach(+=(_))
  def incrementCurrentValues(weight:Double): Unit = for (m <- marginals) m.incrementCurrentValue(weight)
  //def maximize(implicit d:DiffList): Unit = for (m <- marginals) m._1.asInstanceOf[DiscreteVariable].set(m.proportions.maxIndex)
  def factorMarginals = Nil
  def logZ = throw new Error("DiscreteSummary1 does not define logZ")
}
