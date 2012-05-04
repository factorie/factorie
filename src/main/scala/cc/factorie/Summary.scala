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

/** The result of inference: a collection of Marginal objects. */
trait Summary[+M<:Marginal] {
  def marginals: Iterable[M]
  def getMarginal(vs:Variable*): Option[M] = { val m = marginal(vs:_*); if (m eq null) None else Some(m) } 
  def marginal(vs:Variable*): M // TODO Think carefully about how order of arguments should not matter.
  def setToMaximize(implicit d:DiffList): Unit = marginals.foreach(_.setToMaximize(d)) // Note that order may matter here if Marginals overlap with each other!
  // def variables: Iterable[Variable] // TODO Should we also have a method like this?
}

/** A Summary that can be used to gather weighted samples into its Marginals. */
trait IncrementableSummary[+M<:Marginal] extends Summary[M] {
  def incrementCurrentValues(weight:Double): Unit
}

/** A Summary containing only one Marginal. */
class SingletonSummary[M<:Marginal](val marginal:M) extends Summary[M] {
  def marginals = Seq(marginal)
  // TODO In the conditional below, order shouldn't matter!
  def marginal(vs:Variable*): M = if (vs == marginal.variables) marginal else null.asInstanceOf[M]
}

/** A Summary with all its probability on one variable-value Assignment.  Note that Assignment inherits from Marginal. */
class AssignmentSummary(val assignment:Assignment) extends Summary[Assignment] {
  def marginals = Seq(assignment)
  def marginal(vs:Variable*): Assignment = if (vs.toSet == assignment.variables.toSet) assignment else null
  override def setToMaximize(implicit d:DiffList): Unit = assignment.globalize(d)
}

/** A summary with a separate Proportions distribution for each of its DiscreteVars */
// TODO Rename FullyFactorizedDiscreteSummary or IndependentDiscreteSummary or PerVariableDiscreteSummary
class DiscreteSummary1[V<:DiscreteVar] extends IncrementableSummary[DiscreteMarginal1[V]] {
  def this(vs:Iterable[V]) = { this(); ++=(vs) }
  //val variableClass = m.erasure
  protected val _marginals1 = new scala.collection.mutable.HashMap[V,DiscreteMarginal1[V]]
  def marginals = _marginals1.values
  def variables = _marginals1.keys
  def marginal1(v1:V) = _marginals1(v1)
  def marginal(vs:Variable*): DiscreteMarginal1[V] = vs match {
    case Seq(v:V) => _marginals1(v) // Note, this doesn't actually check for a type match on V, because of erasure, but it shoudn't matter
    case _ => null
  }
  def +=(v:V): Unit = _marginals1(v) = new DiscreteMarginal1(v, null) // but not yet initialized
  def ++=(vs:Iterable[V]): Unit = vs.foreach(+=(_))
  def +=(m:DiscreteMarginal1[V]): Unit = _marginals1(m._1) = m
  //def ++=(ms:Iterable[DiscreteMarginal1[V]]): Unit = ms.foreach(+=(_))
  def incrementCurrentValues(weight:Double): Unit = for (m <- marginals) m.incrementCurrentValue(weight)
  //def maximize(implicit d:DiffList): Unit = for (m <- marginals) m._1.asInstanceOf[DiscreteVariable].set(m.proportions.maxIndex)
}
