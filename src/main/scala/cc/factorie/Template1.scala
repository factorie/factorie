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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._


abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Family1[N1] with Template
{
  val neighborClass1 = nm1.erasure
  val neighborClass1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  // Factors
  def factors(v:Variable): Iterable[FactorType] = {
    // TODO Given the surprise about how slow Manifest <:< was, I wonder how slow this is when there are lots of traits!
    // When I substituted "isAssignable" for HashMap caching in GenericSampler I got 42.8 versus 44.4 seconds ~ 3.7%  Perhaps worth considering?
    var ret = new ListBuffer[FactorType]
    // Create Factor iff variable class matches and the variable domain matches
    if (neighborClass1.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain1 eq v.domain) || (_neighborDomain1 eq null))) ret ++= unroll1(v.asInstanceOf[N1])
    if ((neighborClass1a ne null) && neighborClass1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    // TODO It would be so easy for the user to define Variable.unrollCascade to cause infinite recursion.  Can we make better checks for this?
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType] = new Factor(v)
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
}


abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1] with Statistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1, vals.inner.map(_.statistics))
}

abstract class TemplateWithVectorStatistics1[N1<:DiscreteVectorVar](implicit nm1:Manifest[N1]) extends Template1[N1] with VectorStatistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1, vals.inner.map(_.statistics))
}

class TemplateWithDotStatistics1[N1<:DiscreteVectorVar](implicit nm1:Manifest[N1]) extends Template1[N1] with DotStatistics1[N1#Value] {
  type FamilyType <: TemplateWithDotStatistics1[N1]
  def statistics(vals:Values): StatisticsType = Stat(vals._1, vals.inner.map(_.statistics))
}



/*
trait DiscreteFactorSettings1 extends Template {
  this: VectorTemplate { type Neighbor1Type <: DiscreteVar; type FactorType <: { def _1:DiscreteVariable } } =>
  val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  val nds1 = ndd1.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = this.isInstanceOf[Template { type FactorType <: { def _1:DiscreteVariable } }]
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = factor._1 match {
    case v1: DiscreteVariable => {
      if (settingsSparsified && (sparseSettingsValues ne null))
        forIndex(sparseSettingsValues.length)(i => { v1.set(sparseSettingsValues(i))(null); f })
      else
        forIndex(nds1)(i => { v1.set(i)(null); f })
    }
    case _ => throw new RuntimeException("Settings of this factor are not iterable")
  }
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = {
    require(vs.size == 1); require(factor._1 == vs.head)
    forSettings(factor)(f)
  }
  def forSettingsExcept(factor:FactorType, v:Variable)(f: =>Unit): Unit = require(factor._1 == v)
  private var settingsSparsified = false
  private var sparseSettingsValues: Array[Int] = null
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    val sparseInts = new HashSet[Int]
    // Only works for DiscreteVar
    vs.foreach(_ match { case v:DiscreteVar => sparseInts += v.intValue })
    sparseSettingsValues = sparseInts.toArray
    settingsSparsified = true
  }
}
*/

