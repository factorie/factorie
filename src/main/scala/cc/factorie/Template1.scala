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

abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template
{
  type NeighborType1 = N1
  val neighborClass1 = nm1.erasure
  val neighborClass1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  private var _neighborDomain1: Domain[N1#Value] = null
  def neighborDomain1: Domain[N1#Value] = 
    if (_neighborDomain1 eq null) 
      throw new Error("You must override neighborDomain1 if you want to access it before creating any Factor objects")
    else
      _neighborDomain1

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
  type FactorType = Factor
  final case class Factor(_1:N1, override var outer:cc.factorie.Factor = null) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomain1 = _1.domain.asInstanceOf[Domain[N1#Value]]
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _neighborDomain1
    }
    def numVariables = 1
    def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
    override def variables: IndexedSeq[Variable] = IndexedSeq(_1)
    def values: ValuesType = new Values(_1.value)
    def statistics: StatisticsType = Template1.this.statistics(values)
    override def cachedStatistics: StatisticsType = Template1.this.cachedStatistics(values)
    // Note.  If someone subclasses Factor, then you might not get that subclass!  This is why class Factor is final.
    def copy(s:Substitutions) = Factor(s.sub(_1))
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value) extends super.Values {
    def statistics = Template1.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  //def stats(v:Variable): Iterable[StatisticsType] = factors(v).map(_.statistics) // TODO Do we need to consider a flatMap here?
  private var cachedStatisticsArray: Array[StatisticsType] = null
  override def cachedStatistics(vals:Values, stats:(ValuesType)=>StatisticsType): StatisticsType =
    if (Template.enableCachedStatistics) {
    vals._1 match {
    case v:DiscreteValue => {
      if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v.domain.size).asInstanceOf[Array[StatisticsType]]
      val i = v.intValue
      if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = stats(vals)
      cachedStatisticsArray(i)
    }
    case _ => stats(vals)
  }} else stats(vals)
  /** You must clear cache the cache if DotTemplate.weights change! */
  override def clearCachedStatistics: Unit =  cachedStatisticsArray = null
  /** valuesIterator in style of specifying fixed neighbors */
  def valuesIterator(factor:FactorType, fixed: Assignment): Iterator[Values] = {
    if (fixed.contains(factor._1)) Iterator.single(Values(fixed(factor._1)))
    else factor._1.domain match {
      case d:IterableDomain[_] => d.asInstanceOf[IterableDomain[N1#Value]].values.iterator.map(value => Values(value))
    }
  }
  /** valuesIterator in style of specifying varying neighbors */
  def valuesIterator(factor:FactorType, varying:Seq[Variable]): Iterator[Values] = {
    if (varying.size != 1 || varying.head != factor._1)
      throw new Error("Template1.valuesIterator cannot vary arguments.")
    else factor._1.domain match {
      case d:IterableDomain[_] => d.asInstanceOf[IterableDomain[N1#Value]].values.iterator.map(value => Values(value))
    }
  }
}

trait Statistics1[S1] extends Template {
  final case class Stat(_1:S1) extends super.Statistics
  type StatisticsType = Stat
}

trait VectorStatistics1[S1<:DiscreteVectorValue] extends VectorTemplate {
  type StatisticsType = Stat
  // Use Scala's "pre-initialized fields" syntax because super.Stat needs vector to initialize score
  final case class Stat(_1:S1) extends { val vector: Vector = _1 } with super.Statistics { 
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
    } else if (_1.domain != _statisticsDomains(0)) throw new Error("Domain doesn't match previously cached domain.")
  }
}

trait DotStatistics1[S1<:DiscreteVectorValue] extends VectorStatistics1[S1] with DotTemplate {
  def setWeight(entry:S1, w:Double) = entry match {
    case d:DiscreteValue => weights(d.intValue) = w
    case ds:DiscreteVectorValue => ds.activeDomain.foreach(i => weights(i) = w)
  }
}

abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1] with Statistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
}

abstract class TemplateWithVectorStatistics1[N1<:DiscreteVectorVar](implicit nm1:Manifest[N1]) extends Template1[N1] with VectorStatistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
}

class TemplateWithDotStatistics1[N1<:DiscreteVectorVar](implicit nm1:Manifest[N1]) extends Template1[N1] with DotStatistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
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

