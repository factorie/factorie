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

/** A Factor with one neighboring variable */
trait Factor1[N1<:Variable] extends Factor {
  factor =>
  type NeighborType1 = N1
  type StatisticsType <: cc.factorie.Statistics
  protected def thisFactor: this.type = this
  def _1: N1
  def numVariables = 1
  override def variables = IndexedSeq(_1)
  def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
  override def values = new Values(_1.value)
  // Should the next line read instead V<:N1 ?? -akm
  def valuesAssigning[V<:Variable](variable:V, value:V#Value): Unit = if (variable eq _1) new Values(value.asInstanceOf[N1#Value]) else throw new Error
  case class Values(_1:N1#Value) extends cc.factorie.Values {
    override def apply[B <: Variable](v: B) = get(v).get
    def variables = Seq(factor._1)
    def get[B <: Variable](v: B) = if(contains(v)) Some(_1.asInstanceOf[B#Value]) else None
    def contains(v: Variable) = v == factor._1
    override def statistics: StatisticsType = Factor1.this.statistics(this)
    override def index(varying:Set[Variable]): Int = {
      if(varying.contains(factor._1)) {
        _1 match {
          case dv: DiscreteValue => dv.intValue
          case _ => -1
        }
      } else -1
    }
  }
  def statistics: StatisticsType = statistics(values)
  def statistics(v:Values): StatisticsType

  // For implementing sparsity in belief propagation
  def isLimitingValuesIterator = false
  def limitedDiscreteValuesIterator: Iterator[Int] = Iterator.empty // TODO Replace with IntSeq for efficiency

  /** valuesIterator in style of specifying fixed neighbors */
  def valuesIterator(fixed: Assignment): Iterator[Values] = {
    if (fixed.contains(_1)) Iterator.single(Values(fixed(_1)))
    else if (isLimitingValuesIterator) {
      val d = _1.domain.asInstanceOf[DiscreteDomain]
      limitedDiscreteValuesIterator.map(i => Values(d.getValue(i).asInstanceOf[N1#Value]))
    } else _1.domain match {
      case d:IterableDomain[_] => d.asInstanceOf[IterableDomain[N1#Value]].values.iterator.map(value => Values(value))
    }
  }
  /** valuesIterator in style of specifying varying neighbors */
  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
    if (varying.size != 1 || varying.head != _1)
      throw new Error("Factor1.valuesIterator cannot vary arguments.")
    else if (isLimitingValuesIterator) {
      val d = _1.domain.asInstanceOf[DiscreteDomain]
      limitedDiscreteValuesIterator.map(i => Values(d.getValue(i).asInstanceOf[N1#Value]))
    } else _1.domain match {
      case d:IterableDomain[_] => d.asInstanceOf[IterableDomain[N1#Value]].values.iterator.map(value => Values(value))
    }
  }
}

/** A Factor with one neighboring variable, whose statistics are simply the value of that neighboring variable. */
trait FactorWithStatistics1[N1<:Variable] extends Factor1[N1] {
  self =>
  type StatisticsType = Statistics // TODO Consider making this <: and defining separate StatisticsReturnType and StatisticsArgumentType
  case class Statistics(_1:N1#Value) extends cc.factorie.Statistics {
    lazy val score = self.score(this)
  }
  override def statistics(v:Values) = Statistics(v._1).asInstanceOf[StatisticsType]
  def score(s:Statistics): Double
}

trait Family1[N1<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomains = Seq(neighborDomain1)
  type FactorType = Factor
  type ValuesType = Factor#Values

  // For implementing sparsity in belief propagation
  var isLimitingValuesIterator = false
  lazy val limitedDiscreteValues = new scala.collection.mutable.HashSet[Int]
  def addLimitedDiscreteValues(values:Iterable[Int]): Unit = limitedDiscreteValues ++= values
  
  final case class Factor(_1:N1) extends super.Factor with Factor1[N1] {
    type StatisticsType = Family1.this.StatisticsType
    override def statistics(values:Values): StatisticsType = thisFamily.statistics(values)
    override def isLimitingValuesIterator = Family1.this.isLimitingValuesIterator
    override def limitedDiscreteValuesIterator: Iterator[Int] = limitedDiscreteValues.iterator
  } 
  // Cached statistics
  private var cachedStatisticsArray: Array[StatisticsType] = null
  override def cachedStatistics(values:ValuesType): StatisticsType =
    if (Template.enableCachedStatistics) {
    values._1 match {
    case v:DiscreteValue => {
      if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v.domain.size).asInstanceOf[Array[StatisticsType]]
      val i = v.intValue
      if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = values.statistics
      cachedStatisticsArray(i)
    }
    case _ => values.statistics
  }} else values.statistics
  /** You must clear cache the cache if DotTemplate.weights change! */
  override def clearCachedStatistics: Unit =  cachedStatisticsArray = null
}

trait Statistics1[S1] extends Family {
  self =>
  type StatisticsType = Stat
  // TODO Consider renaming "Stat" to "Statistics"
  case class Stat(_1:S1) extends super.Statistics {
    lazy val score = self.score(this) 
  }
  def score(s:Stat): Double
}

trait VectorStatistics1[S1<:DiscreteVectorValue] extends VectorFamily {
  self =>
  type StatisticsType = Stat
  // Use Scala's "pre-initialized fields" syntax because super.Stat needs vector to initialize score
  final case class Stat(_1:S1) extends { val vector: Vector = _1 } with super.Statistics {
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
    } else if (_1.domain != _statisticsDomains(0)) throw new Error("Domain doesn't match previously cached domain.")
    lazy val score = self.score(this)
  }
  def score(s:Stat): Double
}

trait DotStatistics1[S1<:DiscreteVectorValue] extends VectorStatistics1[S1] with DotFamily {
  def setWeight(entry:S1, w:Double) = entry match {
    case d:DiscreteValue => weights(d.intValue) = w
    case ds:DiscreteVectorValue => ds.activeDomain.foreach(i => weights(i) = w)
  }
}

trait FamilyWithStatistics1[N1<:Variable] extends Family1[N1] with Statistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
}

trait FamilyWithVectorStatistics1[N1<:DiscreteVectorVar] extends Family1[N1] with VectorStatistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
}

trait FamilyWithDotStatistics1[N1<:DiscreteVectorVar] extends Family1[N1] with DotStatistics1[N1#Value] {
  def statistics(vals:Values): StatisticsType = Stat(vals._1)
}

