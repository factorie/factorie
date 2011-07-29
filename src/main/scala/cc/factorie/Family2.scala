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

/*trait Factor2[N1<:Variable,N2<:Variable] extends FamilyWithNeighborDomains {
  trait Factor extends super.Factor {
    def _1: N1
    def _2: N2
  }
}*/

// TODO Consider making a family-less
// class Factor2[N1<:Variable,N2<:Variable](_1:N1, _2:N2)
// or
// trait Factor2[N1<:Variable,N2<:Variable] { def _1:N1; def _2:N2 }

trait Family2[N1<:Variable,N2<:Variable] extends FamilyWithNeighborDomains /*with Factor2[N1,N2]*/ {
  type NeighborType1 = N1
  type NeighborType2 = N2
  protected var _neighborDomain1: Domain[N1#Value] = null
  protected var _neighborDomain2: Domain[N2#Value] = null
  def neighborDomain1: Domain[N1#Value] = if (_neighborDomain1 eq null) throw new Error("You must override neighborDomain1 if you want to access it before creating any Factor objects") else _neighborDomain1
  def neighborDomain2: Domain[N2#Value] = if (_neighborDomain2 eq null) throw new Error("You must override neighborDomain2 if you want to access it before creating any Factor objects") else _neighborDomain2

  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, override var inner:Seq[cc.factorie.Factor] = Nil, override var outer:cc.factorie.Factor = null) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomain1 = _1.domain.asInstanceOf[Domain[N1#Value]]
      _neighborDomain2 = _2.domain.asInstanceOf[Domain[N2#Value]]
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _neighborDomain1
      _neighborDomains += _neighborDomain2
    }
    def numVariables = 2
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
    override def variables: IndexedSeq[Variable] = IndexedSeq(_1, _2)
    def values: ValuesType = new Values(_1.value, _2.value, inner.map(_.values))
    def statistics: StatisticsType = Family2.this.statistics(values)
    override def cachedStatistics: StatisticsType = Family2.this.cachedStatistics(values)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2), inner.map(_.copy(s)), outer)
  }
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value, override val inner:Seq[cc.factorie.Values] = Nil) extends super.Values {
    def statistics = Family2.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  private var cachedStatisticsArray: Array[StatisticsType] = null
  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
  override def cachedStatistics(values:Values, stats:(ValuesType)=>StatisticsType): StatisticsType =
    if (Template.enableCachedStatistics) values._1 match {
    case v1:DiscreteValue => { 
      values._2 match {
        case v2:DiscreteValue => {
          //println("Template2.cachedStatistics")
          if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v1.domain.size * v2.domain.size).asInstanceOf[Array[StatisticsType]]
          val i = v1.intValue * _neighborDomain2.asInstanceOf[DiscreteVectorDomain].dimensionSize + v2.intValue
          if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = stats(values)
          cachedStatisticsArray(i)
        }
        case v2:DiscreteVectorValue if (true /*v2.isConstant*/) => {
          //println("Template2.cachedStatistics")
          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType] { override protected def initialSize = 512 }
          val i = ((v1.intValue,v2))
          cachedStatisticsHash.getOrElseUpdate(i, stats(values))
        }
        case _ => stats(values)
      }
    }
    case v1:DiscreteVectorValue if (true /*v1.isConstant*/) => {
      values._2 match {
        case v2:DiscreteValue => {
          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
          val i = ((v2.intValue,v1))
          cachedStatisticsHash.getOrElseUpdate(i, stats(values))
        }
        case _ => stats(values)
      }
    }
    case _ => stats(values)
  } else stats(values)
  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
  /** valuesIterator in style of specifying fixed neighbors */
  def valuesIterator(factor:FactorType, fixed: Assignment): Iterator[Values] = {
    val fixed1 = fixed.contains(factor._1)
    val fixed2 = fixed.contains(factor._2)
    if (fixed1 && fixed2) 
      Iterator.single(Values(fixed(factor._1), fixed(factor._2)))
    else if (fixed1) {
      val val1 = fixed(factor._1)
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      d2.values.iterator.map(value => Values(val1, value))
    } else if (fixed2) {
      val val2 = fixed(factor._2)
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      d1.values.iterator.map(value => Values(value, val2))
    } else {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      (for (val1 <- d1.values; val2 <- d2.values) yield Values(val1, val2)).iterator
    }
  }
  /** valuesIterator in style of specifying varying neighbors */
  def valuesIterator(factor:FactorType, varying:Seq[Variable]): Iterator[Values] = {
    val varying1 = varying.contains(factor._1)
    val varying2 = varying.contains(factor._2)
    if (varying1 && varying2) {
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      (for (val1 <- d1.values; val2 <- d2.values) yield Values(val1, val2)).iterator
    } else if (varying1) {
      val val2 = factor._2.value
      val d1 = factor._1.domain.asInstanceOf[IterableDomain[N1#Value]]
      d1.values.iterator.map(value => Values(value, val2))
    } else if (varying2) {
      val val1 = factor._1.value
      val d2 = factor._2.domain.asInstanceOf[IterableDomain[N2#Value]]
      d2.values.iterator.map(value => Values(val1, value))
    } else {
      Iterator.single(Values(factor._1.value, factor._2.value))
    }
  }
}


trait Statistics2[S1,S2] extends Family {
  final case class Stat(_1:S1, _2:S2, override val inner:Seq[cc.factorie.Statistics] = Nil) extends super.Statistics
  type StatisticsType = Stat
}

trait VectorStatistics2[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue] extends VectorFamily {
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, override val inner:Seq[cc.factorie.Statistics] = Nil) extends { val vector: Vector = _1 flatOuter _2 } with super.Statistics { 
    if (_statisticsDomains eq null) { 
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
    }
  }
}

trait DotStatistics2[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue] extends VectorStatistics2[S1,S2] with DotFamily

trait FamilyWithStatistics2[N1<:Variable,N2<:Variable] extends Family2[N1,N2] with Statistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

trait FamilyWithVectorStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar] extends Family2[N1,N2] with VectorStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

trait FamilyWithDotStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar] extends Family2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

