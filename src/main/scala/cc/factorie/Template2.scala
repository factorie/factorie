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

// TODO Pull unroll methods into DynamicTemplate2 and look for VarWithFactors in Template.factors.
abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template
{
  type NeighborType1 = N1
  type NeighborType2 = N2
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  private var _neighborDomain1: Domain[N1#Value] = null
  private var _neighborDomain2: Domain[N2#Value] = null
  def neighborDomain1: Domain[N1#Value] = if (_neighborDomain1 eq null) throw new Error("You must override neighborDomain1 if you want to access it before creating any Factor objects") else _neighborDomain1
  def neighborDomain2: Domain[N2#Value] = if (_neighborDomain2 eq null) throw new Error("You must override neighborDomain2 if you want to access it before creating any Factor objects") else _neighborDomain2

  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (neighborClass1.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain1 eq v.domain) || (_neighborDomain1 eq null))) ret ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain2 eq v.domain) || (_neighborDomain2 eq null))) ret ++= unroll2(v.asInstanceOf[N2])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, override var outer:cc.factorie.Factor = null) extends super.Factor {
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
    def values: ValuesType = new Values(_1.value, _2.value)
    def statistics: StatisticsType = Template2.this.statistics(values)
    override def cachedStatistics: StatisticsType = Template2.this.cachedStatistics(values)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2))
  }
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value) extends super.Values {
    def statistics = Template2.this.statistics(this)
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
trait Statistics2[S1,S2] extends Template {
  final case class Stat(_1:S1, _2:S2) extends super.Statistics
  type StatisticsType = Stat
}
trait VectorStatistics2[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue] extends VectorTemplate {
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2) extends { val vector: Vector = _1 flatOuter _2 } with super.Statistics { 
    if (_statisticsDomains eq null) { 
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
    }
  }
}
trait DotStatistics2[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue] extends VectorStatistics2[S1,S2] with DotTemplate
abstract class TemplateWithStatistics2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with Statistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2)
}
abstract class TemplateWithVectorStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with VectorStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2)
}
abstract class TemplateWithDotStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2)
}

/*
trait DiscreteFactorSettings2 extends Template {
  this: VectorTemplate {
    type TemplateType <: DotTemplate
    type Neighbor1Type <: DiscreteVar
    type Neighbor2Type <: DiscreteVar
    type FactorType <: { def _1:DiscreteVariable ; def _2:DiscreteVariable }
  } =>
  lazy val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize1 = ndd1.size
  lazy val ndd2: DiscreteDomain = throw new Error // TODO nd2.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize2 = ndd2.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = true
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = {
    if (settingsSparsified) {
      forIndex(sparseSettings1.length)(i => {
        factor._1.set(i)(null)
        forIndex(sparseSettings1(i).length)(j => {
          factor._2.set(j)(null)
          f
        })
      })
    } else {
      var i = 0
      while (i < ndsize1) {
        factor._1.set(i)(null)
        var j = 0
        while (j < ndsize2) {
          factor._2.set(j)(null)
          f
          j += 1
        }
      }
    }
  }
  // Call function f for each valid (possibly sparsified) variable value setting 
  // of the neighboring variables specified in 'vs'. 
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = {
    if (vs.size == 1) {
      val v = vs.head
      if (factor._1 eq v) {
        // vary v1, keep v2 constant
        val v = factor._1 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings2(factor._2.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize1)(i => { v.set(i)(null); f })
      } else if (factor._2 eq v) {
        // vary v2, keep v1 constant
        val v = factor._2 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings1(factor._1.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize2)(i => { v.set(i)(null); f })
      }
    } else if (vs.size == 2) {
      throw new Error("Not yet implemented.")
    } else throw new Error("Asked to vary settings of too many variables.")
  }

  private var settingsSparsified = false
  // Redundant storage of valid v1,v2 value pairs
  private var sparseSettings1: Array[Array[Int]] = null // first index=v1, second index=v2
  private var sparseSettings2: Array[Array[Int]] = null // first index=v2, second index=v1
  // Initialize sparseSettings1 and sparseSettings2 to cover all values in factors touching the variables in 'vs'.
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    println("Template sparsifySettingsFor ndsize1="+ndsize1+" ndsize2="+ndsize2)
    assert (ndsize1 > 0, "sparsifySettingsFor before Domain size properly set.")
    assert (ndsize2 > 0, "sparsifySettingsFor before Domain size properly set.")
    val sparse1 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    val sparse2 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    vs.foreach(v => {
      this.factors(v).foreach(f => {
        sparse1.getOrElseUpdate(f._1.intValue, new HashSet[Int]) += f._2.intValue
        sparse2.getOrElseUpdate(f._2.intValue, new HashSet[Int]) += f._1.intValue
      })
    })
    sparseSettings1 = new Array[Array[Int]](ndsize1)
    sparseSettings2 = new Array[Array[Int]](ndsize2)
    forIndex(sparseSettings1.length)(i => sparseSettings1(i) = sparse1.getOrElse(i, new HashSet[Int]).toArray)
    forIndex(sparseSettings2.length)(i => sparseSettings2(i) = sparse2.getOrElse(i, new HashSet[Int]).toArray)
    settingsSparsified = true 
  }
}
*/


