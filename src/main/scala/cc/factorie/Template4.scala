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

abstract class Family4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  val neighborClass4 = nm4.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass3)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc4a = { val ta = nm4.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass4)) { assert(ta.length == 1); ta.head.erasure } else null }
  protected var _neighborDomain1: Domain[N1#Value] = null
  protected var _neighborDomain2: Domain[N2#Value] = null
  protected var _neighborDomain3: Domain[N3#Value] = null
  protected var _neighborDomain4: Domain[N4#Value] = null
  def neighborDomain1: Domain[N1#Value] = if (_neighborDomain1 eq null) throw new Error("You must override neighborDomain1 if you want to access it before creating any Factor objects") else _neighborDomain1
  def neighborDomain2: Domain[N2#Value] = if (_neighborDomain2 eq null) throw new Error("You must override neighborDomain2 if you want to access it before creating any Factor objects") else _neighborDomain2
  def neighborDomain3: Domain[N3#Value] = if (_neighborDomain3 eq null) throw new Error("You must override neighborDomain3 if you want to access it before creating any Factor objects") else _neighborDomain3
  def neighborDomain4: Domain[N4#Value] = if (_neighborDomain4 eq null) throw new Error("You must override neighborDomain4 if you want to access it before creating any Factor objects") else _neighborDomain4

  type FactorType = Factor
  final case class Factor(_1:N1, _2:N2, _3:N3, _4:N4, override var inner:Seq[cc.factorie.Factor] = Nil, override var outer:cc.factorie.Factor = null) extends super.Factor {
    if (_neighborDomains eq null) {
      _neighborDomain1 = _1.domain.asInstanceOf[Domain[N1#Value]]
      _neighborDomain2 = _2.domain.asInstanceOf[Domain[N2#Value]]
      _neighborDomain3 = _3.domain.asInstanceOf[Domain[N3#Value]]
      _neighborDomain4 = _4.domain.asInstanceOf[Domain[N4#Value]]
      _neighborDomains = _newNeighborDomains
      _neighborDomains += _neighborDomain1
      _neighborDomains += _neighborDomain2
      _neighborDomains += _neighborDomain3
      _neighborDomains += _neighborDomain4
    }
    def numVariables = 4
    def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }
    override def variables: IndexedSeq[Variable] = IndexedSeq(_1, _2, _3, _4)
    def values: ValuesType = new Values(_1.value, _2.value, _3.value, _4.value, inner.map(_.values))
    def statistics: StatisticsType = Family4.this.statistics(values)
    override def cachedStatistics: StatisticsType = Family4.this.statistics(values)
    // TODO override def cachedStatistics: StatisticsType = Template4.this.cachedStatistics(_1.value, _2.value, _3.value, _4.value)
    def copy(s:Substitutions) = Factor(s.sub(_1), s.sub(_2), s.sub(_3), s.sub(_4))
  } 
  // Values
  type ValuesType = Values
  final case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value, override val inner:Seq[cc.factorie.Values]) extends super.Values {
    def statistics = Family4.this.statistics(this)
  }
  // Statistics
  def statistics(values:Values): StatisticsType
  def valuesIterator(factor:FactorType, fixed: Assignment): Iterator[Values] = throw new Error("Not yet implemented")
}

abstract class Template4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Family4[N1,N2,N3,N4] with Template {
  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (neighborClass1.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain1 eq v.domain) || (_neighborDomain1 eq null))) ret ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain2 eq v.domain) || (_neighborDomain2 eq null))) ret ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain3 eq v.domain) || (_neighborDomain3 eq null))) ret ++= unroll3(v.asInstanceOf[N3])
    if (neighborClass4.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain4 eq v.domain) || (_neighborDomain4 eq null))) ret ++= unroll4(v.asInstanceOf[N4])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    if ((nc4a ne null) && nc4a.isAssignableFrom(v.getClass)) ret ++= unroll4s(v.asInstanceOf[N4#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll4(v:N4): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")
  def unroll4s(v:N4#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll4s.")

}

trait Statistics4[S1,S2,S3,S4] extends Family {
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4, override val inner:Seq[cc.factorie.Statistics] = Nil) extends super.Statistics
}

trait VectorStatistics4[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue,S3<:DiscreteVectorValue,S4<:DiscreteVectorValue] extends VectorFamily {
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4, override val inner:Seq[cc.factorie.Statistics] = Nil) extends  { val vector: Vector = _1 flatOuter (_2 flatOuter (_3 flatOuter _4)) } with super.Statistics {
    if (_statisticsDomains eq null) {
      _statisticsDomains = _newStatisticsDomains
      _statisticsDomains += _1.domain
      _statisticsDomains += _2.domain
      _statisticsDomains += _3.domain
      _statisticsDomains += _4.domain
    }
  }
}

trait DotStatistics4[S1<:DiscreteVectorValue,S2<:DiscreteVectorValue,S3<:DiscreteVectorValue,S4<:DiscreteVectorValue] extends VectorStatistics4[S1,S2,S3,S4] with DotFamily

abstract class FamilyWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Family4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4, values.inner.map(_.statistics))
}

abstract class TemplateWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4, values.inner.map(_.statistics))
}

abstract class TemplateWithVectorStatistics4[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar,N3<:DiscreteVectorVar,N4<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with VectorStatistics4[N1#Value,N2#Value,N3#Value,N4#Value]  {
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4, values.inner.map(_.statistics))
}

abstract class TemplateWithDotStatistics4[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar,N3<:DiscreteVectorVar,N4<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with DotStatistics4[N1#Value,N2#Value,N3#Value,N4#Value]  {
  type FamilyType <: TemplateWithDotStatistics4[N1,N2,N3,N4]
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4, values.inner.map(_.statistics))
}

