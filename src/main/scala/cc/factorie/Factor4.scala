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

/** The only abstract things are _1, _2, _3, statistics(Values), and StatisticsType */
trait Factor4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4
  type StatisticsType <: cc.factorie.Statistics
  def _1: N1
  def _2: N2
  def _3: N3
  def _4: N4
  def numVariables = 4
  override def variables = IndexedSeq(_1, _2, _3, _4)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }
  override def values = new Values(_1.value, _2.value, _3.value, _4.value)
  case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value) extends cc.factorie.Values {
    override def apply[B <: Variable](v: B) = get(v).get
    def variables = Seq(factor._1, factor._2, factor._3, factor._4)
    def get[B <: Variable](v: B) =
      if(v == factor._1) Some(_1.asInstanceOf[B#Value])
      else if(v == factor._2) Some(_2.asInstanceOf[B#Value])
      else if(v == factor._3) Some(_3.asInstanceOf[B#Value])
      else if(v == factor._4) Some(_4.asInstanceOf[B#Value])
      else None
    def contains(v: Variable) = v == factor._1 || v == factor._2 || v == factor._3 || v == factor._4
    override def statistics: StatisticsType = Factor4.this.statistics(this)
  }
  def statistics: StatisticsType = statistics(values)
  def statistics(v:Values): StatisticsType
  /** valuesIterator in style of specifying varying neighbors */
  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
    val values1 = if(varying.contains(_1)) _1.domain.asInstanceOf[Seq[N1#Value]] else Seq(_1.value)
    val values2 = if(varying.contains(_2)) _2.domain.asInstanceOf[Seq[N2#Value]] else Seq(_2.value)
    val values3 = if(varying.contains(_3)) _3.domain.asInstanceOf[Seq[N3#Value]] else Seq(_3.value)
    val values4 = if(varying.contains(_4)) _4.domain.asInstanceOf[Seq[N4#Value]] else Seq(_4.value)
    (for (val1 <- values1; val2 <- values2; val3 <- values3; val4 <- values4) yield Values(val1, val2, val3, val4)).iterator
  }
}

/** The only abstract things are _1, _2, _3, and score(Statistics) */
trait FactorWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Factor4[N1,N2,N3,N4] {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value) extends cc.factorie.Statistics {
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    lazy val score = self.score(this)
  }
  def statistics(v:Values) = new Statistics(v._1, v._2, v._3, v._4).asInstanceOf[StatisticsType]
  def score(s:Statistics): Double
}


trait Family4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4
    /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomain2: Domain[N2#Value] = null
  def neighborDomain3: Domain[N3#Value] = null
  def neighborDomain4: Domain[N4#Value] = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2, neighborDomain3, neighborDomain4)

  type FactorType = Factor
  type ValuesType = Factor#Values
  final case class Factor(_1:N1, _2:N2, _3:N3, _4:N4) extends super.Factor with Factor4[N1,N2,N3,N4] {
    type StatisticsType = Family4.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family4.this
    override def statistics(values:Values): StatisticsType = thisFamily.statistics(values)
  }
}

trait Statistics4[S1,S2,S3,S4] extends Family {
  self =>
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Stat): Double
}

trait TensorStatistics4[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue,S4<:DiscreteTensorValue] extends TensorFamily {
  self =>
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3, _4:S4) extends  { val tensor: Tensor = Tensor.outer(_1, _2, _3, _4) } with super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Stat): Double
}

trait DotStatistics4[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue,S4<:DiscreteTensorValue] extends TensorStatistics4[S1,S2,S3,S4] with DotFamily

trait FamilyWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Family4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(values:Values) = Stat(values._1, values._2, values._3, values._4)
}

// TODO Also implement FamilyWithVectorStatistics4 and FamilyWithDotStatistics4
