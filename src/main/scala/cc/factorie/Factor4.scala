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

trait ValuesIterator4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Iterator[AbstractAssignment4[N1,N2,N3,N4]] with AbstractAssignment4[N1,N2,N3,N4] with ValuesIterator

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
//  override def values = new Values(_1.value, _2.value, _3.value, _4.value)
//  case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value) extends cc.factorie.Values {
//    override def apply[B <: Variable](v: B) = get(v).get
//    def variables = Seq(factor._1, factor._2, factor._3, factor._4)
//    def get[B <: Variable](v: B) =
//      if(v == factor._1) Some(_1.asInstanceOf[B#Value])
//      else if(v == factor._2) Some(_2.asInstanceOf[B#Value])
//      else if(v == factor._3) Some(_3.asInstanceOf[B#Value])
//      else if(v == factor._4) Some(_4.asInstanceOf[B#Value])
//      else None
//    def contains(v: Variable) = v == factor._1 || v == factor._2 || v == factor._3 || v == factor._4
//    override def statistics: StatisticsType = Factor4.this.statistics(this)
//  }
//  def statistics: StatisticsType = statistics(values)
//  def statistics(v:Values): StatisticsType

  def statistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType
  // TODO Consider a method like this?  Replaces score(Values)
  def scoreValues(value1:N1#Value, value2:N2#Value, value3:N3#Value, value4:N4#Value): Double = statistics(value1, value2, value3, value4).score
  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment4(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value], _3, _3.value.asInstanceOf[N3#Value], _4, _4.value.asInstanceOf[N4#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment4[N1,N2,N3,N4] if ((a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) && (a._4 eq _4)) => statistics(a.value1, a.value2, a.value3, a.value4).score
    case _ => statistics(a(_1), a(_2), a(_3), a(_4)).score
  }

  def valuesIterator: ValuesIterator4[N1,N2,N3,N4] = new ValuesIterator4[N1,N2,N3,N4] {
    def factor = Factor4.this
    var _1: N1 = null.asInstanceOf[N1]
    var _2: N2 = null.asInstanceOf[N2]
    var _3: N3 = null.asInstanceOf[N3]
    var _4: N4 = null.asInstanceOf[N4]
    var value1: N1#Value = null.asInstanceOf[N1#Value]
    var value2: N2#Value = null.asInstanceOf[N2#Value]
    var value3: N3#Value = null.asInstanceOf[N3#Value]
    var value4: N4#Value = null.asInstanceOf[N4#Value]
    def hasNext = false
    def next() = this
    def score: Double = Double.NaN
    def valuesTensor: Tensor = null
  }
  
  
//  /** valuesIterator in style of specifying varying neighbors */
//  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
//    val values1 = if(varying.contains(_1)) _1.domain.asInstanceOf[Seq[N1#Value]] else Seq(_1.value)
//    val values2 = if(varying.contains(_2)) _2.domain.asInstanceOf[Seq[N2#Value]] else Seq(_2.value)
//    val values3 = if(varying.contains(_3)) _3.domain.asInstanceOf[Seq[N3#Value]] else Seq(_3.value)
//    val values4 = if(varying.contains(_4)) _4.domain.asInstanceOf[Seq[N4#Value]] else Seq(_4.value)
//    (for (val1 <- values1; val2 <- values2; val3 <- values3; val4 <- values4) yield Values(val1, val2, val3, val4)).iterator
//  }
}

/** The only abstract things are _1, _2, _3, and score(Statistics) */
trait FactorWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Factor4[N1,N2,N3,N4] {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:N1#Value, _2:N2#Value, _3:N3#Value, _4:N4#Value) extends cc.factorie.Statistics {
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    lazy val score = self.score(this)
  }
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = new Statistics(v1, v2, v3, v4).asInstanceOf[StatisticsType]
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
//  type ValuesType = Factor#Values
  final case class Factor(_1:N1, _2:N2, _3:N3, _4:N4) extends super.Factor with Factor4[N1,N2,N3,N4] {
    type StatisticsType = Family4.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family4.this
    override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType = thisFamily.statistics(v1, v2, v3, v4)
  }
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType
}

trait Statistics4[S1,S2,S3,S4] extends Family {
  self =>
  type StatisticsType = Statistics
  final case class Statistics(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Statistics): Double
}

trait TensorStatistics4[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue,S4<:DiscreteTensorValue] extends TensorFamily {
  self =>
  type StatisticsType = Statistics
  //override def statisticsDomains: Tuple4[DiscreteTensorDomain with Domain[S1], DiscreteTensorDomain with Domain[S2], DiscreteTensorDomain with Domain[S3], DiscreteTensorDomain with Domain[S4]]
  final case class Statistics(_1:S1, _2:S2, _3:S3, _4:S4) extends  { val tensor: Tensor = Tensor.outer(_1, _2, _3, _4) } with super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Statistics): Double
}

trait DotStatistics4[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue,S4<:DiscreteTensorValue] extends TensorStatistics4[S1,S2,S3,S4] with DotFamily {
  def weights: Tensor4
}

trait FamilyWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Family4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
}

trait FamilyWithTensorStatistics4[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar,N4<:DiscreteTensorVar] extends Family4[N1,N2,N3,N4] with TensorStatistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
}

trait FamilyWithDotStatistics4[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar,N4<:DiscreteTensorVar] extends Family4[N1,N2,N3,N4] with DotStatistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
  def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor) // reflecting the fact that there is no transformation between values and statistics
}
