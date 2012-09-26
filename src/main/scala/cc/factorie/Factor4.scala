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
abstract class Factor4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](val _1:N1, val _2:N2, val _3:N3, val _4:N4) extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4

  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType = ((v1, v2, v3, v4)).asInstanceOf[StatisticsType] // Just a stand-in default
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = (score(v1, v2, v3, v4), statistics(v1, v2, v3, v4))
  def score: Double = score(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])
  override def statistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])
  override def scoreAndStatistics: (Double,StatisticsType) = scoreAndStatistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])

  def numVariables = 4
  override def variables = IndexedSeq(_1, _2, _3, _4)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }

  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment4(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value], _3, _3.value.asInstanceOf[N3#Value], _4, _4.value.asInstanceOf[N4#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment4[N1,N2,N3,N4] if ((a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) && (a._4 eq _4)) => score(a.value1, a.value2, a.value3, a.value4)
    case _ => score(a(_1), a(_2), a(_3), a(_4))
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

/** The only abstract thing is score(N1#Value, N2#Value, N3#Value, N3#Value) */
abstract class TupleFactor4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value, N4#Value))
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = ((v1, v2, v3, v4))
}

/** The only abstract thing is scoreStatistics(Tensor) */
abstract class TensorFactorWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) {
  type StatisticsType = Tensor
  final override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Tensor = throw new Error("Not yet implemented")
  //final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = cc.factorie.la.Tensor,outer(v1, v2, v3).asInstanceOf[StatisticsType] // TODO Why is this cast necessary?
  final def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = scoreStatistics(statistics(v1, v2, v3, v4))
  def scoreStatistics(t:Tensor): Double
}

/** The only abstract thing is weights:Tensor3 */
abstract class DotFactorWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends TensorFactorWithStatistics4[N1,N2,N3,N4](_1, _2, _3, _4) {
  def weights: Tensor3 // TODO This might not be Tensor3 if some of the neighbors have values that are not Tensor1
  def scoreStatistics(t:Tensor): Double = weights dot t
  override def scoreValues(valueTensor:Tensor) = weights dot valueTensor
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

  final case class Factor(override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) with super.Factor {
    //type StatisticsType = Family4.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family4.this
    override def score(value1:N1#Value, value2:N2#Value, value3:N3#Value, value4:N4#Value): Double = Family4.this.score(value1, value2, value3, value4)
    override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType = thisFamily.statistics(v1, v2, v3, v4)
    override def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = Family4.this.scoreAndStatistics(v1, v2, v3, v4)
  }
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = (score(v1, v2, v3, v4), statistics(v1, v2, v3, v4))
}

trait TupleFamily4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Family4[N1,N2,N3,N4] {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value, N4#Value))
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): ((N1#Value, N2#Value, N3#Value, N4#Value))
}

trait TupleFamilyWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends TupleFamily4[N1,N2,N3,N4] {
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = ((v1, v2, v3, v4))
}

trait TensorFamily4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Family4[N1,N2,N3,N4] with TensorFamily {
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Tensor
}

trait TensorFamilyWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends TensorFamily4[N1,N2,N3,N4] {
  //type StatisticsType = Tensor
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Tensor.outer(v1, v2, v3, v4)
}

trait DotFamily4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends TensorFamily4[N1,N2,N3,N4] with DotFamily {
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = scoreStatistics(statistics(v1, v2, v3, v4))
}

trait DotFamilyWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends TensorFamilyWithStatistics4[N1,N2,N3,N4] with DotFamily4[N1,N2,N3,N4] {
  override def weights: Tensor4
  //def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = weights dot statistics(v1, v2, v3, v4)
  override def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor)
}

//
//
//
//trait Statistics4[S1,S2,S3,S4] extends Family {
//  self =>
//  type StatisticsType = Statistics
//  final case class Statistics(_1:S1, _2:S2, _3:S3, _4:S4) extends super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait TensorStatistics4[S1<:Tensor,S2<:Tensor,S3<:Tensor,S4<:Tensor] extends TensorFamily {
//  self =>
//  type StatisticsType = Statistics
//  final case class Statistics(_1:S1, _2:S2, _3:S3, _4:S4) extends  { val tensor: Tensor = Tensor.outer(_1, _2, _3, _4) } with super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait DotStatistics4[S1<:Tensor,S2<:Tensor,S3<:Tensor,S4<:Tensor] extends TensorStatistics4[S1,S2,S3,S4] with DotFamily {
//  def weights: Tensor4
//}
//
//trait FamilyWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable] extends Family4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
//}
//
//trait FamilyWithTensorStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends Family4[N1,N2,N3,N4] with TensorStatistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
//}
//
//trait FamilyWithDotStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends Family4[N1,N2,N3,N4] with DotStatistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
//  def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor) // reflecting the fact that there is no transformation between values and statistics
//}
