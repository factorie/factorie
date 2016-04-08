/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.model

import cc.factorie.la._
import cc.factorie.variable._

/** The only abstract things are _1, _2, _3, statistics(Values), and StatisticsType 
    @author Andrew McCallum */
abstract class Factor4[N1<:Var,N2<:Var,N3<:Var,N4<:Var](val _1:N1, val _2:N2, val _3:N3, val _4:N4) extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4

  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = (score(v1, v2, v3, v4), statistics(v1, v2, v3, v4))
  def currentScore: Double = score(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])
  override def currentStatistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])
  override def currentScoreAndStatistics: (Double,StatisticsType) = scoreAndStatistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value], _4.value.asInstanceOf[N4#Value])

  def numVariables = 4
  override def variables = IndexedSeq(_1, _2, _3, _4)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case 3 => _4; case _ => throw new IndexOutOfBoundsException(i.toString) }

  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment4(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value], _3, _3.value.asInstanceOf[N3#Value], _4, _4.value.asInstanceOf[N4#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def assignmentScore(a:Assignment) = a match {
    case a:AbstractAssignment4[N1,N2,N3,N4] if (a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) && (a._4 eq _4) => score(a.value1, a.value2, a.value3, a.value4)
    case _ => score(a(_1), a(_2), a(_3), a(_4))
  }
  override final def assignmentStatistics(a:Assignment): StatisticsType = a match {
    case a:AbstractAssignment4[N1,N2,N3,N4] if (a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) && (a._4 eq _4) => statistics(a.value1, a.value2, a.value3, a.value4)
    case _ => statistics(a(_1), a(_2), a(_3), a(_4))
  }
}

/** A 4-neighbor Factor whose statistics have type Tuple2.
    Only "score" method is abstract. 
    @author Andrew McCallum */
abstract class TupleFactorWithStatistics4[N1<:Var,N2<:Var,N3<:Var,N4<:Var](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value, N4#Value))
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = (v1, v2, v3, v4)
  final override def statisticsAreValues: Boolean = true
}

/** A 4-neighbor Factor whose statistics have type Tensor.
    Only "statistics" and "score" methods are abstract. 
    @author Andrew McCallum */
abstract class TensorFactor4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) {
  type StatisticsType = Tensor
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Tensor
  final def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = statisticsScore(statistics(v1, v2, v3, v4))
  override def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double, Tensor) = {
    val tensor = statistics(v1, v2, v3, v4)
    (statisticsScore(tensor), tensor)
  } 
  def statisticsScore(t:Tensor): Double
}

/** A trait for 4-neighbor Factor whose neighbors have Tensor values,
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  DotFactorWithStatistics2 is also a subclass of this. 
    @author Andrew McCallum */
trait TensorFactorStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends TensorFactor4[N1,N2,N3,N4] {
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = cc.factorie.la.Tensor.outer(v1, v2, v3, v4)
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
  final override def statisticsAreValues: Boolean = true
}

/** A 4-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract. 
    @author Andrew McCallum */
abstract class TensorFactorWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends TensorFactor4[N1,N2,N3,N4](_1, _2, _3, _4) with TensorFactorStatistics4[N1,N2,N3,N4]

/** A 4-neighbor Factor whose statistics have type Tensor, 
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "statistics" and "weightsSet" methods are abstract. 
    @author Andrew McCallum */
abstract class DotFactor4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends TensorFactor4[N1,N2,N3,N4](_1, _2, _3, _4) {
  def weights: Weights
  def statisticsScore(t:Tensor): Double = weights.value dot t
}

/** A 4-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values,
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "weightsSet" method is abstract. 
    @author Andrew McCallum */
abstract class DotFactorWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends DotFactor4[N1,N2,N3,N4](_1, _2, _3, _4) with TensorFactorStatistics4[N1,N2,N3,N4] {
  override def valuesScore(valueTensor:Tensor) = weights.value dot valueTensor
}


/** Family containing Factor4 (Families of Factors having four neighbors).
    @author Andrew McCallum */
trait Family4[N1<:Var,N2<:Var,N3<:Var,N4<:Var] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type NeighborType4 = N4
    /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain { type Value = N1#Value } = null
  def neighborDomain2: Domain { type Value = N2#Value } = null
  def neighborDomain3: Domain { type Value = N3#Value } = null
  def neighborDomain4: Domain { type Value = N4#Value } = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2, neighborDomain3, neighborDomain4)
  type FactorType = Factor

  final case class Factor(override val _1:N1, override val _2:N2, override val _3:N3, override val _4:N4) extends Factor4[N1,N2,N3,N4](_1, _2, _3, _4) with super.Factor {
    //type StatisticsType = Family4.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family4.this
    def score(value1:N1#Value, value2:N2#Value, value3:N3#Value, value4:N4#Value): Double = Family4.this.score(value1, value2, value3, value4)
    def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType = thisFamily.statistics(v1, v2, v3, v4)
    override def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = Family4.this.scoreAndStatistics(v1, v2, v3, v4)
    override def valuesStatistics(tensor:Tensor): Tensor = Family4.this.valuesStatistics(tensor)
    override def statisticsAreValues: Boolean = Family4.this.statisticsAreValues
  }
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): StatisticsType
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): (Double,StatisticsType) = (score(v1, v2, v3, v4), statistics(v1, v2, v3, v4))
  def valuesStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor)")
  def statisticsAreValues: Boolean = false
}

trait TupleFamily4[N1<:Var,N2<:Var,N3<:Var,N4<:Var] extends Family4[N1,N2,N3,N4] {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value, N4#Value))
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): ((N1#Value, N2#Value, N3#Value, N4#Value))
}

trait TupleFamilyWithStatistics4[N1<:Var,N2<:Var,N3<:Var,N4<:Var] extends TupleFamily4[N1,N2,N3,N4] {
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = (v1, v2, v3, v4)
}

trait TensorFamily4[N1<:Var,N2<:Var,N3<:Var,N4<:Var] extends Family4[N1,N2,N3,N4] with TensorFamily {
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Tensor
}

trait TensorFamilyWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends TensorFamily4[N1,N2,N3,N4] {
  //type StatisticsType = Tensor
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Tensor.outer(v1, v2, v3, v4)
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
}

trait DotFamily4[N1<:Var,N2<:Var,N3<:Var,N4<:Var] extends TensorFamily4[N1,N2,N3,N4] with DotFamily {
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = statisticsScore(statistics(v1, v2, v3, v4))
}

trait DotFamilyWithStatistics4[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar,N4<:TensorVar] extends TensorFamilyWithStatistics4[N1,N2,N3,N4] with DotFamily4[N1,N2,N3,N4] {
  override def weights: Weights4
  //def score(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value): Double = weightsSet dot statistics(v1, v2, v3, v4)
  override def valuesScore(tensor:Tensor): Double = statisticsScore(tensor)
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
//  def weightsSet: Tensor4
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
//  def valuesScore(tensor:Tensor): Double = statisticsScore(tensor) // reflecting the fact that there is no transformation between values and statistics
//}
