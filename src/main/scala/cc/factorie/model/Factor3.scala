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
abstract class Factor3[N1<:Var,N2<:Var,N3<:Var](val _1:N1, val _2:N2, val _3:N3) extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): (Double,StatisticsType) = (score(v1, v2, v3), statistics(v1, v2, v3))
  def currentScore: Double = score(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value])
  override def currentStatistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value])
  override def currentScoreAndStatistics: (Double,StatisticsType) = scoreAndStatistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value])

  def numVariables = 3
  override def variables = IndexedSeq(_1, _2, _3)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case _ => throw new IndexOutOfBoundsException(i.toString) }

  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment3(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value], _3, _3.value.asInstanceOf[N3#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def assignmentScore(a:Assignment) = a match {
    case a:AbstractAssignment3[N1,N2,N3] if (a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) => score(a.value1, a.value2, a.value3)
    case _ => score(a(_1), a(_2), a(_3))
  }
  override final def assignmentStatistics(a:Assignment): StatisticsType = a match {
    case a:AbstractAssignment3[N1,N2,N3] if (a._1 eq _1) && (a._2 eq _2) && (a._3 eq _3) => statistics(a.value1, a.value2, a.value3)
    case _ => statistics(a(_1), a(_2), a(_3))
  }
  // For implementing sparsity in belief propagation.  
  // Note that we do not provide all combinations/orderings of neighbors.  
  // Generally predicted DiscreteVars should be the first neighbors.
  def hasLimitedDiscreteValues123: Boolean = limitedDiscreteValues123 != null && limitedDiscreteValues123.activeDomainSize < limitedDiscreteValues123.length
  def limitedDiscreteValues123: SparseBinaryTensor3 = null // throw new Error("This Factor type does not implement limitedDiscreteValues123: "+getClass)
  def hasLimitedDiscreteValues12: Boolean = limitedDiscreteValues12 != null && limitedDiscreteValues12.activeDomainSize < limitedDiscreteValues12.length
  def limitedDiscreteValues12: SparseBinaryTensor2 = null // throw new Error("This Factor type does not implement limitedDiscreteValues12: "+getClass)
  def hasLimitedDiscreteValues1: Boolean = limitedDiscreteValues1 != null && limitedDiscreteValues1.activeDomainSize < limitedDiscreteValues1.length
  def limitedDiscreteValues1: SparseBinaryTensor1 = null // throw new Error("This Factor type does not implement limitedDiscreteValues1: "+getClass)

  /** Given the Tensor value of neighbors _2 and _3, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def valuesScore1(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores1")
  def valuesScore2(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
  def valuesScore3(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")

//  /** Values iterator in style of specifying fixed neighbors. */
//  def valuesIterator(fixed: Assignment): Iterator[Values] = {
//    val fixed1 = fixed.contains(_1)
//    val fixed2 = fixed.contains(_2)
//    val fixed3 = fixed.contains(_3)
//    if (fixed1 && fixed2 && fixed3) 
//      Iterator.single(Values(fixed(_1), fixed(_2), fixed(_3)))
//    else if (fixed1 && fixed2) {
//      val val1 = fixed(_1)
//      val val2 = fixed(_2)
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => (t._1 == intVal1) && (t._2 == intVal2)).map(t => new Values(val1, val2, d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        d3.iterator.map(value => Values(val1, val2, value))
//      }
//    } else if (fixed2 && fixed3) {
//      val val2 = fixed(_2)
//      val val3 = fixed(_3)
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => (t._2 == intVal2) && (t._3 == intVal3)).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2, val3))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        d1.iterator.map(value => Values(value, val2, val3))
//      }
//    } else if (fixed1 && fixed3) {
//      val val1 = fixed(_1)
//      val val3 = fixed(_3)
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => (t._1 == intVal1) && (t._3 == intVal3)).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value], val3))
//      }
//      else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        d2.iterator.map(value => Values(val1, value, val3))
//      }
//    } else if (fixed1) {
//      val val1 = fixed(_1)
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => t._1 == intVal1).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value], d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val2 <- d2; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (fixed2) {
//      val val2 = fixed(_2)
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => t._2 == intVal2).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2, d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val1 <- d1; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (fixed3) {
//      val val3 = fixed(_3)
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._3 == intVal3).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value], val3))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        (for (val1 <- d1; val2 <- d2) yield Values(val1, val2, val3)).iterator
//      }
//    } else {
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value], d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val1 <- d1; val2 <- d2; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    }
//  }
//
//  /** valuesIterator in style of specifying varying neighbors */
//  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
//    val varying1 = varying.contains(_1)
//    val varying2 = varying.contains(_2)
//    val varying3 = varying.contains(_3)
//    if (varying1 && varying2 && varying3) {
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value], d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val1 <- d1; val2 <- d2; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying1 && varying2) {
//      val val3 = _3.value
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._3 == intVal3).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value], val3))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        (for (val1 <- d1; val2 <- d2) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying2 && varying3) {
//      val val1 = _1.value
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => t._1 == intVal1).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value], d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val2 <- d2; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying1 && varying3) {
//      val val2 = _2.value
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => t._2 == intVal2).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2, d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val1 <- d1; val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying1) {
//      val val2 = _2.value
//      val val3 = _3.value
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => (t._2 == intVal2) && (t._3 == intVal3)).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2, val3))
//      }
//      else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        (for (val1 <- d1) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying2) {
//      val val1 = _1.value
//      val val3 = _3.value
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal3 = val3.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => (t._1 == intVal1) && (t._3 == intVal3)).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value], val3))
//      }
//      else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        (for (val2 <- d2) yield Values(val1, val2, val3)).iterator
//      }
//    } else if (varying3) {
//      val val1 = _1.value
//      val val2 = _2.value
//      if (isLimitingValuesIterator) {
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        val d3 = _3.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.filter(t => (t._1 == intVal1) && (t._2 == intVal2)).map(t => new Values(val1, val2, d3.apply(t._3).asInstanceOf[N3#Value]))
//      }
//      else {
//        val d3 = _3.domain.asInstanceOf[Seq[N3#Value]]
//        (for (val3 <- d3) yield Values(val1, val2, val3)).iterator
//      }
//    } else {
//      Iterator.single(Values(_1.value, _2.value, _3.value))
//    }
//  }
}

/** A 3-neighbor Factor whose statistics have type Tuple2.
    Only "score" method is abstract.  
    @author Andrew McCallum */
abstract class TupleFactorWithStatistics3[N1<:Var,N2<:Var,N3<:Var](override val _1:N1, override val _2:N2, override val _3:N3) extends Factor3[N1,N2,N3](_1, _2, _3) {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value))
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = (v1, v2, v3)
  final override def statisticsAreValues: Boolean = true
}

/** A 3-neighbor Factor whose statistics have type Tensor.
    Only "statistics" and "score" methods are abstract.  
    @author Andrew McCallum */
abstract class TensorFactor3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3) extends Factor3[N1,N2,N3](_1, _2, _3) {
  type StatisticsType = Tensor
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): Tensor
  final def score(v1:N1#Value, v2:N2#Value, v3:N3#Value): Double = statisticsScore(statistics(v1, v2, v3))
  override def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): (Double, Tensor) = {
    val tensor = statistics(v1, v2, v3)
    (statisticsScore(tensor), tensor)
  } 
  def statisticsScore(t:Tensor): Double
}

/** A trait for 3-neighbor Factor whose neighbors have Tensor values,
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  DotFactorWithStatistics2 is also a subclass of this.  
    @author Andrew McCallum */
trait TensorFactorStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar] extends TensorFactor3[N1,N2,N3] {
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = cc.factorie.la.Tensor.outer(v1, v2, v3)
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
  final override def statisticsAreValues: Boolean = true
}

/** A 3-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  
    @author Andrew McCallum */
abstract class TensorFactorWithStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3) extends TensorFactor3[N1,N2,N3](_1, _2, _3) with TensorFactorStatistics3[N1,N2,N3]

/** A 3-neighbor Factor whose statistics have type Tensor, 
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "statistics" and "weightsSet" methods are abstract.  
    @author Andrew McCallum */
abstract class DotFactor3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3) extends TensorFactor3[N1,N2,N3](_1, _2, _3) {
  def weights: Weights
  def statisticsScore(t:Tensor): Double = weights.value dot t
}

/** A 3-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values,
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "weightsSet" method is abstract.  
    @author Andrew McCallum */
abstract class DotFactorWithStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar](override val _1:N1, override val _2:N2, override val _3:N3) extends DotFactor3(_1, _2, _3) with TensorFactorStatistics3[N1,N2,N3] {
  override def valuesScore(valueTensor:Tensor) = weights.value dot valueTensor
}


/** Family containing Factor3 (Families of Factors having three neighbors).
    @author Andrew McCallum */
trait Family3[N1<:Var,N2<:Var,N3<:Var] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  /** Override this if you need to use them. */
  def neighborDomain1: Domain { type Value = N1#Value } = null
  def neighborDomain2: Domain { type Value = N2#Value } = null
  def neighborDomain3: Domain { type Value = N3#Value } = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2, neighborDomain3)
  type FactorType = Factor
  
//  type ValuesType = Factor#Values
  final case class Factor(override val _1:N1, override val _2:N2, override val _3:N3) extends Factor3[N1,N2,N3](_1, _2, _3) with super.Factor {
    //type StatisticsType = Family3.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family3.this
    def score(value1:N1#Value, value2:N2#Value, value3:N3#Value): Double = Family3.this.score(value1, value2, value3)
    def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType = thisFamily.statistics(v1, v2, v3)
    override def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): (Double,StatisticsType) = Family3.this.scoreAndStatistics(v1, v2, v3)
    override def valuesStatistics(tensor:Tensor): Tensor = Family3.this.valuesStatistics(tensor)
    override def statisticsAreValues: Boolean = Family3.this.statisticsAreValues
    //override def valuesScore(tensor:Tensor): Double = thisFamily.valuesScore(tensor) // TODO Consider implementing match here to use available _1 domain
    override def limitedDiscreteValues123: SparseBinaryTensor3 = Family3.this.limitedDiscreteValues123
    override def limitedDiscreteValues12: SparseBinaryTensor2 = Family3.this.limitedDiscreteValues12
    override def limitedDiscreteValues1: SparseBinaryTensor1 = Family3.this.limitedDiscreteValues1
  }
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): (Double,StatisticsType) = (score(v1, v2, v3), statistics(v1, v2, v3))
  def valuesStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor)")
  def statisticsAreValues = false

  override def valuesScore(tensor:Tensor): Double = tensor match {
    case v: SingletonBinaryTensor3 => {
      val domain1 = neighborDomain1.asInstanceOf[DiscreteDomain { type Value = N1#Value }] // TODO Yipes.  This is a bit shaky (and inefficient?)
      val domain2 = neighborDomain2.asInstanceOf[DiscreteDomain { type Value = N2#Value }]
      val domain3 = neighborDomain3.asInstanceOf[DiscreteDomain { type Value = N3#Value }]
      score(domain1(v.singleIndex1), domain2(v.singleIndex2), domain3(v.singleIndex3))
      //statistics(new SingletonBinaryTensor1(v.dim1, v.singleIndex1), new SingletonBinaryTensor1(v.dim2, v.singleIndex2)).score
    }
  }
  // For implementing sparsity in belief propagation
  def hasLimitedDiscreteValues123 = limitedDiscreteValues123 != null && limitedDiscreteValues123.activeDomainSize < limitedDiscreteValues123.length
  protected def getLimitedDiscreteValues123(factor:Factor3[VectorVar,VectorVar,VectorVar]): SparseBinaryTensor3 = { if (limitedDiscreteValues123 eq null) limitedDiscreteValues123 = new SparseBinaryTensor3(factor._1.domain.dimensionSize, factor._2.domain.dimensionSize, factor._3.domain.dimensionSize); limitedDiscreteValues123 }
  var limitedDiscreteValues123: SparseBinaryTensor3 = null

  def hasLimitedDiscreteValues12 = limitedDiscreteValues12 != null && limitedDiscreteValues12.activeDomainSize < limitedDiscreteValues12.length
  protected def getLimitedDiscreteValues12(factor:Factor3[VectorVar,VectorVar,_]): SparseBinaryTensor2 = { if (limitedDiscreteValues12 eq null) limitedDiscreteValues12 = new SparseBinaryTensor2(factor._1.domain.dimensionSize, factor._2.domain.dimensionSize); limitedDiscreteValues12 }
  var limitedDiscreteValues12: SparseBinaryTensor2 = null
  
  def hasLimitedDiscreteValues1 = limitedDiscreteValues1 != null && limitedDiscreteValues1.activeDomainSize < limitedDiscreteValues1.length
  protected def getLimitedDiscreteValues1(factor:Factor3[VectorVar,_,_]): SparseBinaryTensor1 = { if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new SparseBinaryTensor1(factor._1.domain.dimensionSize); limitedDiscreteValues1 }
  var limitedDiscreteValues1: SparseBinaryTensor1 = null
  
}

trait TupleFamily3[N1<:Var,N2<:Var,N3<:Var] extends Family3[N1,N2,N3] {
  type StatisticsType = ((N1#Value, N2#Value, N3#Value))
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): ((N1#Value, N2#Value, N3#Value))
}

trait TupleFamilyWithStatistics3[N1<:Var,N2<:Var,N3<:Var] extends TupleFamily3[N1,N2,N3] {
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = (v1, v2, v3)
}

trait TensorFamily3[N1<:Var,N2<:Var,N3<:Var] extends Family3[N1,N2,N3] with TensorFamily {
  override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): Tensor
}

trait TensorFamilyWithStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar] extends TensorFamily3[N1,N2,N3] {
  //type StatisticsType = Tensor
  final def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Tensor.outer(v1, v2, v3)
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
}

trait DotFamily3[N1<:Var,N2<:Var,N3<:Var] extends TensorFamily3[N1,N2,N3] with DotFamily {
  def score(v1:N1#Value, v2:N2#Value, v3:N3#Value): Double = statisticsScore(statistics(v1, v2, v3))
}

trait DotFamilyWithStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar] extends TensorFamilyWithStatistics3[N1,N2,N3] with DotFamily3[N1,N2,N3] {
  override def weights: Weights3
  //def score(v1:N1#Value, v2:N2#Value, v3:N3#Value): Double = weightsSet dot statistics(v1, v2, v3)
  override def valuesScore(tensor:Tensor): Double = statisticsScore(tensor)
  // TODO Consider a more efficient implementation of some cases
  // TODO Should we consider the capability for something other than *summing* over elements of tensor2?
  /** Given the Tensor value of neighbors _2 and _3, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def scores1(tensor2:Tensor, tensor3:Tensor): Tensor1 = {
    val outer = Tensor.outer(tensor2, tensor3)
    val dim = weights.value.dim1 //statisticsDomains._1.dimensionDomain.size
    val result = new DenseTensor1(dim)
    outer.foreachActiveElement((j,v) => for (i <- 0 until dim) result(i) += weights.value(i*dim + j) * v)
    result
  }
  def scores2(tensor1:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
  def scores3(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
}

//trait Statistics3[S1,S2,S3] extends Family {
//  self =>
//  type StatisticsType = Statistics
//  final case class Statistics(_1:S1, _2:S2, _3:S3) extends super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait TensorStatistics3[S1<:Tensor,S2<:Tensor,S3<:Tensor] extends TensorFamily {
//  self =>
//  type StatisticsType = Statistics
//  //override def statisticsDomains: Tuple3[VectorDomain with Domain[S1], VectorDomain with Domain[S2], VectorDomain with Domain[S3]]
//  final case class Statistics(_1:S1, _2:S2, _3:S3) extends { val tensor: Tensor = Tensor.outer(_1, _2, _3) } with super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait DotStatistics3[S1<:Tensor,S2<:Tensor,S3<:Tensor] extends TensorStatistics3[S1,S2,S3] with DotFamily {
//  override def weightsSet: Tensor3
//  /** Given the Tensor value of neighbors _2 and _3, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
//      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
//      If _1 is not a DiscreteVar then throws an Error. */
//  def scores1(tensor2:Tensor, tensor3:Tensor): Tensor1 = {
//    val outer = Tensor.outer(tensor2, tensor3)
//    val dim = weightsSet.dim1 //statisticsDomains._1.dimensionDomain.size
//    val result = new DenseTensor1(dim)
//    outer.foreachActiveElement((j,v) => for (i <- 0 until dim) result(i) += weightsSet(i*dim + j) * v)
//    result
//  }
//  def scores2(tensor1:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
//  def scores3(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
//}
//
//trait FamilyWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable] extends Family3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType = Statistics(v1, v2, v3)
//}
//
//trait FamilyWithTensorStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar] extends Family3[N1,N2,N3] with TensorStatistics3[N1#Value,N2#Value,N3#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Statistics(v1, v2, v3)
//}
//
//trait FamilyWithDotStatistics3[N1<:TensorVar,N2<:TensorVar,N3<:TensorVar] extends Family3[N1,N2,N3] with DotStatistics3[N1#Value,N2#Value,N3#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Statistics(v1, v2, v3)
//  def valuesScore(tensor:Tensor): Double = statisticsScore(tensor) // reflecting the fact that there is no transformation between values and statistics
//}