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

import cc.factorie.variable._

import cc.factorie.la._

/** The only abstract things are _1, _2, statistics(Values), and StatisticsType  
    @author Andrew McCallum */
abstract class Factor2[N1<:Var,N2<:Var](val _1:N1, val _2:N2) extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2

  def score(v1:N1#Value, v2:N2#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value): StatisticsType = (v1, v2).asInstanceOf[StatisticsType]
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value): (Double,StatisticsType) = (score(v1, v2), statistics(v1, v2))
  def currentScore: Double = score(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value])
  override def currentStatistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value])
  override def currentScoreAndStatistics: (Double,StatisticsType) = scoreAndStatistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value])
  
  def numVariables = 2
  override def variables = IndexedSeq(_1, _2)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
  //val _variables = Seq(factor._1, factor._2)
  
  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment2(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def assignmentScore(a:Assignment) = a match {
    case a:AbstractAssignment2[N1,N2] if (a._1 eq _1) && (a._2 eq _2) => score(a.value1, a.value2)
    case _ =>
      score(a(_1), a(_2))
  }
  override final def assignmentStatistics(a:Assignment): StatisticsType = a match {
    case a:AbstractAssignment2[N1,N2] if (a._1 eq _1) && (a._2 eq _2) => statistics(a.value1, a.value2)
    case _ => statistics(a(_1), a(_2))
  }
  /** Given multiplicative factors on values of neighbor _1 (which allow for limited iteration), and given the Tensor value of neighbor _2,
      return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def valuesScore1(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("Not implemented in Factor "+getClass)
  def valuesScore2(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("Not implemented in Factor "+getClass)

  // For implementing sparsity in belief propagation
  def hasLimitedDiscreteValues12 = limitedDiscreteValues12 != null && limitedDiscreteValues12.activeDomainSize < limitedDiscreteValues12.length
  def limitedDiscreteValues12: SparseBinaryTensor2 = null // throw new Error("This Factor type does not implement limitedDiscreteValues1: "+getClass)
  def addLimitedDiscreteValues12(i:Int, j:Int): Unit = limitedDiscreteValues12.+=(i, j)
  def addLimitedDiscreteCurrentValues12(): Unit = addLimitedDiscreteValues12(_1.asInstanceOf[DiscreteVar].intValue, _2.asInstanceOf[DiscreteVar].intValue)
  
  def hasLimitedDiscreteValues1 = limitedDiscreteValues1 != null && limitedDiscreteValues1.activeDomainSize < limitedDiscreteValues1.length
  def limitedDiscreteValues1: SparseBinaryTensor1 = null // throw new Error("This Factor type does not implement limitedDiscreteValues1: "+getClass)
  def addLimitedDiscreteValues1(i:Int): Unit = limitedDiscreteValues1.+=(i)
  def addLimitedDiscreteCurrentValues1(): Unit = addLimitedDiscreteValues1(this._1.asInstanceOf[DiscreteVar].intValue)
  
  
  // TODO Consider something like this?
  // def assignmentIterator(fixed: Assignment): Iterator[Assignment2]


//  /** valuesIterator in style of specifying fixed neighbors */
//  def valuesIterator(fixed: Assignment): Iterator[Values] = {
//    val fixed1 = fixed.contains(_1)
//    val fixed2 = fixed.contains(_2)
//    if (fixed1 && fixed2) 
//      Iterator.single(new Values(fixed(_1), fixed(_2)))
//    else if (fixed1) {
//      val val1 = fixed(_1)
//      if (isLimitingValuesIterator) {
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._1 == intVal1).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value]))
//      } else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        d2.iterator.map(value => new Values(val1, value))
//      }
//    } else if (fixed2) {
//      val val2 = fixed(_2)
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._2 == intVal2).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2))
//      } else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        d1.iterator.map(value => new Values(value, val2))
//      }
//    } else {
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value])) 
//      } else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        (for (val1 <- d1; val2 <- d2) yield new Values(val1, val2)).iterator
//      }
//    }
//  }
//  
//  /** valuesIterator in style of specifying varying neighbors */
//  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
//    val varying1 = varying.contains(_1)
//    val varying2 = varying.contains(_2)
//    if (varying1 && varying2) {
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        limitedDiscreteValuesIterator.map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], d2.apply(t._2).asInstanceOf[N2#Value])) 
//      } else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        (for (val1 <- d1; val2 <- d2) yield new Values(val1, val2)).iterator
//      }
//    } else if (varying1) {
//      val val2 = _2.value
//      if (isLimitingValuesIterator) {
//        val d1 = _1.domain.asInstanceOf[DiscreteDomain]
//        val intVal2 = val2.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._2 == intVal2).map(t => new Values(d1.apply(t._1).asInstanceOf[N1#Value], val2))
//      } else {
//        val d1 = _1.domain.asInstanceOf[Seq[N1#Value]]
//        d1.iterator.map(value => new Values(value, val2))
//      }
//    } else if (varying2) {
//      val val1 = _1.value
//      if (isLimitingValuesIterator) {
//        val d2 = _2.domain.asInstanceOf[DiscreteDomain]
//        val intVal1 = val1.asInstanceOf[DiscreteVar].intValue
//        limitedDiscreteValuesIterator.filter(t => t._1 == intVal1).map(t => new Values(val1, d2.apply(t._2).asInstanceOf[N2#Value]))
//      } else {
//        val d2 = _2.domain.asInstanceOf[Seq[N2#Value]]
//        d2.iterator.map(value => new Values(val1, value))
//      }
//    } else {
//      Iterator.single(new Values(_1.value, _2.value))
//    }
//  }

}

/** A 2-neighbor Factor whose statistics have type Tuple2.
    Only "score" method is abstract.  
    @author Andrew McCallum */
abstract class TupleFactorWithStatistics2[N1<:Var,N2<:Var](override val _1:N1, override val _2:N2) extends Factor2[N1,N2](_1, _2) {
  type StatisticsType = ((N1#Value, N2#Value))
  final override def statistics(v1:N1#Value, v2:N2#Value) = (v1, v2)
  final override def statisticsAreValues: Boolean = true
}

/** A 2-neighbor Factor whose statistics have type Tensor.
    Only "statistics" and "score" methods are abstract.  
    @author Andrew McCallum */
abstract class TensorFactor2[N1<:Var,N2<:Var](override val _1:N1, override val _2:N2) extends Factor2[N1,N2](_1, _2) {
  type StatisticsType = Tensor
  override def statistics(v1:N1#Value, v2:N2#Value): Tensor
  final def score(v1:N1#Value, v2:N2#Value): Double = statisticsScore(statistics(v1, v2))
  override def scoreAndStatistics(v1:N1#Value, v2:N2#Value): (Double, Tensor) = {
    val tensor = statistics(v1, v2)
    (statisticsScore(tensor), tensor)
  } 
  def statisticsScore(t:Tensor): Double
}

/** A trait for 2-neighbor Factor whose neighbors have Tensor values,
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  DotFactorWithStatistics2 is also a subclass of this.  
    @author Andrew McCallum */
trait TensorFactorStatistics2[N1<:TensorVar,N2<:TensorVar] extends TensorFactor2[N1,N2] {
  final override def statistics(v1:N1#Value, v2:N2#Value): Tensor = v1 outer v2
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
  final override def statisticsAreValues: Boolean = true
}

/** A 2-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  
    @author Andrew McCallum */
abstract class TensorFactorWithStatistics2[N1<:TensorVar,N2<:TensorVar](override val _1:N1, override val _2:N2) extends TensorFactor2[N1,N2](_1, _2) with TensorFactorStatistics2[N1,N2]

/** A 2-neighbor Factor whose statistics have type Tensor, 
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "statistics" and "weightsSet" methods are abstract.  
    @author Andrew McCallum */
abstract class DotFactor2[N1<:TensorVar,N2<:TensorVar](override val _1:N1, override val _2:N2) extends TensorFactor2[N1,N2](_1, _2) {
  def weights: Weights
  def statisticsScore(t:Tensor): Double = weights.value dot t
}

/** A 2-neighbor Factor whose neighbors have Tensor values, 
    and whose statistics are the outer product of those values,
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "weightsSet" method is abstract.  
    @author Andrew McCallum */
abstract class DotFactorWithStatistics2[N1<:TensorVar,N2<:TensorVar](override val _1:N1, override val _2:N2) extends DotFactor2(_1, _2) with TensorFactorStatistics2[N1,N2] {
  override def valuesScore(valueTensor:Tensor) = weights.value dot valueTensor
}

/** Family containing Factor2 (Families of Factors having two neighbors).
    @author Andrew McCallum */
trait Family2[N1<:Var,N2<:Var] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain { type Value <: N1#Value } = null
  def neighborDomain2: Domain { type Value <: N2#Value } = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2)

  type FactorType = Factor
  final case class Factor(override val _1:N1, override val _2:N2) extends Factor2[N1,N2](_1, _2) with super.Factor {
    //type StatisticsType = Family2.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family2.this
    def score(value1:N1#Value, value2:N2#Value): Double = Family2.this.score(value1, value2)
    override def statistics(v1:N1#Value, v2:N2#Value): StatisticsType = Family2.this.statistics(v1, v2)
    override def scoreAndStatistics(v1:N1#Value, v2:N2#Value): (Double,StatisticsType) = Family2.this.scoreAndStatistics(v1, v2)
    override def valuesScore(tensor:Tensor): Double = Family2.this.valuesScore(tensor) // TODO Consider implementing match here to use available _1 domain
    override def statisticsScore(tensor:Tensor): Double = Family2.this.statisticsScore(tensor)
    override def valuesStatistics(tensor:Tensor): Tensor = Family2.this.valuesStatistics(tensor)
    override def statisticsAreValues: Boolean = Family2.this.statisticsAreValues
    override def limitedDiscreteValues12: SparseBinaryTensor2 = Family2.this.limitedDiscreteValues12 //(this.asInstanceOf[Factor2[VectorVar,VectorVar]])
    override def limitedDiscreteValues1: SparseBinaryTensor1 = Family2.this.limitedDiscreteValues1 //(this.asInstanceOf[Factor2[VectorVar,N2]])
  }
  def score(v1:N1#Value, v2:N2#Value): Double
  def statistics(v1:N1#Value, v2:N2#Value): StatisticsType = (v1, v2).asInstanceOf[StatisticsType]
  def scoreAndStatistics(v1:N1#Value, v2:N2#Value): (Double,StatisticsType) = (score(v1, v2), statistics(v1, v2))
  def valuesStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor)")
  def statisticsAreValues: Boolean = false
  
  override def valuesScore(tensor:Tensor): Double = tensor match {
    case v: SingletonBinaryTensorLike2 => {
      val domain0 = neighborDomain1.asInstanceOf[DiscreteDomain { type Value <: N1#Value }]
      val domain1 = neighborDomain2.asInstanceOf[DiscreteDomain { type Value <: N2#Value }]
      score(domain0(v.singleIndex1), domain1(v.singleIndex2))
      //statistics(new SingletonBinaryTensor1(v.dim1, v.singleIndex1), new SingletonBinaryTensor1(v.dim2, v.singleIndex2)).score
    }
    case v: SingletonBinaryLayeredTensor2 => {
      val domain0 = if (neighborDomain1 ne null) neighborDomain1.asInstanceOf[DiscreteDomain { type Value <: N1#Value }] else new DiscreteDomain(Int.MaxValue).asInstanceOf[DiscreteDomain { type Value  = N1#Value }]
      score(domain0(v.singleIndex1), v.inner.asInstanceOf[N2#Value])
    }
    case v: Outer1Tensor2 => {
      (v.tensor1, v.tensor2) match {
        case (v1: SingletonBinaryTensor1, v2: SingletonBinaryTensor1) =>
          val domain0 = neighborDomain1.asInstanceOf[DiscreteDomain { type Value <: N1#Value }] // TODO Yipes.  This is a bit shaky (and inefficient?)
          val domain1 = neighborDomain2.asInstanceOf[DiscreteDomain { type Value <: N2#Value }]
          v.scale*score(domain0(v1.singleIndex), domain1(v1.singleIndex))
        case (v1: SingletonBinaryTensor1, v2: N2#Value @unchecked) =>
          val domain0 = neighborDomain1.asInstanceOf[DiscreteDomain { type Value <: N1#Value }] // TODO Yipes.  This is a bit shaky (and inefficient?)
          v.scale*score(domain0(v1.singleIndex), v2)
      }
    }
  }

  // For implementing sparsity in belief propagation
  
  def hasLimitedDiscreteValues12 = limitedDiscreteValues12 != null && limitedDiscreteValues12.activeDomainSize < limitedDiscreteValues12.length
  //protected def getLimitedDiscreteValues12(factor:Factor2[VectorVar,VectorVar]): SparseBinaryTensor2 = { if (limitedDiscreteValues12 eq null) limitedDiscreteValues12 = new SparseBinaryTensor2(factor._1.domain.dimensionSize, factor._2.domain.dimensionSize); limitedDiscreteValues12 }
  var limitedDiscreteValues12: SparseBinaryTensor2 = null
  
  def hasLimitedDiscreteValues1 = limitedDiscreteValues1 != null && limitedDiscreteValues1.activeDomainSize < limitedDiscreteValues1.length
  //protected def getLimitedDiscreteValues1(factor:Factor2[VectorVar,_]): SparseBinaryTensor1 = { if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new SparseBinaryTensor1(factor._1.domain.dimensionSize); limitedDiscreteValues1 }
  var limitedDiscreteValues1: SparseBinaryTensor1 = null

  def hasLimitedDiscreteValues2 = limitedDiscreteValues2 != null && limitedDiscreteValues2.activeDomainSize < limitedDiscreteValues2.length
  //protected def getLimitedDiscreteValues2(factor:Factor2[_, VectorVar]): SparseBinaryTensor1 = { if (limitedDiscreteValues2 eq null) limitedDiscreteValues2 = new SparseBinaryTensor1(factor._2.domain.dimensionSize); limitedDiscreteValues2 }
  var limitedDiscreteValues2: SparseBinaryTensor1 = null

//  // Cached Statistics
//  private var cachedStatisticsArray: Array[StatisticsType] = null
//  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
//  /** It is callers responsibility to clearCachedStatistics if weightsSet or other relevant state changes. */
//  override def cachedStatistics(values:Values): StatisticsType =
//    if (Template.enableCachedStatistics) values._1 match {
//    case v1:DiscreteValue => { 
//      values._2 match {
//        case v2:DiscreteValue => {
//          //println("Template2.cachedStatistics")
//          if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v1.domain.size * v2.domain.size).asInstanceOf[Array[StatisticsType]]
//          val i = v1.intValue * v2.domain.dimensionSize + v2.intValue
//          if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = values.statistics
//          cachedStatisticsArray(i)
//        }
//        case v2:VectorValue if (true /*v2.isConstant*/) => {
//          //println("Template2.cachedStatistics")
//          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType] { override protected def initialSize = 512 }
//          val i = ((v1.intValue,v2))
//          cachedStatisticsHash.getOrElseUpdate(i, values.statistics)
//        }
//        case _ => values.statistics
//      }
//    }
//    case v1:VectorValue if (true /*v1.isConstant*/) => {
//      values._2 match {
//        case v2:DiscreteValue => {
//          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
//          val i = ((v2.intValue,v1))
//          cachedStatisticsHash.getOrElseUpdate(i, values.statistics)
//        }
//        case _ => values.statistics
//      }
//    }
//    case _ => values.statistics
//  } else values.statistics
//  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
}

trait TupleFamily2[N1<:Var,N2<:Var] extends Family2[N1,N2] {
  type StatisticsType = ((N1#Value, N2#Value))
  //def statistics(v1:N1#Value, v2:N2#Value): ((N1#Value, N2#Value))
}

trait TupleFamilyWithStatistics2[N1<:Var,N2<:Var] extends TupleFamily2[N1,N2] {
  final override def statistics(v1:N1#Value, v2:N2#Value): ((N1#Value, N2#Value)) = (v1, v2)
  final override def statisticsAreValues: Boolean = true
}

trait TensorFamily2[N1<:Var,N2<:Var] extends Family2[N1,N2] with TensorFamily {
  override def statistics(v1:N1#Value, v2:N2#Value): Tensor
}

trait TensorFamilyWithStatistics2[N1<:TensorVar,N2<:TensorVar] extends TensorFamily2[N1,N2] {
  //type StatisticsType = Tensor
  final override def statistics(v1:N1#Value, v2:N2#Value) = v1 outer v2
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
  final override def statisticsAreValues: Boolean = true
}

trait DotFamily2[N1<:Var,N2<:Var] extends TensorFamily2[N1,N2] with DotFamily {
  def score(v1:N1#Value, v2:N2#Value): Double = statisticsScore(statistics(v1, v2))
}

trait DotFamilyWithStatistics2[N1<:TensorVar,N2<:TensorVar] extends TensorFamilyWithStatistics2[N1,N2] with DotFamily2[N1,N2] {
  override def weights: Weights2
  //def score(v1:N1#Value, v2:N2#Value): Double = weightsSet dot statistics(v1, v2)
  override def valuesScore(tensor:Tensor): Double = statisticsScore(tensor)
  // TODO Consider a more efficient implementation of some cases
  // TODO Should we consider the capability for something other than *summing* over elements of tensor2?
  def valueScores1(tensor2:Tensor): Tensor1 = weights.value match {
    case weights: Tensor2 => {
      val dim = weights.dim1 // statisticsDomains._1.dimensionDomain.size
      val result = new DenseTensor1(dim)
      tensor2 match {
        case tensor2:SingletonBinaryTensor1 => {
          val j = tensor2.singleIndex
          for (i <- 0 until dim) result(i) = weights(i, j)
        }
        case tensor2:SingletonTensor1 => {
          val j = tensor2.singleIndex
          val v = tensor2.singleValue
          for (i <- 0 until dim) result(i) = v * weights(i, j)
        }
        case tensor2:UnaryTensor1 => {
          for (i <- 0 until dim; j <- 0 until tensor2.length) result(i) += weights(i, j) 
        }
        case tensor2:UniformTensor1 => {
          val v = tensor2.uniformValue
          for (i <- 0 until dim; j <- 0 until tensor2.length) result(i) += v * weights(i, j) 
        }
        case _ => {
          tensor2.foreachActiveElement((j,v) => for (i <- 0 until dim) result(i) += v * weights(i, j))
        }
      }
      result
    }
  }
  // TODO Consider a more efficient implementation of some cases
  // TODO Should we consider the capability for something other than *summing* over elements of tensor1?
  def valueScores2(tensor1:Tensor): Tensor1 = weights.value match {
    case weights: Tensor2 => {
      val dim = weights.dim2 //statisticsDomains._2.dimensionDomain.size
      val result = new DenseTensor1(dim)
      tensor1 match {
        case tensor1:SingletonBinaryTensor1 => {
          val i = tensor1.singleIndex
          for (j <- 0 until dim) result(j) = weights(i, j)
        }
        case tensor1:SingletonTensor1 => {
          val i = tensor1.singleIndex
          val v = tensor1.singleValue
          for (j <- 0 until dim) result(j) = v * weights(i, j)
        }
        case tensor1:UnaryTensor1 => {
          for (i <- 0 until tensor1.length; j <- 0 until dim) result(i) += weights(i, j) 
        }
        case tensor1:UniformTensor1 => {
          val v = tensor1.uniformValue
          for (i <- 0 until tensor1.length; j <- 0 until dim) result(j) += v * weights(i, j) 
        }
        case _ => {
          tensor1.foreachActiveElement((i,v) => for (j <- 0 until dim) result(j) += v * weights(i, j))
        }
      }
      result
    }
  }
}


//trait Statistics2[S1,S2] extends Family {
//  self =>
//  type StatisticsType = Statistics
//  final case class Statistics(_1:S1, _2:S2) extends super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait TensorStatistics2[S1<:Tensor,S2<:Tensor] extends TensorFamily {
//  self =>
//  type StatisticsType = Statistics
//  //override def statisticsDomains: Tuple2[VectorDomain with Domain[S1], VectorDomain with Domain[S2]]
//  final case class Statistics(_1:S1, _2:S2) extends { val tensor: Tensor = Tensor.outer(_1, _2) } with super.Statistics {
//    val score = self.score(this)
//  }
//}
//
//trait DotStatistics2[S1<:Tensor,S2<:Tensor] extends TensorStatistics2[S1,S2] with DotFamily {
//  override def weightsSet: Tensor2
//  //def statisticsScore(tensor:Tensor) = weightsSet dot tensor
//}
//
//trait FamilyWithStatistics2[N1<:Variable,N2<:Variable] extends Family2[N1,N2] with Statistics2[N1#Value,N2#Value] {
////  def statistics(values:Values) = Stat(values._1, values._2)
//  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
//}
//
//trait FamilyWithTensorStatistics2[N1<:VectorVar,N2<:VectorVar] extends Family2[N1,N2] with TensorStatistics2[N1#Value,N2#Value] {
////  def statistics(values:Values) = Stat(values._1, values._2)
//  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
//}
//
//trait FamilyWithDotStatistics2[N1<:VectorVar,N2<:VectorVar] extends Family2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
////  def statistics(values:Values) = Stat(values._1, values._2)
//  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
//  override def valuesScore(tensor:Tensor): Double = statisticsScore(tensor)
//  // TODO Consider a more efficient implementation of some cases
//  // TODO Should we consider the capability for something other than *summing* over elements of tensor2?
//  def valueScores1(tensor2:Tensor): Tensor1 = weightsSet match {
//    case weightsSet: Tensor2 => {
//      val dim = weightsSet.dim1 // statisticsDomains._1.dimensionDomain.size
//      val result = new DenseTensor1(dim)
//      tensor2 match {
//        case tensor2:SingletonBinaryTensor1 => {
//          val j = tensor2.singleIndex
//          for (i <- 0 until dim) result(i) = weightsSet(i, j)
//        }
//        case tensor2:SingletonTensor1 => {
//          val j = tensor2.singleIndex
//          val v = tensor2.singleValue
//          for (i <- 0 until dim) result(i) = v * weightsSet(i, j)
//        }
//        case tensor2:UnaryTensor1 => {
//          for (i <- 0 until dim; j <- 0 until tensor2.length) result(i) += weightsSet(i, j)
//        }
//        case tensor2:UniformTensor1 => {
//          val v = tensor2.uniformValue
//          for (i <- 0 until dim; j <- 0 until tensor2.length) result(i) += v * weightsSet(i, j)
//        }
//        case _ => {
//          tensor2.foreachActiveElement((j,v) => for (i <- 0 until dim) result(i) += v * weightsSet(i, j))
//        }
//      }
//      result
//    }
//  }
//  // TODO Consider a more efficient implementation of some cases
//  // TODO Should we consider the capability for something other than *summing* over elements of tensor1?
//  def valueScores2(tensor1:Tensor): Tensor1 = weightsSet match {
//    case weightsSet: Tensor2 => {
//      val dim = weightsSet.dim2 //statisticsDomains._2.dimensionDomain.size
//      val result = new DenseTensor1(dim)
//      tensor1 match {
//        case tensor1:SingletonBinaryTensor1 => {
//          val i = tensor1.singleIndex
//          for (j <- 0 until dim) result(j) = weightsSet(i, j)
//        }
//        case tensor1:SingletonTensor1 => {
//          val i = tensor1.singleIndex
//          val v = tensor1.singleValue
//          for (j <- 0 until dim) result(j) = v * weightsSet(i, j)
//        }
//        case tensor1:UnaryTensor1 => {
//          for (i <- 0 until tensor1.length; j <- 0 until dim) result(i) += weightsSet(i, j)
//        }
//        case tensor1:UniformTensor1 => {
//          val v = tensor1.uniformValue
//          for (i <- 0 until tensor1.length; j <- 0 until dim) result(j) += v * weightsSet(i, j)
//        }
//        case _ => {
//          tensor1.foreachActiveElement((i,v) => for (j <- 0 until dim) result(j) += v * weightsSet(i, j))
//        }
//      }
//      result
//    }
//  }
//}

