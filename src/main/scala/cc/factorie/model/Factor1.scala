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

/** A Factor with one neighboring variable
    @author Andrew McCallum */
abstract class Factor1[N1<:Var](val _1:N1) extends Factor {
  type NeighborType1 = N1

  def score(v1:N1#Value): Double
  def statistics(v1:N1#Value): StatisticsType = throw new Error("statistics not defined") // ((v1)).asInstanceOf[StatisticsType]
  def scoreAndStatistics(v1:N1#Value): (Double,StatisticsType) = (score(v1), statistics(v1))
  def currentScore: Double = score(_1.value.asInstanceOf[N1#Value])
  override def currentStatistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value])
  override def currentScoreAndStatistics: (Double,StatisticsType) = scoreAndStatistics(_1.value.asInstanceOf[N1#Value])
  
  def numVariables = 1
  override def variables = IndexedSeq(_1)
  def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment1(_1, _1.value.asInstanceOf[N1#Value]) // TODO Rename to simply "assignment", because "score" doesn't have "current" in its name.
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def assignmentScore(a:Assignment) = a match {
    case a:AbstractAssignment1[N1] if a._1 eq _1 => score(a.value1)
    case _ => score(a(_1))
  }
  override final def assignmentStatistics(a:Assignment): StatisticsType = a match {
    case a:AbstractAssignment1[N1] if a._1 eq _1 => statistics(a.value1)
    case _ => statistics(a(_1))
  }

  def hasLimitedDiscreteValues1 = limitedDiscreteValues1.activeDomainSize > 0
  def limitedDiscreteValues1: SparseBinaryTensor1 = throw new Error("This Factor type does not implement limitedDiscreteValues1: "+getClass)
  def addLimitedDiscreteValues1(i:Int): Unit = limitedDiscreteValues1.+=(i)
  def addLimitedDiscreteCurrentValues1(): Unit = addLimitedDiscreteValues1(this._1.asInstanceOf[DiscreteVar].intValue)

  
  /** Return a Tensor1 containing the scores for each possible value of neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def valuesScore1: Tensor1 = throw new Error("This Factor type does not implement scores1")

  // For implementing sparsity in belief propagation
//  def isLimitingValuesIterator = false
//  def limitedDiscreteValuesIterator: Iterator[Int] = Iterator.empty // TODO Replace with IntSeq for efficiency

//  /** valuesIterator in style of specifying fixed neighbors */
//  def valuesIterator(fixed: Assignment): Iterator[Values] = {
//    if (fixed.contains(_1)) Iterator.single(Values(fixed(_1)))
//    else if (isLimitingValuesIterator) {
//      val d = _1.domain.asInstanceOf[DiscreteDomain]
//      limitedDiscreteValuesIterator.map(i => Values(d.apply(i).asInstanceOf[N1#Value]))
//    } else _1.domain match {
//      case d:Seq[N1#Value] => d.iterator.map(value => Values(value))
//    }
//  }
//  /** valuesIterator in style of specifying varying neighbors */
//  def valuesIterator(varying:Set[Variable]): Iterator[Values] = {
//    if (varying.size != 1 || varying.head != _1)
//      throw new Error("Factor1.valuesIterator cannot vary arguments.")
//    else if (isLimitingValuesIterator) {
//      val d = _1.domain.asInstanceOf[DiscreteDomain]
//      limitedDiscreteValuesIterator.map(i => Values(d.apply(i).asInstanceOf[N1#Value]))
//    } else _1.domain match {
//      case d:Seq[N1#Value] => d.iterator.map(value => Values(value))
//    }
//  }
}

/** A 1-neighbor Factor whose statistics have type Tuple1.
    Only "score" method is abstract.  
    @author Andrew McCallum */
abstract class FactorWithStatistics1[N1<:Var](override val _1:N1) extends Factor1[N1](_1) {
  type StatisticsType = N1#Value
  final override def statistics(v1:N1#Value): N1#Value = v1
  final override def statisticsAreValues: Boolean = true
}

/** A 1-neighbor Factor whose statistics have type Tensor.
    Only "statistics" and "score" methods are abstract.  
    @author Andrew McCallum */
abstract class TensorFactor1[N1<:Var](override val _1:N1) extends Factor1[N1](_1) {
  type StatisticsType = Tensor
  override def statistics(v1:N1#Value): Tensor
  final def score(v1:N1#Value): Double = statisticsScore(statistics(v1))
  override def scoreAndStatistics(v1:N1#Value): (Double, Tensor) = {
    val tensor = statistics(v1)
    (statisticsScore(tensor), tensor)
  } 
  def statisticsScore(t:Tensor): Double
}

/** A trait for 1-neighbor Factor whose neighbor has Tensor values, and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  DotFactorWithStatistics2 is also a subclass of this.  
    @author Andrew McCallum */
trait TensorFactorStatistics1[N1<:TensorVar] extends TensorFactor1[N1] {
  final override def statistics(v1:N1#Value): N1#Value = v1
  final override def currentStatistics = _1.value.asInstanceOf[N1#Value] // TODO Why is this cast necessary?
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
  final override def statisticsAreValues: Boolean = true
}

/** A 1-neighbor Factor whose neighbor has Tensor values, 
    and whose statistics are the outer product of those values.
    Only "statisticsScore" method is abstract.  
    @author Andrew McCallum */
abstract class TensorFactorWithStatistics1[N1<:TensorVar](override val _1:N1) extends TensorFactor1[N1](_1) with TensorFactorStatistics1[N1]

/** A 1-neighbor Factor whose statistics have type Tensor, 
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "statistics" and "weightsSet" methods are abstract.  
    @author Andrew McCallum */
abstract class DotFactor1[N1<:TensorVar](override val _1:N1) extends TensorFactor1[N1](_1) {
  def weights: Weights
  def statisticsScore(t:Tensor): Double = weights.value dot t
}

/** A 1-neighbor Factor whose neighbor has Tensor values, 
    and whose statistics are the outer product of those values,
    and whose score is the dot product between this Tensor and a "weightsSet" parameter Tensor.
    Only "weightsSet" method is abstract.  
    @author Andrew McCallum */
abstract class DotFactorWithStatistics1[N1<:TensorVar](override val _1:N1) extends DotFactor1[N1](_1) with TensorFactorStatistics1[N1] {
  override def valuesScore(valueTensor:Tensor) = valueTensor dot weights.value // Because valueTensor is the same as the statisticsTensor
}


/** Family containing Factor1 (Families of Factors having one neighbor) 
    @author Andrew McCallum */
trait Family1[N1<:Var] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain { type Value = N1#Value } = null
  def neighborDomains = Seq(neighborDomain1)
  type FactorType = Factor
  
  final case class Factor(override val _1:N1) extends Factor1[N1](_1) with super.Factor {
    //type StatisticsType = Family1.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family1.this
    def score(v1:N1#Value): Double = Family1.this.score(v1)
    override def statistics(v1:N1#Value): StatisticsType = Family1.this.statistics(v1)
    override def scoreAndStatistics(v1:N1#Value): (Double,StatisticsType) = Family1.this.scoreAndStatistics(v1)
    override def valuesStatistics(tensor:Tensor): Tensor = Family1.this.valuesStatistics(tensor)
    override def limitedDiscreteValues1: SparseBinaryTensor1 = Family1.this.limitedDiscreteValues1 //(this.asInstanceOf[Factor1[VectorVar]])
    //override def limitedDiscreteValuesIterator: Iterator[Int] = limitedDiscreteValues.iterator
  }
  def score(v1:N1#Value): Double
  def statistics(v1:N1#Value): StatisticsType = v1.asInstanceOf[StatisticsType] // TODO Make this throw an Error instead?
  def scoreAndStatistics(v1:N1#Value): (Double,StatisticsType) = (score(v1), statistics(v1))
  def valuesStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor)")
  // For implementing sparsity in belief propagation

  def hasLimitedDiscreteValues1 = limitedDiscreteValues1 != null && limitedDiscreteValues1.activeDomainSize > 0
  //protected def getLimitedDiscreteValues1(factor:Factor1[VectorVar]): SparseBinaryTensor1 = { if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new SparseBinaryTensor1(factor._1.domain.dimensionDomain.size); limitedDiscreteValues1 }
  //protected def getLimitedDiscreteValues1(v:VectorVar): SparseBinaryTensor1 = { if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new SparseBinaryTensor1(v.domain.dimensionDomain.size); limitedDiscreteValues1 }
  var limitedDiscreteValues1: SparseBinaryTensor1 = null
}

trait TupleFamily1[N1<:Var] extends Family1[N1] {
  type StatisticsType = ((N1#Value))
  //override def statistics(v1:N1#Value): ((N1#Value))
}

trait TupleFamilyWithStatistics1[N1<:Var] extends TupleFamily1[N1] {
  @inline final override def statistics(v1:N1#Value) = v1
}

trait TensorFamily1[N1<:Var] extends Family1[N1] with TensorFamily {
  override def statistics(v1:N1#Value): Tensor
}

trait TensorFamilyWithStatistics1[N1<:TensorVar] extends TensorFamily1[N1] {
  //type StatisticsTypze = N1#Value
  @inline final override def statistics(v1:N1#Value) = v1
  final override def valuesStatistics(tensor:Tensor): Tensor = tensor
}

trait DotFamily1[N1<:Var] extends TensorFamily1[N1] with DotFamily {
  def score(v1:N1#Value): Double = statisticsScore(statistics(v1))
}

trait DotFamilyWithStatistics1[N1<:TensorVar] extends TensorFamilyWithStatistics1[N1] with DotFamily1[N1] {
  override def weights: Weights1 // TODO Perhaps this should just be Tensor because neighbor value might be Tensor2,...
  //def score(v1:N1#Value): Double = statisticsScore(v1)
  def setWeight(entry:Tensor1, w:Double) = entry match {
    case t:SingletonBinaryTensor1 => weights.value(t.singleIndex) = w // e.g. DiscreteValue
  }
  def scores1(): Tensor1 = weights.value.copy
}

//trait Statistics1[S1] extends FamilyWithStatistics[S1] {
//  self =>
//  type StatisticsType = Statistics
//  case class Statistics(_1:S1) extends super.Statistics {
//    val score = self.score(this) 
//  }
//  def score(s:Statistics): Double
//}
//
//trait TensorStatistics1[S1<:Tensor] extends FamilyWithTensorStatistics {
//  self =>
//  type StatisticsType = Statistics
//  // Use Scala's "pre-initialized fields" syntax because super.Statistics needs tensor to initialize score
//  final case class Statistics(_1:S1) extends { val tensor: Tensor = _1 } with super.Statistics {
//    val score = self.score(this)
//  }
//  def score(s:Statistics): Double
//}
//
//trait DotStatistics1[S1<:Tensor] extends TensorStatistics1[S1] with FamilyWithDotStatistics {
//  override def weightsSet: Tensor1
//  def setWeight(entry:S1, w:Double) = entry match {
//    case t:SingletonBinaryTensor1 => weightsSet(t.singleIndex) = w // e.g. DiscreteValue
//    //case t:SingletonTensor1 => weightsSet(t.singleIndex) = w * t.singleValue
//    //case ds:Tensor => ds.activeDomain.foreach(i => weightsSet(i) = w)
//  }
//  def scores1(): Tensor1 = weightsSet.copy //  match { case weightsSet: Tensor1 => weightsSet.copy }
//}
//
//trait FamilyWithStatistics1[N1<:Variable] extends Family1WithStatistics[N1] with Statistics1[N1#Value] {
//  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
//}

//trait FamilyWithTensorStatistics1[N1<:TensorVar] extends Family1WithStatistics[N1] with TensorStatistics1[N1#Value] {
//  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
//}
//
//trait FamilyWithDotStatistics1[N1<:TensorVar] extends Family1WithStatistics[N1] with DotStatistics1[N1#Value] {
//  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
//  def valuesScore(tensor:Tensor): Double = statisticsScore(tensor) // reflecting the fact that there is no transformation between values and statistics
//}

