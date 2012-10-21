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
//import cc.factorie.util.Substitutions
import java.io._

trait ValuesIterator1[N1<:Variable] extends Iterator[AbstractAssignment1[N1]] with AbstractAssignment1[N1] with ValuesIterator


/** A Factor with one neighboring variable */
abstract class Factor1[N1<:Variable](val _1:N1) extends Factor {
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
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment1[N1] if (a._1 eq _1) => score(a.value1)
    case _ => score(a(_1))
  }

  /** Iterate over all value assignments of both neighbors, making available the score for each. 
      Future alternative versions of this method would allow for iterating over restricted subsets. */
  def valuesIterator: ValuesIterator1[N1] = new ValuesIterator1[N1] { //Iterator[AbstractAssignment2[N1,N2]] with AbstractAssignment2[N1,N2]
    def factor = Factor1.this
    var _1: N1 = null.asInstanceOf[N1]
    var value1: N1#Value = null.asInstanceOf[N1#Value]
    def hasNext = false
    def next() = this
    def score: Double = Double.NaN
    def valuesTensor: Tensor = null
  }
  
  /** Return a Tensor1 containing the scores for each possible value of neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def scoreValues1: Tensor1 = throw new Error("This Factor type does not implement scores1")

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

// FIXME: what is the point of the 1-tuple here?
/** A 1-neighbor Factor whose statistics have type Tuple2.
    Only "score" method is abstract. */
abstract class TupleFactorWithStatistics1[N1<:Variable](override val _1:N1) extends Factor1[N1](_1) {
  type StatisticsType = Tuple1[N1#Value]
  final override def statistics(v1:N1#Value) = Tuple1(v1)
}

/** A 1-neighbor Factor whose statistics have type Tensor.
    Only "statistics" and "score" methods are abstract. */
abstract class TensorFactor1[N1<:Variable](override val _1:N1) extends Factor1[N1](_1) {
  type StatisticsType = Tensor
  override def statistics(v1:N1#Value): Tensor
  final def score(v1:N1#Value): Double = scoreStatistics(statistics(v1))
  override def scoreAndStatistics(v1:N1#Value): (Double, Tensor) = {
    val tensor = statistics(v1)
    (scoreStatistics(tensor), tensor)
  } 
  def scoreStatistics(t:Tensor): Double
}

/** A trait for 1-neighbor Factor whose neighbor has Tensor values, and whose statistics are the outer product of those values.
    Only "scoreStatistics" method is abstract.  DotFactorWithStatistics2 is also a subclass of this. */
trait TensorFactorStatistics1[N1<:TensorVar] extends TensorFactor1[N1] {
  final override def statistics(v1:N1#Value): N1#Value = v1
  final override def currentStatistics = _1.value.asInstanceOf[N1#Value] // TODO Why is this cast necessary?
  final override def valueStatistics(tensor:Tensor): Tensor = tensor
}

/** A 1-neighbor Factor whose neighbor has Tensor values, 
    and whose statistics are the outer product of those values.
    Only "scoreStatistics" method is abstract. */
abstract class TensorFactorWithStatistics1[N1<:TensorVar](override val _1:N1) extends TensorFactor1[N1](_1) with TensorFactorStatistics1[N1]

/** A 1-neighbor Factor whose statistics have type Tensor, 
    and whose score is the dot product between this Tensor and a "weights" parameter Tensor.
    Only "statistics" and "weights" methods are abstract. */
abstract class DotFactor1[N1<:TensorVar](override val _1:N1) extends TensorFactor1[N1](_1) {
  def weights: Tensor
  def scoreStatistics(t:Tensor): Double = weights dot t
}

/** A 1-neighbor Factor whose neighbor has Tensor values, 
    and whose statistics are the outer product of those values,
    and whose score is the dot product between this Tensor and a "weights" parameter Tensor.
    Only "weights" method is abstract. */
abstract class DotFactorWithStatistics1[N1<:TensorVar](override val _1:N1) extends DotFactor1[N1](_1) with TensorFactorStatistics1[N1] {
  override def scoreValues(valueTensor:Tensor) = valueTensor dot weights // Because valueTensor is the same as the statisticsTensor
}


// Family containing Factor1 (Families of Factors having one neighbor)

trait Family1[N1<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomains = Seq(neighborDomain1)
  type FactorType = Factor
  
  final case class Factor(override val _1:N1) extends Factor1[N1](_1) with super.Factor {
    //type StatisticsType = Family1.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family1.this
    def score(v1:N1#Value): Double = Family1.this.score(v1)
    override def statistics(v1:N1#Value): StatisticsType = Family1.this.statistics(v1)
    override def scoreAndStatistics(v1:N1#Value): (Double,StatisticsType) = Family1.this.scoreAndStatistics(v1)
    override def valuesIterator: ValuesIterator1[N1] = Family1.this.valuesIterator(this) 
    override def valueStatistics(tensor:Tensor): Tensor = Family1.this.valueStatistics(tensor)
    //override def isLimitingValuesIterator = Family1.this.isLimitingValuesIterator
    //override def limitedDiscreteValuesIterator: Iterator[Int] = limitedDiscreteValues.iterator
  }
  def score(v1:N1#Value): Double
  def statistics(v1:N1#Value): StatisticsType = ((v1)).asInstanceOf[StatisticsType] // TODO Make this throw an Error instead?
  def scoreAndStatistics(v1:N1#Value): (Double,StatisticsType) = (score(v1), statistics(v1))
  def valueStatistics(tensor:Tensor): Tensor = throw new Error("This Factor class does not implement valuesStatistics(Tensor)")
  // For implementing sparsity in belief propagation
  var isLimitingValuesIterator = false
  lazy val limitedDiscreteValues = new scala.collection.mutable.HashSet[Int]
  def addLimitedDiscreteValues(values:Iterable[Int]): Unit = limitedDiscreteValues ++= values
  def valuesIterator(f:Factor): ValuesIterator1[N1] = throw new Error("Not yet implemented") // TODO Here could be the option to iterate over a subset of values for restricted FSA connectivity
}

trait TupleFamily1[N1<:Variable] extends Family1[N1] {
  type StatisticsType = ((N1#Value))
  //override def statistics(v1:N1#Value): ((N1#Value))
}

trait TupleFamilyWithStatistics1[N1<:Variable] extends TupleFamily1[N1] {
  @inline final override def statistics(v1:N1#Value) = ((v1))
}

trait TensorFamily1[N1<:Variable] extends Family1[N1] with TensorFamily {
  override def statistics(v1:N1#Value): Tensor
}

trait TensorFamilyWithStatistics1[N1<:TensorVar] extends TensorFamily1[N1] {
  //type StatisticsTypze = N1#Value
  @inline final override def statistics(v1:N1#Value) = v1
  final override def valueStatistics(tensor:Tensor): Tensor = tensor
}

trait DotFamily1[N1<:Variable] extends TensorFamily1[N1] with DotFamily {
  def score(v1:N1#Value): Double = scoreStatistics(statistics(v1))
}

trait DotFamilyWithStatistics1[N1<:TensorVar] extends TensorFamilyWithStatistics1[N1] with DotFamily1[N1] {
  override def weights: Tensor1 // TODO Perhaps this should just be Tensor because neighbor value might be Tensor2,...
  //def score(v1:N1#Value): Double = scoreStatistics(v1)
  def setWeight(entry:Tensor1, w:Double) = entry match {
    case t:SingletonBinaryTensor1 => weights(t.singleIndex) = w // e.g. DiscreteValue
  }
  def scores1(): Tensor1 = weights.copy
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
//  override def weights: Tensor1
//  def setWeight(entry:S1, w:Double) = entry match {
//    case t:SingletonBinaryTensor1 => weights(t.singleIndex) = w // e.g. DiscreteValue
//    //case t:SingletonTensor1 => weights(t.singleIndex) = w * t.singleValue 
//    //case ds:Tensor => ds.activeDomain.foreach(i => weights(i) = w)
//  }
//  def scores1(): Tensor1 = weights.copy //  match { case weights: Tensor1 => weights.copy }
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
//  def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor) // reflecting the fact that there is no transformation between values and statistics
//}

