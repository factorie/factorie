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

trait ValuesIterator1[N1<:Variable] extends Iterator[AbstractAssignment1[N1]] with AbstractAssignment1[N1] with ValuesIterator

/** A Factor with one neighboring variable */
trait Factor1[N1<:Variable] extends Factor {
  factor =>
  type NeighborType1 = N1
  type StatisticsType <: cc.factorie.Statistics
  protected def thisFactor: this.type = this
  def _1: N1
  def numVariables = 1
  override def variables = IndexedSeq(_1)
  def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
//  override def values = new Values(_1.value)
//  /** Return the score of a value assignment represented by the Tensor argument.  
//      If this Factor's values cannot be represented by a Tensor, throw an Error.
//      This method is overriden and very efficient in FamilyWithDotStatistics classes. */
//  override def valueScore(tensor:Tensor) = tensor match {
//    case t: SingletonBinaryTensorLike1 => {
//      val domain0 = _1.domain.asInstanceOf[DiscreteDomain with Domain[N1#Value]]
//      new Values(domain0(t.singleIndex)).score
//    }
//  }
//  // Should the next line read instead V<:N1 ?? -akm
//  def valuesAssigning[V<:Variable](variable:V, value:V#Value): Unit = if (variable eq _1) new Values(value.asInstanceOf[N1#Value]) else throw new Error
//  case class Values(_1:N1#Value) extends cc.factorie.Values {
//    override def apply[B <: Variable](v: B) = get(v).get
//    def variables = Seq(factor._1)
//    def get[B <: Variable](v: B) = if(contains(v)) Some(_1.asInstanceOf[B#Value]) else None
//    def contains(v: Variable) = v == factor._1
//    override def statistics: StatisticsType = Factor1.this.statistics(this)
//    override def index(varying:Set[Variable]): Int = {
//      if(varying.contains(factor._1)) {
//        _1 match {
//          case dv: DiscreteValue => dv.intValue
//          case _ => -1
//        }
//      } else -1
//    }
//  }
//  def statistics(v:Values): StatisticsType
  def statistics(v1:N1#Value): StatisticsType
  def statistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value])
  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment1(_1, _1.value.asInstanceOf[N1#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment1[N1] if (a._1 eq _1) => statistics(a.value1).score
    case _ => statistics(a(_1)).score
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

/** A Factor with one neighboring variable, whose statistics are simply the value of that neighboring variable. */
trait FactorWithStatistics1[N1<:Variable] extends Factor1[N1] {
  self =>
  type StatisticsType = Statistics // TODO Consider making this <: and defining separate StatisticsReturnType and StatisticsArgumentType
  case class Statistics(_1:N1#Value) extends cc.factorie.Statistics {
    lazy val score = self.score(this)
  }
  override def statistics(v1:N1#Value) = Statistics(v1).asInstanceOf[StatisticsType]
  def score(s:Statistics): Double
}

abstract class FactorWithDotStatistics1[N1<:DiscreteTensorVar] extends FactorWithStatistics1[N1] {
  //type V1 = N1#Value
  //def statisticsDomains: Tuple1[DiscreteTensorDomain]
  def weights: Tensor1 // = new DenseTensor1(statisticsDomains._1.dimensionDomain.size)
  def score(s:Statistics) = s._1.asInstanceOf[Tensor] dot weights // TODO!!!! Why is this cast to Tensor necessary?
  override def scoreValues(valueTensor:Tensor) = valueTensor dot weights
}

trait Family1[N1<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomains = Seq(neighborDomain1)
  type FactorType = Factor

  // For implementing sparsity in belief propagation
  var isLimitingValuesIterator = false
  lazy val limitedDiscreteValues = new scala.collection.mutable.HashSet[Int]
  def addLimitedDiscreteValues(values:Iterable[Int]): Unit = limitedDiscreteValues ++= values
  
  final case class Factor(_1:N1) extends super.Factor with Factor1[N1] {
    type StatisticsType = Family1.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family1.this
    override def statistics(v1:N1#Value): StatisticsType = thisFamily.statistics(v1)
    override def valuesIterator: ValuesIterator1[N1] = Family1.this.valuesIterator(this) 
    //override def isLimitingValuesIterator = Family1.this.isLimitingValuesIterator
    //override def limitedDiscreteValuesIterator: Iterator[Int] = limitedDiscreteValues.iterator
  }
  def statistics(v1:N1#Value): StatisticsType
  def valuesIterator(f:Factor): ValuesIterator1[N1] = throw new Error("Not yet implemented") // TODO Here could be the option to iterate over a subset of values for restricted FSA connectivity
  
  // Cached statistics
//  private var cachedStatisticsArray: Array[StatisticsType] = null
//  override def cachedStatistics(values:ValuesType): StatisticsType =
//    if (Template.enableCachedStatistics) {
//    values._1 match {
//    case v:DiscreteValue => {
//      if (cachedStatisticsArray eq null) cachedStatisticsArray = new Array[Statistics](v.domain.size).asInstanceOf[Array[StatisticsType]]
//      val i = v.intValue
//      if (cachedStatisticsArray(i) eq null) cachedStatisticsArray(i) = values.statistics
//      cachedStatisticsArray(i)
//    }
//    case _ => values.statistics
//  }} else values.statistics
//  /** You must clear cache the cache if DotTemplate.weights change! */
//  override def clearCachedStatistics: Unit =  cachedStatisticsArray = null
}

trait Statistics1[S1] extends Family {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:S1) extends super.Statistics {
    val score = self.score(this) 
  }
  def score(s:Statistics): Double
}

trait TensorStatistics1[S1<:Tensor] extends TensorFamily {
  self =>
  type StatisticsType = Statistics
  // Use Scala's "pre-initialized fields" syntax because super.Statistics needs tensor to initialize score
  final case class Statistics(_1:S1) extends { val tensor: Tensor = _1 } with super.Statistics {
    val score = self.score(this)
  }
  def score(s:Statistics): Double
}

trait DotStatistics1[S1<:Tensor] extends TensorStatistics1[S1] with DotFamily {
  override def weights: Tensor1
  def setWeight(entry:S1, w:Double) = entry match {
    case d:SingletonTensor1 => weights(d.singleIndex) = w // e.g. DiscreteValue
    case ds:Tensor => ds.activeDomain.foreach(i => weights(i) = w)
  }
  def scores1(): Tensor1 = weights.copy //  match { case weights: Tensor1 => weights.copy }
}

trait FamilyWithStatistics1[N1<:Variable] extends Family1[N1] with Statistics1[N1#Value] {
  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
}

trait FamilyWithTensorStatistics1[N1<:TensorVar] extends Family1[N1] with TensorStatistics1[N1#Value] {
  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
}

trait FamilyWithDotStatistics1[N1<:TensorVar] extends Family1[N1] with DotStatistics1[N1#Value] {
  def statistics(v1:N1#Value): StatisticsType = Statistics(v1)
  def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor) // reflecting the fact that there is no transformation between values and statistics
}

