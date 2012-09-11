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

trait ValuesIterator2[N1<:Variable,N2<:Variable] extends Iterator[AbstractAssignment2[N1,N2]] with AbstractAssignment2[N1,N2] with ValuesIterator

/** The only abstract things are _1, _2, statistics(Values), and StatisticsType */
trait Factor2[N1<:Variable,N2<:Variable] extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type StatisticsType <: cc.factorie.Statistics
  def _1: N1
  def _2: N2
  def numVariables = 2
  override def variables = IndexedSeq(_1, _2)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
  val _variables = Seq(factor._1, factor._2)
//  override def values: Values = new Values(_1.value, _2.value)
//  /** Return the score of a value assignment represented by the Tensor argument.  
//      If this Factor's values cannot be represented by a Tensor, throw an Error.
//      This method is overridden and very efficient in FamilyWithDotStatistics classes. */
//  @deprecated("Use scoreValue instead.")
//  override def valueScore(tensor:Tensor): Double = tensor match {
//    case v: SingletonBinaryTensorLike2 => {
//      val domain0 = _1.domain.asInstanceOf[DiscreteDomain with Domain[N1#Value]]
//      val domain1 = _2.domain.asInstanceOf[DiscreteDomain with Domain[N2#Value]]
//      new Values(domain0(v.singleIndex1), domain1(v.singleIndex2)).score
//    }
//    case t: SingletonBinaryLayeredTensor2 => {
//      val domain0 = _1.domain.asInstanceOf[DiscreteDomain with Domain[N1#Value]]
//      new Values(domain0(t.singleIndex1), t.inner.asInstanceOf[N2#Value]).score
//    }
//  }
//  case class Values(_1:N1#Value, _2:N2#Value) extends cc.factorie.Values {
//    override def apply[B <: Variable](v: B) = get(v).get
//    def variables = _variables
//    def get[B <: Variable](v: B) =
//      if(v == factor._1) Some(_1.asInstanceOf[B#Value])
//      else if(v == factor._2) Some(_2.asInstanceOf[B#Value])
//      else None
//    def contains(v: Variable) = v == factor._1 || v == factor._2
//    override def statistics: StatisticsType = Factor2.this.statistics(this)
//  }
//  def statistics(v:Values): StatisticsType
  def statistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value])
  
  // New work, testing ideas for getting rid of Values
  /** Iterate over all value assignments of both neighbors, making available the score for each. 
      Future alternative versions of this method would allow for iterating over restricted subsets. */
  def valuesIterator: ValuesIterator2[N1,N2] = new ValuesIterator2[N1,N2] { //Iterator[AbstractAssignment2[N1,N2]] with AbstractAssignment2[N1,N2]
    def factor: Factor2[N1,N2] = Factor2.this
    var var1: N1 = null.asInstanceOf[N1]
    var var2: N2 = null.asInstanceOf[N2]
    var value1: N1#Value = null.asInstanceOf[N1#Value]
    var value2: N2#Value = null.asInstanceOf[N2#Value]
    def hasNext = false
    def next() = this
    def score: Double = Double.NaN
    def valuesTensor: Tensor = null
  }
  /** This replaces the current statistics(Values), and avoids the awkward stat._1, stat._2 access. */
  def statistics(value1:N1#Value, value2:N2#Value): StatisticsType
  // TODO Consider a method like this?  Replaces score(Values)
  def scoreValues(value1:N1#Value, value2:N2#Value): Double = statistics(value1, value2).score
  /** Given multiplicative factors on values of neighbor _1 (which allow for limited iteration), and given the Tensor value of neighbor _2, 
      return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def scoreValues1(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("Not implemented in Factor "+getClass)
  def scoreValues2(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("Not implemented in Factor "+getClass)

  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment2(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment2[N1,N2] if ((a.var1 eq _1) && (a.var2 eq _2)) => statistics(a.value1, a.value2).score
    case _ => statistics(a(_1), a(_2)).score
  }
  
  
  
  /** Given the Tensor value of neighbor _2, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  @deprecated("Will be removed")
  def valueScores1(tensor2:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores1")
  def valueScores2(tensor1:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")

  // For implementing sparsity in belief propagation
//  def isLimitingValuesIterator = false
//  def limitedDiscreteValuesIterator: Iterator[(Int,Int)] = Iterator.empty

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

/** The only abstract things are _1, _2, and score(Statistics) */
trait FactorWithStatistics2[N1<:Variable,N2<:Variable] extends Factor2[N1,N2] {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:N1#Value, _2:N2#Value) extends cc.factorie.Statistics {
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    lazy val score = self.score(this)
  }
//  def statistics(v:Values) = new Statistics(v._1, v._2).asInstanceOf[StatisticsType]
  def statistics(v1:N1#Value, v2:N2#Value) = new Statistics(v1, v2).asInstanceOf[StatisticsType]
  def score(s:Statistics): Double
  //def score(t:Tensor2): Double = throw new Error("")
}

trait Family2[N1<:Variable,N2<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  def statistics(v1:N1#Value, v2:N2#Value): StatisticsType
    /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomain2: Domain[N2#Value] = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2)
  type FactorType = Factor
//  type ValuesType = Factor#Values

  // For implementing sparsity in belief propagation
//  var isLimitingValuesIterator = false
//  lazy val limitedDiscreteValues = new scala.collection.mutable.HashSet[(Int,Int)]
//  def addLimitedDiscreteValues(values:Iterable[(Int,Int)]): Unit = limitedDiscreteValues ++= values
  //def limitDiscreteValuesIterator
  
  final case class Factor(_1:N1, _2:N2) extends super.Factor with Factor2[N1,N2] {
    type StatisticsType = Family2.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family2.this
    override def statistics(v1:N1#Value, v2:N2#Value): StatisticsType = thisFamily.statistics(v1, v2)
    override def scoreValues(value1:N1#Value, value2:N2#Value): Double = thisFamily.scoreValues(value1, value2)
    override def scoreValues(tensor:Tensor): Double = thisFamily.scoreValues(tensor)
    //override def scoreStatistics(tensor:Tensor): Double = thisFamily.scoreStatistics(tensor)
    
//    override def isLimitingValuesIterator = Family2.this.isLimitingValuesIterator
//    override def limitedDiscreteValuesIterator: Iterator[(Int,Int)] = limitedDiscreteValues.iterator
  }
  def scoreValues(v1:N1#Value, v2:N2#Value): Double = statistics(v1, v2).score
  
  def scoreValues(tensor:Tensor): Double = tensor match {
    case v: SingletonBinaryTensorLike2 => {
      val domain0 = neighborDomain1.asInstanceOf[DiscreteDomain with Domain[N1#Value]] // TODO Yipes.  This is a bit shaky (and inefficient?)
      val domain1 = neighborDomain2.asInstanceOf[DiscreteDomain with Domain[N2#Value]]
      statistics(domain0(v.singleIndex1), domain1(v.singleIndex2)).score
      //statistics(new SingletonBinaryTensor1(v.dim1, v.singleIndex1), new SingletonBinaryTensor1(v.dim2, v.singleIndex2)).score
    }
    case v: SingletonBinaryLayeredTensor2 => {
      val domain0 = neighborDomain1.asInstanceOf[DiscreteDomain with Domain[N1#Value]] // TODO Yipes.  This is a bit shaky (and inefficient?)
      statistics(domain0(v.singleIndex1), v.inner.asInstanceOf[N2#Value]).score
    }
  }

//  // Cached Statistics
//  private var cachedStatisticsArray: Array[StatisticsType] = null
//  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
//  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
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
//        case v2:DiscreteTensorValue if (true /*v2.isConstant*/) => {
//          //println("Template2.cachedStatistics")
//          if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType] { override protected def initialSize = 512 }
//          val i = ((v1.intValue,v2))
//          cachedStatisticsHash.getOrElseUpdate(i, values.statistics)
//        }
//        case _ => values.statistics
//      }
//    }
//    case v1:DiscreteTensorValue if (true /*v1.isConstant*/) => {
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


trait Statistics2[S1,S2] extends Family {
  self =>
  type StatisticsType = Statistics
  final case class Statistics(_1:S1, _2:S2) extends super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Statistics): Double
}

trait TensorStatistics2[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue] extends TensorFamily {
  self =>
  type StatisticsType = Statistics
  override def statisticsDomains: Tuple2[DiscreteTensorDomain with Domain[S1], DiscreteTensorDomain with Domain[S2]]
  final case class Statistics(_1:S1, _2:S2) extends { val tensor: Tensor = Tensor.outer(_1, _2) } with super.Statistics {
    lazy val score = self.score(this)
  }
}

trait DotStatistics2[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue] extends TensorStatistics2[S1,S2] with DotFamily {
  //def statisticsScore(tensor:Tensor) = weights dot tensor
}

trait FamilyWithStatistics2[N1<:Variable,N2<:Variable] extends Family2[N1,N2] with Statistics2[N1#Value,N2#Value] {
//  def statistics(values:Values) = Stat(values._1, values._2)
  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
}

trait FamilyWithTensorStatistics2[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar] extends Family2[N1,N2] with TensorStatistics2[N1#Value,N2#Value] {
//  def statistics(values:Values) = Stat(values._1, values._2)
  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
}

trait FamilyWithDotStatistics2[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar] extends Family2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
//  def statistics(values:Values) = Stat(values._1, values._2)
  def statistics(v1:N1#Value, v2:N2#Value) = Statistics(v1, v2)
  override def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor)
  // TODO Consider a more efficient implementation of some cases
  // TODO Should we consider the capability for something other than *summing* over elements of tensor2?
  def valueScores1(tensor2:Tensor): Tensor1 = weights match {
    case weights: Tensor2 => {
      val dim = statisticsDomains._1.dimensionDomain.size
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
  def valueScores2(tensor1:Tensor): Tensor1 = weights match {
    case weights: Tensor2 => {
      val dim = statisticsDomains._2.dimensionDomain.size
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

