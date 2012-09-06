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

import scala.collection.mutable.HashMap
import cc.factorie.la._

trait ValuesIterator3[N1<:Variable,N2<:Variable,N3<:Variable] extends Iterator[AbstractAssignment3[N1,N2,N3]] with AbstractAssignment3[N1,N2,N3] with ValuesIterator

/** The only abstract things are _1, _2, _3, statistics(Values), and StatisticsType */
trait Factor3[N1<:Variable,N2<:Variable,N3<:Variable] extends Factor {
  factor =>
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
  type StatisticsType <: cc.factorie.Statistics
  def _1: N1
  def _2: N2
  def _3: N3
  def numVariables = 3
  override def variables = IndexedSeq(_1, _2, _3)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case 2 => _3; case _ => throw new IndexOutOfBoundsException(i.toString) }
//  override def values = new Values(_1.value, _2.value, _3.value)
//  case class Values(_1:N1#Value, _2:N2#Value, _3:N3#Value) extends cc.factorie.Values {
//    override def apply[B <: Variable](v: B) = get(v).get
//    def variables = Seq(factor._1, factor._2, factor._3)
//    def get[B <: Variable](v: B) =
//      if(v == factor._1) Some(_1.asInstanceOf[B#Value])
//      else if(v == factor._2) Some(_2.asInstanceOf[B#Value])
//      else if(v == factor._3) Some(_3.asInstanceOf[B#Value])
//      else None
//    def contains(v: Variable) = v == factor._1 || v == factor._2 || v == factor._3
//    override def statistics: StatisticsType = Factor3.this.statistics(this)
//  }
//  def statistics(v:Values): StatisticsType
  def statistics: StatisticsType = statistics(_1.value.asInstanceOf[N1#Value], _2.value.asInstanceOf[N2#Value], _3.value.asInstanceOf[N3#Value])
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType
  // TODO Consider a method like this?  Replaces score(Values)
  def scoreValues(value1:N1#Value, value2:N2#Value, value3:N3#Value): Double = statistics(value1, value2, value3).score
  /** Return a record of the current values of this Factor's neighbors. */
  def currentAssignment = new Assignment3(_1, _1.value.asInstanceOf[N1#Value], _2, _2.value.asInstanceOf[N2#Value], _3, _3.value.asInstanceOf[N3#Value])
  /** The ability to score a Values object is now removed, and this is its closest alternative. */
  def scoreAssignment(a:TypedAssignment[Variable]) = a match {
    case a:AbstractAssignment3[N1,N2,N3] if ((a.var1 eq _1) && (a.var2 eq _2) && (a.var3 eq _3)) => statistics(a.value1, a.value2, a.value3).score
    case _ => statistics(a(_1), a(_2), a(_3)).score
  }

  def valuesIterator: ValuesIterator3[N1,N2,N3] = new ValuesIterator3[N1,N2,N3] {
    def factor = Factor3.this
    var var1: N1 = null.asInstanceOf[N1]
    var var2: N2 = null.asInstanceOf[N2]
    var var3: N3 = null.asInstanceOf[N3]
    var value1: N1#Value = null.asInstanceOf[N1#Value]
    var value2: N2#Value = null.asInstanceOf[N2#Value]
    var value3: N3#Value = null.asInstanceOf[N3#Value]
    def hasNext = false
    def next() = this
    def score: Double = Double.NaN
    def valuesTensor: Tensor = null
  }
  // For implementing sparsity in belief propagation
//  def isLimitingValuesIterator = false
//  def limitedDiscreteValuesIterator: Iterator[(Int,Int,Int)] = Iterator.empty

  /** Given the Tensor value of neighbors _2 and _3, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def scoreValues1(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores1")
  def scoreValues2(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
  def scoreValues3(tensor1:Tensor, tensor2:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")

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

/** The only abstract things are _1, _2, _3, and score(Statistics) */
trait FactorWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable] extends Factor3[N1,N2,N3] {
  self =>
  type StatisticsType = Statistics
  case class Statistics(_1:N1#Value, _2:N2#Value, _3:N3#Value) extends cc.factorie.Statistics {
    // TODO Make this non-lazy later, when _statisticsDomains can be initialized earlier
    lazy val score = self.score(this)
  }
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = new Statistics(v1, v2, v3).asInstanceOf[StatisticsType]
  def score(s:Statistics): Double
}

trait Family3[N1<:Variable,N2<:Variable,N3<:Variable] extends FamilyWithNeighborDomains {
  type NeighborType1 = N1
  type NeighborType2 = N2
  type NeighborType3 = N3
    /** Override this if you want to matchNeighborDomains */
  def neighborDomain1: Domain[N1#Value] = null
  def neighborDomain2: Domain[N2#Value] = null
  def neighborDomain3: Domain[N3#Value] = null
  def neighborDomains = Seq(neighborDomain1, neighborDomain2, neighborDomain3)

  type FactorType = Factor
//  type ValuesType = Factor#Values
  final case class Factor(_1:N1, _2:N2, _3:N3) extends super.Factor with Factor3[N1,N2,N3] {
    type StatisticsType = Family3.this.StatisticsType
    override def equalityPrerequisite: AnyRef = Family3.this
    override def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType = thisFamily.statistics(v1, v2, v3)
//    override def limitedDiscreteValuesIterator: Iterator[(Int,Int,Int)] = limitedDiscreteValues.iterator
//    override def isLimitingValuesIterator = thisFamily.isLimitingValuesIterator
  }
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType

  // For implementing sparsity in belief propagation
//  var isLimitingValuesIterator = false
//  lazy val limitedDiscreteValues = new scala.collection.mutable.HashSet[(Int,Int,Int)]
//  def addLimitedDiscreteValues(values:Iterable[(Int,Int,Int)]): Unit = limitedDiscreteValues ++= values

  // Cached Statistics
//  private var cachedStatisticsArray: Array[StatisticsType] = null
//  private var cachedStatisticsHash: HashMap[Product,StatisticsType] = null
//  /** It is callers responsibility to clearCachedStatistics if weights or other relevant state changes. */
//  override def cachedStatistics(values:Values): StatisticsType =
//    if (Template.enableCachedStatistics) {
//    //println("Template3.cachedStatistics")
//    if (values._1.isInstanceOf[DiscreteValue] && values._2.isInstanceOf[DiscreteValue] && values._3.isInstanceOf[DiscreteTensorValue] /*&& f._3.isConstant*/ ) {
//      val v1 = values._1.asInstanceOf[DiscreteValue]
//      val v2 = values._2.asInstanceOf[DiscreteValue]
//      val v3 = values._3.asInstanceOf[DiscreteTensorValue]
//      if (cachedStatisticsHash eq null) cachedStatisticsHash = new HashMap[Product,StatisticsType]
//      val i = ((v1.intValue, v2.intValue, v3))
//      //print(" "+((v1.intValue, v2.intValue))); if (cachedStatisticsHash.contains(i)) println("*") else println(".")
//      cachedStatisticsHash.getOrElseUpdate(i, values.statistics)
//    } else {
//      values.statistics
//    }
//  } else values.statistics
//  override def clearCachedStatistics: Unit =  { cachedStatisticsArray = null; cachedStatisticsHash = null }
}

trait Statistics3[S1,S2,S3] extends Family {
  self =>
  type StatisticsType = Stat
  final case class Stat(_1:S1, _2:S2, _3:S3) extends super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Stat): Double
}

trait TensorStatistics3[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue] extends TensorFamily {
  self =>
  type StatisticsType = Stat
  override def statisticsDomains: Tuple3[DiscreteTensorDomain with Domain[S1], DiscreteTensorDomain with Domain[S2], DiscreteTensorDomain with Domain[S3]]
  final case class Stat(_1:S1, _2:S2, _3:S3) extends { val tensor: Tensor = Tensor.outer(_1, _2, _3) } with super.Statistics {
    lazy val score = self.score(this)
  }
  def score(s:Stat): Double
}

trait DotStatistics3[S1<:DiscreteTensorValue,S2<:DiscreteTensorValue,S3<:DiscreteTensorValue] extends TensorStatistics3[S1,S2,S3] with DotFamily {
  /** Given the Tensor value of neighbors _2 and _3, return a Tensor1 containing the scores for each possible value neighbor _1, which must be a DiscreteVar.
      Note that the returned Tensor may be sparse if this factor is set up for limited values iteration.
      If _1 is not a DiscreteVar then throws an Error. */
  def scores1(tensor2:Tensor, tensor3:Tensor): Tensor1 = weights match {
    case weights:Tensor3 => {
      val outer = Tensor.outer(tensor2, tensor3)
      val dim = statisticsDomains._1.dimensionDomain.size
      val result = new DenseTensor1(dim)
      outer.foreachActiveElement((j,v) => for (i <- 0 until dim) result(i) += weights(i*dim + j) * v)
      result
    }
  }
  def scores2(tensor1:Tensor, tensor3:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
  def scores3(tensor1:Tensor, tensor2:Tensor): Tensor1 = throw new Error("This Factor type does not implement scores2")
}

trait FamilyWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable] extends Family3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value): StatisticsType = Stat(v1, v2, v3)
}

trait FamilyWithTensorStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar] extends Family3[N1,N2,N3] with TensorStatistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Stat(v1, v2, v3)
}

trait FamilyWithDotStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar] extends Family3[N1,N2,N3] with DotStatistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value) = Stat(v1, v2, v3)
  def scoreValues(tensor:Tensor): Double = scoreStatistics(tensor) // reflecting the fact that there is no transformation between values and statistics
}
