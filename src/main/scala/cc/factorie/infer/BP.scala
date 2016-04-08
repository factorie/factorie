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

package cc.factorie.infer

import cc.factorie.la._
import cc.factorie.maths
import cc.factorie.model._
import cc.factorie.util.{DoubleSeq, RangeIntSeq, SparseDoubleSeq}
import cc.factorie.variable._

import scala.collection.{Set, mutable}
import scala.collection.mutable.ArrayBuffer

trait BPRing {
  def sum(a: Double, b: Double): Double
}

object BPSumProductRing extends BPRing {
  def sum(a: Double, b: Double) = maths.sumLogProb(a, b)
}

object BPMaxProductRing extends BPRing {
  def sum(a: Double, b: Double) = math.max(a, b)
}

object BPFactorFactory {
  def newBPVariable(v: DiscreteVar) = new BPVariable1(v)
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary) = {
    if (varying == null) sys.error("Can't call newBPFactor with null list of varying variables.")
    factor match {
      case factor:Factor1[DiscreteVar @unchecked] =>
        new BPFactor1Factor1(factor, new BPEdge(summary.bpVariable(factor._1)), summary)
      case factor:Factor2[DiscreteVar @unchecked,DiscreteVar @unchecked] =>
        if (varying.contains(factor._1) && varying.contains(factor._2)) new BPFactor2Factor2(factor, new BPEdge(summary.bpVariable(factor._1)), new BPEdge(summary.bpVariable(factor._2)), summary, summary.ring)
        else if (varying.contains(factor._1)) new BPFactor1Factor2(factor.asInstanceOf[Factor2[DiscreteVar,VectorVar]], new BPEdge(summary.bpVariable(factor._1)), summary)
        else if (factor._1.isInstanceOf[DiscreteVar]) new BPFactor1Factor2(factor.asInstanceOf[Factor2[DiscreteVar,VectorVar]], new BPEdge(summary.bpVariable(factor._2)), summary)
        else new BPFactor1Factor2Left(factor.asInstanceOf[Factor2[VectorVar,DiscreteVar]], new BPEdge(summary.bpVariable(factor._2)), summary)
      case factor:Factor3[VectorVar @unchecked,VectorVar @unchecked,VectorVar @unchecked] =>
        val neighbors = factor.variables.filter(v => v.isInstanceOf[DiscreteVar] && varying.contains(v.asInstanceOf[DiscreteVar]))
        if (neighbors.size == 2)
          new BPFactor2Factor3(factor.asInstanceOf[Factor3[DiscreteVar,DiscreteVar,VectorVar]], neighbors.map(v => new BPEdge(summary.bpVariable(v.asInstanceOf[DiscreteVar]))), summary, summary.ring)
        else if (neighbors.size == 1)
          new BPFactor1Factor3(factor, new BPEdge(summary.bpVariable(neighbors.head.asInstanceOf[DiscreteVar])), summary)
        else if (neighbors.size == 3)
          new BPFactor3Factor3(factor.asInstanceOf[Factor3[DiscreteVar, DiscreteVar, DiscreteVar]], neighbors.map(v => new BPEdge(summary.bpVariable(v.asInstanceOf[DiscreteVar]))), summary, summary.ring)
        else throw new Error("Can't create the factor")
      case factor: Factor4[DiscreteVar @unchecked, DiscreteVar @unchecked, DiscreteVar @unchecked, DiscreteVar @unchecked] =>
        val neighbors = factor.variables.filter(v => v.isInstanceOf[DiscreteVar] && varying.contains(v.asInstanceOf[DiscreteVar]))
        assert(neighbors.size == 4)
        val edges = neighbors.map(v => new BPEdge(summary.bpVariable(v.asInstanceOf[DiscreteVar])))
        new BPFactor4Factor4(factor, edges, summary, summary.ring)
    }
  }
}


/** A dumb container for messages factor->variable and variable->factor */
class BPEdge(val bpVariable: BPVariable1) {
  // TODO Eventually we should not require that this is a BPVariable1, but work for general BPVariable
  bpVariable.addEdge(this)
  var bpFactor: BPFactor = null
  // Note:  For Bethe cluster graphs with BPVariable1, these messages will be Tensor1, but for other cluster graphs they could have higher dimensionality
  var messageFromVariable: Tensor = new UniformTensor1(bpVariable.variable.domain.size, 0.0)
  var messageFromFactor: Tensor = new UniformTensor1(bpVariable.variable.domain.size, 0.0)
  def variable = bpVariable.variable
  def factor = bpFactor.factor
}


trait BPVariable {
  def edges: Seq[BPEdge]
  def calculateOutgoing(e:BPEdge): Tensor
  def updateOutgoing(e:BPEdge): Unit = e.messageFromVariable = calculateOutgoing(e)
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing)
  def calculateBelief: Tensor
  def betheObjective: Double = {
    var bethe = 0.0
    calculateMarginal match {
      case t: DenseTensor =>
        var i = 0
        val arr = t.asArray
        while (i < arr.length) {
          bethe += arr(i)*math.log(arr(i))
          i += 1
        }
    }
    bethe * (edges.length-1)
  }
  def calculateMarginal: Tensor = { val t = calculateBelief; t.expNormalize(); t }
}
class BPVariable1(val _1: DiscreteVar) extends DiscreteMarginal1[DiscreteVar] with BPVariable {
  val variable = _1
  private var _edges: List[BPEdge] = Nil
  def addEdge(e:BPEdge): Unit = _edges = e :: _edges
  final def edges: List[BPEdge] = _edges
  def calculateOutgoing(e:BPEdge): Tensor = {
    edges.size match {
      case 0 => throw new Error("BPVariable1 with no edges")
      case 1 => { require(edges.head == e); new UniformTensor1(variable.domain.size, 0.0) }
      case 2 => if (edges.head == e) edges.last.messageFromFactor else if (edges.last == e) edges.head.messageFromFactor else throw new Error
      case _ => new DenseTensor1(variable.domain.size) ++= edges.filter(_ != e).map(_.messageFromFactor)
    }
  }
  def calculateBelief: Tensor1 = new DenseTensor1(variable.domain.size) ++= edges.map(_.messageFromFactor) // TODO Make this more efficient for cases of 1, 2, 3 edges?
  def proportions: Proportions1 = new DenseTensorProportions1(calculateMarginal.asInstanceOf[Tensor1].asArray)  // TODO Think about avoiding re-calc every time
  override def value1: DiscreteVar#Value = variable.domain.dimensionDomain(calculateBelief.maxIndex).asInstanceOf[DiscreteVar#Value] // TODO Ug.  This casting is rather sad.  // To avoid normalization compute time
  def globalize(implicit d:DiffList): Unit = variable match { case v:MutableDiscreteVar => v.set(calculateBelief.maxIndex)(d) }  // To avoid normalization compute time
  override def setToMaximize(implicit d: DiffList=null) = variable.asInstanceOf[MutableDiscreteVar].set(calculateBelief.maxIndex)
  def updateOutgoingMAP(maxValue: Int) {
    for (e <- edges) {
      e.messageFromVariable = new Tensor1 with SparseDoubleSeq with ReadOnlyTensor {
        def isDense = false
        def activeDomain = new RangeIntSeq(0, variable.domain.size)
        def activeDomainSize = variable.domain.size
        def dot(ds: DoubleSeq) = throw new Error("can't dot this marginal")
        def dim1 = activeDomainSize
        def apply(i: Int) = if (i == maxValue) 0 else Double.NegativeInfinity
        def foreachActiveElement(f: (Int, Double) => Unit) = foreachElement(f)
      }
      e.bpFactor.updateOutgoing()
    }
  }
}


trait BPFactor extends FactorMarginal {
  def factor: Factor
  def edges: Seq[BPEdge]
  def summary: BPSummary
  /** Re-calculate the message from this factor to edge "e" and set e.messageFromFactor to the result. */
  def updateOutgoing(e: BPEdge): Unit
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing)
  def scores: Tensor // All local scores across all dimensions of varying neighbors; does not use messages from variables
  /** Unnormalized log scores over values of varying neighbors */
  def calculateBeliefsTensor: Tensor
  /** Normalized probabilities over values of only the varying neighbors, in the form of a Proportions */
  def betheObjective = calculateMarginalTensor match {
      case t:DenseTensor => {
        var z = 0.0
        val l = t.length
        var i = 0
        while (i < l) {
          if (t(i) > 0)
            z += t(i) * (-math.log(t(i)) + scores(i))
          i += 1
        }
        z
      }
      case t:SparseIndexedTensor => {
        var z = Double.NegativeInfinity
        t.foreachActiveElement((i,v) => {
          z = v * (math.log(z) + scores(i))
        })
        z
      }
    }
  /** Normalized probabilities over values of varying neighbors */
  def calculateMarginalTensor: Tensor = {
    val v = calculateBeliefsTensor
    summary.expNormalize(v)
    v
  }
  /** The logSum of all entries in the beliefs tensor */
  def calculateLogZ: Double = calculateBeliefsTensor match {
    case t:DenseTensor => { var z = Double.NegativeInfinity; val l = t.length; var i = 0; while (i < l) { z = maths.sumLogProb(z, t(i)); i += 1 }; z }
    case t:SparseIndexedTensor => { var z = Double.NegativeInfinity; t.foreachActiveElement((i,v) => { z = maths.sumLogProb(z, v) }); z }
  }
}

// An abstract class for BPFactors that has 1 varying neighbor.  They may have additional constant neighbors.
abstract class BPFactor1(val edge1: BPEdge, val summary: BPSummary) extends SimpleDiscreteMarginal1(edge1.bpVariable.variable, null) with BPFactor {
  override def scores: Tensor1
  def hasLimitedDiscreteValues1: Boolean
  def limitedDiscreteValues1: SparseBinaryTensor1
  edge1.bpFactor = this
  val edges = Seq(edge1)
  def updateOutgoing(e: BPEdge): Unit = e match { case this.edge1 => updateOutgoing1() }
  override def updateOutgoing(): Unit = updateOutgoing1()
  def updateOutgoing1(): Unit = edge1.messageFromFactor = calculateOutgoing1
  // TODO See about caching this when possible
  def calculateBeliefsTensor: Tensor1 = (scores + edge1.messageFromVariable).asInstanceOf[Tensor1]
  def calculateOutgoing1: Tensor1 = scores
  override def proportions: Proportions1 = new DenseTensorProportions1(calculateMarginalTensor.asArray, false)
}


// A BPFactor1 with underlying model Factor1, with the one neighbor varying
class BPFactor1Factor1(val factor: Factor1[DiscreteVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1[DiscreteVar] with DiscreteMarginal1Factor1[DiscreteVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = factor match {
    case factor:DotFamily#Factor if factor.family.isInstanceOf[DotFamily] => factor.family.weights.value.asInstanceOf[Tensor1]
    case _ => {
      val valueTensor = new SingletonBinaryTensor1(edge1.variable.domain.size, 0)
      val result = new DenseTensor1(edge1.variable.domain.size)
      val len = edge1.variable.domain.size; var i = 0; while (i < len) {
        valueTensor.singleIndex = i
        result(i) = factor.valuesScore(valueTensor)
        i += 1
      }
      result
    }
  }
}

// A BPFactor1 with underlying model Factor2, with the first neighbor varying and the second neighbor constant 
class BPFactor1Factor2(val factor: Factor2[DiscreteVar,VectorVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1[DiscreteVar] with DiscreteMarginal1Factor2[DiscreteVar,VectorVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = {
    val valueTensor = new SingletonBinaryLayeredTensor2(edge1.variable.domain.size, factor._2.domain.dimensionDomain.size, 0, factor._2.value)
    val len = edge1.variable.domain.size
    val result = new DenseTensor1(len)
    var i = 0; while (i < len) {
      valueTensor.singleIndex1 = i
      result(i) = factor.valuesScore(valueTensor)
      i += 1
    }
    result
  }
}

class BPFactor1Factor2Left(val factor: Factor2[VectorVar,DiscreteVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1[DiscreteVar] with DiscreteMarginal1Factor2Other[VectorVar,DiscreteVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = {
    factor.asInstanceOf[DotFamily#Factor].family.weights.value.asInstanceOf[Tensor2].leftMultiply(factor.variables.head.value.asInstanceOf[Tensor1])
  }
}

class BPFactor1Factor3(val factor: Factor3[VectorVar,VectorVar,VectorVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1Factor3[VectorVar,VectorVar,VectorVar,DiscreteVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = {
    val v = edge1.variable
    val result = new DenseTensor1(v.domain.dimensionSize)
    var i = 0
    while (i < v.domain.dimensionSize) {
      result(i) = factor.assignmentScore(new Assignment1[DiscreteVar](v, v.domain(i).asInstanceOf[v.Value]))
      i += 1
    }
    result
  }
}


// An abstract class for BPFactors that have 2 varying neighbors.  They may have additional constant neighbors.
abstract class BPFactor2(val edge1: BPEdge, val edge2: BPEdge, val summary: BPSummary, final val ring: BPRing) extends DiscreteMarginal2(edge1.bpVariable.variable, edge2.bpVariable.variable, null) with BPFactor {
  lazy val edge1Max2 = new Array[Int](edge1.variable.domain.size) // The index value of edge2.variable that lead to the MaxProduct value for each index value of edge1.variable
  lazy val edge2Max1 = new Array[Int](edge2.variable.domain.size)
  override def scores: Tensor2
  def hasLimitedDiscreteValues12: Boolean
  def limitedDiscreteValues12: SparseBinaryTensor2
  edge1.bpFactor = this
  edge2.bpFactor = this
  val edges = Seq(edge1, edge2)
  override def updateOutgoing(): Unit = { updateOutgoing1(); updateOutgoing2() }
  def updateOutgoing(e: BPEdge): Unit = e match { case this.edge1 => updateOutgoing1(); case this.edge2 => updateOutgoing2() }
  def updateOutgoing1(): Unit = edge1.messageFromFactor = calculateOutgoing1
  def updateOutgoing2(): Unit = edge2.messageFromFactor = calculateOutgoing2
  // TODO See about caching this when possible
  def calculateBeliefsTensor: Tensor2 = {
    val result = new DenseTensor2(edge1.messageFromVariable.length, edge2.messageFromVariable.length)
    val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
    while (j < lenj) {
      i = 0; while (i < leni) {
        result(i,j) = scores(i,j) + edge1.messageFromVariable(i) + edge2.messageFromVariable(j)
        i += 1
      }
      j += 1
    }
    result
  }
  def calculateOutgoing1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size, Double.NegativeInfinity)
    if (hasLimitedDiscreteValues12) {
      val indices = limitedDiscreteValues12._indices.toSet
      val len = edge1.variable.domain.size * edge2.variable.domain.size;  /* require(len > 0, "limitedDiscreteValues12 can't everything"); */ var ii = 0
      while (ii < len) {
        val i = scores.index1(ii)
        val j = scores.index2(ii)
        val s = if (indices.contains(ii)) scores(i,j) + edge2.messageFromVariable(j) else edge2.messageFromVariable(j)
        if ((ring eq BPMaxProductRing) && (s > result(i))) { edge1Max2(i) = j }
        result(i) = ring.sum(result(i), s)
        ii += 1
      }
    } else {
      val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
      while (i < leni) {
        j = 0; while (j < lenj) {
          val s = scores(i,j) + edge2.messageFromVariable(j)
          if ((ring eq BPMaxProductRing) && (s > result(i))) { edge1Max2(i) = j }
          result(i) = ring.sum(result(i), s)
          j += 1
        }
        i += 1
      }
    }
    result
  }
  def calculateOutgoing2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size, Double.NegativeInfinity)
    if (hasLimitedDiscreteValues12) {
      val indices = limitedDiscreteValues12._indices.toSet
      val len = edge1.variable.domain.size * edge2.variable.domain.size
      var ii = 0
      while (ii < len) {
        val i = scores.index1(ii)
        val j = scores.index2(ii)
        val s = edge1.messageFromVariable(i) + (if (indices.contains(ii)) scores(i,j) else 0)
        if ((ring eq BPMaxProductRing) && (s > result(j))) { edge2Max1(j) = i }
        result(j) = ring.sum(result(j), s)
        ii += 1
      }
    } else {
      val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
      while (j < lenj) {
        i = 0; while (i < leni) {
          val s = scores(i,j) + edge1.messageFromVariable(i)
          if ((ring eq BPMaxProductRing) && (s > result(j))) { edge2Max1(j) = i }
          result(j) = ring.sum(result(j), s)
          i += 1
        }
        j += 1
      }
    }
    result
  }
  override def proportions: Proportions2 = new DenseTensorProportions2(calculateMarginalTensor.asArray, edge1.variable.domain.dimensionSize, edge2.variable.domain.dimensionSize, false)
}

// A BPFactor2 with underlying model Factor2, with both neighbors varying
class BPFactor2Factor2(val factor:Factor2[DiscreteVar,DiscreteVar], edge1:BPEdge, edge2:BPEdge, sum: BPSummary, ring: BPRing) extends BPFactor2(edge1, edge2, sum, ring) with DiscreteMarginal2Factor2[DiscreteVar,DiscreteVar] {
  // TODO Consider making this calculate scores(i,j) on demand, with something like
  // val scores = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size, Double.NaN) { override def apply(i:Int) = if (_values(i).isNaN)... }
  val hasLimitedDiscreteValues12: Boolean = factor.hasLimitedDiscreteValues12
  def limitedDiscreteValues12: SparseBinaryTensor2 = factor.limitedDiscreteValues12
  val scores: Tensor2 = factor match {
    // This only works if statistics has not been overridden.  Hence factor.statisticsAreValues.  (See TestBP.loop2 for an example where this fails.)
    // TODO Try to be more efficient even when statisticsAreValues is false. -akm
    case factor:DotFamily#Factor if factor.family.isInstanceOf[DotFamily] && factor.statisticsAreValues && factor.family.weights.value.isInstanceOf[Tensor2] => {
      assert(factor.family.weights.value.isInstanceOf[Tensor2], "Expected Tensor2, got "+factor.family.weights.value.getClass+" from family "+factor.family.getClass)
      factor.family.weights.value.asInstanceOf[Tensor2]
    }
    case _ => {
      // TODO Replace this with just efficiently getting factor.family.weightsSet
      val valueTensor = new SingletonBinaryTensor2(edge1.variable.domain.size, edge2.variable.domain.size, 0, 0)
      val result = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size)
      val leni = edge1.variable.domain.size; val lenj = edge2.variable.domain.size; var i = 0; var j = 0
      while (i < leni) {
        valueTensor.singleIndex1 = i
        j = 0
        while (j < lenj) {
          valueTensor.singleIndex2 = j
          if (hasLimitedDiscreteValues12 && !factor.limitedDiscreteValues12.contains(result.singleIndex(i,j))) result(i, j) = Double.NegativeInfinity
          else result(i, j) = factor.valuesScore(valueTensor)
          j += 1
        }
        i += 1
      }
      result
    }
  }
}

// A BPFactor2 with underlying model Factor3, having two varying neighbors and one constant neighbor
// Note that the varying neighbors are assumed to be factor._1 and factor._2, and the constant neighbor factor._3
class BPFactor2Factor3(val factor:Factor3[DiscreteVar,DiscreteVar,VectorVar], edges: Seq[BPEdge], sum: BPSummary, ring: BPRing) extends BPFactor2(edges(0), edges(1), sum, ring) with DiscreteMarginal2Factor3[DiscreteVar, DiscreteVar, VectorVar] {
  val hasLimitedDiscreteValues12: Boolean = factor.hasLimitedDiscreteValues12
  def limitedDiscreteValues12: SparseBinaryTensor2 = factor.limitedDiscreteValues12
  val scores: Tensor2 = {
    val valueTensor = new Singleton2LayeredTensor3(edge1.variable.domain.size, edge2.variable.domain.size, factor._3.domain.dimensionDomain.size, 0, 0, 1.0, 1.0, factor._3.value)
    val result = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size)
    val leni = edge1.variable.domain.size; val lenj = edge2.variable.domain.size; var i = 0; var j = 0
    while (i < leni) {
      valueTensor.singleIndex1 = i
      j = 0
      while (j < lenj) {
        valueTensor.singleIndex2 = j
        if (hasLimitedDiscreteValues12 && !factor.limitedDiscreteValues12.contains(result.singleIndex(i,j))) result(i, j) = Double.NegativeInfinity
        else result(i, j) = factor.valuesScore(valueTensor)
        j += 1
      }
      i += 1
    }
    result
  }
}

class BPFactor3Factor3(val factor: Factor3[DiscreteVar, DiscreteVar, DiscreteVar], val edges: Seq[BPEdge], val summary: BPSummary, final val ring: BPRing) extends DiscreteMarginal3[DiscreteVar, DiscreteVar, DiscreteVar](factor._1, factor._2, factor._3) with BPFactor with DiscreteMarginal3Factor3[DiscreteVar, DiscreteVar, DiscreteVar] {
  def scores = factor.asInstanceOf[DotFamily3[DiscreteVar, DiscreteVar, DiscreteVar]#Factor].family.weights.value.asInstanceOf[Tensor3]
  val Seq(edge1, edge2, edge3) = edges
  edges.foreach(e => e.bpFactor = this)
  val v1 = edge1.variable
  val v2 = edge2.variable
  val v3 = edge3.variable
  val d1 = v1.domain
  val d2 = v2.domain
  val d3 = v3.domain
  /** Unnormalized log scores over values of varying neighbors */
  def calculateBeliefsTensor = {
    val beliefs = new DenseTensor3(d1.size, d2.size, d3.size)
    for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size) {
      beliefs(i, j, k) = scores(i, j, k) + edge1.messageFromVariable(i) + edge2.messageFromVariable(j) + edge3.messageFromVariable(k)
    }
    beliefs
  }
  def updateOutgoing(e: BPEdge) {
    if (e eq edge1) {
      val newMessage = new DenseTensor1(d1.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size) {
        newMessage(i) = ring.sum(newMessage(i), edge2.messageFromVariable(j) + edge3.messageFromVariable(k) + scores(i, j, k))
      }
      e.messageFromFactor = newMessage
    } else if (e eq edge2) {
      val newMessage = new DenseTensor1(d2.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size) {
        newMessage(j) = ring.sum(newMessage(j), edge1.messageFromVariable(i) + edge3.messageFromVariable(k) + scores(i, j, k))
      }
      e.messageFromFactor = newMessage
    } else if (e eq edge3) {
      val newMessage = new DenseTensor1(d3.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size) {
        newMessage(k) = ring.sum(newMessage(k), edge1.messageFromVariable(i) + edge2.messageFromVariable(j) + scores(i, j, k))
      }
      e.messageFromFactor = newMessage
    } else { throw new Error("Can't send messages through edge not in the factor.")}
  }
  override def proportions: Proportions3 = new DenseTensorProportions3(calculateMarginalTensor.asArray, factor._1.domain.dimensionSize, factor._2.domain.dimensionSize, factor._3.domain.dimensionSize, false)
}



class BPFactor4Factor4(val factor: Factor4[DiscreteVar, DiscreteVar, DiscreteVar, DiscreteVar], val edges: Seq[BPEdge], val summary: BPSummary, final val ring: BPRing) extends DiscreteMarginal4[DiscreteVar, DiscreteVar, DiscreteVar, DiscreteVar](factor._1, factor._2, factor._3, factor._4) with BPFactor with DiscreteMarginal4Factor4[DiscreteVar, DiscreteVar, DiscreteVar, DiscreteVar] {
  def scores = factor.asInstanceOf[DotFamily4[DiscreteVar, DiscreteVar, DiscreteVar, DiscreteVar]#Factor].family.weights.value.asInstanceOf[Tensor4]
  val Seq(edge1, edge2, edge3, edge4) = edges
  edges.foreach(e => e.bpFactor = this)
  val v1 = edge1.variable
  val v2 = edge2.variable
  val v3 = edge3.variable
  val v4 = edge4.variable
  val d1 = v1.domain
  val d2 = v2.domain
  val d3 = v3.domain
  val d4 = v4.domain
  /** Unnormalized log scores over values of varying neighbors */
  def calculateBeliefsTensor = {
    val beliefs = new DenseTensor4(d1.size, d2.size, d3.size, d4.size)
    for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size; l <- 0 until d4.size) {
      beliefs(i, j, k, l) = scores(i, j, k, l) + edge1.messageFromVariable(i) + edge2.messageFromVariable(j) + edge3.messageFromVariable(k) + edge4.messageFromVariable(l)
    }
    beliefs
  }
  def updateOutgoing(e: BPEdge) {
    if (e eq edge1) {
      val newMessage = new DenseTensor1(d1.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size; l <- 0 until d4.size) {
        newMessage(i) = ring.sum(newMessage(i), edge2.messageFromVariable(j) + edge3.messageFromVariable(k) + edge4.messageFromVariable(l) + scores(i, j, k, l))
      }
      e.messageFromFactor = newMessage
    } else if (e eq edge2) {
      val newMessage = new DenseTensor1(d2.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size; l <- 0 until d4.size) {
        newMessage(j) = ring.sum(newMessage(j), edge1.messageFromVariable(i) + edge3.messageFromVariable(k) + edge4.messageFromVariable(l) + scores(i, j, k, l))
      }
      e.messageFromFactor = newMessage
    } else if (e eq edge3) {
      val newMessage = new DenseTensor1(d2.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size; l <- 0 until d4.size) {
        newMessage(k) = ring.sum(newMessage(k), edge2.messageFromVariable(j) + edge1.messageFromVariable(i) + edge4.messageFromVariable(l) + scores(i, j, k, l))
      }
      e.messageFromFactor = newMessage
    } else if (e eq edge4) {
      val newMessage = new DenseTensor1(d4.size, Double.NegativeInfinity)
      for (i <- 0 until d1.size; j <- 0 until d2.size; k <- 0 until d3.size; l <- 0 until d4.size) {
        newMessage(l) = ring.sum(newMessage(l), edge2.messageFromVariable(j) + edge3.messageFromVariable(k) + edge1.messageFromVariable(i) + scores(i, j, k, l))
      }
      e.messageFromFactor = newMessage
    } else { throw new Error("Can't send messages through edge not in the factor.")}
  }
  override def proportions: Proportions4 = new DenseTensorProportions4(calculateMarginalTensor.asArray, factor._1.domain.dimensionSize, factor._2.domain.dimensionSize, factor._3.domain.dimensionSize, factor._4.domain.dimensionSize, false)
}

object BPSummary {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
    val summary = new BPSummary(ring)
    val varyingSet = varying.toSet
    val factors = model.factors(varying)
    for (factor <- factors) summary._bpFactors(factor) = BPFactorFactory.newBPFactor(factor, varyingSet, summary)
    summary
  }

  def apply(varying:Iterable[DiscreteVar], model:Model): BPSummary = apply(varying, BPSumProductRing, model)
}

object LoopyBPSummary {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
      val summary = new LoopyBPSummary(ring)
      val varyingSet = varying.toSet
      for (factor <- model.factors(varying)) summary._bpFactors(factor) = BPFactorFactory.newBPFactor(factor, varyingSet, summary)
      summary
    }
}

object LoopyBPSummaryMaxProduct {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
      val summary = new LoopyBPSummaryMaxProduct(ring)
      val varyingSet = varying.toSet
      for (factor <- model.factors(varying)) summary._bpFactors(factor) = BPFactorFactory.newBPFactor(factor, varyingSet, summary)
      summary
    }
}


/** A collection of marginals inferred by belief propagation.  
    Do not call this constructor directly; instead use the companion object apply methods, 
    which add the appropriate BPFactors, BPVariables and BPEdges. */
class BPSummary(val ring:BPRing) extends Summary {
  protected val _bpFactors = new mutable.LinkedHashMap[Factor, BPFactor]
  protected val _bpVariables = new mutable.LinkedHashMap[VectorVar, BPVariable1]
  def bpVariable(v:DiscreteVar): BPVariable1 = _bpVariables.getOrElseUpdate(v, BPFactorFactory.newBPVariable(v))
  def bpFactors: Iterable[BPFactor] = _bpFactors.values
  def factorMarginals = factors.head.map(marginal)
  def factors: Option[Iterable[Factor]] = Some(_bpFactors.values.map(_.factor))
  def bpVariables: Iterable[BPVariable1] = _bpVariables.values
  override def marginals: Iterable[DiscreteMarginal1[DiscreteVar]] = _bpVariables.values // ++ _bpVariables.values
  override def marginal(v:Var): BPVariable1 = v match { case v:DiscreteVar => _bpVariables(v); case _ => null }
  def marginal(v: DiscreteVar): BPVariable1 = _bpVariables(v)
  def marginal(f: Factor): BPFactor = _bpFactors(f)
  var _logZ = Double.NaN
  override def logZ: Double = {
    if (_logZ != _logZ) { _logZ = if (_bpFactors.size > 0) _bpFactors.values.head.calculateLogZ else 0 }
    _logZ
  }
  
  //def setToMaximizeMarginals(implicit d:DiffList = null): Unit = bpVariables.foreach(_.setToMaximize(d))
  override def setToMaximize(implicit d:DiffList = null): Unit = ring match {
    case BPSumProductRing => bpVariables.foreach(_.setToMaximize(d))
    case BPMaxProductRing => bpVariables.foreach(v => {
      v.setToMaximize(d)
      v.updateOutgoingMAP(v.variable.value.intValue)
    })
    case _ => throw new Error("Not yet implemented arbitrary backwards pass.")
  }
  def maximizingAssignment: Assignment = ring match {
    case BPMaxProductRing =>
      val assignment = new HashMapAssignment(true)
      bpVariables.foreach(v => {
        val value = v.calculateBelief.maxIndex
        assignment.update(v.variable, v.variable.domain(value).asInstanceOf[DiscreteVar#Value])
        v.updateOutgoingMAP(value.intValue)
    })
      assignment
    case _ => throw new Error("Not yet implemented arbitrary backwards pass.")
  }
  def expNormalize(t: Tensor) {
    t += -logZ
    t.exponentiate()
  }
}

class LoopyBPSummary(val rng: BPRing) extends BPSummary(rng) {
  override def logZ = _bpFactors.values.map(_.betheObjective).sum + _bpVariables.values.map(_.betheObjective).sum
  override def expNormalize(t: Tensor) { t.expNormalize() }
}

class LoopyBPSummaryMaxProduct(val rng: BPRing) extends BPSummary(rng) {
  override def logZ = _bpFactors.values.map(f => f.calculateMarginalTensor.dot(f.scores)).sum
  override def expNormalize(t: Tensor) { t.expNormalize() }
}

object BPUtil {
  
  def bfs(varying: Set[DiscreteVar], root: BPVariable, checkLoops: Boolean): Seq[(BPEdge, Boolean)] = {
    val visited = new mutable.HashSet[BPEdge]
    val result = new ArrayBuffer[(BPEdge, Boolean)]
    val toProcess = new mutable.Queue[(BPEdge, Boolean)]
    val visitedVariables = new mutable.HashSet[DiscreteVar]
    root.edges foreach (e => toProcess += e -> true)
    while (!toProcess.isEmpty) {
      val (edge, v2f) = toProcess.dequeue()
      if (!checkLoops || !visited(edge)) {
        visited += edge
        visitedVariables += edge.variable
        result += edge -> v2f
        val edges =
          if (v2f) edge.bpFactor.edges.filter(_ != edge)
          else {
            if (varying.contains(edge.bpVariable.variable))
              edge.bpVariable.edges.filter(_ != edge) 
            else Seq.empty[BPEdge]
          }
        edges.foreach(ne => toProcess += ne -> !v2f)
      }
    }
    require(varying.forall(visitedVariables.contains), "Treewise BP assumes the graph is connected")
    result
  }

  def loopyBfs(varying: Set[DiscreteVar], summary: BPSummary): Seq[(BPEdge, Boolean)] = {
    val visited = new mutable.HashSet[BPEdge]
    val result = new ArrayBuffer[(BPEdge, Boolean)]
    val toProcess = new mutable.Queue[(BPEdge, Boolean)]
    val visitedVariables = mutable.HashSet[DiscreteVar]()
    while (!varying.forall(visitedVariables.contains)) {
      val root = summary.bpVariable(varying.collectFirst({case v if !visitedVariables.contains(v) => v}).head)
      root.edges foreach (e => toProcess += e -> true)
      while (!toProcess.isEmpty) {
        val (edge, v2f) = toProcess.dequeue()
        if (!visited(edge)) {
          visited += edge
          result += edge -> v2f
          visitedVariables += edge.bpVariable.variable
          val edges =
            if (v2f) edge.bpFactor.edges.filter(_ != edge)
            else {
              if (varying.contains(edge.bpVariable.variable))
                edge.bpVariable.edges.filter(_ != edge)
              else Seq.empty[BPEdge]
            }
          edges.foreach(ne => toProcess += ne -> !v2f)
        }
      }
    }
    result
  }

  def sendAccordingToOrdering(edgeSeq: Seq[(BPEdge, Boolean)]) {
    for ((e, v2f) <- edgeSeq) {
      if (v2f) {
        e.bpVariable.updateOutgoing(e)
        e.bpFactor.updateOutgoing(e)
      }
      else {
        e.bpFactor.updateOutgoing(e)
        e.bpVariable.updateOutgoing(e)
      }
    }
  }
  
}

object BP {
  def inferLoopy(summary: BPSummary, numIterations: Int = 10): Unit = {
    for (iter <- 0 to numIterations) { // TODO Make a more clever convergence detection
      for (bpf <- summary.bpFactors) {
        for (e <- bpf.edges) e.bpVariable.updateOutgoing(e)  // get all the incoming messages
        for (e <- bpf.edges) e.bpFactor.updateOutgoing(e)    // send messages
      }
    }
  }

  def inferLoopyMax(summary: BPSummary, numIterations: Int = 10): Unit = {
    for (iter <- 0 to numIterations) { // TODO Make a more clever convergence detection
      for (bpf <- summary.bpFactors) {
        for (e <- bpf.edges) e.bpVariable.updateOutgoing(e)  // get all the incoming messages
        for (e <- bpf.edges) e.bpFactor.updateOutgoing(e)    // send messages
      }
    }
    val assignment = summary.maximizingAssignment
    new MAPSummary(assignment, summary.factors.get.toVector)
  }

  def inferLoopyTreewise(varying: Iterable[DiscreteVar], model: Model, numIterations: Int = 2) = {
    val summary = LoopyBPSummary(varying, BPSumProductRing, model)
    val bfsSeq = BPUtil.loopyBfs(varying.toSet, summary)
    for (i <- 0 to numIterations) {
      BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
      BPUtil.sendAccordingToOrdering(bfsSeq)
    }
    summary
  }

  def inferLoopyTreewiseMax(varying: Iterable[DiscreteVar], model: Model, numIterations: Int = 2) = {
    val summary = LoopyBPSummary(varying, BPMaxProductRing, model)
    val bfsSeq = BPUtil.loopyBfs(varying.toSet, summary)
    for (i <- 0 to numIterations) {
      BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
      BPUtil.sendAccordingToOrdering(bfsSeq)
    }
    val assignment = summary.maximizingAssignment
    new MAPSummary(assignment, summary.factors.get.toVector)
  }

  def inferTreeSum(varying:Iterable[DiscreteVar], model:Model, root: DiscreteVar = null): BPSummary = {
    val summary = BPSummary(varying, BPSumProductRing, model)
    val _root = if (root != null) summary.bpVariable(root) else summary.bpVariables.head
    val bfsSeq = BPUtil.bfs(varying.toSet, _root, checkLoops = true)
    BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
    BPUtil.sendAccordingToOrdering(bfsSeq)
    summary
  }
  def inferTreeMarginalMax(varying:Iterable[DiscreteVar], model:Model, root:DiscreteVar = null): MAPSummary = {
    if (varying.size == 0) return new MAPSummary(new HashMapAssignment(ignoreNonPresent = true), Seq())
    val summary = BPSummary(varying, BPMaxProductRing, model)
    val _root = if (root != null) summary.bpVariable(root) else summary.bpVariables.head
    val bfsSeq = BPUtil.bfs(varying.toSet, _root, checkLoops = true)
    BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
    val assignment = new HashMapAssignment(ignoreNonPresent = true)
    val rt = _root.variable
    val rootValue = _root.calculateBelief.maxIndex
    assignment.update(rt, rt.domain(rootValue).asInstanceOf[DiscreteVar#Value])
    for ((edge, v2f) <- bfsSeq; if v2f) {
      edge.bpFactor match {
        case f: BPFactor2 =>
          if (edge eq f.edge2) {
            val value = f.edge2Max1(assignment(f.edge2.variable).asInstanceOf[DiscreteValue].intValue)
            assignment.update(f.edge1.variable, f.edge1.variable.domain(value).asInstanceOf[DiscreteVar#Value])
          } else {
            val value = f.edge1Max2(assignment(f.edge1.variable).asInstanceOf[DiscreteValue].intValue)
            assignment.update(f.edge2.variable, f.edge2.variable.domain(value).asInstanceOf[DiscreteVar#Value])
          }
        case _ => 
      }
    }
    new MAPSummary(assignment, summary.factors.get.toSeq)
  }

  // Works specifically on a linear-chain with factors Factor2[Label,Features], Factor1[Label] and Factor2[Label1,Label2]
  def inferChainMax(varying:Seq[DiscreteVar], model:Model)(implicit d: DiffList=null): MAPSummary = {
    varying.size match {
      case 0 => new MAPSummary(new HashMapAssignment(ignoreNonPresent = true), Seq())
      case 1 =>
        val factors =  model.factors(varying.head)
        val value = varying.head.proportions(factors).maxIndex
        new MAPSummary(new Assignment1[DiscreteVar](varying.head, varying.head.domain(value).asInstanceOf[DiscreteVar#Value]), factors.toSeq)
      case _ =>
        val summary = BPSummary(varying, BPMaxProductRing, model)
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1])
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2]]
        assert(obsBPFactors.size + markovBPFactors.size == summary.bpFactors.size)
        obsBPFactors.foreach(_.updateOutgoing())
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1)
          f.updateOutgoing(f.edge2)
        }
        var maxIndex = markovBPFactors.last.edge2.bpVariable.calculateBelief.maxIndex
        val assignment = new HashMapAssignment(ignoreNonPresent = true)
        assignment.update(varying.last, varying.last.domain(maxIndex).asInstanceOf[DiscreteVar#Value])
        var n = varying.length - 2
        for (f <- markovBPFactors.reverse) {
          maxIndex = f.edge2Max1(maxIndex)
          assignment.update(varying(n), varying(n).domain(maxIndex).asInstanceOf[DiscreteVar#Value])
          n -= 1
        }
        new MAPSummary(assignment, summary.factors.get.toSeq)
    }
  }

  // Works specifically on a linear-chain with factors Factor2[Label,Features], Factor1[Label] and Factor2[Label1,Label2]
  def inferChainSum(varying:Seq[DiscreteVar], model:Model): BPSummary = {
    val summary = BPSummary(varying, BPSumProductRing, model)
    varying.size match {
      case 0 => {}
      case 1 =>
        summary.bpFactors.foreach(_.updateOutgoing())
        summary.bpVariables.head.updateOutgoing()
      case _ =>
        // TODO There is a tricky dependency here: "varying" comes in order, and we are relying on the summary.bpFactors returning factors in chain order also!  Make this safer. -akm  
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1]).asInstanceOf[Seq[BPFactor1]].toArray // this includes both Factor1[Label], Factor2[Label,Features]
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2]].toArray
        assert(obsBPFactors.size + markovBPFactors.size == summary.bpFactors.size)
        // assert(markovBPFactors.length < 2 || markovBPFactors.sliding(2).forall(fs => fs(0).edge2.bpVariable == fs(1).edge1.bpVariable)) // Make sure we got the Markov chain factors in order!
        // Send all messages from observations to labels in parallel
        obsBPFactors.foreach(_.edge1.bpFactor.updateOutgoing())
        // Send forward messages
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1) // send message from neighbor1 to factor // TODO Note that Markov factors must in sequence order!  Assert this somewhere!
          f.updateOutgoing(f.edge2)   // send message from factor to neighbor2
        }
        // Send backward messages
        for (f <- markovBPFactors.reverse) {
          f.edge2.bpVariable.updateOutgoing(f.edge2) // send message from neighbor2 to factor
          f.updateOutgoing(f.edge1)   // send message from factor to neighbor1
        }
        // Send messages out to obs factors so that they have the right logZ
        obsBPFactors.foreach(f => {
          f.edge1.bpVariable.updateOutgoing(f.edge1)
        })
    }
    summary
  }
  
}

trait InferByBP extends Infer[Iterable[DiscreteVar],Model] {
  override def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary): Summary
}
trait MaximizeByBP extends InferByBP with Maximize[Iterable[DiscreteVar],Model]

object InferByBPTree extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    assert(variables.size == variables.toSet.size)
    BP.inferTreeSum(variables, model)
  }
}

object InferByBPLoopy extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    assert(variables.size == variables.toSet.size)
    val summary = LoopyBPSummary(variables, BPSumProductRing, model)
    BP.inferLoopy(summary)
    summary
  }
}

object InferByBPLoopyTreewise extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    BP.inferLoopyTreewise(variables, model)
  }
}

object MaximizeByBPLoopyTreewise extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    BP.inferLoopyTreewiseMax(variables, model)
  }
}

object MaximizeByBPLoopy extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    val summary = LoopyBPSummaryMaxProduct(variables, BPMaxProductRing, model)
    BP.inferLoopyMax(summary)
    summary
  }
  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = {
    val summary = LoopyBPSummaryMaxProduct(varying, BPMaxProductRing, model)
    BP.inferLoopyMax(summary)
    summary
  }
}

object InferByBPChain extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables, model)
  }
  def apply(varying:Iterable[DiscreteVar], model:Model): BPSummary = BP.inferChainSum(varying.toSeq, model)
}

object MaximizeByBPChain extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null): MAPSummary = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables, model)
  }
  def apply(varying:Iterable[DiscreteVar], model:Model): MAPSummary = {
    val s = BP.inferChainMax(varying.toSeq, model)
    s.setToMaximize(null)
    s
  }
}

object MaximizeByBPTree extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary=null) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables.toSet, model)
  }
  def apply(varying:Set[DiscreteVar], model:Model): MAPSummary = BP.inferTreeMarginalMax(varying, model)
}
