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

import cc.factorie.la._
import scala.collection.mutable.{ArrayBuffer, HashMap, LinkedHashMap, LinkedHashSet}
import scala.collection.{Set}
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import cc.factorie.util.{DoubleSeq, RangeIntSeq, SparseDoubleSeq}

/** A factory object creating BPFactors and BPVariables, each of which contain methods for calculating messages. 
    "Ring" refers to whether we are using sum/product or max/product. */
trait BPRing {
  def newBPVariable(v:DiscreteVar): BPVariable1
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary): BPFactor
}

// TODO this code duplication is why mixins are usually a bad way to factor stuff -luke
object BPSumProductRing extends BPRingDefaults[BPFactorSumProduct, BPFactor2SumProduct, BPVariableSumProduct] {
  def newBPVariable(v: DiscreteVar) = new BPVariable1(v) with BPVariableSumProduct
  def newBPFactor1Factor1(factor: Factor1[DiscreteVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor1(factor, edge1, sum) with BPFactorSumProduct
  def newBPFactor1Factor2(factor: Factor2[DiscreteVar, VectorVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor2(factor, edge1, sum) with BPFactorSumProduct
  def newBPFactor2Factor2(factor: Factor2[DiscreteVar, DiscreteVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary) = new BPFactor2Factor2(factor, edge1, edge2, sum) with BPFactor2SumProduct
  def newBPFactor1Factor3(factor: Factor3[VectorVar, VectorVar, VectorVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor3(factor, edge1, sum) with BPFactorSumProduct
  def newBPFactor2Factor3(factor: Factor3[DiscreteVar, DiscreteVar, VectorVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary) = new BPFactor2Factor3(factor, edge1, edge2, sum) with BPFactor2SumProduct
}

object BPMaxProductRing extends BPRingDefaults[BPFactorMaxProduct, BPFactor2MaxProduct, BPVariableMaxProduct] {
  def newBPVariable(v: DiscreteVar) = new BPVariable1(v) with BPVariableMaxProduct
  def newBPFactor1Factor1(factor: Factor1[DiscreteVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor1(factor, edge1, sum) with BPFactorMaxProduct
  def newBPFactor1Factor2(factor: Factor2[DiscreteVar, VectorVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor2(factor, edge1, sum) with BPFactorMaxProduct
  def newBPFactor2Factor2(factor: Factor2[DiscreteVar, DiscreteVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary) = new BPFactor2Factor2(factor, edge1, edge2, sum) with BPFactor2MaxProduct
  def newBPFactor1Factor3(factor: Factor3[VectorVar, VectorVar, VectorVar], edge1: BPEdge, sum: BPSummary) = new BPFactor1Factor3(factor, edge1, sum) with BPFactorMaxProduct
  def newBPFactor2Factor3(factor: Factor3[DiscreteVar, DiscreteVar, VectorVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary) = new BPFactor2Factor3(factor, edge1, edge2, sum) with BPFactor2MaxProduct
}

trait BPRingDefaults[F1 <: BPFactor, F2 <: F1, V <: BPVariable] extends BPRing {
  def newBPVariable(v: DiscreteVar): BPVariable1 with V
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary): BPFactor with F1 = {
    if (varying == null) sys.error("Can't call newBPFactor with null list of varying variables.")
    factor match {
      case factor:Factor1[DiscreteVar @unchecked] =>
        newBPFactor1Factor1(factor, new BPEdge(summary.bpVariable(factor._1)), summary)
      case factor:Factor2[DiscreteVar @unchecked,DiscreteVar @unchecked] =>
        if (varying.contains(factor._1) && varying.contains(factor._2)) newBPFactor2Factor2(factor, new BPEdge(summary.bpVariable(factor._1)), new BPEdge(summary.bpVariable(factor._2)), summary)
        else if (varying.contains(factor._1)) newBPFactor1Factor2(factor.asInstanceOf[Factor2[DiscreteVar,VectorVar]], new BPEdge(summary.bpVariable(factor._1)), summary)
        else newBPFactor1Factor2(factor.asInstanceOf[Factor2[DiscreteVar,VectorVar]], new BPEdge(summary.bpVariable(factor._2)), summary)
      case factor:Factor3[VectorVar @unchecked,VectorVar @unchecked,VectorVar @unchecked] =>
        val neighbors = factor.variables.toSet.intersect(varying.toSet)
        if (neighbors.size == 2)
          newBPFactor2Factor3(factor.asInstanceOf[Factor3[DiscreteVar,DiscreteVar,VectorVar]], new BPEdge(summary.bpVariable(factor._1.asInstanceOf[DiscreteVar])), new BPEdge(summary.bpVariable(factor._2.asInstanceOf[DiscreteVar])), summary)
        else if (neighbors.size == 1)
          newBPFactor1Factor3(factor, new BPEdge(summary.bpVariable(neighbors.head.asInstanceOf[DiscreteVar])), summary)
        else throw new Error("Can't create the factor")
    }
  }

  def newBPFactor1Factor1(factor: Factor1[DiscreteVar], edge1: BPEdge, sum: BPSummary): BPFactor1Factor1 with F1
  def newBPFactor1Factor2(factor: Factor2[DiscreteVar, VectorVar], edge1: BPEdge, sum: BPSummary): BPFactor1Factor2 with F1
  def newBPFactor2Factor2(factor: Factor2[DiscreteVar, DiscreteVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary): BPFactor2Factor2 with F2
  def newBPFactor1Factor3(factor: Factor3[VectorVar, VectorVar, VectorVar], edge1: BPEdge, sum: BPSummary): BPFactor1Factor3 with F1
  def newBPFactor2Factor3(factor: Factor3[DiscreteVar, DiscreteVar, VectorVar], edge1: BPEdge, edge2: BPEdge, sum: BPSummary): BPFactor2Factor3 with F2
}

// TODO
// object BPSumProductBeamRing extends BPRing
// object BPSumProductNonLogRing extends BPRing // Not in log space, avoiding maths.logSum

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
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing(_))
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
  def calculateMarginal: Tensor
}
abstract class BPVariable1(val _1: DiscreteVar) extends DiscreteMarginal1[DiscreteVar] with BPVariable {
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
  def globalize(implicit d:DiffList): Unit = variable match { case v:MutableDiscreteVar[_] => v.set(calculateBelief.maxIndex)(d) }  // To avoid normalization compute time
  override def setToMaximize(implicit d: DiffList=null) = variable.asInstanceOf[MutableDiscreteVar[_]].set(calculateBelief.maxIndex)
  def updateOutgoingMAP() {
    val maxValue = variable.intValue
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
    }
  }
}

trait BPVariableMaxProduct { self: BPVariable =>
  // TODO BUG FIXME: I set this to return max-marginals instead of MAP assignment. This is to keep unit tests from breaking, but is inconsistent with factor marginals -luke
  def calculateMarginal: Tensor = { val t = calculateBelief; t.expNormalize(); t }
}

trait BPVariableSumProduct { self: BPVariable =>
  def calculateMarginal: Tensor = { val t = calculateBelief; t.expNormalize(); t }
}
// TODO class BPVariable{2,3,4} would be used for cluster graphs

trait BPFactor extends DiscreteMarginal with FactorMarginal {
  def factor: Factor
  def edges: Seq[BPEdge]
  def summary: BPSummary
  /** Re-calculate the message from this factor to edge "e" and set e.messageFromFactor to the result. */
  def updateOutgoing(e: BPEdge): Unit
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing(_))
  def scores: Tensor // All local scores across all dimensions of varying neighbors; does not use messages from variables
  /** Unnormalized log scores over values of varying neighbors */
  def calculateBeliefsTensor: Tensor
  /** The logSum of all entries in the beliefs tensor */
  def calculateLogZ: Double
  /** Normalized probabilities over values of varying neighbors */
  def calculateMarginalTensor: Tensor
  /** Normalized probabilities over values of only the varying neighbors, in the form of a Proportions */
  override def proportions: Proportions // Must be overridden to return "new NormalizedTensorProportions{1,2,3,4}(calculateMarginalTensor, false)"
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
  def marginalTensorValues: Tensor = throw new Error("Not yet implemented")
}

trait BPFactorSumProduct extends BPFactor {
  override def calculateMarginalTensor: Tensor = {
    val v = calculateBeliefsTensor
    summary.expNormalize(v)
    v
  }
  
  def calculateLogZ: Double = calculateBeliefsTensor match {
    case t:DenseTensor => { var z = Double.NegativeInfinity; val l = t.length; var i = 0; while (i < l) { z = maths.sumLogProb(z, t(i)); i += 1 }; z }
    case t:SparseIndexedTensor => { var z = Double.NegativeInfinity; t.foreachActiveElement((i,v) => { z = maths.sumLogProb(z, v) }); z }
  }
}

trait BPFactorMaxProduct extends BPFactor {
  override def calculateMarginalTensor: Tensor = { val t = calculateBeliefsTensor; t.maxNormalize(); t }
  override def calculateLogZ: Double = calculateBeliefsTensor match {
    case t:DenseTensor => { t.max }
    case t:SparseIndexedTensor => { t._values.max }
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
  override def proportions: Proportions1 = new DenseTensorProportions1(calculateMarginalTensor.asArray, false)
  def calculateOutgoing1: Tensor1 = scores
}


// A BPFactor1 with underlying model Factor1, with the one neighbor varying
abstract class BPFactor1Factor1(val factor: Factor1[DiscreteVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1[DiscreteVar] with DiscreteMarginal1Factor1[DiscreteVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = factor match {
    case factor:DotFamily#Factor if (factor.family.isInstanceOf[DotFamily]) => factor.family.weights.value.asInstanceOf[Tensor1]
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
abstract class BPFactor1Factor2(val factor: Factor2[DiscreteVar,VectorVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1[DiscreteVar] with DiscreteMarginal1Factor2[DiscreteVar,VectorVar] {
  def hasLimitedDiscreteValues1: Boolean = factor.hasLimitedDiscreteValues1
  def limitedDiscreteValues1: SparseBinaryTensor1 = factor.limitedDiscreteValues1
  val scores: Tensor1 = {
    val valueTensor = new SingletonBinaryLayeredTensor2(edge1.variable.domain.size, factor._2.domain.dimensionDomain.size, 0, factor._2.value.asInstanceOf[Tensor1])
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

abstract class BPFactor1Factor3(val factor: Factor3[VectorVar,VectorVar,VectorVar], edge1:BPEdge, sum: BPSummary) extends BPFactor1(edge1, sum) with DiscreteMarginal1Factor3[VectorVar,VectorVar,VectorVar,DiscreteVar] {
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
abstract class BPFactor2(val edge1: BPEdge, val edge2: BPEdge, val summary: BPSummary) extends DiscreteMarginal2(edge1.bpVariable.variable, edge2.bpVariable.variable, null) with BPFactor {
  override def scores: Tensor2
  def calculateOutgoing1: Tensor
  def calculateOutgoing2: Tensor
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
//    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size)
//      result(i,j) = scores(i,j) + edge1.messageFromVariable(i) + edge2.messageFromVariable(j)
    result
  }
  override def proportions: Proportions2 = new DenseTensorProportions2(calculateMarginalTensor.asArray, scores.dim1, scores.dim2, false)
}

trait BPFactor2SumProduct extends BPFactorSumProduct { this: BPFactor2 =>
  def calculateOutgoing1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size, Double.NegativeInfinity)
    if (hasLimitedDiscreteValues12) {
      //throw new Error("This code path leads to incorrect marginals")
      //println("BPFactor2SumProduct calculateOutgoing1")
      val indices = limitedDiscreteValues12._indices.toSet
      val len = edge1.variable.domain.size * edge2.variable.domain.size;  /* require(len > 0, "limitedDiscreteValues12 can't everything"); */ var ii = 0
      while (ii < len) {
        val i = scores.index1(ii)
        val j = scores.index2(ii)
        if (indices.contains(ii))
          result(i) = cc.factorie.maths.sumLogProb(result(i), scores(i,j) + edge2.messageFromVariable(j)) // TODO This could be scores(ij)
        else
          result(i) = cc.factorie.maths.sumLogProb(result(i), edge2.messageFromVariable(j))
        ii += 1
      }
    } else {
      val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
      while (i < leni) {
        j = 0; while (j < lenj) {
          result(i) = cc.factorie.maths.sumLogProb(result(i), scores(i,j) + edge2.messageFromVariable(j))
          j += 1
        }
        i += 1
      }
    }
//    for (i <- 0 until edge1.variable.domain.size; j <- 0 until edge2.variable.domain.size)
//      result(i) = cc.factorie.maths.sumLogProb(result(i), scores(i,j) + edge2.messageFromVariable(j))
    result
  }
  def calculateOutgoing2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size, Double.NegativeInfinity)
    if (hasLimitedDiscreteValues12) {
      //throw new Error("This code path leads to incorrect marginals")
      val indices = limitedDiscreteValues12._indices.toSet
      val len = edge1.variable.domain.size * edge2.variable.domain.size //limitedDiscreteValues12.activeDomainSize; /* require(len > 0, "limitedDiscreteValues12 limits everything"); */
      var ii = 0
      while (ii < len) {
        val i = scores.index1(ii)
        val j = scores.index2(ii)
        if (indices.contains(ii)) {
          result(j) = cc.factorie.maths.sumLogProb(result(j), scores(i,j) + edge1.messageFromVariable(i)) // TODO This could be scores(ij)
        } else {
          result(j) = cc.factorie.maths.sumLogProb(result(j), edge1.messageFromVariable(i))
        }
        ii += 1
      }
    } else {
      val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
      while (j < lenj) {
        i = 0; while (i < leni) {
          result(j) = cc.factorie.maths.sumLogProb(result(j), scores(i,j) + edge1.messageFromVariable(i))
          i += 1
        }
        j += 1
      }
    }
//    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size)
//      result(j) = cc.factorie.maths.sumLogProb(result(j), scores(i,j) + edge1.messageFromVariable(i))
    result
  }
}

trait BPFactor2MaxProduct extends BPFactor2 with BPFactorMaxProduct { this: BPFactor2 =>
  val edge1Max2 = new Array[Int](edge1.variable.domain.size) // The index value of edge2.variable that lead to the MaxProduct value for each index value of edge1.variable
  var edge2Max1 = new Array[Int](edge2.variable.domain.size)
  def calculateOutgoing1: Tensor = {
    scores match {
      case scores:SparseIndexedTensor2 => { // SparseTensor case
        val result = new DenseTensor1(scores.dim1, Double.NegativeInfinity) // TODO Consider a non-Dense tensor here
        for (element <- scores.activeElements2) {
          val i = element.index1; val j = element.index2
          val s = element.value + edge2.messageFromVariable(j)
          if (s > result(i)) { result(i) = s; edge1Max2(i) = j }
        }
        result
      }
      case scores: Tensor2 => {
        val result = new DenseTensor1(edge1.variable.domain.size, Double.NegativeInfinity)
        val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
        while (i < leni) {
          j = 0; while (j < lenj) {
            val s = scores(i,j) + edge2.messageFromVariable(j)
            if (s > result(i)) { result(i) = s; edge1Max2(i) = j } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
            j += 1
          }
          i += 1
        }
        result
      }
    }
  }
  def calculateOutgoing2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size, Double.NegativeInfinity)
    val lenj = edge2.variable.domain.size; val leni = edge1.variable.domain.size; var j = 0; var i = 0
    while (j < lenj) {
      i = 0
      while (i < leni) {
        val s = scores(i,j) + edge1.messageFromVariable(i)
        if (s > result(j)) { result(j) = s; edge2Max1(j) = i } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
        i += 1
      }
      j += 1
    }
//    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size) {
//      val s = scores(i,j) + edge1.messageFromVariable(i)
//      if (s > result(j)) { result(j) = s; edge2Max1(j) = i } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
//    }
    result
  }
}

// A BPFactor2 with underlying model Factor2, with both neighbors varying
abstract class BPFactor2Factor2(val factor:Factor2[DiscreteVar,DiscreteVar], edge1:BPEdge, edge2:BPEdge, sum: BPSummary) extends BPFactor2(edge1, edge2, sum) with DiscreteMarginal2Factor2[DiscreteVar,DiscreteVar] {
  // TODO Consider making this calculate scores(i,j) on demand, with something like
  // val scores = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size, Double.NaN) { override def apply(i:Int) = if (_values(i).isNaN)... }
  val hasLimitedDiscreteValues12: Boolean = factor.hasLimitedDiscreteValues12
  def limitedDiscreteValues12: SparseBinaryTensor2 = factor.limitedDiscreteValues12
  val scores: Tensor2 = factor match {
    // This only works if statistics has not been overridden.  Hence factor.statisticsAreValues.  (See TestBP.loop2 for an example where this fails.)
    // TODO Try to be more efficient even when statisticsAreValues is false. -akm
    case factor:DotFamily#Factor if (factor.family.isInstanceOf[DotFamily] && factor.statisticsAreValues && factor.family.weights.value.isInstanceOf[Tensor2]) => {
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
abstract class BPFactor2Factor3(val factor:Factor3[DiscreteVar,DiscreteVar,VectorVar], edge1:BPEdge, edge2:BPEdge, sum: BPSummary) extends BPFactor2(edge1, edge2, sum) with DiscreteMarginal2Factor3[DiscreteVar, DiscreteVar, VectorVar] {
  val hasLimitedDiscreteValues12: Boolean = factor.hasLimitedDiscreteValues12
  def limitedDiscreteValues12: SparseBinaryTensor2 = factor.limitedDiscreteValues12
  val scores: Tensor2 = {
    val valueTensor = new Singleton2LayeredTensor3(edge1.variable.domain.size, edge2.variable.domain.size, factor._3.domain.dimensionDomain.size, 0, 0, 1.0, 1.0, factor._3.value.asInstanceOf[Tensor1])
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


object BPSummary {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
    val summary = new BPSummary(ring)
    val varyingSet = varying.toSet
    //print("object BPSummary variables "+varying.size)
    val factors = model.factors(varying)
    //println("  factors "+factors.size)
    for (factor <- factors) summary._bpFactors(factor) = ring.newBPFactor(factor, varyingSet, summary)
    summary
  }
  def apply(varying:Iterable[DiscreteVar], model:Model): BPSummary = apply(varying, BPSumProductRing, model)
}

object LoopyBPSummary {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
      val summary = new LoopyBPSummary(ring)
      val varyingSet = varying.toSet
      for (factor <- model.factors(varying)) summary._bpFactors(factor) = ring.newBPFactor(factor, varyingSet, summary)
      summary
    }
}

object LoopyBPSummaryMaxProduct {
  def apply(varying:Iterable[DiscreteVar], ring:BPRing, model:Model): BPSummary = {
      val summary = new LoopyBPSummaryMaxProduct(ring)
      val varyingSet = varying.toSet
      for (factor <- model.factors(varying)) summary._bpFactors(factor) = ring.newBPFactor(factor, varyingSet, summary)
      summary
    }
}

// Just in case we want to create different BPSummary implementations
// TODO Consider removing this
trait AbstractBPSummary extends Summary {
  def ring: BPRing
  //def factors: Iterable[Factor]
  def bpVariable(v:DiscreteVar): BPVariable1
  def bpFactors: Iterable[BPFactor]
  def bpVariables: Iterable[BPVariable1]
  def marginal(v: DiscreteVar): BPVariable1
  def expNormalize(t: Tensor)
}

/** A collection of marginals inferred by belief propagation.  
    Do not call this constructor directly; instead use the companion object apply methods, 
    which add the appropriate BPFactors, BPVariables and BPEdges. */
class BPSummary(val ring:BPRing) extends AbstractBPSummary {
  protected val _bpFactors = new LinkedHashMap[Factor, BPFactor]
  protected val _bpVariables = new LinkedHashMap[VectorVar, BPVariable1]
  def bpVariable(v:DiscreteVar): BPVariable1 = _bpVariables.getOrElseUpdate(v, ring.newBPVariable(v))
  def bpFactors: Iterable[BPFactor] = _bpFactors.values
  def factorMarginals = factors.head.map(marginal)
  def factors: Option[Iterable[Factor]] = Some(_bpFactors.values.map(_.factor))
  //def factors: Iterable[Factor] = _bpFactors.values.map(_.factor)
  def bpVariables: Iterable[BPVariable1] = _bpVariables.values
  override def marginals: Iterable[DiscreteMarginal1[DiscreteVar]] = _bpVariables.values // ++ _bpVariables.values
  override def marginal(v:Var): BPVariable1 = v match { case v:DiscreteVar => _bpVariables(v); case _ => null }
  //def marginal(f:Factor): DiscreteMarginal = { val vset = f.variables.toSet; val factors = _bpFactors.values.filter(bpf => bpf.variables.toSet == vset); require(factors.size == 1); factors.head } // Need to actually combine if more than one
  //def marginal(vs: Var*): DiscreteMarginal = {val factors = _bpFactors.values.filter(f => f.variables.toSet == vs.toSet); require(factors.size == 1); factors.head} // Need to actually combine if more than one
  def marginal(v: DiscreteVar): BPVariable1 = _bpVariables(v)
  def marginal(f: Factor): BPFactor = _bpFactors(f)
  // TODO I think we are calculating logZ many time redundantly, including in BPFactor.calculateMarginalTensor.
  var _logZ = Double.NaN
  override def logZ: Double = {
    if (_logZ != _logZ) _logZ = _bpFactors.values.head.calculateLogZ
    _logZ
  }
  
  //def setToMaximizeMarginals(implicit d:DiffList = null): Unit = bpVariables.foreach(_.setToMaximize(d))
  override def setToMaximize(implicit d:DiffList = null): Unit = ring match {
    case BPSumProductRing => bpVariables.foreach(_.setToMaximize(d))
    case BPMaxProductRing => bpVariables.foreach(v => {
      v.setToMaximize(d)
      v.updateOutgoingMAP()
    })
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
    val visited: HashSet[BPEdge] = new HashSet
    val result = new ArrayBuffer[(BPEdge, Boolean)]
    val toProcess = new Queue[(BPEdge, Boolean)]
    val visitedVariables = HashSet[DiscreteVar]()
    root.edges foreach (e => toProcess += Pair(e, true))
    while (!toProcess.isEmpty) {
      val (edge, v2f) = toProcess.dequeue()
      if (!checkLoops || !visited(edge)) {
        visited += edge
        visitedVariables += edge.variable
        result += Pair(edge, v2f)
        val edges =
          if (v2f) edge.bpFactor.edges.filter(_ != edge)
          else {
            if (varying.contains(edge.bpVariable.variable))
              edge.bpVariable.edges.filter(_ != edge) 
            else Seq.empty[BPEdge]
          }
        edges.foreach(ne => toProcess += Pair(ne, !v2f))
      }
    }
    require(varying.forall(visitedVariables.contains(_)), "Treewise BP assumes the graph is connected")
    result
  }

  def loopyBfs(varying: Set[DiscreteVar], summary: BPSummary): Seq[(BPEdge, Boolean)] = {
    val visited: HashSet[BPEdge] = new HashSet
    val result = new ArrayBuffer[(BPEdge, Boolean)]
    val toProcess = new Queue[(BPEdge, Boolean)]
    val visitedVariables = HashSet[DiscreteVar]()
    while (!varying.forall(visitedVariables.contains(_))) {
      val root = summary.bpVariable(varying.collectFirst({case v if !visitedVariables.contains(v) => v}).head)
      root.edges foreach (e => toProcess += Pair(e, true))
      while (!toProcess.isEmpty) {
        val (edge, v2f) = toProcess.dequeue()
        if (!visited(edge)) {
          visited += edge
          result += Pair(edge, v2f)
          visitedVariables += edge.bpVariable.variable
          val edges =
            if (v2f) edge.bpFactor.edges.filter(_ != edge)
            else {
              if (varying.contains(edge.bpVariable.variable))
                edge.bpVariable.edges.filter(_ != edge)
              else Seq.empty[BPEdge]
            }
          edges.foreach(ne => toProcess += Pair(ne, !v2f))
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

  def inferLoopyTreewise(varying: Iterable[DiscreteVar], model: Model, numIterations: Int = 2) = {
    val summary = LoopyBPSummary(varying, BPSumProductRing, model)
    val bfsSeq = BPUtil.loopyBfs(varying.toSet, summary)
    for (i <- 0 to numIterations) {
      BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
      BPUtil.sendAccordingToOrdering(bfsSeq)
    }
    summary
  }

  def inferTreeSum(varying:Iterable[DiscreteVar], model:Model, root: DiscreteVar = null): BPSummary = {
    val summary = BPSummary(varying, BPSumProductRing, model)
    val _root = if (root != null) summary.bpVariable(root) else summary.bpVariables.head
    val bfsSeq = BPUtil.bfs(varying.toSet, _root, checkLoops = true)
    BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
    BPUtil.sendAccordingToOrdering(bfsSeq)
    summary
  }
  def inferTreeMarginalMax(varying:Iterable[DiscreteVar], model:Model, root:DiscreteVar = null): BPSummary = {
    val summary = BPSummary(varying, BPMaxProductRing, model)
    if (varying.size == 0) return summary 
    val _root = if (root != null) summary.bpVariable(root) else summary.bpVariables.head
    val bfsSeq = BPUtil.bfs(varying.toSet, _root, checkLoops = true)
    BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
    BPUtil.sendAccordingToOrdering(bfsSeq)
    summary
  }
  // TODO: add inferTreewiseMax and associated test
  def inferSingle(v:MutableDiscreteVar[_<:DiscreteValue], model:Model): BPSummary = {
    val summary = BPSummary(Seq(v), BPSumProductRing, model)
    summary.bpFactors.foreach(_.updateOutgoing())
    summary
  }
  // Works specifically on a linear-chain with factors Factor2[Label,Features] and Factor2[Label1,Label2]
  def inferChainMax(varying:Seq[DiscreteVar], model:Model)(implicit d: DiffList=null): BPSummary = {
    val summary = BPSummary(varying, BPMaxProductRing, model)
    summary.bpFactors.foreach(f => assert(f.isInstanceOf[BPFactorMaxProduct] && !f.isInstanceOf[BPFactorSumProduct]))
    varying.size match {
      case 0 => {}
      case 1 => { summary.bpFactors.foreach(_.updateOutgoing()); summary.bpVariables.head.setToMaximize(null) }
      case _ => {
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1])
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2 with BPFactor2MaxProduct]]
        //assert(markovBPFactors.size > 0)
        //val bfsSeq = BPUtil.bfs(varying.toSet, summary.bpVariable(varying.head), false)
        //throw new Error("This is not yet working. -akm")
        assert(obsBPFactors.size + markovBPFactors.size == summary.bpFactors.size)
        //println("BP.inferChainMax  markovBPFactors.size = "+markovBPFactors.size)
        // Send all messages from observations to labels in parallel
        obsBPFactors.foreach(_.updateOutgoing())
        // Send forward Viterbi messages
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1) // send message from neighbor1 to factor
          f.updateOutgoing(f.edge2)   // send message from factor to neighbor2
        }

        for (f <- markovBPFactors.reverse) {
          f.edge2.bpVariable.updateOutgoing(f.edge2) // send message from neighbor1 to factor
          f.updateOutgoing(f.edge1)   // send message from factor to neighbor2
        }

        obsBPFactors.foreach(f => f.edges.foreach(e => e.bpVariable.updateOutgoing(e)))
        // Do Viterbi backtrace, setting label values
        // TODO Perhaps this should be removed from here, and put into a method on BPSummary?
        // Because we might want to run this inference, but not change global state.
        var maxIndex = markovBPFactors.last.edge2.bpVariable.proportions.maxIndex // We don't actually need to expNormalize here; save computation by avoiding this
        markovBPFactors.last.edge2.variable.asInstanceOf[MutableDiscreteVar[_]] := maxIndex
        for (f <- markovBPFactors.reverse) {
          maxIndex = f.edge2Max1(maxIndex)
          f.edge1.variable.asInstanceOf[MutableDiscreteVar[_]].set(maxIndex)(null)
        }
      }
    }
    summary
  }
  
  // Works specifically on a linear-chain with factors Factor2[Label,Features], Factor1[Label] and Factor2[Label1,Label2]
  def inferChainSum(varying:Seq[DiscreteVar], model:Model): BPSummary = {
    val summary = BPSummary(varying, BPSumProductRing, model)
    varying.size match {
      case 0 => {}
      case 1 => summary.bpFactors.foreach(_.updateOutgoing())
      case _ => {
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
        // Update marginals    //summary.bpVariables.foreach(_.updateProportions)
        // TODO Also update BPFactor marginals
      }
    }
    summary
  }
  
}

trait InferByBP extends Infer[Iterable[DiscreteVar],Model] {
  override def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary): BPSummary
  //override def infer(variables:Iterable[DiscreteVar], model:Model): BPSummary = infer(variables, model, null)
}
trait MaximizeByBP extends InferByBP with Maximize[Iterable[DiscreteVar],Model]

object InferByBPTreeSum extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    //apply(variables.toSet, model)
    assert(variables.size == variables.toSet.size) // If this becomes a problem, use "variables.distinct" in the next line instead.
    BP.inferTreeSum(variables, model)
  }
  //def apply(varying:Set[DiscreteVar], model:Model): BPSummary = BP.inferTreeSum(varying, model)
}

object InferByBPLoopy extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    //apply(variables.toSet, model)
    assert(variables.size == variables.toSet.size) // If this becomes a problem, use "variables.distinct" in the next line instead.
    val summary = LoopyBPSummary(variables, BPSumProductRing, model)
    BP.inferLoopy(summary)
    summary
  }
//  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = {
//    val summary = LoopyBPSummary(varying, BPSumProductRing, model)
//    BP.inferLoopy(summary)
//    summary
//  }
}

object InferByBPLoopyTreewise extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    //apply(variables.toSet, model)
    assert(variables.size == variables.toSet.size) // If this becomes a problem, use "variables.distinct" in the next line instead.
    BP.inferLoopyTreewise(variables, model)
  }
//  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = {
//    BP.inferLoopyTreewise(varying, model)
//  }
}

object MaximizeByBPLoopy extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    //apply(variables.toSet, model)
    assert(variables.size == variables.toSet.size) // If this becomes a problem, use "variables.distinct" in the next line instead.
    val summary = LoopyBPSummaryMaxProduct(variables, BPMaxProductRing, model)
    BP.inferLoopy(summary)
    summary
  }
  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = {
    val summary = LoopyBPSummaryMaxProduct(varying, BPMaxProductRing, model)
    BP.inferLoopy(summary)
    summary
  }
}

object InferByBPChainSum extends InferByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables, model)
  }
  def apply(varying:Iterable[DiscreteVar], model:Model): BPSummary = BP.inferChainSum(varying.toSeq, model)
}

object MaximizeByBPChain extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary): BPSummary = { 
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables, model)
  }
  def apply(varying:Iterable[DiscreteVar], model:Model): BPSummary = BP.inferChainMax(varying.toSeq, model)
}

object MaximizeByBPTree extends MaximizeByBP {
  def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary) = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables.toSet, model)
  }
  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = BP.inferTreeMarginalMax(varying, model)
}

//object InferByBPIndependent extends InferByBP {
//  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[BPSummary] = variables match {
//    case variables:Seq[DiscreteVar] if (variables.forall(_.isInstanceOf[DiscreteVar])) => Some(apply(variables, model))
//    case _ => None
//  }
//  def apply(varying:Seq[DiscreteVar], model:Model): BPSummary = BP.inferChainSum(varying, model)
//}
