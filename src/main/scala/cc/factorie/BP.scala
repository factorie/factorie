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

// TODO Consider if we can put mehods for message operations in here, rather than just using them as enums for hard code in BPFactors
trait BPRing
object BPSumProductRing extends BPRing
object BPMaxProductRing extends BPRing

/** A dumb container for messages factor->variable and variable->factor */
class BPEdge(val bpVariable: BPVariable) {
  bpVariable.edges += this
  var bpFactor: BPFactor = null
  var factorNeighborIndex: Int = -1
  var messageFromVariable: Tensor = bpVariable.scores.blankCopy
  var messageFromFactor: Tensor = bpVariable.scores.blankCopy
  def variable = bpVariable.variable
  def factor = bpFactor.factor
}

// TODO Should we get rid of this.scores?  It isn't actually used for anything except blankCopy and as temporary work-space for updateProportions
// TODO Same issue for the BPFactor subclasses
// TODO Get rid of ring, it isn't actually necessary, right?
class BPVariable(val variable: DiscreteVar, val ring: BPRing, val scores: Tensor1) extends DiscreteMarginal1(variable, Proportions.blankCopy(scores)) {
  val edges = new ArrayBuffer[BPEdge]
  protected def computeMessageFromFactorProduct(edges:Seq[BPEdge]): Tensor = edges.size match {
    case 0 => scores.blankCopy
    case 1 => edges.head.messageFromFactor
    case 2 => edges.head.messageFromFactor + edges.last.messageFromFactor
    case _ => { val result = scores.blankCopy; for (edge <- edges) result += edge.messageFromFactor; result }
  }
  def computeOutgoing(e:BPEdge): Tensor = {
    edges.size match {
      case 1 => { require(edges.head == e); scores.blankCopy }
      case 2 => if (edges.head == e) edges.last.messageFromFactor else if (edges.last == e) edges.head.messageFromFactor else throw new Error
      case _ => computeMessageFromFactorProduct(edges.filter(_ != e))
    }
  }
  def updateOutgoing(e:BPEdge): Unit = e.messageFromVariable = computeOutgoing(e)
  def updateOutgoing: Unit = edges.foreach(updateOutgoing(_))
  def updateProportions: Unit = {
    scores := computeMessageFromFactorProduct(edges); scores.expNormalize()
    super.proportions.masses := scores
  }
  override def proportions: Proportions1 = { updateProportions; super.proportions }  // TODO Think about avoiding re-calc every time
}
// TODO class BPVariable2 would be used for cluster graphs; consider renaming BPVariable to BPVariable1?

trait BPFactor extends DiscreteMarginal {
  def factor: Factor
  def edges: Seq[BPEdge]
  def ring: BPRing
  def updateOutgoing(e: BPEdge): Unit
  def updateOutgoing: Unit = edges.foreach(updateOutgoing(_))
}


// An abstract class for BPFactors that has 1 varying neighbor.  They may have additional constant neighbors.
abstract class BPFactor1(val edge1: BPEdge, val ring: BPRing, val scores: Tensor1) extends DiscreteMarginal1(edge1.bpVariable.variable, Proportions.blankCopy(scores)) with BPFactor {
  def factor: Factor
  edge1.bpFactor = this
  val edges = Seq(edge1)
  def updateOutgoing(e: BPEdge): Unit = e match {
    case this.edge1 => updateOutgoing1
  } 
  override def updateOutgoing: Unit = updateOutgoing1
  def updateOutgoing1: Unit = edge1.messageFromFactor = calculateOutgoing1
  def calculateOutgoing1: Tensor
}

// A BPFactor1 with underlying model Factor1, with the one neighbor varying
class BPFactor1Factor1(val factor: Factor1[DiscreteVar], edge1:BPEdge, ring:BPRing, scores:Tensor1) extends BPFactor1(edge1, ring, scores) {
  val valueTensor = new SingletonBinaryTensor1(edge1.variable.domain.size, 0)
  def calculateOutgoing1: Tensor = {
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex = i
      scores(i) = factor.valueScore(valueTensor)
    }
    scores
  }
}

// A BPFactor1 with underlying model Factor2, with the first neighbor varying and the second neighbor constant 
class BPFactor1Factor2(val factor: Factor2[DiscreteVar,DiscreteTensorVar], edge1:BPEdge, ring:BPRing, scores:Tensor1) extends BPFactor1(edge1, ring, scores) {
  val valueTensor = new SingletonBinaryLayeredTensor2(edge1.variable.domain.size, factor._2.domain.dimensionDomain.size, 0, factor._2.value.asInstanceOf[Tensor1])
  def calculateOutgoing1: Tensor = {
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      scores(i) = factor.valueScore(valueTensor)
    }
    scores
  }
}


// An abstract class for BPFactors that have 2 varying neighbors.  They may have additional constant neighbors.
abstract class BPFactor2(val edge1: BPEdge, val edge2: BPEdge, val ring: BPRing, val scores: Tensor2) extends DiscreteMarginal2(edge1.bpVariable.variable, edge2.bpVariable.variable, null) with BPFactor {
  def factor: Factor
  edge1.bpFactor = this
  edge2.bpFactor = this
  val edge1Max2 = new Array[Int](edge1.variable.domain.size) // The index value of edge2.variable that lead to the MaxProduct value for each index value of edge1.variable 
  var edge2Max1 = new Array[Int](edge2.variable.domain.size)
  val edges = Seq(edge1, edge2)
  def updateOutgoing(e: BPEdge): Unit = e match {
    case this.edge1 => updateOutgoing1
    case this.edge2 => updateOutgoing2
  } 
  def updateOutgoing1: Unit = edge1.messageFromFactor = calculateOutgoing1
  def updateOutgoing2: Unit = edge2.messageFromFactor = calculateOutgoing2
  def calculateOutgoing1: Tensor = ring match {
    case BPSumProductRing => calculateOutgoingSum1
    case BPMaxProductRing => calculateOutgoingMax1
  }
  def calculateOutgoing2: Tensor = ring match {
    case BPSumProductRing => calculateOutgoingSum2
    case BPMaxProductRing => calculateOutgoingMax2
  }
  def calculateOutgoingSum1: Tensor
  def calculateOutgoingSum2: Tensor
  def calculateOutgoingMax1: Tensor
  def calculateOutgoingMax2: Tensor
}

// A BPFactor2 with underlying model Factor2, with both neighbors varying
class BPFactor2Factor2(val factor:Factor2[DiscreteVar,DiscreteVar], edge1:BPEdge, edge2:BPEdge, ring:BPRing, scores:Tensor2) extends BPFactor2(edge1, edge2, ring, scores) {
  val valueTensor = new SingletonBinaryTensor2(edge1.variable.domain.size, edge2.variable.domain.size, 0, 0)
  def calculateOutgoingSum1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        result(i) = cc.factorie.maths.sumLogProb(result(i), factor.valueScore(valueTensor) + edge2.messageFromVariable(j))
      }
    }
    result
  }
  def calculateOutgoingMax1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      result(i) = Double.NegativeInfinity 
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        val s = factor.valueScore(valueTensor) + edge2.messageFromVariable(j)
        if (s > result(i)) { result(i) = s; edge1Max2(i) = j } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
      }
    }
    result
  }
  
  def calculateOutgoingSum2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size)
    for (i <- 0 until edge2.variable.domain.size) {
      valueTensor.singleIndex2 = i
      for (j <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex1 = j
        result(i) = cc.factorie.maths.sumLogProb(result(i), factor.valueScore(valueTensor) + edge1.messageFromVariable(j))
      }
    }
    result
  }
  def calculateOutgoingMax2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size)
    for (i <- 0 until edge2.variable.domain.size) {
      result(i) = Double.NegativeInfinity 
      valueTensor.singleIndex2 = i
      for (j <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex1 = j
        val s = factor.valueScore(valueTensor) + edge1.messageFromVariable(j)
        if (s > result(i)) { result(i) = s; edge2Max1(i) = j }
      }
    }
    result
  }
}

// A BPFactor2 with underlying model Factor3, having two varying neighbors and one constant neighbor
// Note that the varying neighbors are assumed to be factor._1 and factor._2, and the contant neighbor factor._3
class BPFactor2Factor3(val factor:Factor3[DiscreteVar,DiscreteVar,DiscreteTensorVar], edge1:BPEdge, edge2:BPEdge, ring:BPRing, scores:Tensor2) extends BPFactor2(edge1, edge2, ring, scores) {
  val valueTensor = new Singleton2LayeredTensor3(edge1.variable.domain.size, edge2.variable.domain.size, factor._3.domain.dimensionDomain.size, 0, 0, 1.0, 1.0, factor._3.value.asInstanceOf[Tensor1])
  def calculateOutgoingSum1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        result(i) = cc.factorie.maths.sumLogProb(result(i), factor.valueScore(valueTensor) + edge2.messageFromVariable(j))
      }
    }
    result
  }
  def calculateOutgoingMax1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      result(i) = Double.NegativeInfinity 
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        val s = factor.valueScore(valueTensor) + edge2.messageFromVariable(j)
        if (s > result(i)) { result(i) = s; edge1Max2(i) = j }
      }
    }
    result
  }
  
  def calculateOutgoingSum2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size)
    for (i <- 0 until edge2.variable.domain.size) {
      valueTensor.singleIndex2 = i
      for (j <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex1 = j
        result(i) = cc.factorie.maths.sumLogProb(result(i), factor.valueScore(valueTensor) + edge1.messageFromVariable(j))
      }
    }
    result
  }
  def calculateOutgoingMax2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size)
    for (i <- 0 until edge2.variable.domain.size) {
      valueTensor.singleIndex2 = i
      for (j <- 0 until edge1.variable.domain.size) {
        result(i) = Double.NegativeInfinity 
        valueTensor.singleIndex1 = j
        val s = factor.valueScore(valueTensor) + edge1.messageFromVariable(j)
        if (s > result(i)) { result(i) = s; edge2Max1(i) = j }
      }
    }
    result
  }
}


class BPFactor3(val factor: Factor, val edge1: BPEdge, val edge2: BPEdge, val edge3:BPEdge, val ring: BPRing, val scores: Tensor3) extends DiscreteMarginal3(edge1.bpVariable.variable, edge2.bpVariable.variable, edge3.bpVariable.variable, Proportions.blankCopy(scores)) with BPFactor {
  edge1.bpFactor = this
  edge2.bpFactor = this
  edge3.bpFactor = this
  val edges = Seq(edge1, edge2, edge3)
  def updateOutgoing(e: BPEdge): Unit = e match {
    case this.edge1 => updateOutgoing1
    case this.edge2 => updateOutgoing2
    case this.edge3 => updateOutgoing3
  } 
  def updateOutgoing1: Unit = edge1.messageFromFactor = calculateOutgoing1
  def updateOutgoing2: Unit = edge2.messageFromFactor = calculateOutgoing2
  def updateOutgoing3: Unit = edge3.messageFromFactor = calculateOutgoing3
  def calculateOutgoing1: Tensor = throw new Error("Not yet implemented")
  def calculateOutgoing2: Tensor = throw new Error("Not yet implemented")
  def calculateOutgoing3: Tensor = throw new Error("Not yet implemented")
}



class BPSummary(val ring:BPRing, val model:Model) extends Summary[DiscreteMarginal] {
  private val _bpFactors = new LinkedHashMap[Factor, BPFactor]
  private val _bpVariables = new LinkedHashMap[DiscreteTensorVar, BPVariable]
  // The caller is responsible for making sure there are no duplicates in "varying".
  def this(varying:Iterable[DiscreteVar], ring:BPRing, model:Model) = {
    this(ring, model)
    // Initialize BPVariables and BPEdge structure
    //println("BPSummary varying.size = "+varying.size)
    val varyingSet = varying.toSet
    //println("BPSummary varyingSet.size = "+varyingSet.size)
    for (factor <- model.factors(varying)) {
      val factorVarying = factor.variables.filter(_ match {case v: DiscreteVar => varyingSet.contains(v); case _ => false}).asInstanceOf[Seq[DiscreteVar]]
      val edges = factorVarying.map(v => new BPEdge(_bpVariables.getOrElseUpdate(v, new BPVariable(v, ring, new DenseTensor1(v.domain.size)))))
      val bpFactor = edges.size match {
        case 1 => factor match {
          case factor:Factor1[DiscreteVar] => new BPFactor1Factor1(factor, edges(0), ring, new DenseTensor1(edges(0).variable.domain.size)) 
          case factor:Factor2[DiscreteVar,DiscreteTensorVar] => new BPFactor1Factor2(factor, edges(0), ring, new DenseTensor1(edges(0).variable.domain.size)) 
        }
        case 2 => factor match {
          case factor:Factor2[DiscreteVar,DiscreteVar] => new BPFactor2Factor2(factor, edges(0), edges(1), ring, new DenseTensor2(edges(0).variable.domain.size, edges(1).variable.domain.size))
          case factor:Factor3[DiscreteVar,DiscreteVar,DiscreteTensorVar] => new BPFactor2Factor3(factor, edges(0), edges(1), ring, new DenseTensor2(edges(0).variable.domain.size, edges(1).variable.domain.size))
        }
      }
      _bpFactors(factor) = bpFactor
    }
    //println("BPSummary bpFactors.size = "+bpFactors.size)
  }
  def this(varying:Iterable[DiscreteVar], model:Model) = this(varying, BPSumProductRing, model)
  def bpFactors: Iterable[BPFactor] = _bpFactors.values
  def bpVariables: Iterable[BPVariable] = _bpVariables.values
  def marginals = _bpFactors.values ++ _bpVariables.values
  def marginal(vs: Variable*): DiscreteMarginal = vs.size match {
    case 1 => _bpVariables(vs.head.asInstanceOf[DiscreteVar])
    case 2 => {val factors = _bpFactors.values.filter(f => f.variables.toSet == vs.toSet); factors.head} // Need to actually combine if more than one
  }
  def marginal(v: DiscreteVar): BPVariable = _bpVariables(v)
  def marginal(f: Factor): BPFactor = _bpFactors(f)
  override def setToMaximize(implicit d:DiffList): Unit = 
    if (ring == BPSumProductRing) super.setToMaximize(d)
    else throw new Error("Not yet implemented backwards pass of Viterbi")
}

object BP {
  def inferLoopy(summary: BPSummary, numIterations: Int = 10): Unit = {
    for (iter <- 0 to numIterations) {
      for (bpf <- summary.bpFactors) {
        for (e <- bpf.edges) e.bpVariable.updateOutgoing(e)  // get all the incoming messages
        for (e <- bpf.edges) e.bpFactor.updateOutgoing(e)    // send messages
      }
    }
  }
  def inferTreewiseSum(varying:Set[DiscreteVar], model:Model): BPSummary = {
    val summary = new BPSummary(varying, BPSumProductRing, model)
    throw new Error("Not yet implemented")
  }
  // Works specifically on a linear-chain with factors Factor2[Label,Features] and Factor2[Label1,Label2]
  def inferChainMax(varying:Seq[DiscreteVariable], model:Model): BPSummary = {
    val summary = new BPSummary(varying, BPMaxProductRing, model)
    varying.size match {
      case 0 => {}
      case 1 => { summary.bpFactors.foreach(_.updateOutgoing); summary.bpVariables.head.setToMaximize(null) }
      case _ => {
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1])
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2]]
        //println("BP.inferChainMax  markovBPFactors.size = "+markovBPFactors.size)
        // Send all messages from observations to labels in parallel
        obsBPFactors.par.foreach(_.updateOutgoing)
        // Send forward Viterbi messages
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1) // send message from neighbor1 to factor
          f.edge1.bpFactor.updateOutgoing(f.edge2)   // send message from factor to neighbor2
        }
        // Do Viterbi backtrace, setting label values
        var maxIndex = markovBPFactors.last.edge2.bpVariable.proportions.maxIndex // TODO We don't actually need to expNormalize here; save computation by avoiding this
        markovBPFactors.last.edge2.variable.asInstanceOf[DiscreteVariable] := maxIndex
        for (f <- markovBPFactors.reverse) {
          maxIndex = f.edge2Max1(maxIndex)
          f.edge1.variable.asInstanceOf[DiscreteVariable] := maxIndex
        }
      }
    }
    summary
  }
}
