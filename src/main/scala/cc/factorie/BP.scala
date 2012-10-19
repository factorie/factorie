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

/** A factory object creating BPFactors and BPVariables, each of which contain methods for calculating messages. */
trait BPRing {
  def newBPVariable(v:DiscreteVar): BPVariable1
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary): BPFactor
}

object BPSumProductRing extends BPRing {
  def newBPVariable(v:DiscreteVar): BPVariable1 = new BPVariable1(v)
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary): BPFactor = {
    val factorVarying = factor.variables.filter(_ match {case v: DiscreteVar => varying.contains(v); case _ => false}).asInstanceOf[Seq[DiscreteVar]]
    val edges = factorVarying.map(v => new BPEdge(summary.bpVariable(v))) //_bpVariables.getOrElseUpdate(v, new BPVariable1(v, ring)))
    edges.size match {
      case 1 => factor match {
        case factor:Factor1[DiscreteVar] => new BPFactor1Factor1(factor, edges(0)) 
        case factor:Factor2[DiscreteVar,DiscreteTensorVar] => new BPFactor1Factor2(factor, edges(0)) 
      }
      case 2 => factor match {
        case factor:Factor2[DiscreteVar,DiscreteVar] => new BPFactor2Factor2(factor, edges(0), edges(1)) with BPFactor2SumProduct
        case factor:Factor3[DiscreteVar,DiscreteVar,DiscreteTensorVar] => new BPFactor2Factor3(factor, edges(0), edges(1)) with BPFactor2SumProduct
      }
    }
  }
}

object BPMaxProductRing extends BPRing {
  def newBPVariable(v:DiscreteVar): BPVariable1 = new BPVariable1(v)
  def newBPFactor(factor:Factor, varying:Set[DiscreteVar], summary:BPSummary): BPFactor = {
    val factorVarying = factor.variables.filter(_ match {case v: DiscreteVar => varying.contains(v); case _ => false}).asInstanceOf[Seq[DiscreteVar]]
    val edges = factorVarying.map(v => new BPEdge(summary.bpVariable(v))) //_bpVariables.getOrElseUpdate(v, new BPVariable1(v, ring)))
    edges.size match {
      case 1 => factor match {
        case factor:Factor1[DiscreteVar] => new BPFactor1Factor1(factor, edges(0)) 
        case factor:Factor2[DiscreteVar,DiscreteTensorVar] => new BPFactor1Factor2(factor, edges(0)) 
      }
      case 2 => factor match {
        case factor:Factor2[DiscreteVar,DiscreteVar] => new BPFactor2Factor2(factor, edges(0), edges(1)) with BPFactor2MaxProduct
        case factor:Factor3[DiscreteVar,DiscreteVar,DiscreteTensorVar] => new BPFactor2Factor3(factor, edges(0), edges(1)) with BPFactor2MaxProduct
      }
    }
  }
}

// TODO
// object BPSumProductBeamRing extends BPRing
// object BPSumProductNonLogRing extends BPRing // Not in log space, avoiding maths.logSum

/** A dumb container for messages factor->variable and variable->factor */
class BPEdge(val bpVariable: BPVariable1) {
  // TODO Eventually we should not require that this is a BPVariable1, but work for general BPVariable
  bpVariable.addEdge(this)
  var bpFactor: BPFactor = null
  var factorNeighborIndex: Int = -1 // TODO Is this necessary?
  // Note:  For Bethe cluster graphs with BPVariable1, these messages will be Tensor1, but for other cluster graphs they could have higher dimensionality
  var messageFromVariable: Tensor = new UniformTensor1(bpVariable.variable.domain.size, 0.0) // null // bpVariable.scores.blankCopy
  var messageFromFactor: Tensor = new UniformTensor1(bpVariable.variable.domain.size, 0.0) // null // bpVariable.scores.blankCopy
  def variable = bpVariable.variable
  def factor = bpFactor.factor
}


trait BPVariable {
  def edges: Seq[BPEdge]
  def calculateOutgoing(e:BPEdge): Tensor
  def updateOutgoing(e:BPEdge): Unit = e.messageFromVariable = calculateOutgoing(e)
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing(_))
}
class BPVariable1(val variable: DiscreteVar) extends DiscreteMarginal1(variable, null) with BPVariable {
  private var _edges: List[BPEdge] = Nil
  def addEdge(e:BPEdge): Unit = _edges = e :: _edges
  final def edges: List[BPEdge] = _edges
  def calculateOutgoing(e:BPEdge): Tensor = {
    edges.size match {
      case 0 => throw new Error("BPVariable1 with no edges")
      case 1 => { require(edges.head == e); new UniformTensor1(variable.domain.size, 0.0) }
      case 2 => if (edges.head == e) edges.tail.head.messageFromFactor else if (edges.tail.head == e) edges.head.messageFromFactor else throw new Error
      case _ => Tensor.sum(edges.filter(_ != e).map(_.messageFromFactor)) 
    }
  }
  def calculateBelief: Tensor1 = Tensor.sum(edges.map(_.messageFromFactor)).asInstanceOf[Tensor1] // TODO
  def calculateMarginal: Tensor1 = calculateBelief.expNormalized.asInstanceOf[Tensor1]
  override def proportions: Proportions1 = new NormalizedTensorProportions1(calculateMarginal, false)  // TODO Think about avoiding re-calc every time
  override def value1: DiscreteVar#Value = _1.domain.dimensionDomain(calculateBelief.maxIndex).asInstanceOf[DiscreteVar#Value] // TODO Ug.  This casting is rather sad.  // To avoid normalization compute time
  override def globalize(implicit d:DiffList): Unit = variable match { case v:MutableDiscreteVar[_] => v.set(calculateBelief.maxIndex)(d) }  // To avoid normalization compute time
}
// TODO class BPVariable{2,3,4} would be used for cluster graphs


trait BPFactor extends DiscreteMarginal {
  def factor: Factor
  def edges: Seq[BPEdge]
  /** Re-calculate the message from this factor to edge "e" and set e.messageFromFactor to the result. */
  def updateOutgoing(e: BPEdge): Unit
  def updateOutgoing(): Unit = edges.foreach(updateOutgoing(_))
  def scores: Tensor // All local scores across all dimensions of varying neighbors; does not use messages from variables
  /** Unnormalized log scores over values of varying neighbors */
  def calculateBeliefs: Tensor
  /** The logSum of all entries in the beliefs tensor */
  def calculateLogZ: Double = calculateBeliefs match {
    case t:DenseTensor => { var z = Double.NegativeInfinity; val l = t.length; var i = 0; while (i < l) { z = maths.sumLogProb(z, t(i)); i += 1 }; z }
    case t:SparseIndexedTensor => { var z = Double.NegativeInfinity; t.foreachActiveElement((i,v) => { z = maths.sumLogProb(z, v) }); z }
  }
  /** Normalized probabilities over values of varying neighbors */
  def calculateMarginal: Tensor = calculateBeliefs.expNormalized
  /** Normalized probabilities over values of varying neighbors, in the form of a Proportions */
  override def proportions: Proportions // Must be overridden to return "new NormalizedTensorProportions{1,2,3,4}(calculateMarginal, false)"
  /** Add to t the normalized probabilities over values of all neighbors. */
  def addExpectationInto(t:Tensor, f:Double): Unit
}


// An abstract class for BPFactors that has 1 varying neighbor.  They may have additional constant neighbors.
abstract class BPFactor1(val edge1: BPEdge) extends DiscreteMarginal1(edge1.bpVariable.variable, null) with BPFactor {
  override def scores: Tensor1
  edge1.bpFactor = this; edge1.factorNeighborIndex = 0
  val edges = Seq(edge1)
  def updateOutgoing(e: BPEdge): Unit = e match { case this.edge1 => updateOutgoing1() }
  override def updateOutgoing(): Unit = updateOutgoing1()
  def updateOutgoing1(): Unit = edge1.messageFromFactor = calculateOutgoing1
  // TODO See about caching this when possible
  def calculateBeliefs: Tensor1 = (scores + edge1.messageFromVariable).asInstanceOf[Tensor1]
  override def calculateMarginal: Tensor1 = calculateBeliefs.expNormalized.asInstanceOf[Tensor1]
  override def proportions: Proportions1 = new NormalizedTensorProportions1(calculateMarginal, false)
  def calculateOutgoing1: Tensor1 = scores
}

// A BPFactor1 with underlying model Factor1, with the one neighbor varying
class BPFactor1Factor1(val factor: Factor1[DiscreteVar], edge1:BPEdge) extends BPFactor1(edge1) {
  val scores: Tensor1 = factor match {
    case factor:DotFamily#Factor if (factor.family.isInstanceOf[DotFamily]) => factor.family.weights.asInstanceOf[Tensor1]
    case _ => {
      // TODO Make this more efficient by getting factor.family.weights when we can
      val valueTensor = new SingletonBinaryTensor1(edge1.variable.domain.size, 0)
      val result = new DenseTensor1(edge1.variable.domain.size)
      for (i <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex = i
       result(i) = factor.scoreValues(valueTensor)
      }
      result
    }
  }
  def addExpectationInto(t:Tensor, f:Double): Unit = t.+=(calculateMarginal, f)
}

// A BPFactor1 with underlying model Factor2, with the first neighbor varying and the second neighbor constant 
class BPFactor1Factor2(val factor: Factor2[DiscreteVar,DiscreteTensorVar], edge1:BPEdge) extends BPFactor1(edge1) {
  val scores: Tensor1 = {
    val valueTensor = new SingletonBinaryLayeredTensor2(edge1.variable.domain.size, factor._2.domain.dimensionDomain.size, 0, factor._2.value.asInstanceOf[Tensor1])
    val result = new DenseTensor1(edge1.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      result(i) = factor.scoreValues(valueTensor)
    }
    result
  }
  def addExpectationInto(t:Tensor, f:Double): Unit = {
    val marginal = calculateMarginal
    // TODO One could imagine more efficient implementations that for DenseTensor t and SparseIndexedTensor factor._2.value, we iterator through the later only once -akm
    val valueTensor = new SingletonBinaryLayeredTensor2(edge1.variable.domain.size, factor._2.domain.dimensionDomain.size, 0, factor._2.value.asInstanceOf[Tensor1])
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      t.+=(valueTensor, marginal(i) * f)
    }
  }
}


// An abstract class for BPFactors that have 2 varying neighbors.  They may have additional constant neighbors.
abstract class BPFactor2(val edge1: BPEdge, val edge2: BPEdge) extends DiscreteMarginal2(edge1.bpVariable.variable, edge2.bpVariable.variable, null) with BPFactor {
  override def scores: Tensor2
  def calculateOutgoing1: Tensor
  def calculateOutgoing2: Tensor
  edge1.bpFactor = this; edge1.factorNeighborIndex = 0
  edge2.bpFactor = this; edge1.factorNeighborIndex = 1
  val edges = Seq(edge1, edge2)
  override def updateOutgoing(): Unit = { updateOutgoing1(); updateOutgoing2() }
  def updateOutgoing(e: BPEdge): Unit = e match { case this.edge1 => updateOutgoing1(); case this.edge2 => updateOutgoing2() }
  def updateOutgoing1(): Unit = edge1.messageFromFactor = calculateOutgoing1
  def updateOutgoing2(): Unit = edge2.messageFromFactor = calculateOutgoing2
  // TODO See about caching this when possible
  def calculateBeliefs: Tensor2 = {
    val result = new DenseTensor2(edge1.messageFromVariable.length, edge2.messageFromVariable.length)
    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size)
      result(i,j) = scores(i,j) + edge1.messageFromVariable(i) + edge2.messageFromVariable(j)
    result
  }
  override def calculateMarginal: Tensor2 = calculateBeliefs.expNormalized.asInstanceOf[Tensor2]
  override def proportions: Proportions2 = new NormalizedTensorProportions2(calculateMarginal, false)
}

trait BPFactor2SumProduct { this: BPFactor2 =>
  def calculateOutgoing1: Tensor = {
    val result = new DenseTensor1(edge1.variable.domain.size, Double.NegativeInfinity)
    for (i <- 0 until edge1.variable.domain.size; j <- 0 until edge2.variable.domain.size)
      result(i) = cc.factorie.maths.sumLogProb(result(i), scores(i,j) + edge2.messageFromVariable(j))
    result
  }
  def calculateOutgoing2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size, Double.NegativeInfinity)
    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size)
      result(j) = cc.factorie.maths.sumLogProb(result(j), scores(i,j) + edge1.messageFromVariable(i))
    result
  }
}

trait BPFactor2MaxProduct { this: BPFactor2 =>
  val edge1Max2 = new Array[Int](edge1.variable.domain.size) // The index value of edge2.variable that lead to the MaxProduct value for each index value of edge1.variable 
  var edge2Max1 = new Array[Int](edge2.variable.domain.size)
  def calculateOutgoing1: Tensor = {
    scores match {
      case scores:DenseTensor2 => {
        val result = new DenseTensor1(edge1.variable.domain.size, Double.NegativeInfinity)
        for (i <- 0 until edge1.variable.domain.size; j <- 0 until edge2.variable.domain.size) {
          val s = scores(i,j) + edge2.messageFromVariable(j)
          if (s > result(i)) { result(i) = s; edge1Max2(i) = j } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
        }
        result
      }
      case scores:SparseIndexedTensor2 => { // SparseTensor case
        val result = new DenseTensor1(scores.dim1, Double.NegativeInfinity) // TODO Consider a non-Dense tensor here
        for (element <- scores.activeElements2) {
          val i = element.index1; val j = element.index2
          val s = element.value + edge2.messageFromVariable(j)
          if (s > result(i)) { result(i) = s; edge1Max2(i) = j }
        }
        result
      }
    }
  }
  def calculateOutgoing2: Tensor = {
    val result = new DenseTensor1(edge2.variable.domain.size, Double.NegativeInfinity)
    for (j <- 0 until edge2.variable.domain.size; i <- 0 until edge1.variable.domain.size) {
      val s = scores(i,j) + edge1.messageFromVariable(i)
      if (s > result(j)) { result(j) = s; edge2Max1(j) = i } // Note that for a BPFactor3 we would need two such indices.  This is why they are stored in the BPFactor
    }
    result
  }
}

// A BPFactor2 with underlying model Factor2, with both neighbors varying
abstract class BPFactor2Factor2(val factor:Factor2[DiscreteVar,DiscreteVar], edge1:BPEdge, edge2:BPEdge) extends BPFactor2(edge1, edge2) {
  // TODO Consider making this calculate scores(i,j) on demand, with something like
  // val scores = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size, Double.NaN) { override def apply(i:Int) = if (_values(i).isNaN)... }
  val scores: Tensor2 = factor match {
    case factor:DotFamily#Factor if (factor.family.isInstanceOf[DotFamily]) => factor.family.weights.asInstanceOf[Tensor2]
    case _ => {
      // TODO Replace this with just efficiently getting factor.family.weights
      val valueTensor = new SingletonBinaryTensor2(edge1.variable.domain.size, edge2.variable.domain.size, 0, 0)
      val result = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size)
      for (i <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex1 = i
        for (j <- 0 until edge2.variable.domain.size) {
          valueTensor.singleIndex2 = j
          result(i, j) = factor.scoreValues(valueTensor)
        }
      }
      result
    }
  }
  def addExpectationInto(t:Tensor, f:Double): Unit = t.+=(calculateMarginal,f) // TODO Use a TensorAccumulator; add f:Double to TensorAccumulator
}

// A BPFactor2 with underlying model Factor3, having two varying neighbors and one constant neighbor
// Note that the varying neighbors are assumed to be factor._1 and factor._2, and the constant neighbor factor._3
abstract class BPFactor2Factor3(val factor:Factor3[DiscreteVar,DiscreteVar,DiscreteTensorVar], edge1:BPEdge, edge2:BPEdge) extends BPFactor2(edge1, edge2) {
  val scores: Tensor2 = {
    val valueTensor = new Singleton2LayeredTensor3(edge1.variable.domain.size, edge2.variable.domain.size, factor._3.domain.dimensionDomain.size, 0, 0, 1.0, 1.0, factor._3.value.asInstanceOf[Tensor1])
    val result = new DenseTensor2(edge1.variable.domain.size, edge2.variable.domain.size)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        result(i, j) = factor.scoreValues(valueTensor)
      }
    }
    result
  }
  def addExpectationInto(t:Tensor, f:Double): Unit = {
    val marginal = calculateMarginal
    val valueTensor = new Singleton2LayeredTensor3(edge1.variable.domain.size, edge2.variable.domain.size, factor._3.domain.dimensionDomain.size, 0, 0, 1.0, 1.0, factor._3.value.asInstanceOf[Tensor1])
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        t.+=(valueTensor, marginal(i,j) * f)
      }
    }
  }
}


class BPFactor3(val factor: Factor, val edge1: BPEdge, val edge2: BPEdge, val edge3:BPEdge, val ring: BPRing) extends DiscreteMarginal3(edge1.bpVariable.variable, edge2.bpVariable.variable, edge3.bpVariable.variable, null) with BPFactor {
  edge1.bpFactor = this
  edge2.bpFactor = this
  edge3.bpFactor = this
  val edges = Seq(edge1, edge2, edge3)
  def scores: Tensor3 = throw new Error("Not yet implemented")
  def updateOutgoing(e: BPEdge): Unit = e match {
    case this.edge1 => updateOutgoing1()
    case this.edge2 => updateOutgoing2()
    case this.edge3 => updateOutgoing3()
  } 
  def updateOutgoing1(): Unit = edge1.messageFromFactor = calculateOutgoing1
  def updateOutgoing2(): Unit = edge2.messageFromFactor = calculateOutgoing2
  def updateOutgoing3(): Unit = edge3.messageFromFactor = calculateOutgoing3
  def calculateOutgoing1: Tensor = throw new Error("Not yet implemented")
  def calculateOutgoing2: Tensor = throw new Error("Not yet implemented")
  def calculateOutgoing3: Tensor = throw new Error("Not yet implemented")
  def calculateBeliefs: Tensor3 = throw new Error("Not yet implemented")
  override def calculateMarginal: Tensor3 = calculateBeliefs.expNormalized.asInstanceOf[Tensor3]
  override def proportions: Proportions3 = throw new Error("Not yet implemented") // Must be overridden to return "new NormalizedTensorProportions{1,2,3,4}(calculateMarginal, false)"
  def addExpectationInto(t:Tensor, f:Double): Unit = throw new Error("Not yet implemented")
}



class BPSummary(val ring:BPRing, val model:Model[Variable]) extends Summary[DiscreteMarginal] {
  private val _bpFactors = new LinkedHashMap[Factor, BPFactor]
  private val _bpVariables = new LinkedHashMap[DiscreteTensorVar, BPVariable1]
  def bpVariable(v:DiscreteVar): BPVariable1 = _bpVariables.getOrElseUpdate(v, ring.newBPVariable(v))
  /** Create a new BPSummary to infer the given varying variables.  Other non-varying neighbors of relevant factors are considered constant. */
  def this(varying:Iterable[DiscreteVar], ring:BPRing, model:Model[Variable]) = {
    this(ring, model)
    val varyingSet = varying.toSet
    for (factor <- model.factors(varying)) _bpFactors(factor) = ring.newBPFactor(factor, varyingSet, this)
  }
  def this(varying:Iterable[DiscreteVar], model:Model[Variable]) = this(varying, BPSumProductRing, model)
  def bpFactors: Iterable[BPFactor] = _bpFactors.values
  def bpVariables: Iterable[BPVariable1] = _bpVariables.values
  def marginals = _bpFactors.values ++ _bpVariables.values
  def marginal(vs: Variable*): DiscreteMarginal = vs.size match {
    case 1 => _bpVariables(vs.head.asInstanceOf[DiscreteVar])
    case 2 => {val factors = _bpFactors.values.filter(f => f.variables.toSet == vs.toSet); factors.head} // Need to actually combine if more than one
  }
  def marginal(v: DiscreteVar): BPVariable1 = _bpVariables(v)
  override def marginal(f: Factor): BPFactor = _bpFactors(f)
  override def logZ: Double = _bpFactors.values.head.calculateLogZ
  override def setToMaximize(implicit d:DiffList = null): Unit = ring match {
    case BPSumProductRing => bpVariables.foreach(_.setToMaximize(d))
    case BPMaxProductRing => throw new Error("Not yet implemented.  Note: If you're using a chain model BP.inferChainMax already sets the variables to max values.")
    case _ => throw new Error("Not yet implemented arbitrary backwards pass.")
  }
}

object BPUtil {
  
  def bfs(varying: Set[DiscreteVar], root: BPVariable, checkLoops: Boolean): Seq[(BPEdge, Boolean)] = {
    val visited: HashSet[BPEdge] = new HashSet
    val result = new ArrayBuffer[(BPEdge, Boolean)]
    val toProcess = new Queue[(BPEdge, Boolean)]
    root.edges foreach (e => toProcess += Pair(e, true))
    while (!toProcess.isEmpty) {
      val (edge, v2f) = toProcess.dequeue()
      if (!checkLoops || !visited(edge)) {
        visited += edge
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
  def inferTreeSum(varying:Set[DiscreteVar], model:Model[Variable], root: DiscreteVar = null): BPSummary = {
    val summary = new BPSummary(varying, BPSumProductRing, model)
    val _root = if (root != null) summary.bpVariable(root) else summary.bpVariables.head
    val bfsSeq = BPUtil.bfs(varying, _root, checkLoops = true)
    BPUtil.sendAccordingToOrdering(bfsSeq.reverse)
    BPUtil.sendAccordingToOrdering(bfsSeq)
    summary
  }
  // TODO: add inferTreewiseMax and associated test
  def inferSingle(v:MutableDiscreteVar[_<:DiscreteValue], model:Model[Variable]): BPSummary = {
    val summary = new BPSummary(Seq(v), BPSumProductRing, model)
    summary.bpFactors.foreach(_.updateOutgoing())
    summary
  }
  // Works specifically on a linear-chain with factors Factor2[Label,Features] and Factor2[Label1,Label2]
  def inferChainMax(varying:Seq[DiscreteVar], model:Model[Variable]): BPSummary = {
    val summary = new BPSummary(varying, BPMaxProductRing, model)
    varying.size match {
      case 0 => {}
      case 1 => { summary.bpFactors.foreach(_.updateOutgoing()); summary.bpVariables.head.setToMaximize(null) }
      case _ => {
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1])
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2 with BPFactor2MaxProduct]]
        //println("BP.inferChainMax  markovBPFactors.size = "+markovBPFactors.size)
        // Send all messages from observations to labels in parallel
        obsBPFactors.foreach(_.updateOutgoing())
        // Send forward Viterbi messages
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1) // send message from neighbor1 to factor
          f.edge1.bpFactor.updateOutgoing(f.edge2)   // send message from factor to neighbor2
        }
        // Do Viterbi backtrace, setting label values
        var maxIndex = markovBPFactors.last.edge2.bpVariable.proportions.maxIndex // TODO We don't actually need to expNormalize here; save computation by avoiding this
        markovBPFactors.last.edge2.variable.asInstanceOf[MutableDiscreteVar[_]] := maxIndex
        for (f <- markovBPFactors.reverse) {
          maxIndex = f.edge2Max1(maxIndex)
          f.edge1.variable.asInstanceOf[MutableDiscreteVar[_]] := maxIndex
        }
      }
    }
    summary
  }
  
  // Works specifically on a linear-chain with factors Factor2[Label,Features] and Factor2[Label1,Label2]
  def inferChainSum(varying:Seq[DiscreteVar], model:Model[Variable]): BPSummary = {
    val summary = new BPSummary(varying, BPSumProductRing, model)
    varying.size match {
      case 0 => {}
      case 1 => summary.bpFactors.foreach(_.updateOutgoing())
      case _ => {
        val obsBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor1])
        val markovBPFactors = summary.bpFactors.toSeq.filter(_.isInstanceOf[BPFactor2]).asInstanceOf[Seq[BPFactor2]]
        assert(obsBPFactors.size + markovBPFactors.size == summary.bpFactors.size)
        // Send all messages from observations to labels in parallel
        obsBPFactors.foreach(_.updateOutgoing())
        // Send forward messages
        for (f <- markovBPFactors) {
          f.edge1.bpVariable.updateOutgoing(f.edge1) // send message from neighbor1 to factor
          f.edge1.bpFactor.updateOutgoing(f.edge2)   // send message from factor to neighbor2
        }
        // Send backward messages
        for (f <- markovBPFactors.reverse) {
          f.edge2.bpVariable.updateOutgoing(f.edge2) // send message from neighbor1 to factor
          f.edge2.bpFactor.updateOutgoing(f.edge1)   // send message from factor to neighbor2
        }
        // Update marginals    //summary.bpVariables.foreach(_.updateProportions)
        // TODO Also update BPFactor marginals
      }
    }
    summary
  }
  
}

trait InferByBP extends Infer {
  override def infer(variables:Iterable[Variable], model:Model[Variable], summary:Summary[Marginal] = null): Option[BPSummary] = None
}

object InferByBPTreeSum extends InferByBP {
  override def infer(variables:Iterable[Variable], model:Model[Variable], summary:Summary[Marginal] = null): Option[BPSummary] = variables match {
    case variables:Iterable[DiscreteVar] if (variables.forall(_.isInstanceOf[DiscreteVar])) => Some(apply(variables.toSet, model))
  }
  def apply(varying:Set[DiscreteVar], model:Model[Variable]): BPSummary = BP.inferTreeSum(varying, model)
}

object InferByBPChainSum extends InferByBP {
  override def infer(variables:Iterable[Variable], model:Model[Variable], summary:Summary[Marginal] = null): Option[BPSummary] = variables match {
    case variables:Seq[DiscreteVar] if (variables.forall(_.isInstanceOf[DiscreteVar])) => Some(apply(variables, model))
    case _ => None
  }
  def apply(varying:Seq[DiscreteVar], model:Model[Variable]): BPSummary = BP.inferChainSum(varying, model)
}

//object InferByBPIndependent extends InferByBP {
//  override def infer(variables:Iterable[Variable], model:Model[Variable], summary:Summary[Marginal] = null): Option[BPSummary] = variables match {
//    case variables:Seq[DiscreteVar] if (variables.forall(_.isInstanceOf[DiscreteVar])) => Some(apply(variables, model))
//    case _ => None
//  }
//  def apply(varying:Seq[DiscreteVar], model:Model[Variable]): BPSummary = BP.inferChainSum(varying, model)
//}
