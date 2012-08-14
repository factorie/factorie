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
import scala.collection.mutable.{ArrayBuffer, HashMap}

trait BPRing {
  // multiply two messages
  def *(t1: Tensor, t2: Tensor): Tensor

  def *=(t1: Tensor, t2: Tensor): Unit

  // divide two messages
  def /(t1: Tensor, t2: Tensor): Tensor

  def /=(t1: Tensor, t2: Tensor): Unit
}

object BPSumProductRing extends BPRing {
  def *(t1: Tensor, t2: Tensor): Tensor = t1 + t2

  def *=(t1: Tensor, t2: Tensor): Unit = t1 += t2

  def /(t1: Tensor, t2: Tensor): Tensor = t1 - t2

  def /=(t1: Tensor, t2: Tensor): Unit = t1 -= t2
}

class BPEdge(val bpVariable: BPVariable) {
  var bpFactor: BPFactor = null
  var factorNeighborIndex: Int = -1

  // ???
  def variable = bpVariable.variable

  def factor = bpFactor.factor

  var messageFromVariable: Tensor = bpVariable.scores.blankCopy
  var messageFromFactor: Tensor = bpVariable.scores.blankCopy
  bpVariable.edges += this
}

class BPVariable(val variable: DiscreteVar, val ring: BPRing, val scores: Tensor1) extends DiscreteMarginal1(variable, null) {
  val edges = new ArrayBuffer[BPEdge]

  def computeOutgoing(e: BPEdge): Tensor = {
    val result = scores.blankCopy
    for (edge <- edges; if edge != e) {
      ring *=(result, edge.messageFromFactor)
    }
    result
  }

  def updateOutgoing(e: BPEdge): Unit = e.messageFromVariable = computeOutgoing(e)

  def updateOutgoing: Unit = edges.foreach(updateOutgoing(_))

  override def proportions = {
    scores.zero()
    for (e <- edges) {
      ring *=(scores, e.messageFromFactor)
    }
    scores.expNormalize()
    new DenseProportions1(scores)
  }
}

// class BPVariable2 would be used for cluster graphs

trait BPFactor extends DiscreteMarginal {
  def factor: Factor

  def edges: Seq[BPEdge]

  def ring: BPRing

  def updateOutgoing(e: BPEdge): Unit

  def updateOutgoing: Unit = edges.foreach(updateOutgoing(_))
}

class BPFactor1(val factor: Factor, val edge1: BPEdge, val ring: BPRing, val scores: Tensor1) extends DiscreteMarginal1(edge1.bpVariable.variable, null) with BPFactor {
  edge1.bpFactor = this
  val edges = Seq(edge1)

  def updateOutgoing(e: BPEdge): Unit = e match {
    case edge1 => updateOutgoing1
  }

  def updateOutgoing1: Unit = edge1.messageFromFactor = calculateOutgoing1

  lazy val calculateOutgoing1: Tensor = {
    val valueTensor = new MutableSingletonBinaryTensor1(edge1.variable.domain.size, 0)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex = i
      var s = factor.scoreValueTensor(valueTensor)
      //s += edge1.messageFromVariable(i)
      scores(i) = s
    }
    scores
  }
}

class BPFactor2(val factor: Factor, val edge1: BPEdge, val edge2: BPEdge, val ring: BPRing, val scores: Tensor2) extends DiscreteMarginal2(edge1.bpVariable.variable, edge2.bpVariable.variable, null) with BPFactor {
  edge1.bpFactor = this
  edge2.bpFactor = this
  val edges = Seq(edge1, edge2)

  def updateOutgoing(e: BPEdge): Unit = {
    if (e eq edge1) updateOutgoing1
    else if (e eq edge2) updateOutgoing2
    else throw new Error
  }

  def updateOutgoing1: Unit = edge1.messageFromFactor = calculateOutgoing1

  def updateOutgoing2: Unit = edge2.messageFromFactor = calculateOutgoing2

  def calculateOutgoing1: Tensor = {
    val valueTensor = new MutableSingletonBinaryTensor2(edge1.variable.domain.size, edge2.variable.domain.size, 0, 0)
    for (i <- 0 until edge1.variable.domain.size) {
      valueTensor.singleIndex1 = i
      var s = 0.0
      for (j <- 0 until edge2.variable.domain.size) {
        valueTensor.singleIndex2 = j
        s += factor.scoreValueTensor(valueTensor)
        s += edge2.messageFromVariable(j)
      }
      scores(i) = s
    }
    scores
  }

  // do the work of normalization
  def calculateOutgoing2: Tensor = {
    val valueTensor = new MutableSingletonBinaryTensor2(edge1.variable.domain.size, edge2.variable.domain.size, 0, 0)
    for (i <- 0 until edge2.variable.domain.size) {
      valueTensor.singleIndex2 = i
      var s = 0.0
      for (j <- 0 until edge1.variable.domain.size) {
        valueTensor.singleIndex1 = j
        s += factor.scoreValueTensor(valueTensor)
        s += edge1.messageFromVariable(j)
      }
      scores(i) = s
    }
    scores
  }
}

class BPSummary(varying: Set[DiscreteVar], model: Model) extends Summary[DiscreteMarginal] {
  private val _bpFactors = new HashMap[Factor, BPFactor]
  private val _bpVariables = new HashMap[DiscreteTensorVar, BPVariable]

  def bpVariable(v: DiscreteVar): BPVariable = {
    _bpVariables.getOrElseUpdate(v, new BPVariable(v, BPSumProductRing, new DenseTensor1(v.domain.size)))
  }

  for (factor <- model.factors(varying)) {
    val factorVarying = factor.variables.filter(_ match {case v: DiscreteVar => varying.contains(v); case _ => false}).asInstanceOf[Seq[DiscreteVar]]
    val edges = factorVarying.map(v => new BPEdge(bpVariable(v)))
    val bpFactor = edges.size match {
      case 1 => new BPFactor1(factor, edges(0), BPSumProductRing, new DenseTensor1(edges(0).variable.domain.size))
      case 2 => new BPFactor2(factor, edges(0), edges(1), BPSumProductRing, new DenseTensor2(edges(0).variable.domain.size, edges(1).variable.domain.size))
    }
    _bpFactors(factor) = bpFactor
  }

  def bpFactors: Iterable[BPFactor] = _bpFactors.values

  def bpVariables: Iterable[BPVariable] = _bpVariables.values

  def marginals = _bpFactors.values ++ _bpVariables.values

  def marginal(vs: Variable*): DiscreteMarginal = vs.size match {
    case 1 => _bpVariables(vs.head.asInstanceOf[DiscreteVar])
    case 2 => {val factors = _bpFactors.values.filter(f => f.variables.toSet == vs.toSet); factors.head} // Need to actually combine if more than one
  }

  def marginal(v: DiscreteVar) = _bpVariables(v)

  def marginal(f: Factor) = _bpFactors(f)
}

object LoopyBP {
  def infer(summary: BPSummary, numIterations: Int = 10) {
    for (iter <- 0 to numIterations)
      for (bpf: BPFactor <- summary.bpFactors) {
        // get all the incoming messages
        for (e <- bpf.edges) {
          e.bpVariable.updateOutgoing(e)
        }
        // send messages
        for (e <- bpf.edges) {
          e.bpFactor.updateOutgoing(e)
        }
      }
  }
}
