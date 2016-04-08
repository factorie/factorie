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
package cc.factorie.app.chain

import cc.factorie.la._
import cc.factorie.model._
import cc.factorie.optimize.Example
import cc.factorie.util.DoubleAccumulator
import cc.factorie.variable.{Var, DiscreteDomain, DiscreteVar}

import scala.collection.mutable.ArrayBuffer

/** These classes provide a lightweight, flexible version of chain models that can incorporate arbitrary features for both
  * edges and nodes. For example, sparse edge weights or low-dimensional embeddings for labels and cliques could be used.
  * Serialization can be done with BinarySerializer and inference with ChainHelper, to keep things simple.
  */

class BasicChainModel[Y <: DiscreteVar](val domain: DiscreteDomain, val featureDomain: DiscreteDomain, val feats: Y => Tensor)
  extends LiteChainModel[Y] {
  val nodeWeights: Weights2 = Weights(new DenseTensor2(featureDomain.size, domain.size))
  val edgeWeights: Weights2 = Weights(new DenseTensor2(domain.size, domain.size))
  def nodeFeatures(node: Y, y: Y#Value): Tensor = feats(node) outer y
  def edgeFeatures(left: Y, right: Y, y1: Y#Value, y2: Y#Value): Tensor = y1 outer y2
}

class SparseEdgeFeatureChainModel[Y <: DiscreteVar](
  val domain: DiscreteDomain, val nodeFeatureDomain: DiscreteDomain, val edgeFeatureDomain: DiscreteDomain,
  val nodeFeats: Y => Tensor, val edgeFeats: (Y, Y) => Tensor)
  extends LiteChainModel[Y] {
  val nodeWeights: Weights2 = Weights(new DenseTensor2(nodeFeatureDomain.size, domain.size))
  val edgeWeights: Weights1 = Weights(new SparseIndexedTensor1(domain.size * domain.size * edgeFeatureDomain.size))
  def nodeFeatures(node: Y, y: Y#Value): Tensor = nodeFeats(node) outer y
  def edgeFeatures(left: Y, right: Y, y1: Y#Value, y2: Y#Value): Tensor = {
    // Take advantage of outer product logic in Sparse1 to map things to good indices
    // This should hit the Sparse outer Sparse case of the pattern match
    val t = new SparseIndexedTensor1(edgeWeights.value.dim1)
    t += edgeFeats(left, right) outer (y1 outer y2)
    t
  }
}

abstract class LiteChainModel[Y <: DiscreteVar] extends Model with Parameters {
  def domain: DiscreteDomain
  def edgeWeights: Weights
  def nodeWeights: Weights

  def edgeFeatures(left: Y, right: Y, y1: Y#Value, y2: Y#Value): Tensor
  def nodeFeatures(node: Y, y: Y#Value): Tensor

  def potentials(obs: Seq[Y]): ChainCliqueValues =
    LiteChainModelHelper.makePotentials(domain.size, obs.size, nodeWeights.value, edgeWeights.value,
      (i, j) => nodeFeatures(obs(i), _idxv(j)),
      (i, j, k) => edgeFeatures(obs(i), obs(i + 1), _idxv(j), _idxv(k)))

  def _idxv(i: Int): Y#Value = domain(i).asInstanceOf[Y#Value]

  override def factors(variables: Iterable[Var]): Iterable[Factor] = {
    val out = new ArrayBuffer[Factor]
    variables.toVector.foreach({
       case v: Y @unchecked if v.isInstanceOf[Y]  =>
        out += new LiteNodeFactor1(v)
    })
    if (variables.size > 1)
      variables.toVector.map({ case v: Y @unchecked if v.isInstanceOf[Y] => v}).sliding(2)
        .map({ case Vector(v1, v2) => new LiteEdgeFactor2(v1, v2)}).foreach(out +=)
    out
  }
  class LiteNodeFactor1(v: Y) extends DotFactor1(v) {
    override def weights: Weights = nodeWeights
    override def statistics(y: Y#Value): Tensor = nodeFeatures(v, y)
  }
  class LiteEdgeFactor2(left: Y, right: Y) extends DotFactor2(left, right) {
    override def weights: Weights = edgeWeights
    override def statistics(y1: Y#Value, y2: Y#Value): Tensor = edgeFeatures(left, right, y1, y2)
  }
}

class LiteChainModelExample[Y <: DiscreteVar](model: LiteChainModel[Y], inst: Seq[Y], label: Y => Y#Value) extends Example {
  override def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    def idxv(i: Int) = model._idxv(i)
    val scores = model.potentials(inst)
    val ChainForwardBackwardResults(logZ, alphas, betas, _) = ChainHelper.inferFast(scores)
    if (value ne null)
      value.accumulate(-logZ)
    val numStates = model.domain.size
    for (n <- 0 until inst.size) {
      val prevAlpha = if (n >= 1) alphas(n - 1) else null.asInstanceOf[Tensor1]
      val curAlpha = alphas(n)
      val curBeta = betas(n)
      val curNodeScores = scores.localValues(n)
      val curEdgeScores = if (n >= 1) scores.transitionValues(n - 1) else null.asInstanceOf[Tensor2]
      val curTargetState = label(inst(n)).intValue
      val prevTargetState = if (n >= 1) label(inst(n - 1)).intValue else -1
      if (value ne null) {
        value.accumulate(curNodeScores(curTargetState))
        if (n >= 1) value.accumulate(curEdgeScores(prevTargetState * numStates + curTargetState))
      }
      if (gradient ne null) {
        gradient.accumulate(model.nodeWeights, model.nodeFeatures(inst(n), idxv(curTargetState)), 1.0)
        val localMarginal = curAlpha + curBeta
        localMarginal.expNormalize(logZ)
        for (s <- 0 until numStates) {
          val sf = model.nodeFeatures(inst(n), idxv(s))
          val m = localMarginal(s)
          gradient.accumulate(model.nodeWeights, sf, -1.0 * m)
        }
        if (n >= 1) {
          gradient.accumulate(model.edgeWeights, model.edgeFeatures(inst(n - 1), inst(n), idxv(prevTargetState), idxv(curTargetState)), 1.0)
          for (s1 <- 0 until numStates; s2 <- 0 until numStates) {
            val ssf = model.edgeFeatures(inst(n - 1), inst(n), idxv(s1), idxv(s2))
            val m = math.exp(prevAlpha(s1) + curEdgeScores(s1 * numStates + s2) + curBeta(s2) + curNodeScores(s2) - logZ)
            gradient.accumulate(model.edgeWeights, ssf, -1.0 * m)
          }
        }
      }
    }
  }
}

object LiteChainModelHelper {
  def makePotentials(numStates: Int, numNodes: Int, nodeWeights: Tensor, edgeWeights: Tensor,
    nodeFeatures: (Int, Int) => Tensor, edgeFeatures: (Int, Int, Int) => Tensor): ChainCliqueValues = {
    val nodePotentials = new ArrayBuffer[DenseTensor1]
    val edgePotentials = new ArrayBuffer[Tensor2]
    val numEdges = numNodes - 1
    for (n <- 0 until numNodes) {
      val nodePotential = new DenseTensor1(numStates)
      for (y <- 0 until numStates) {
        nodePotential(y) = nodeWeights dot nodeFeatures(n, y)
      }
      nodePotentials += nodePotential
    }
    for (e <- 0 until numEdges) {
      val edgePotential = new DenseTensor2(numStates, numStates)
      for (y1 <- 0 until numStates; y2 <- 0 until numStates) {
        edgePotential(y1, y2) = edgeWeights dot edgeFeatures(e, y1, y2)
      }
      edgePotentials += edgePotential
    }
    ChainCliqueValues(nodePotentials, edgePotentials)
  }
}