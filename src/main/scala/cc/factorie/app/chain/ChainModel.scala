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

package cc.factorie.app.chain

import cc.factorie._
import Factorie._
import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import java.io.{InputStream, OutputStream, DataInputStream, DataOutputStream}
import cc.factorie.util.{DoubleAccumulator, BinarySerializer}
import cc.factorie.variable._
import scala.reflect.ClassTag
import cc.factorie.Factorie.DotFamilyWithStatistics3
import cc.factorie.Factorie.Parameters
import cc.factorie.Factorie.DotFamilyWithStatistics2
import cc.factorie.Factorie.Factor
import cc.factorie.la.{SparseIndexedTensor1, WeightsMapAccumulator}
import cc.factorie.DenseTensor1
import cc.factorie.Factorie.CategoricalVectorDomain
import cc.factorie.Factorie.Var
import cc.factorie.Factorie.CategoricalDomain
import cc.factorie.Model
import cc.factorie.Factorie.DotFamilyWithStatistics1



//TODO We should add the ability to explictly permit and forbid label transitions
class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget, Features<:CategoricalVectorVar[String], Token<:Observation[Token]]
(val labelDomain: CategoricalDomain[String],
  val featuresDomain: CategoricalVectorDomain[String],
  val labelToFeatures: Label => Features,
  val labelToToken: Label => Token,
  val tokenToLabel: Token => Label)
  (implicit lm: ClassTag[Label], fm: ClassTag[Features], tm: ClassTag[Token])
  extends Model with Parameters {
  self =>
  val labelClass = lm.runtimeClass
  val featureClass = fm.runtimeClass
  val tokenClass = tm.runtimeClass
  val bias = new DotFamilyWithStatistics1[Label] {
    factorName = "Label"
    val weights = Weights(new la.DenseTensor1(labelDomain.size))
  }
  val obs = new DotFamilyWithStatistics2[Features, Label] {
    factorName = "Label,Token"
    val weights = Weights(new la.DenseTensor2(featuresDomain.dimensionSize, labelDomain.dimensionSize))
  }
  val markov = new DotFamilyWithStatistics2[Label, Label] {
    factorName = "Label,Label"
    val weights = Weights(new la.DenseTensor2(labelDomain.size, labelDomain.size))
  }
  val obsmarkov = new DotFamilyWithStatistics3[Label, Label, Features] {
    factorName = "Label,Label,Token"
    val weights = Weights(if (useObsMarkov) new la.DenseTensor3(labelDomain.size, labelDomain.size, featuresDomain.dimensionSize) else new la.DenseTensor3(1, 1, 1))
  }
  var useObsMarkov = false

  def serialize(stream: OutputStream) {
    val dstream = new DataOutputStream(stream)
    BinarySerializer.serialize(featuresDomain, dstream)
    BinarySerializer.serialize(labelDomain, dstream)
    BinarySerializer.serialize(this, dstream)
    dstream.close()
  }

  def deserialize(stream: InputStream) {
    val dstream = new DataInputStream(stream)
    BinarySerializer.deserialize(featuresDomain, dstream)
    BinarySerializer.deserialize(labelDomain, dstream)
    BinarySerializer.deserialize(this, dstream)
    dstream.close()
  }

  def factors(variables: Iterable[Var]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    variables match {
      case labels: Iterable[Label] if variables.forall(v => labelClass.isAssignableFrom(v.getClass)) =>
        var prevLabel: Label = null.asInstanceOf[Label]
        for (label <- labels) {
          result += bias.Factor(label)
          result += obs.Factor(labelToFeatures(label), label)
          if (prevLabel ne null) {
            result += markov.Factor(prevLabel, label)
            if (useObsMarkov) result += obsmarkov.Factor(prevLabel, label, labelToFeatures(label))
          }
          prevLabel = label
        }
    }
    result
  }

  override def factors(v: Var) = v match {
    case label: Label if label.getClass eq labelClass => {
      val result = new ArrayBuffer[Factor](4)
      result += bias.Factor(label)
      result += obs.Factor(labelToFeatures(label), label)
      val token = labelToToken(label)
      if (token.hasPrev) {
        result += markov.Factor(tokenToLabel(token.prev), label)
        if (useObsMarkov)
          result += obsmarkov.Factor(tokenToLabel(token.prev), label, labelToFeatures(label))
      }
      if (token.hasNext) {
        result += markov.Factor(label, tokenToLabel(token.next))
        if (useObsMarkov)
          result += obsmarkov.Factor(label, tokenToLabel(token.next), labelToFeatures(tokenToLabel(token.next)))
      }
      result
    }
  }

  case class ViterbiResults(mapScore: Double, mapValues: Array[Int], localScores: Array[DenseTensor1])
  case class InferenceResults(logZ: Double, alphas: Array[DenseTensor1], betas: Array[DenseTensor1], localScores: Array[DenseTensor1])

  def viterbiFast(varying: Seq[Label], addToLocalScoresOpt: Option[Array[Tensor1]] = None): ViterbiResults = {
    assert(!useObsMarkov, "obsMarkov factors not supported with efficient max-product")
    val markovScores = markov.weights.value
    val localScores = getLocalScores(varying)
    addToLocalScoresOpt.foreach(l => (0 until varying.length).foreach(i => localScores(i) += l(i)))

    val costs = Array.fill(varying.size)(new DenseTensor1(markovScores.dim1, Double.NegativeInfinity))
    val backPointers = Array.fill(varying.size)(Array.fill[Int](markovScores.dim1)(-1))

    costs(0) := localScores(0)

    val domainSize = markovScores.dim1

    var i = 1
    while (i < varying.size) {
      val curLocalScores = localScores(i)
      val curCost = costs(i)
      val curBackPointers = backPointers(i)
      val prevCost = costs(i - 1)
      var vi = 0
      while (vi < domainSize) {
        var maxScore = Double.NegativeInfinity
        var maxIndex = -1
        var vj = 0
        while (vj < domainSize) {
          val curScore = markovScores(vj, vi) + prevCost(vj) + curLocalScores(vi)
          if (curScore > maxScore) {
            maxScore = curScore
            maxIndex = vj
          }
          vj += 1
        }
        curCost(vi) = maxScore
        curBackPointers(vi) = maxIndex
        vi += 1
      }
      i += 1
    }

    val mapValues = Array.fill[Int](varying.size)(0)
    mapValues(mapValues.size - 1) = costs.last.maxIndex
    var j = mapValues.size - 2
    while (j >= 0) {
      mapValues(j) = backPointers(j + 1)(mapValues(j + 1))
      j -= 1
    }

    ViterbiResults(costs.last.max, mapValues, localScores)
  }

  def maximize(vars: Seq[Label])(implicit d: DiffList): Unit = {
    val result = viterbiFast(vars)
    for (i <- 0 until vars.length) vars(i).set(result.mapValues(i))
  }

  def getHammingLossScores(varying: Seq[Label]): Array[Tensor1] = {
     val localScores = new Array[Tensor1](varying.size)
     val domainSize = localScores.head.size
     for ((v, i) <- varying.zipWithIndex) {
       localScores(i) = new DenseTensor1(domainSize)
       for (wrong <- 0 until domainSize if wrong != v.targetIntValue)
         localScores(i)(wrong) += 1.0
     }
     localScores
   }

  def getLocalScores(varying: Seq[Label]): Array[DenseTensor1] = {
    val biasScores = bias.weights.value
    val obsWeights = obs.weights.value
    val a = Array.fill[DenseTensor1](varying.size)(null)
    var i = 0
    while (i < varying.length) {
      val scores = obsWeights.leftMultiply(labelToFeatures(varying(i)).value.asInstanceOf[Tensor1]).asInstanceOf[DenseTensor1]
      scores += biasScores
      a(i) = scores
      i += 1
    }
    a
  }

  def inferFast(varying: Seq[Label], addToLocalScoresOpt: Option[Array[Tensor1]] = None): InferenceResults = {
    assert(!useObsMarkov, "obsMarkov factors not supported with efficient sum-product")
    val markovScores = markov.weights.value
    val localScores = getLocalScores(varying)
    addToLocalScoresOpt.foreach(l => (0 until varying.length).foreach(i => localScores(i) += l(i)))

    val alphas = Array.fill(varying.size)(new DenseTensor1(markovScores.dim1, Double.NegativeInfinity))
    val betas = Array.fill(varying.size)(new DenseTensor1(markovScores.dim1, Double.NegativeInfinity))

    alphas(0) := localScores(0)

    var i = 1
    val d1 = markovScores.dim1
    val tmpArray = Array.fill(markovScores.dim1)(0.0)
    while (i < varying.size) {
      val ai = alphas(i)
      val aim1 = alphas(i - 1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          tmpArray(vj) = markovScores(vj, vi) + aim1(vj)
          vj += 1
        }
        ai(vi) = maths.sumLogProbs(tmpArray)
        vi += 1
      }
      alphas(i) += localScores(i)
      i += 1
    }

    betas.last.zero()

    i = varying.size - 2
    while (i >= 0) {
      val bi = betas(i)
      val bip1 = betas(i + 1)
      val lsp1 = localScores(i + 1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          tmpArray(vj) = markovScores(vi, vj) + bip1(vj) + lsp1(vj)
          vj += 1
        }
        bi(vi) = maths.sumLogProbs(tmpArray)
        vi += 1
      }
      i -= 1
    }

    val logZ = maths.sumLogProbs(alphas.last)
    InferenceResults(logZ, alphas, betas, localScores)
  }

  class ChainStructuredSVMExample(varying: Seq[Label]) extends ChainViterbiExample(varying, () => Some(getHammingLossScores(varying)))

  class ChainViterbiExample(varying: Seq[Label], addToLocalScoresOpt: () => Option[Array[Tensor1]] = () => None) extends Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
      if (varying.length == 0) return
      val ViterbiResults(mapScore, mapValues, localScores) = viterbiFast(varying, addToLocalScoresOpt())
      val transScores = markov.weights.value
      if (value ne null)
        value.accumulate(-mapScore)
      val domainSize = transScores.dim1
      val transGradient = new DenseTensor2(domainSize, domainSize)
      val len = varying.length
      var i = 0
      while (i < len) {
        val curLabel = varying(i)
        val prevLabel = if (i >= 1) varying(i - 1) else null.asInstanceOf[Label]
        val curLocalScores = localScores(i)
        val curTargetIntValue = curLabel.targetIntValue
        val prevTargetIntValue = if (i >= 1) prevLabel.targetIntValue else -1
        val curPredIntValue = mapValues(i)
        val prevPredIntValue = if (i >= 1) mapValues(i - 1) else -1
        if (value ne null) {
          value.accumulate(curLocalScores(curTargetIntValue))
          if (i >= 1) value.accumulate(transScores(prevTargetIntValue, curTargetIntValue))
        }
        if (gradient ne null) {
          val localMarginal = new SparseIndexedTensor1(domainSize)
          localMarginal(curPredIntValue) += -1
          localMarginal(curTargetIntValue) += 1
          gradient.accumulate(bias.weights, localMarginal)
          gradient.accumulate(obs.weights, labelToFeatures(curLabel).value outer localMarginal)
          if (i >= 1) {
            transGradient(prevTargetIntValue, curTargetIntValue) += 1
            transGradient(prevPredIntValue, curPredIntValue) += -1
          }
        }
        i += 1
      }
      if (gradient ne null)
        gradient.accumulate(markov.weights, transGradient)
    }
  }

  class ChainLikelihoodExample(varying: Seq[Label], addToLocalScoresOpt: () => Option[Array[Tensor1]] = () => None) extends Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
      if (varying.length == 0) return
      val InferenceResults(logZ, alphas, betas, localScores) = inferFast(varying, addToLocalScoresOpt())
      val transScores = markov.weights.value
      if (value ne null)
        value.accumulate(-logZ)
      val domainSize = transScores.dim1
      val transGradient = new DenseTensor2(domainSize, domainSize)
      val len = varying.length
      var i = 0
      while (i < len) {
        val curLabel = varying(i)
        val prevLabel = if (i >= 1) varying(i - 1) else null.asInstanceOf[Label]
        val prevAlpha = if (i >= 1) alphas(i - 1) else null.asInstanceOf[Tensor1]
        val curAlpha = alphas(i)
        val curBeta = betas(i)
        val curLocalScores = localScores(i)
        val curTargetIntValue = curLabel.targetIntValue
        val prevTargetIntValue = if (i >= 1) prevLabel.targetIntValue else -1
        if (value ne null) {
          value.accumulate(curLocalScores(curTargetIntValue))
          if (i >= 1) value.accumulate(transScores(prevTargetIntValue, curTargetIntValue))
        }
        if (gradient ne null) {
          val localMarginal = curAlpha + curBeta
          localMarginal.expNormalize(logZ)
          localMarginal *= -1
          localMarginal(curTargetIntValue) += 1
          gradient.accumulate(bias.weights, localMarginal) // note
          gradient.accumulate(obs.weights, labelToFeatures(curLabel).value outer localMarginal)
          if (i >= 1) {
            var ii = 0
            while (ii < domainSize) {
              var jj = 0
              while (jj < domainSize) {
                transGradient(ii, jj) += -math.exp(prevAlpha(ii) + transScores(ii, jj) + curBeta(jj) + curLocalScores(jj) - logZ)
                jj += 1
              }
              ii += 1
            }
            transGradient(prevTargetIntValue, curTargetIntValue) += 1
          }
        }
        i += 1
      }
      if (gradient ne null)
        gradient.accumulate(markov.weights, transGradient)
    }
  }
}
