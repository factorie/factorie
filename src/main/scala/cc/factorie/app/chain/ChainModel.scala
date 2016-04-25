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

import java.io._

import cc.factorie.la.{SparseIndexedTensor1, WeightsMapAccumulator, _}
import cc.factorie.{la, maths}
import cc.factorie.model._
import cc.factorie.optimize.Example
import cc.factorie.util.{BinarySerializer, DoubleAccumulator}
import cc.factorie.variable._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

// TODO We should add the ability to explicitly permit and forbid label transitions
// Was Label <: LabeledMutableDiscreteVar
class ChainModel[Label <: MutableDiscreteVar, Features <: CategoricalVectorVar[String], Token <: Observation[Token]]
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
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(featuresDomain, dstream)
    BinarySerializer.serialize(labelDomain, dstream)
    BinarySerializer.serialize(this, dstream)
    dstream.close()
  }

  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(new BufferedInputStream(stream))
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
 
  def maximize(vars: Seq[Label])(implicit d: DiffList): Unit = {
    if (vars.isEmpty) return
    val result = ChainHelper.viterbiFast(getCliqueValues(vars))
    val n = vars.length
    var i = 0
    while (i < n) {
      vars(i).set(result.mapValues(i))
      i += 1
    }
  }

  def getMaximizedLabels(vars: Seq[Label])(implicit d: DiffList): Seq[Int] = {
    if (vars.isEmpty) return Seq()
    val result = ChainHelper.viterbiFast(getCliqueValues(vars))
    val n = vars.length
    val arr: Array[Int] = Array.fill(n)(0)
    var i = 0
    while (i < n) {
      arr(i) = result.mapValues(i)
      i += 1
    }
    arr.toSeq
  }

  def getHammingLossScores(varying: Seq[Label with LabeledMutableDiscreteVar]): Array[Tensor1] = {
    val domainSize = varying.head.domain.size
    val localScores = new Array[Tensor1](varying.size)
    for ((v, i) <- varying.zipWithIndex) {
      localScores(i) = new DenseTensor1(domainSize)
      for (wrong <- 0 until domainSize if wrong != v.target.intValue)
        localScores(i)(wrong) += 1.0
    }
    localScores
  }

  def getLocalScores(varying: Seq[Label]): Array[DenseTensor1] = {
    val biasScores = bias.weights.value.asArray
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

  def inferFast(varying: Seq[Label], addToLocalScoresOpt: Option[Array[Tensor1]] = None): ChainForwardBackwardResults =
    ChainHelper.inferFast(getCliqueValues(varying, addToLocalScoresOpt))

  def viterbiFast(varying: Seq[Label], addToLocalScoresOpt: Option[Array[Tensor1]] = None): ChainViterbiResults =
    ChainHelper.viterbiFast(getCliqueValues(varying, addToLocalScoresOpt))

  def getCliqueValues(varying: Seq[Label], addToLocalScoresOpt: Option[Array[Tensor1]] = None): ChainCliqueValues = {
    val markovScoresT = markov.weights.value
    val localScores = getLocalScores(varying)
    addToLocalScoresOpt.foreach(l => (0 until varying.length).foreach(i => localScores(i) += l(i)))
    // WARNING: for efficiency we duplicate the transition scores by reference here. FIX -luke
    ChainCliqueValues(localScores, Seq.fill(math.max(1, varying.size - 1))(markovScoresT))
  }

  def accumulateExtraObsGradients(gradient: WeightsMapAccumulator, obsMarginal: Tensor1, position: Int, labels: Seq[Label]): Unit = {}

  class ChainStructuredSVMExample(varying: Seq[Label with LabeledMutableDiscreteVar]) extends ChainViterbiExample(varying, () => Some(getHammingLossScores(varying)))

  class ChainViterbiExample(varying: Seq[Label with LabeledMutableDiscreteVar], addToLocalScoresOpt: () => Option[Array[Tensor1]] = () => None) extends Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
      if (varying.length == 0) return
      val scores = getCliqueValues(varying, addToLocalScoresOpt())
      val ChainViterbiResults(mapScore, mapValues, _) = ChainHelper.viterbiFast(scores)
      val transScores = markov.weights.value
      if (value ne null)
        value.accumulate(-mapScore)
      val domainSize = transScores.dim1
      val transGradient = new DenseTensor2(domainSize, domainSize)
      val len = varying.length
      var i = 0
      while (i < len) {
        val curLabel = varying(i)
        val prevLabel = if (i >= 1) varying(i - 1) else null.asInstanceOf[Label with LabeledMutableDiscreteVar]
        val curLocalScores = scores.localValues(i)
        val curTargetIntValue = curLabel.target.intValue
        val prevTargetIntValue = if (i >= 1) prevLabel.target.intValue else -1
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
          accumulateExtraObsGradients(gradient, localMarginal, i, varying)
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

  class ChainLikelihoodExample(varying: Seq[Label with LabeledMutableDiscreteVar], addToLocalScoresOpt: () => Option[Array[Tensor1]] = () => None) extends Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
      if (varying.length == 0) return
      val scores = getCliqueValues(varying, addToLocalScoresOpt())
      val ChainForwardBackwardResults(logZ, alphas, betas, _) = ChainHelper.inferFast(scores)
      val transScoresT = markov.weights.value
      val transScores = transScoresT.asArray
      val domainSize = transScoresT.dim1
      if (value ne null)
        value.accumulate(-logZ)
      val transGradient = new DenseTensor2(domainSize, domainSize)
      val len = varying.length
      var i = 0
      while (i < len) {
        val curLabel = varying(i)
        val prevLabel = if (i >= 1) varying(i - 1) else null.asInstanceOf[Label with LabeledMutableDiscreteVar]
        val prevAlpha = if (i >= 1) alphas(i - 1) else null.asInstanceOf[Tensor1]
        val curAlpha = alphas(i)
        val curBeta = betas(i)
        val curLocalScores = scores.localValues(i)
        val curTargetIntValue = curLabel.target.intValue
        val prevTargetIntValue = if (i >= 1) prevLabel.target.intValue else -1
        if (value ne null) {
          value.accumulate(curLocalScores(curTargetIntValue))
          if (i >= 1) value.accumulate(transScores(prevTargetIntValue * domainSize + curTargetIntValue))
        }
        if (gradient ne null) {
          val localMarginal = curAlpha + curBeta
          localMarginal.expNormalize(logZ)
          localMarginal *= -1
          localMarginal(curTargetIntValue) += 1
          gradient.accumulate(bias.weights, localMarginal)
          gradient.accumulate(obs.weights, labelToFeatures(curLabel).value outer localMarginal)
          accumulateExtraObsGradients(gradient, localMarginal, i, varying)
          if (i >= 1) {
            var ii = 0
            while (ii < domainSize) {
              var jj = 0
              while (jj < domainSize) {
                transGradient(ii, jj) += -math.exp(prevAlpha(ii) + transScores(ii * domainSize + jj) + curBeta(jj) + curLocalScores(jj) - logZ)
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

case class ChainViterbiResults(mapScore: Double, mapValues: Array[Int], scores: ChainCliqueValues)
case class ChainForwardBackwardResults(logZ: Double, alphas: Array[DenseTensor1], betas: Array[DenseTensor1], scores: ChainCliqueValues)

// WARNING: transition values might point to the same underlying array for speed. FIX -luke
case class ChainCliqueValues(localValues: Seq[DenseTensor1], transitionValues: Seq[Tensor2]) {
  def +=(other: ChainCliqueValues, f: Double) = {
    for ((m, s) <- localValues.zip(other.localValues)) m +=(s, f)
    for ((m, s) <- transitionValues.zip(other.transitionValues)) m +=(s, f)
  }
  def +=(other: ChainCliqueValues): Unit = +=(other, 1.0)
  def safeDot(t1: Tensor, t2: Tensor): Double = {
    val len = t1.length
    var dot = 0.0
    var i = 0
    while (i < len) {
      val v1 = t1(i)
      val v2 = t2(i)
      if (!(v1 == Double.NegativeInfinity && v2 == 0.0 || v2 == Double.NegativeInfinity && v1 == 0.0))
        dot += v1 * v2
      i += 1
    }
    dot
  }
  def dot(other: ChainCliqueValues): Double = {
    (for ((m, s) <- localValues.zip(other.localValues)) yield safeDot(m, s)).sum +
    (for ((m, s) <- transitionValues.zip(other.transitionValues)) yield safeDot(m, s)).sum
  }
  def *=(f: Double) = {
    localValues.map(_ *= f)
    transitionValues.map(_ *= f)
  }
  def length = localValues.map(_.length).sum + transitionValues.map(_.length).sum
  def blankCopy = ChainCliqueValues(localValues.map(_.blankCopy), transitionValues.map(_.blankCopy))
  def copy = ChainCliqueValues(localValues.map(_.copy), transitionValues.map(_.copy))
  def twoNormSquared = localValues.map(_.twoNormSquared).sum + transitionValues.map(_.twoNormSquared).sum
}

object ChainHelper {
  def inferFast(scores: ChainCliqueValues): ChainForwardBackwardResults = {
    val d1 = scores.transitionValues(0).dim1
    val markovScores = scores.transitionValues
    val localScores = scores.localValues
    val alphas = Array.fill(localScores.size)(new DenseTensor1(d1, Double.NegativeInfinity))
    val betas = Array.fill(localScores.size)(new DenseTensor1(d1, Double.NegativeInfinity))
    alphas(0) := localScores(0)
    var i = 1
    val tmpArray = Array.fill(d1)(0.0)
    while (i < localScores.size) {
      val ai = alphas(i)
      val aim1 = alphas(i - 1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          tmpArray(vj) = markovScores(i - 1)(vj * d1 + vi) + aim1(vj)
          vj += 1
        }
        ai(vi) = maths.sumLogProbs(tmpArray)
        vi += 1
      }
      alphas(i) += localScores(i)
      i += 1
    }
    betas.last.zero()
    i = localScores.size - 2
    while (i >= 0) {
      val bi = betas(i)
      val bip1 = betas(i + 1)
      val lsp1 = localScores(i + 1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          tmpArray(vj) = markovScores(i)(vi * d1 + vj) + bip1(vj) + lsp1(vj)
          vj += 1
        }
        bi(vi) = maths.sumLogProbs(tmpArray)
        vi += 1
      }
      i -= 1
    }
    val logZ = maths.sumLogProbs(alphas.last.asArray)
    ChainForwardBackwardResults(logZ, alphas, betas, ChainCliqueValues(localScores, markovScores))
  }
  def viterbiFast(scores: ChainCliqueValues): ChainViterbiResults = {
    val markovScores = scores.transitionValues
    val localScores = scores.localValues
    val domainSize = markovScores.head.dim1
    val costs = Array.fill(scores.localValues.size)(new DenseTensor1(domainSize, Double.NegativeInfinity))
    val backPointers = Array.fill(scores.localValues.size)(Array.fill[Int](domainSize)(-1))
    costs(0) := localScores(0)
    var i = 1
    while (i < scores.localValues.size) {
      val curMarkovScores = markovScores(i - 1)
      val curLocalScores = localScores(i)
      val curCost = costs(i)
      val curBackPointers = backPointers(i)
      val prevCost = costs(i - 1)
      var vi = 0
      while (vi < domainSize) {
        var maxScore = Double.NegativeInfinity
        var maxIndex = -1
        val curLocalScore = curLocalScores(vi)

        var vj = 0
        while (vj < domainSize) {
          val curScore = curMarkovScores(vj, vi) + prevCost(vj) + curLocalScore
          if (curScore > maxScore) {
            maxScore = curScore
            maxIndex = vj
          }
          vj += 1
        }
        curCost(vi) = maxScore
        if (maxIndex < 0) maxIndex = 0
        curBackPointers(vi) = maxIndex
        vi += 1
      }
      i += 1
    }
    val mapValues = Array.fill[Int](scores.localValues.size)(0)
    mapValues(mapValues.size - 1) = costs.last.maxIndex
    var j = mapValues.size - 2
    while (j >= 0) {
      mapValues(j) = backPointers(j + 1)(mapValues(j + 1))
      j -= 1
    }
    ChainViterbiResults(costs.last.max, mapValues, scores)
  }
  def calculateCliqueMarginals(res: ChainForwardBackwardResults): ChainCliqueValues = {
    val ChainForwardBackwardResults(logZ, alphas, betas, scores) = res
    val domainSize = alphas(0).dim1
    val len = alphas.length
    val localMarginals = scores.localValues.map(_.blankCopy)
    val transitionMarginals = scores.transitionValues.map(_.blankCopy)
    var i = 0
    while (i < len) {
      val prevAlpha = if (i >= 1) alphas(i - 1) else null.asInstanceOf[Tensor1]
      val curAlpha = alphas(i)
      val curBeta = betas(i)
      val curLocalScores = scores.localValues(i)
      val localMarginal = curAlpha + curBeta
      localMarginal.expNormalize(logZ)
      localMarginals(i) := localMarginal
      if (i >= 1) {
        val curTransitionMarginal = transitionMarginals(i - 1)
        var ii = 0
        while (ii < domainSize) {
          var jj = 0
          while (jj < domainSize) {
            curTransitionMarginal(ii, jj) += math.exp(prevAlpha(ii) + scores.transitionValues(i - 1)(ii * domainSize + jj) + curBeta(jj) + curLocalScores(jj) - logZ)
            jj += 1
          }
          ii += 1
        }
      }
      i += 1
    }
    ChainCliqueValues(localMarginals, transitionMarginals)
  }
}