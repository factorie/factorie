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
import java.io.File
import org.junit.Assert._
import cc.factorie.util.{DoubleAccumulator, BinarySerializer}
import cc.factorie.variable._
import scala.reflect.ClassTag
import cc.factorie.Factorie.DotFamilyWithStatistics3
import cc.factorie.Factorie.Parameters
import cc.factorie.Factorie.DotFamilyWithStatistics2
import cc.factorie.Factorie.Factor
import cc.factorie.la.{WeightsMapAccumulator, Tensor1}
import cc.factorie.DenseTensor1
import cc.factorie.Factorie.CategoricalVectorDomain
import cc.factorie.Factorie.Var
import cc.factorie.Factorie.CategoricalDomain
import cc.factorie.Model
import cc.factorie.Factorie.DotFamilyWithStatistics1

class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget, Features<:CategoricalVectorVar[String], Token<:Observation[Token]]
(val labelDomain:CategoricalDomain[String],
 val featuresDomain:CategoricalVectorDomain[String],
 val labelToFeatures:Label=>Features,
 val labelToToken:Label=>Token,
 val tokenToLabel:Token=>Label) 
 (implicit lm:ClassTag[Label], fm:ClassTag[Features], tm:ClassTag[Token])
extends Model with Parameters
{
  self =>
  val labelClass = lm.runtimeClass
  val featureClass = fm.runtimeClass
  val tokenClass = tm.runtimeClass
  val bias = new DotFamilyWithStatistics1[Label] {
    factorName = "Label"
    val weights = Weights(new la.DenseTensor1(labelDomain.size))
  }
  val obs = new DotFamilyWithStatistics2[Label,Features] {
    factorName = "Label,Token"
    val weights = Weights(new la.DenseTensor2(labelDomain.size, featuresDomain.dimensionSize))
  }
  val markov = new DotFamilyWithStatistics2[Label,Label] {
    factorName = "Label,Label"
    val weights = Weights(new la.DenseTensor2(labelDomain.size, labelDomain.size))
  }
  val obsmarkov = new DotFamilyWithStatistics3[Label,Label,Features] {
    factorName = "Label,Label,Token"
    val weights = Weights(if (useObsMarkov) new la.DenseTensor3(labelDomain.size, labelDomain.size, featuresDomain.dimensionSize) else new la.DenseTensor3(1, 1, 1))
  }
  var useObsMarkov = false

  def serialize(prefix: String) {
    val modelFile = new File(prefix + "-model")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    BinarySerializer.serialize(this, modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinarySerializer.serialize(labelDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinarySerializer.serialize(featuresDomain.dimensionDomain, featuresDomainFile)
  }

  def deSerialize(prefix: String) {
    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-labelDomain'")
    BinarySerializer.deserialize(labelDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-featuresDomain'")
    BinarySerializer.deserialize(featuresDomain.dimensionDomain, featuresDomainFile)
    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix + "-model'")
    assertEquals(markov.weights.value.length, labelDomain.length * labelDomain.length)
    BinarySerializer.deserialize(this, modelFile)
  }

  def factors(variables:Iterable[Var]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    variables match {
      case labels: Iterable[Label] if variables.forall(v => labelClass.isAssignableFrom(v.getClass)) =>
        var prevLabel: Label = null.asInstanceOf[Label]
        for (label <- labels) {
          result += bias.Factor(label)
          result += obs.Factor(label, labelToFeatures(label))
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
    case label:Label if label.getClass eq labelClass => {
      val result = new ArrayBuffer[Factor](4)
      result += bias.Factor(label)
      result += obs.Factor(label, labelToFeatures(label))
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
  // only works for dot familys, no structured svm, obs template looks like [Label, Features]
  def inferChainSumFast(varying: Seq[Label]): (Double,Array[DenseTensor1],Array[DenseTensor1],Array[DenseTensor1]) = {
    assert(!useObsMarkov, "obsMarkov factors not supported with efficient sum-product")
    val markovScores = markov.weights.value
    val biasScores = bias.weights.value
    val obsWeights = obs.weights.value


    val localScores = Array.fill[DenseTensor1](varying.size)(null)
    var i = 0
    while (i < varying.length) {
      val scores = (obsWeights * varying(i).value.asInstanceOf[Tensor1]).asInstanceOf[DenseTensor1]
      scores += biasScores
      localScores(i) = scores
      i += 1
    }

    val alphas = Array.fill(varying.size)(new DenseTensor1(markovScores.dim1, Double.NegativeInfinity))
    val betas = Array.fill(varying.size)(new DenseTensor1(markovScores.dim1, Double.NegativeInfinity))

    alphas(0) := localScores(0)

    i = 1
    val d1 = markovScores.dim1
    while (i < varying.size) {
      val ai = alphas(i)
      val aim1 = alphas(i-1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          ai(vi) = maths.sumLogProb(ai(vi), markovScores(vj, vi) + aim1(vj))
          vj += 1
        }
        vi += 1
      }
      alphas(i) += localScores(i)
      i += 1
    }

    betas.last.zero()

    i = varying.size - 2
    while (i >= 0) {
      val bi = betas(i)
      val bip1 = betas(i+1)
      val lsp1 = localScores(i+1)
      var vi = 0
      while (vi < d1) {
        var vj = 0
        while (vj < d1) {
          bi(vi) = maths.sumLogProb(bi(vi), markovScores(vi, vj) + bip1(vj) + lsp1(vj))
          vj += 1
        }
        vi += 1
      }
      i -= 1
    }

    val logZ = maths.sumLogProbs(alphas.last)
    (logZ, alphas, betas, localScores)
  }

  def example(varying: Seq[Label]): Example = {
    new Example {
      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
        val (logZ,alphas,betas,localScores) = inferChainSumFast(varying)
        val transScores = markov.weights.value
        if (value ne null) {
          for (i <- 0 until varying.length) {
            value.accumulate(localScores(i)(varying(i).targetIntValue))
            if (i >= 1) value.accumulate(transScores(varying(i-1).targetIntValue,varying(i).targetIntValue))
          }
          value.accumulate(-logZ)
        }
        if (gradient ne null) {
          val marginal = new DenseTensor2(transScores.dim1, transScores.dim1)
          for (i <- 0 until varying.length) {
            val localMarginal = alphas(i) + betas(i)
            localMarginal.expNormalize(logZ)
            localMarginal *= -1
            localMarginal(varying(i).targetIntValue) += 1
            gradient.accumulate(bias.weights, localMarginal)
            gradient.accumulate(obs.weights, localMarginal outer labelToFeatures(varying(i)).value)
            if (i >= 1) {
              for (ii <- 0 until localMarginal.dim1; jj <- 0 until localMarginal.dim1) {
                marginal(ii, jj) += -math.exp(alphas(i-1)(ii) + transScores(ii,jj) + betas(i)(jj) - logZ)
              }
              marginal(varying(i-1).targetIntValue,varying(i).targetIntValue) += 1
            }
          }
          gradient.accumulate(markov.weights, marginal)
        }
      }
    }
  }


}


