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
import cc.factorie.la._
import cc.factorie.optimize._
import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import java.io.File
import org.junit.Assert._

class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget[_], Features<:CategoricalTensorVar[String], Token<:Observation[Token]]
(val labelDomain:CategoricalDomain[String],
 val featuresDomain:CategoricalTensorDomain[String],
 val labelToFeatures:Label=>Features,
 val labelToToken:Label=>Token,
 val tokenToLabel:Token=>Label) 
 (implicit lm:Manifest[Label], fm:Manifest[Features], tm:Manifest[Token])
extends ModelWithContext[IndexedSeq[Label]] //with Trainer[ChainModel[Label,Features,Token]]
{
  val labelClass = lm.erasure
  val featureClass = fm.erasure
  val tokenClass = tm.erasure
  object bias extends DotFamilyWithStatistics1[Label] {
    factorName = "Label"
    lazy val weights = new la.DenseTensor1(labelDomain.size)
  }
  object obs extends DotFamilyWithStatistics2[Label,Features] {
    factorName = "Label,Token"
    lazy val weights = new la.DenseTensor2(labelDomain.size, featuresDomain.dimensionSize)
  }
  object markov extends DotFamilyWithStatistics2[Label,Label] {
    factorName = "Label,Label"
    lazy val weights = new la.DenseTensor2(labelDomain.size, labelDomain.size)
  }
  object obsmarkov extends DotFamilyWithStatistics3[Label,Label,Features] {
    factorName = "Label,Label,Token"
    lazy val weights = new la.DenseTensor3(labelDomain.size, labelDomain.size, featuresDomain.dimensionSize)
  }
  var useObsMarkov = true
  override def families = if (useObsMarkov) Seq(bias, obs, markov, obsmarkov) else Seq(bias, obs, markov)

  def serialize(prefix: String) {
    val modelFile = new File(prefix + "-model")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    BinaryCubbieFileSerializer.serialize(new ModelCubbie(this), modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(labelDomain), labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(featuresDomain.dimensionDomain), featuresDomainFile)
  }

  def deSerialize(prefix: String) {
    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix+"-labelDomain'")
    val labelDomainCubbie = new CategoricalDomainCubbie(labelDomain)
    BinaryCubbieFileSerializer.deserialize(labelDomainCubbie, labelDomainFile)
    labelDomainCubbie.fetch(labelDomain)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix+"-featuresDomain'")
    val featuresDomainCubbie = new CategoricalDomainCubbie(featuresDomain.dimensionDomain)
    BinaryCubbieFileSerializer.deserialize(featuresDomainCubbie, featuresDomainFile)
    featuresDomainCubbie.fetch(featuresDomain.dimensionDomain)
    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix+"-model'")
    assertEquals(markov.weights.length,labelDomain.length * labelDomain.length)
    BinaryCubbieFileSerializer.deserialize(new ModelCubbie(this), modelFile)
  }

  def factorsWithContext(labels:IndexedSeq[Label]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    for (i <- 0 until labels.length) {
      result += bias.Factor(labels(i))
      result += obs.Factor(labels(i), labelToFeatures(labels(i)))
      if (i > 0) {
        result += markov.Factor(labels(i-1), labels(i))
        if (useObsMarkov) result += obsmarkov.Factor(labels(i-1), labels(i), labelToFeatures(labels(i)))
      }
    }
    result
  }
  override def factors(variables:Iterable[Variable]): Iterable[Factor] = variables match {
    case variables:IndexedSeq[Label] if (variables.forall(v => labelClass.isAssignableFrom(v.getClass))) => factorsWithContext(variables)
    case _ => super.factors(variables)
  }
  def factors(v:Variable) = v match {
    case label:Label if (label.getClass eq labelClass) => {
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
  
  abstract class ChainSummary(val labels: Seq[Label]) extends Summary[DiscreteMarginal] {
    def marginals: Iterable[DiscreteMarginal] = Nil
    def marginal(vs:Variable*): DiscreteMarginal = null

    val localScores = Array.ofDim[Double](labels.size, labelDomain.size)
    def fillLocalScores(): Unit = {
      var t = 0
      val statistics = new SingletonBinaryLayeredTensor2(labelDomain.size, featuresDomain.dimensionSize, 0, null)
      while (t < labels.size) {
        statistics.inner = labelToFeatures(labels(t)).value.asInstanceOf[Tensor1]
        var j = 0
        while (j < labelDomain.size) {
          statistics.singleIndex1 = j
          localScores(t)(j) = bias.weights(j) + obs.weights.dot(statistics)
          j += 1
        }
        t += 1
      }
    }
    fillLocalScores()

    var localTransitionScores = null.asInstanceOf[Array[Array[Array[Double]]]]
    def fillLocalTransitionScores() {
      assert(useObsMarkov)
      localTransitionScores = Array.ofDim[Double](labels.length, labelDomain.size, labelDomain.size)
      var i = 1
      while (i < labels.size) {
        var j = 0
        while (j < labelDomain.size) {
          var k = 0
          while (k < labelDomain.size) {
            localTransitionScores(i)(j)(k) = markov.weights(j,k) + obsmarkov.weights.dot(new Singleton2BinaryLayeredTensor3(labelDomain.size, labelDomain.size,featuresDomain.dimensionSize, j, k, labelToFeatures(labels(i)).value.asInstanceOf[Tensor1]))
            k += 1
          }
          j += 1
        }
        i += 1
      }
    }
    if (useObsMarkov) fillLocalTransitionScores()

    val nodeMarginals = ArrayBuffer[DenseTensor1]()
    val edgeMarginals = ArrayBuffer[DenseTensor2]()
    def computeNodeMarginal(pos: Int): Tensor1
    def computeEdgeMarginal(pos: Int): Tensor2
    def calculateMarginalTensor(factor: Factor) = {
      val f = factor.asInstanceOf[DotFamily#Factor]
      val ds = labelDomain.length
      if ((f.family eq bias) || (f.family eq obs)) {
        val pos = labelToToken(f.variable(0).asInstanceOf[Label]).asInstanceOf[app.nlp.Token].sentencePosition
        val marginal = if (nodeMarginals(pos) != null) nodeMarginals(pos) else computeNodeMarginal(pos)
        if (f.family eq bias)
          marginal
        else
          new Outer1Tensor2(marginal, f.variable(1).asInstanceOf[Features].value.asInstanceOf[Tensor1])
      } else if ((f.family eq markov) || (f.family eq obsmarkov)) {
        val pos = labelToToken(f.variable(0).asInstanceOf[Label]).asInstanceOf[app.nlp.Token].sentencePosition
        val marginal = if (edgeMarginals(pos) != null) edgeMarginals(pos) else computeEdgeMarginal(pos)
        if (f.family eq markov)
          marginal
        else
          new Outer2Tensor3(marginal, f.variable(2).asInstanceOf[Features].value.asInstanceOf[Tensor1])
      } else {
        require(false, "This Summary class can only be used with the correct ChainModel")
        new DenseTensor1(10)
      }
    }
    override def marginalTensorStatistics(factor: Factor) = calculateMarginalTensor(factor)

  }

  class ChainSummaryBP(l: Seq[Label]) extends ChainSummary(l) {
    val alphas = Array.fill(labels.length, labelDomain.length)(Double.NegativeInfinity)
    val betas = Array.fill(labels.length, labelDomain.length)(Double.NegativeInfinity)

    def sendMessages() {
      var j = 0
      val domainSize = labelDomain.length
      val nLabels = labels.length
      while (j < domainSize) {
        alphas(0)(j) = localScores(0)(j)
        j += 1
      }
      nodeMarginals += null
      edgeMarginals += null

      var i = 1
      val trans = markov.weights
      while (i < nLabels) {
        nodeMarginals += null
        edgeMarginals += null
        j = 0
        while (j < domainSize) {
          var k = 0
          if (useObsMarkov) {
            while (k < domainSize) {
              alphas(i)(j) = maths.sumLogProb(alphas(i)(j), alphas(i-1)(k) + localScores(i)(j) + localTransitionScores(i)(k)(j))
              k += 1
            }
          } else {
            while (k < domainSize) {
              alphas(i)(j) = maths.sumLogProb(alphas(i)(j), alphas(i-1)(k) + localScores(i)(j) + trans(k, j))
              k += 1
            }
          }
          j += 1
        }
        i += 1
      }

      j = 0
      while (j < domainSize) {
        betas(nLabels - 1)(j) = 0
        j += 1
      }

      i = nLabels - 2
      while (i >= 0) {
        j = 0
        while (j < domainSize) {
          var k = 0
          if (useObsMarkov) {
            while (k < domainSize) {
              betas(i)(j) = maths.sumLogProb(betas(i)(j), betas(i+1)(k) + localScores(i+1)(k) + localTransitionScores(i)(j)(k))
              k += 1
            }
          } else {
            while (k < domainSize) {
              betas(i)(j) = maths.sumLogProb(betas(i)(j), betas(i+1)(k) + localScores(i+1)(k) + trans(j, k))
              k += 1
            }
          }
          j += 1
        }
        i -= 1
      }
    }
    sendMessages()

    val _logZ: Double = {
      var z = Double.NegativeInfinity
      var i = 0
      while (i < labelDomain.length) {
        z = maths.sumLogProb(z, alphas(labels.length-1)(i))
        i += 1
      }
      z
    }

    override def logZ = _logZ

    def computeNodeMarginal(pos: Int): DenseTensor1 = {
      val marg = new DenseTensor1(labelDomain.length)
      val arr = marg.asArray
      var i = 0
      val al = alphas(pos)
      val bl = betas(pos)
      val lz = logZ
      while (i < arr.length) {
        arr(i) = math.exp(al(i) + bl(i) - lz)
        i += 1
      }
      nodeMarginals(pos) = marg
      marg
    }

    def computeEdgeMarginal(pos: Int) : DenseTensor2 = {
      val marginal = new DenseTensor2(labelDomain.length, labelDomain.length)
      val arr = marginal.asArray
      var i = 0
      val al = alphas(pos)
      val bl = betas(pos+1)
      val ls = localScores(pos+1)
      val ds = labelDomain.size
      val lz = logZ
      val trans = markov.weights.asInstanceOf[DenseTensor2]
      while (i < ds) {
        var j = 0
        if (useObsMarkov) {
          while (j < ds) {
            marginal(i,j) = math.exp(al(i) + ls(j) + localTransitionScores(pos)(i)(j) + bl(j) - lz)
            j += 1
          }
        } else {
          while (j < ds) {
            marginal(i,j) = math.exp(al(i) + ls(j) + trans(i, j) + bl(j) - lz)
            j += 1
          }
        }
        i += 1
      }
      edgeMarginals(pos) = marginal
      marginal
    }


  }

  // Inference
  def inferBySumProduct(labels:IndexedSeq[Label]): ChainSummary = {
    val summary = new ChainSummaryBP(labels)
    summary
  }

  object MarginalInference extends Infer {
    override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = Some(inferBySumProduct(variables.asInstanceOf[IndexedSeq[Label]]))
  }
  // Training
  val objective = new HammingTemplate[Label]
}
