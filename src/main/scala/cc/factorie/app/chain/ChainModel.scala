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
import cc.factorie.app.chain.infer._
import scala.collection.mutable.{ListBuffer,ArrayBuffer, Map}
import java.io.File

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
    modelFile.getParentFile.mkdirs()
    BinaryCubbieFileSerializer.serialize(new ModelCubbie(this), modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(labelDomain), labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(featuresDomain.dimensionDomain), featuresDomainFile)
  }

  def deSerialize(prefix: String) {
    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix+"-model'")
    BinaryCubbieFileSerializer.deserialize(new ModelCubbie(this), modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix+"-labelDomain'")
    BinaryCubbieFileSerializer.deserialize(new CategoricalDomainCubbie(labelDomain), labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix+"-featuresDomain'")
    BinaryCubbieFileSerializer.deserialize(new CategoricalDomainCubbie(featuresDomain.dimensionDomain), featuresDomainFile)
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

  // Inference
  //def inferBySumProduct(labels:IndexedSeq[Label]): ChainSummary = {
  //  ForwardBackward.search(labels, obs, markov, bias, labelToFeatures)
  //}
  def featureExpectationsAndLogZ(labels:IndexedSeq[Label]): (Map[DotFamily, Tensor], Double) = {
    ForwardBackward.featureExpectationsAndLogZ(labels, obs, markov, bias, labelToFeatures)
  }
  def inferByMaxProduct(labels:IndexedSeq[Label]): ChainSummary = {
    // this shouldn't actually set the variables, just used now for fast evaluation
    Viterbi.searchAndSetToMax(labels, obs, markov, bias, labelToFeatures)
    null
  }
  
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = {
    None
  }

  object MarginalInference extends Infer {
    override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = Some(inferBySumProduct(variables.asInstanceOf[IndexedSeq[Label]]))
  }
  // Training
  val objective = new HammingTemplate[Label]
}