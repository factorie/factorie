package cc.factorie

import app.classify.ModelBasedClassifier
import collection.mutable.ArrayBuffer
import cc.factorie.maths._
import cc.factorie.la.{DenseTensor1, Tensor1}

/**A template for factors who scores are the weighted sum of scores of
    label S1 given feature vector S2, according to list of boosted classifiers.*/
abstract class AdaBoostTemplateWithStatistics2[S1 <: DiscreteVar, S2 <: TensorVar]
  (labelToFeatures: S1 => S2, labelDomain: DiscreteDomain, featureDomain: TensorDomain)
  (implicit m1: Manifest[S1], m2: Manifest[S2])
  extends Template2[S1, S2] {

  type StatisticsType = (S1#Value, S2#Value)

  def trainWeakClassifier(labels: ArrayBuffer[S1], instanceWeights: Tensor1): WeakClassifier
  def numIterations: Int

  type WeakClassifier = Template2[S1, S2] /*with Parameters*/

  private var weightedWeakClassifiers: Option[Seq[(WeakClassifier, Double)]] = None

  override def score(s1: S1#Value, s2: S2#Value): Double = {
    val wcs = weightedWeakClassifiers.get
    wcs.foldLeft(0.0)((acc, el) => el._1.score(s1, s2) * el._2 + acc)
  }

  // TODO have to cast here because if we put existentials in the type bounds it breaks in scala 2.10 -luke
  def train(labels: ArrayBuffer[S1]): Unit = {
    val K = labelDomain.size
    val numInstances = labels.length
    val instanceWeights = new DenseTensor1(Array.fill(numInstances)(1.0 / numInstances))
    var converged = false
    var weightedClassifiers = List(): List[(WeakClassifier, Double)]
    var i = 0
    while (!converged) {
      val classifierTemplate = trainWeakClassifier(labels, instanceWeights)
      val currentClassifier = new ModelBasedClassifier[MutableDiscreteVar[_],Model](classifierTemplate, labels.head.domain)
      val classifications = currentClassifier.classifications(labels.map(_.asInstanceOf[MutableDiscreteVar[_]])).toArray
      val isFail = mapIndex(numInstances)(i => classifications(i).bestLabelIndex != labels(i).asInstanceOf[DiscreteVar].intValue)
      val amountOfFail = (0 until numInstances).filter(isFail).foldLeft(0.0)((acc, el) => acc + instanceWeights(el))
      // FIXME: why doesn't this work multiclass? The log(K - 1) term should do this (see "Multi-class AdaBoost" by Zhu et al.) -luke
      val classifierWeight = math.log((1 - amountOfFail) / amountOfFail) + math.log(K - 1)
      forIndex(numInstances)(i => instanceWeights(i) *= math.exp(if (isFail(i)) classifierWeight else 0))
      val dSum = ArrayOps.oneNorm(instanceWeights.asArray)
      forIndex(numInstances)(i => instanceWeights(i) /= dSum)
      weightedClassifiers = (classifierTemplate, classifierWeight) :: weightedClassifiers
      converged = i > numIterations || amountOfFail == 0.0
      i += 1
    }
    weightedWeakClassifiers = if (i == 1) Some(List((weightedClassifiers(0)._1, 1.0))) else Some(weightedClassifiers)
  }
}

class AdaBoostDecisionStumpTemplate[L <: DiscreteVar, F <: TensorVar](
  labelToFeatures: L => F,
  labelDomain: DiscreteDomain,
  featureDomain: TensorDomain)(implicit m1: Manifest[L], m2: Manifest[F])
  extends AdaBoostTemplateWithStatistics2[L, F](labelToFeatures, labelDomain, featureDomain) {
  var numIterations = 5
  //type WeakClassifier = DecisionStumpTemplate[L, F]
  def trainWeakClassifier(labels: ArrayBuffer[L], instanceWeights: Tensor1) = {
    val template = new DecisionStumpTemplate[L, F](labelToFeatures, labelDomain, featureDomain)
    template.train(labels, instanceWeights)
    template
  }
  def unroll1(label: L) = Factor(label, labelToFeatures(label))
  def unroll2(features: F) = throw new Error("Cannot unroll from feature variables.")
}
