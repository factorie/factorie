package cc.factorie

import app.classify.ModelBasedClassifier
import collection.mutable.ArrayBuffer
import cc.factorie.maths._

/**A template for factors who scores are the weighted sum of scores of
    label S1 given feature vector S2, according to list of boosted classifiers.*/
abstract class AdaBoostTemplateWithStatistics2[S1 <: MutableDiscreteVar[_<:DiscreteValue], S2 <: DiscreteTensorVar](implicit m1: Manifest[S1], m2: Manifest[S2])
  extends Template2[S1, S2] {

  type StatisticsType = (S1#Value, S2#Value)

  type WeakClassifier <: Template2[S1, S2]
  def trainWeakClassifier(labels: ArrayBuffer[S1], getInstanceWeight: Int => Double): WeakClassifier
  def numIterations: Int

  private var weightedWeakClassifiers: Option[Seq[(WeakClassifier, Double)]] = None

  override def score(s1: S1#Value, s2: S2#Value): Double = {
    val wcs = weightedWeakClassifiers.get
    wcs.foldLeft(0.0)((acc, el) => el._1.score(s1, s2) * el._2 + acc)
  }

  def train(labels: ArrayBuffer[S1]): Unit = {
    val numInstances = labels.length
    val instanceWeights = Array.fill(numInstances)(1.0 / numInstances)
    var converged = false
    var weightedClassifiers = List(): List[(WeakClassifier, Double)]
    var i = 0
    while (!converged) {
      val classifierTemplate = trainWeakClassifier(labels, instanceWeights)
      val currentClassifier = new ModelBasedClassifier[S1](classifierTemplate, labels.head.domain)
      val classifications = currentClassifier.classifications(labels).toArray
      val isFail = mapIndex(numInstances)(i => classifications(i).bestLabelIndex != labels(i).intValue)
      val amountOfFail = (0 until numInstances).filter(isFail).foldLeft(0.0)((acc, el) => acc + instanceWeights(el))
      val classifierWeight = 0.5 * math.log((1 - amountOfFail) / amountOfFail)
      forIndex(numInstances)(i => instanceWeights(i) *= math.exp(if (isFail(i)) classifierWeight else -classifierWeight))
      val dSum = ArrayOps.oneNorm(instanceWeights)
      forIndex(numInstances)(i => instanceWeights(i) /= dSum)
      weightedClassifiers = (classifierTemplate, classifierWeight) :: weightedClassifiers
      converged = i > numIterations || amountOfFail == 0.0
      i += 1
    }
    weightedWeakClassifiers = if (i == 1) Some(List((weightedClassifiers(0)._1, 1.0))) else Some(weightedClassifiers)
  }
}

class AdaBoostDecisionStumpTemplate[L <: MutableDiscreteVar[_<:DiscreteValue], F <: DiscreteTensorVar](
  val labelToFeatures: L => F,
  val labelDomain: DiscreteDomain with Domain[L#Value],
  val featureDomain: DiscreteTensorDomain with Domain[F#Value])(implicit m1: Manifest[L], m2: Manifest[F])
  extends AdaBoostTemplateWithStatistics2[L, F] {
  var numIterations = 5
  type WeakClassifier = DecisionStumpTemplate[L, F]
  def trainWeakClassifier(labels: ArrayBuffer[L], getInstanceWeight: Int => Double) = {
    val template = new DecisionStumpTemplate[L, F](labelToFeatures, labelDomain, featureDomain)
    template.train(labels, getInstanceWeight)
    template
  }
  def unroll1(label: L) = Factor(label, labelToFeatures(label))
  def unroll2(features: F) = throw new Error("Cannot unroll from feature variables.")
}
