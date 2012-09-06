//package cc.factorie
//
//import app.classify.ModelBasedClassifier
//import collection.mutable.ArrayBuffer
//import cc.factorie.maths._
//
///**A template for factors who scores are the weighted sum of scores of
//    label S1 given feature vector S2, according to list of boosted classifiers.*/
//abstract class AdaBoostTemplateWithStatistics2[S1 <: MutableDiscreteVar[_<:DiscreteValue], S2 <: DiscreteTensorVar](implicit m1: Manifest[S1], m2: Manifest[S2])
//  extends TemplateWithStatistics2[S1, S2] {
//
//  type WeakClassifier <: Template2[S1, S2] with TensorStatistics2[S1#Value, S2#Value]
//  def trainWeakClassifier(labels: ArrayBuffer[S1], getInstanceWeight: Int => Double): WeakClassifier
//  def numIterations: Int
//
//  private var weightedWeakClassifiers: Option[Seq[(WeakClassifier, Double)]] = None
//
//  override def score(s: StatisticsType): Double = {
//    val wcs = weightedWeakClassifiers.get
//    wcs.foldLeft(0.0)((acc, el) => el._1.score(el._1.Stat(s._1, s._2)) * el._2 + acc)
//  }
//
//  def train(labels: ArrayBuffer[S1]): Unit = {
//    val trueLabels = labels.map(_.intValue)
//    val numSamples = labels.length
//    val d = Array.fill(numSamples)(1.0 / numSamples)
//    var converged = false
//    var weightedClassifiers = List(): List[(WeakClassifier, Double)]
//    var i = 0
//    while (!converged) {
//      val classifierTemplate = trainWeakClassifier(labels, d)
//      val currentClassifier = new ModelBasedClassifier[S1](classifierTemplate, labels.head.domain)
//      val classifications = currentClassifier.classify(labels).toArray
//      forIndex(numSamples)(i => labels(i).set(trueLabels(i))(null)) // set back variables to original values so we can feed them to next classifier
//      val isFail = mapIndex(numSamples)(i => classifications(i).bestLabelIndex != trueLabels(i))
//      val amountOfFail = (0 until numSamples).filter(isFail).foldLeft(0.0)((acc, el) => acc + d(el))
//      val alpha_t = 0.5 * math.log((1 - amountOfFail) / amountOfFail)
//      forIndex(numSamples)(i => d(i) *= math.exp(if (isFail(i)) alpha_t else -alpha_t))
//      val dSum = ArrayOps.oneNorm(d)
//      forIndex(numSamples)(i => d(i) /= dSum)
//      weightedClassifiers = (classifierTemplate, alpha_t) :: weightedClassifiers
//      converged = i > numIterations || amountOfFail == 0.0
//      i += 1
//    }
//    weightedWeakClassifiers = if (i == 1) Some(List((weightedClassifiers(0)._1, 1.0))) else Some(weightedClassifiers)
//  }
//}
//
//class AdaBoostDecisionStumpTemplate[L <: MutableDiscreteVar[_<:DiscreteValue], F <: DiscreteTensorVar](
//  val labelToFeatures: L => F, val labelDomain: DiscreteDomain, val featureDomain: DiscreteTensorDomain)(implicit m1: Manifest[L], m2: Manifest[F])
//  extends AdaBoostTemplateWithStatistics2[L, F] {
//  var numIterations = 5
//  type WeakClassifier = DecisionStumpTemplate[L, F]
//  def trainWeakClassifier(labels: ArrayBuffer[L], getInstanceWeight: Int => Double) = {
//    val template = new DecisionStumpTemplate[L, F](labelToFeatures, labelDomain, featureDomain)
//    template.train(labels, getInstanceWeight)
//    template
//  }
//  def unroll1(label: L) = Factor(label, labelToFeatures(label))
//  def unroll2(features: F) = throw new Error("Cannot unroll from feature variables.")
//}
