package cc.factorie.app.classify

import cc.factorie._
import cc.factorie.optimize.LinearL2SVM
import cc.factorie.la.Tensor1

class SVMTrainer(parallel: Boolean = true) extends ClassifierTrainer {

  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteDimensionTensorVar](ll: LabelList[L, F]): Classifier[L] = {
    
    val template = new LogLinearTemplate2[L,F](ll.labelToFeatures, ll.labelDomain, ll.instanceDomain)(ll.labelManifest, ll.featureManifest)
    val model = new TemplateModel(template)
    
    val numLabels = ll.labelDomain.size
    val numFeatures = ll.featureDomain.size
    val xs: Seq[Tensor1] = ll.map(ll.labelToFeatures(_).tensor.asInstanceOf[Tensor1])
    val ys: Array[Int]   = ll.map(_.intValue).toArray // TODO: tighten the bounds on L so targetIntValue is available here.
    val weightTensor = {
      if (parallel)
        (0 until numLabels).par.map { label => (new LinearL2SVM).train(xs, ys, label) }
      else
        (0 until numLabels).map { label => (new LinearL2SVM).train(xs, ys, label) }
    }
    for (f <- 0 until numFeatures;
         (l,t) <- (0 until numLabels).zip(weightTensor)) {
      template.weights(l,f) = t(f)
    }

    new ModelBasedClassifier[L](model, ll.head.domain)
  }
}
