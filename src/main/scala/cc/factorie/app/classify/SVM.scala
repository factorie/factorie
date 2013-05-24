package cc.factorie.app.classify

import cc.factorie._
import cc.factorie.optimize.LinearL2SVM
import cc.factorie.la.{DenseTensor2, Tensor2, Tensor1}

object SVMTrainer {

  def train[L<: LabeledMutableDiscreteVar[_],F<:DiscreteDimensionTensorVar](model: Parameters, numLabels: Int, numFeatures: Int, ll: Seq[L], ff: Seq[F], parallel: Boolean = true) {
    val xs = ff.map(_.value.asInstanceOf[Tensor1])
    val ys = ll.map(_.targetIntValue).toArray
    val weightTensor = {
      if (parallel) (0 until numLabels).par.map { label => (new LinearL2SVM).train(xs, ys, label) }
      else (0 until numLabels).map { label => (new LinearL2SVM).train(xs, ys, label) }
    }
    val weightsValue = model.parameters.tensors.head.asInstanceOf[Tensor2]
    for (f <- 0 until numFeatures; (l,t) <- (0 until numLabels).zip(weightTensor)) {
      weightsValue(l,f) = t(f)
    }
  }
}

class SVMTrainer(parallel: Boolean = true) extends ClassifierTrainer {
  def train[L <: LabeledMutableDiscreteVar[_], F <: DiscreteDimensionTensorVar](ll: LabelList[L, F]): Classifier[L] = {
    val model = new TemplateModel()
    model += new LogLinearTemplate2[L,F](model, ll.labelToFeatures, ll.labelDomain, ll.instanceDomain)(ll.labelManifest, ll.featureManifest)
    SVMTrainer.train(model, ll.labelDomain.length, ll.featureDomain.dimensionSize, ll.toSeq, ll.toSeq.map(ll.labelToFeatures), parallel)
    new ModelBasedClassifier(model, ll.head.domain)
  }
}