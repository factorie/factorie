package cc.factorie.app.nlp.coref

import cc.factorie.optimize.{Example, LinearObjectives, LinearBinaryExample}
import cc.factorie.la.{Tensor1, WeightsMapAccumulator, SparseBinaryTensor}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.model.Weights1

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:22 PM
 */

class LeftRightExample(weights: Weights1, label: MentionPairLabel, recSlackRescale: Double = 3.0)
  extends LinearBinaryExample(weights, label.features.value, if (label.intValue == 0) 1 else -1, LinearObjectives.hingeScaledBinary(negSlackRescale = recSlackRescale))

class LeftRightImplicitConjunctionExample(model: ImplicitCrossProductCorefModel, label: MentionPairLabel, recSlackRescale: Double = 3.0) extends Example {
  val y = if (label.intValue == 0) 1 else -1
  val objective = LinearObjectives.hingeScaledBinary(negSlackRescale = recSlackRescale)
  val featureVector: Tensor1 = label.features.value
  val hashCross = new ImplicitFeatureConjunctionTensor(model.MentionPairCrossFeaturesDomain.dimensionSize, featureVector.asInstanceOf[SparseBinaryTensor], model.domain)
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    val score = (model.pairwise.value dot featureVector) + (model.products.value dot hashCross)
    val (obj, sgrad) = objective.valueAndGradient(score, y)
    if (value != null) value.accumulate(obj)
    if (gradient != null) {
      gradient.accumulate(model.pairwise, featureVector, sgrad)
      gradient.accumulate(model.products, hashCross, sgrad)
    }
  }
}