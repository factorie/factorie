package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import cc.factorie.variable.{Var, LabeledMutableVar, DiffList}
import cc.factorie.model.{Parameters, Model, DotFamily}
import cc.factorie.infer.Sampler

/**
  * A training example for using contrastive divergence.
  * @param context The argument to the sampler
  * @param model The model to be optimized
  * @param sampler The sampler.
  * @param k The number of steps to sample for.
  * @tparam C The type of sampler context
  */
class ContrastiveDivergenceExample[C](val context: C, model: Model with Parameters, val sampler: Sampler[C], val k: Int = 1) extends Example {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceExample needs a gradient accumulator")
    val proposalDiff = new DiffList
    repeat(k) { proposalDiff ++= sampler.process(context) }
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
    proposalDiff.undo()
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
  }
}

/**
 * A variant of the contrastive divergence algorithm which does not reset to the ground truth.
 * @param context The argument to the sampler
 * @param model The model
 * @param sampler The sampler
 * @tparam C The type of sampler context
 */
class PersistentContrastiveDivergenceExample[C <: LabeledMutableVar](val context: C, model: Model with Parameters, val sampler: Sampler[Var]) extends Example {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceExample needs a gradient accumulator")
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
    groundTruthDiff.undo()
    val proposalDiff = sampler.process(context)
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
  }
}

/**
 * Contrastive divergence with the hinge loss.
 * @param context The argument to the sampler
 * @param model The model
 * @param sampler The sampler
 * @param learningMargin The margin in the hinge loss
 * @param k The number of iterations to sample for
 * @tparam C The type of sampler context
 */
class ContrastiveDivergenceHingeExample[C <: Var](
  val context: C, model: Model with Parameters, val sampler: Sampler[C], val learningMargin: Double = 1.0, val k: Int = 1) extends Example {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(value != null, "The ContrastiveDivergenceHingeExample needs a value accumulator")
    val truthScore = model.currentScore(context)
    val proposalDiff = new DiffList
    repeat(k) { proposalDiff ++= sampler.process(context) }
    val proposalScore = model.currentScore(context)
    if (truthScore - proposalScore < learningMargin) {
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      proposalDiff.undo()
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      value.accumulate(truthScore - proposalScore)
    } else
      proposalDiff.undo()
  }
}

/**
 * A contrastive divergence hinge example which keeps the chain going.
 * @param context The argument to the sampler
 * @param model The model
 * @param sampler The sampler
 * @param learningMargin The hinge loss margin
 * @tparam C The type of sampler context
 */
class PersistentContrastiveDivergenceHingeExample[C <: LabeledMutableVar](
  val context: C, model: Model with Parameters, val sampler: Sampler[Var], val learningMargin: Double = 1.0) extends Example {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(value != null, "The PersistentContrastiveDivergenceHingeExample needs a value accumulator")
    val proposalDiff = sampler.process(context)
    val currentConfigScore = model.currentScore(context)
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    val truthScore = model.currentScore(context)
    if (truthScore - currentConfigScore < learningMargin) {
      model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      groundTruthDiff.undo()
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      value.accumulate(truthScore - currentConfigScore)
    } else
      groundTruthDiff.undo()
  }
}