package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator

class ContrastiveDivergenceExample[C](model: Model with Parameters, val context: C, val sampler: Sampler[C], val k: Int = 1) extends Example {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceExample needs a gradient accumulator")
    val proposalDiff = new DiffList
    repeat(k) { proposalDiff ++= sampler.process(context) }
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
    proposalDiff.undo
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
  }
}

class PersistentContrastiveDivergenceExample[C <: LabeledMutableVar[_]](model: Model with Parameters, val context: C, val sampler: Sampler[Var]) extends Example {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceExample needs a gradient accumulator")
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
    groundTruthDiff.undo
    val proposalDiff = sampler.process(context)
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
  }
}

class ContrastiveDivergenceHingeExample[C <: Var](
  model: Model with Parameters, val context: C, val sampler: Sampler[C], val learningMargin: Double = 1.0, val k: Int = 1) extends Example {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(value != null, "The ContrastiveDivergenceHingeExample needs a value accumulator")
    val truthScore = model.currentScore(context)
    val proposalDiff = new DiffList
    repeat(k) { proposalDiff ++= sampler.process(context) }
    val proposalScore = model.currentScore(context)
    if (truthScore - proposalScore < learningMargin) {
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      proposalDiff.undo
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      value.accumulate(truthScore - proposalScore)
    } else
      proposalDiff.undo
  }
}

class PersistentContrastiveDivergenceHingeExample[C <: LabeledMutableVar[_]](
  model: Model with Parameters, val context: C, val sampler: Sampler[Var], val learningMargin: Double = 1.0) extends Example {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(value != null, "The PersistentContrastiveDivergenceHingeExample needs a value accumulator")
    val proposalDiff = sampler.process(context)
    val currentConfigScore = model.currentScore(context)
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    val truthScore = model.currentScore(context)
    if (truthScore - currentConfigScore < learningMargin) {
      model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      groundTruthDiff.undo
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      value.accumulate(truthScore - currentConfigScore)
    } else
      groundTruthDiff.undo
  }
}