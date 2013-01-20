package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la.WeightsTensorAccumulator
import cc.factorie.util.DoubleAccumulator

class ContrastiveDivergenceExample[C](val context: C, val sampler: ProposalSampler[C]) extends Example[Model] {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceExample needs a gradient accumulator")
    val proposal = sampler.pickProposal(sampler.proposals(context))
    proposal.diff.redo
    model.factorsOfFamilyClass[DotFamily](proposal.diff).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
    proposal.diff.undo
    model.factorsOfFamilyClass[DotFamily](proposal.diff).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
  }
}

class PersistentContrastiveDivergenceExample[C <: LabeledMutableVar[_]](val context: C, val sampler: ProposalSampler[Var]) extends Example[Model] {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceExample needs a gradient accumulator")
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
    groundTruthDiff.undo
    val proposalDiff = sampler.processProposals(sampler.proposals(context))
    model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
  }
}

class ContrastiveDivergenceHingeExample[C <: Var](
  val context: C, val sampler: ProposalSampler[C], val learningMargin: Double = 1.0) extends Example[Model] {
  // NOTE: this assumes that variables are set to the ground truth when this method is called
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator): Unit = {
    require(gradient != null, "The ContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(margin != null, "The ContrastiveDivergenceHingeExample needs a margin accumulator")
    val proposal = sampler.pickProposal(sampler.proposals(context))
    if (-proposal.modelScore < learningMargin) {
      proposal.diff.redo
      model.factorsOfFamilyClass[DotFamily](proposal.diff).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      proposal.diff.undo
      model.factorsOfFamilyClass[DotFamily](proposal.diff).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
      margin.accumulate(-proposal.modelScore)
      println("margin: %f" format (-proposal.modelScore))
    }
  }
}

class PersistentContrastiveDivergenceHingeExample[C <: LabeledMutableVar[_]](
  val context: C, val sampler: ProposalSampler[Var], val learningMargin: Double = 1.0) extends Example[Model] {
  // NOTE: this assumes that the initial configuration is the ground truth
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator): Unit = {
    require(gradient != null, "The PersistentContrastiveDivergenceHingeExample needs a gradient accumulator")
    require(margin != null, "The PersistentContrastiveDivergenceHingeExample needs a margin accumulator")
    val proposalDiff = sampler.processProposals(sampler.proposals(context))
    val currentConfigScore = model.currentScore(context)
    val groundTruthDiff = new DiffList
    context.setToTarget(groundTruthDiff)
    val truthScore = model.currentScore(context)
    if (truthScore - currentConfigScore < learningMargin) {
      model.factorsOfFamilyClass[DotFamily](groundTruthDiff).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
      groundTruthDiff.undo
      model.factorsOfFamilyClass[DotFamily](proposalDiff).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      margin.accumulate(truthScore - currentConfigScore)
      println("margin: %f" format (truthScore - currentConfigScore))
    } else
      groundTruthDiff.undo
  }
}