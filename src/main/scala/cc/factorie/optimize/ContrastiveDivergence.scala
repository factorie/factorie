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