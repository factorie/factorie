/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.optimize
import cc.factorie._
import cc.factorie.infer.ProposalSampler
import cc.factorie.la._
import cc.factorie.model.{DotFamily, Model, Parameters, WeightsSet}
import cc.factorie.util.DoubleAccumulator

/** Provides a gradient that encourages the model.score to rank its best proposal the same as the objective.score would, with a margin.
    @author Andrew McCallum */
class SampleRankExample[C](val context: C, val sampler: ProposalSampler[C]) extends Example {
  var learningMargin = 1.0
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    require(gradient != null, "The SampleRankExample needs a gradient accumulator")
    require(value != null, "The SampleRankExample needs a value accumulator")
    //val familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
    val proposals = sampler.proposals(context)
    val bestModel = proposals.maxByDouble(_.modelScore)
    val bestObjectiveScore = proposals.map(_.objectiveScore).max
    if (bestModel.objectiveScore != bestObjectiveScore) {
      val bestObjective = proposals.filter(_.objectiveScore == bestObjectiveScore).maxByDouble(_.modelScore)
      bestObjective.diff.redo()
      sampler.model.factorsOfFamilyClass[DotFamily](bestObjective.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      bestObjective.diff.undo()
      sampler.model.factorsOfFamilyClass[DotFamily](bestObjective.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      bestModel.diff.redo()
      sampler.model.factorsOfFamilyClass[DotFamily](bestModel.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
      bestModel.diff.undo()
      sampler.model.factorsOfFamilyClass[DotFamily](bestModel.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
      value.accumulate(bestObjective.modelScore - bestModel.modelScore - learningMargin)
    } else {
      val diffProposals = proposals.filter(_.objectiveScore != bestModel.objectiveScore)
      if (diffProposals.nonEmpty) {
        val bestModel2 = diffProposals.maxByDouble(_.modelScore)
        if (bestModel.modelScore - bestModel2.modelScore < learningMargin) {
          bestModel.diff.redo()
          sampler.model.factorsOfFamilyClass[DotFamily](bestModel.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
          bestModel.diff.undo()
          sampler.model.factorsOfFamilyClass[DotFamily](bestModel.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
          bestModel2.diff.redo()
          sampler.model.factorsOfFamilyClass[DotFamily](bestModel2.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0))
          bestModel2.diff.undo()
          sampler.model.factorsOfFamilyClass[DotFamily](bestModel2.diff).foreach(f => gradient.accumulate(f.family.weights, f.currentStatistics))
          value.accumulate(bestModel.modelScore - bestModel2.modelScore - learningMargin)
        }
      }
    }
    sampler.processProposals(proposals)
  }
}

/** A Trainer that packages together the SampleRankExample and a GradientOptimizer.
    @author Andrew McCallum */
class SampleRankTrainer[C](weightsSet: WeightsSet, sampler: ProposalSampler[C], optimizer: GradientOptimizer = new optimize.AdaGrad, logEveryN: Int = Int.MaxValue)
 extends optimize.OnlineTrainer(weightsSet,optimizer,maxIterations=10000, logEveryN=logEveryN) {
  def this(sampler: ProposalSampler[C], optimizer: GradientOptimizer) = this(sampler.model.asInstanceOf[Model with Parameters].parameters, sampler, optimizer)
  def this(sampler: ProposalSampler[C]) = this(sampler.model.asInstanceOf[Model with Parameters].parameters, sampler, new optimize.AdaGrad)
  def processContext(context: C): Unit = process(new SampleRankExample(context, sampler))
  def processContext(context: C, iterations: Int): Unit = for (i <- 0 until iterations) process(new SampleRankExample(context, sampler))
  def processContexts(contexts: Iterable[C]): Unit = processExamples(contexts.map(c => new SampleRankExample(c, sampler)))
  def processContexts(contexts: Iterable[C], iterations: Int): Unit = for (i <- 0 until iterations) processContexts(contexts)
  def process(example: Example): Unit = processExamples(Seq(example))
  def processExamples(examples: Iterable[Example], iterations: Int): Unit = for (i <- 0 until iterations) processExamples(examples)
}

// In the old SampleRank there was something like the following.  Do we need this for any reason?
/*
  def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)      
    } else {
      // the objective function has some preference (e.g. we don't have an unlabeled example here)
      (bestObjective1.objectiveScore > bestObjective2.objectiveScore || bestObjective1.objectiveScore > bestModel1.objectiveScore) &&
      // the model got it wrong, or isn't confident enough about being right
      // TODO should this be based on acceptanceScore instead of modelScore?
      ((bestModel1 ne bestObjective1) || math.abs(bestModel1.modelScore - bestModel.modelScore) < learningMargin)
    }
  }

 */

