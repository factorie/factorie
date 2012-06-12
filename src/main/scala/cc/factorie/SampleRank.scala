/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie
import cc.factorie.la._
import cc.factorie.optimize.GradientOptimizer
import collection.mutable.HashMap

/** Set the parameters so that the model.score ranks the top sample the same as the objective.score, with a margin. */
class SampleRank[C](val model:Model, sampler:ProposalSampler[C], optimizer:GradientOptimizer) {
  def this(sampler:ProposalSampler[C], optimizer:GradientOptimizer) = this(sampler.model, sampler, optimizer)
  var learningMargin = 1.0
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  def gradientAndMargin(proposals:Seq[Proposal]): (Tensor,Double) = {
    var g: WeightsTensor = null // model.newSparseWeightsTensor
    //val g2 = new TensorMap; def gg(df:DotFamily): Tensor = g.getOrElseUpdate(df, Tensor.newSparse(df.weights)
    val bestModels = proposals.max2ByDouble(_ modelScore); val bestModel1 = bestModels._1; val bestModel2 = bestModels._2
    val bestObjectives = proposals.max2ByDouble(_ objectiveScore); val bestObjective1 = bestObjectives._1; val bestObjective2 = bestObjectives._2
    val margin = bestModel1.modelScore - bestModel2.modelScore
    if (bestModel1 ne bestObjective1) {
      // ...update parameters by adding sufficient stats of truth, and subtracting error
      g = model.newSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
    }
    else if (margin < learningMargin) {
      // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
      g = model.newSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel2.diff.redo
      model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel2.diff.undo
      model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
    }
    (g, margin)
  }
  def process(c:C): DiffList = {
    val proposals = sampler.proposals(c)
    val (gradient,margin) = gradientAndMargin(proposals)
    if (gradient ne null)
      optimizer.step(model.weightsTensor, gradient, Double.NaN, margin)
    val bestProposal = proposals.maxByDouble(_.modelScore)
    bestProposal.diff.redo
    bestProposal.diff
  }
  def process(c:C, repeat:Int): Unit = for (i <- 0 until repeat) process(c)
  def processAll(cs:Iterable[C]): Unit = cs.foreach(process(_))
  def processAll(cs:Iterable[C], repeat:Int): Unit = for (i <- 0 until repeat) cs.foreach(process(_))
}

// In the old SampleRank there was something like the following.  Do we need this for any reason?
/*
  def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)      
    } else {
      // the objective function has some preference (e.g. we don't have an unlabeled example here)
      (bestObjective1.objectiveScore > bestObjective2.objectiveScore || bestObjective1.objectiveScore > bestModel1.objectiveScore) &&
      // the model got it wrong, or isn't confident enough about being right
      // TODO should this be based on acceptanceScore instead of modelScore?
      ((bestModel1 ne bestObjective1) || math.abs(bestModel1.modelScore - bestModel2.modelScore) < learningMargin)
    }
  }

 */

