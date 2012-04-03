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
import collection.mutable.HashMap

/** Set the parameters so that the model.score ranks the top sample the same as the objective.score, with a margin. */
trait SampleRank extends ProposalSampler0 with SettingsSampler0 {
  this: ProposalSampler[_] =>
  def model: Model
  def familiesToUpdate: Seq[DotFamily] // provided by GradientAscentUpdates
  var learningMargin = 1.0
  def updateWeights: Unit
  val amIMetropolis = this.isInstanceOf[MHSampler[_/*Variable*/]] // TODO Can we find a way to avoid this special case?
  var logLevel = 0

  /** If objective has not yet been set non-null, get it from the cc.factorie.defaultObjective. */
  abstract override def objective = if (super.objective == null) cc.factorie.defaultObjective else super.objective
  
  var bestModel1, bestModel2, bestObjective1, bestObjective2, changeProposal : Proposal = null

  def predictedScore = changeProposal.modelScore
  def targetScore = changeProposal.objectiveScore

  abstract override def proposalsHook(proposals:Seq[Proposal]) : Unit = {
    if (proposals.length < 2) return
    super.proposalsHook(proposals)
    val bestModels = proposals.max2ByDouble(_ modelScore)
    val bestObjectives = proposals.max2ByDouble(_ objectiveScore)
    bestModel1 = bestModels._1
    bestModel2 = bestModels._2
    bestObjective1 = bestObjectives._1
    bestObjective2 = bestObjectives._2
    changeProposal = if(bestModel1.diff.size>0) bestModel1 else bestModel2
    assert(bestObjective1.objectiveScore == bestObjective1.objectiveScore) // Check not NaN 
    assert(bestObjective2.objectiveScore == bestObjective2.objectiveScore)  

    if (shouldUpdate) updateWeights
  }
  
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

  def projectGradient(f: DotFamily, g: Vector): Unit = {}
 
  def addGradient(accumulator: DotFamily=>Vector, rate:Double): Unit = {

    // Only do learning if the trueScore has a preference
    // It would not have a preference if the variable in question is unlabeled
    // TODO Is this the right way to test this though?  Could there be no preference at the top, but the model is selecting something else that is worse?
    if (shouldUpdate) {
      val gradient = new HashMap[DotFamily,Vector] {
        override def default(f: DotFamily) = {
          val vector = f.newWeightsTypeVector(0.0)
          this(f) = vector
          vector
        }
      }
      // If the model doesn't score the truth highest, then update parameters
      if (bestModel1 ne bestObjective1) { // TODO  I changed != to "ne"  OK?  Should I be comparing values here instead?
        // ...update parameters by adding sufficient stats of truth, and subtracting error
        bestObjective1.diff.redo
        model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  rate)
        bestObjective1.diff.undo
        model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  -rate)
        bestModel1.diff.redo
        model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  -rate)
        bestModel1.diff.undo
        model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  rate)
      }
      else if (bestModel1.modelScore - bestModel2.modelScore < learningMargin) {
        // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
        bestObjective1.diff.redo
        model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  rate)
        bestObjective1.diff.undo
        model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  -rate)
        bestModel2.diff.redo
        model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  -rate)
        bestModel2.diff.undo
        model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => gradient(f.family) += f.statistics.vector *  rate)
      }
      // project gradients
      gradient.foreach(g => {
        projectGradient(g._1,g._2)
        accumulator(g._1) += g._2
      })
    }
  }
}
