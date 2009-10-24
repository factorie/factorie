package cc.factorie
import scala.collection.mutable.HashMap
import scala.reflect.Manifest

 
abstract class MHSampleRank[C](model:Model, val objective:Model)(implicit mc:Manifest[C]) extends MHSampler[C](model)(mc) {
  var learningMargin = 1.0

  //def updatePostProposal(diff:DiffList, modelDiff:Double, objectiveDiff:Double) : Unit
  def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit 

  protected var objectiveDiff = 0.0
  protected var lastIteration = -1
  
  override def postProposalHook(difflist:DiffList) : Unit = {
    lastIteration = iterationCount
    objectiveDiff = difflist.score(objective)
  }
  
  override def postUndoHook(modelDiff:Double, difflist:DiffList) : Unit = {
    assert (iterationCount == lastIteration) // Make sure that objectiveDiff is getting subtracted from correct callback
    objectiveDiff -= difflist.score(objective)
    //println("MHSampleRank modelDiff="+modelDiff+" objectiveDiff="+objectiveDiff)
    if (objectiveDiff * modelDiff <= 0 || Math.abs(modelDiff) < learningMargin) {
      // We have some learning to do
      val stayProposal = new Proposal(new DiffList, 0.0, 0.0)
      val goProposal = new Proposal(difflist, modelDiff, objectiveDiff)
      var bestModel1, bestModel2, bestObjective1, bestObjective2 : Proposal = null
    	if (modelDiff > 0) {
    		bestModel1 = goProposal; bestModel2 = stayProposal
    	} else {
    	  bestModel1 = stayProposal; bestModel2 = goProposal
    	}
      if (objectiveDiff > 0) {
        bestObjective1 = goProposal; bestObjective2 = stayProposal
      } else {
        bestObjective1 = stayProposal; bestObjective2 = goProposal
      }
      updateWeights(bestModel1, bestModel2, bestObjective1, bestObjective2)
    }
  }

}


/*
class SampleRankLearner(model:Model,objective:Objective) extends Sampler {
    //the update rule to change the weights given a feature delta,
model and objective ratio
    var updateRule:UpdateRule //could default to PerceptronUpdateRule

    //note that we could have template specific update rules by just using
    //val updateRules = new HashMap[Template,UpdateRule]

    //this is the model we store the feature deltas in
    val deltaModel = model.clone

    def learn(context:C) = {
         deltaModel.setWeightsToAllZeros
         val ratio = sampler.propose(context, difflist)
         var newObjective = difflist.score(objective)
         deltaModel.factors(difflist).foreach(f => f.template.weights
+= f.statistic.vector)
         difflist.undo
         var oldObjective = difflist.score(objective)
         deltaModel.factors(difflist).foreach(f => f.template.weights
-= f.statistic.vector)
         //now update the weights of the model given the delta
weights, the model ratio and the objective delta
         update(model, deltaModel,newObjective-oldObjective, ratio,
deltaModel.factors(difflist))

         //here would be the code that actually makes the jump
according to model ratio etc.
    }
}
*/