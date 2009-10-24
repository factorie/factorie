package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap

import scalala.tensor.dense.DenseVector
import scalala.tensor.Vector

trait ConfidenceWeightedUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
	def learningRate : Double
	def learningRate_=(x:Double) : Unit
  def model : Model
  def learningMargin : Double

  abstract override def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit = {
    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    //learningRate = kktMultiplier(changeProposal, 1, true);
    super.updateWeights(bestModel1, bestModel2, bestObjective1, bestObjective2)
	}
  
	/**Initialize the diagonal covariance matrix; this is the value in the diagonal elements */
	val initialVariance = 0.1;

	val sigma = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = { 
      template.freezeDomains
      val vector = DenseVector(template.statsize)(initialVariance)
      this(template) = vector
      vector
    }
  }
	val scratch = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = { 
      template.freezeDomains
      val vector = new DenseVector(template.statsize)
      this(template) = vector
      vector
    }
  }
 
	val epsilon = 0.0000001
	val eta = 0.95; //it does not really make sense for this to be less than 0.75, since probit(0.5)=0 and probit(x<0.5)<0

	/**Function of the confidence, precomputed because probit is expensive. */
	val gaussDeviate = Maths.probit(eta);

	private def kktMultiplier(theModelScoreRatio: Double, fnu: Boolean): Double =	{
		var marginMean = theModelScoreRatio;
		if (!fnu) marginMean = -marginMean;
		val v = 1 + 2 * gaussDeviate * marginMean;
		val marginVar = 0 // TODO varianceOfMargin; // What goes here???? !!!
		var lambda = 0.0;
		if (marginMean >= gaussDeviate * marginVar)
			return 0.0;
		if (marginVar > 0 + epsilon || marginVar < 0 - epsilon)
			lambda = (-v + Math.sqrt(v * v - 8 * gaussDeviate * (marginMean - gaussDeviate * marginVar))) / (4 * gaussDeviate * marginVar);
		Math.max(0, lambda);
	}

	//puts a densevector difference on the scratch tape
	private def putDiffOnScratch(difflist:DiffList): Unit = {
		model.templatesOf[TemplatesToUpdate].foreach(t => scratch(t).zero)
		difflist.redo;
		model.factorsOf[TemplatesToUpdate](difflist).foreach(factor => scratch(factor.template) += factor.statistic.vector)
		difflist.undo;
		model.factorsOf[TemplatesToUpdate](difflist).foreach(factor => scratch(factor.template) -= factor.statistic.vector)
	}

	/**Returns variance of margin */
	private def varianceOfMargin(difflist:DiffList): Double = {
		var result = 0.0
		putDiffOnScratch(difflist)
		model.templatesOf[TemplatesToUpdate].foreach(t => {
		  val templateSigma = sigma(t)
			for ((i, v) <- scratch(t).activeElements)
				result += v * v * templateSigma(i);
		})
		result
	}

	private def updateMu(difflist:DiffList, incr: Double): Unit = {
		//System.out.println("inc="+incr);
		model.factorsOf[TemplatesToUpdate](difflist).foreach(factor => {
			//factor.template.weights += factor.vector * factor.template.sigma * incr // Why doesn't this work?;
		  val templateSigma = sigma(factor.template)
			for (i <- factor.statistic.vector.activeDomain)
				factor.template.weights(i) += factor.statistic.vector(i) * templateSigma(i) * incr
		})
	}

	def updateSigma(difflist:DiffList, incr: Double): Unit = {
		putDiffOnScratch(difflist)
		// Square diff to obtain diag(\bf{x}_i)
		model.templatesOf[TemplatesToUpdate].foreach(t => {
		  val templateSigma = sigma(t)
			for ((index, value) <- scratch(t).activeElements) {
				templateSigma(index) = 1.0 / ((1.0 / templateSigma(index)) + 2 * incr * gaussDeviate * value * value)
			}})
	}

 
}


 /*
		override def process(context:C, difflist:DiffList): Unit = {
				learningIterations += 1
						proposalAccepted = false;
						// Jump until difflist has changes
						while (difflist.size <= 0) modelTransitionRatio = propose(context, difflist)
						val newTruthScore = difflist.score(objective)
						modelScoreRatio = difflist.scoreAndUndo(model)
						val oldTruthScore = difflist.score(objective)
						val modelRatio = modelScoreRatio // + modelTransitionRatio
						bWeightsUpdated = false
						bFalsePositive = false;
						bFalseNegative = false;
						//System.out.println("newTruthScore="+newTruthScore+"  oldTS="+oldTruthScore);
						if (newTruthScore > oldTruthScore && modelRatio <= 0)
							{
								learningRate = kktMultiplier(modelScoreRatio, true);
								//putDiffOnScratch
								updateMu(-learningRate)
								difflist.redo
								updateMu(learningRate)
								difflist.undo
								bWeightsUpdated = true
								bFalseNegative = true
							}
						else if (newTruthScore < oldTruthScore && modelRatio >= 0)
							{
								learningRate = kktMultiplier(modelScoreRatio, false);
								//putDiffOnScratch
								updateMu(learningRate)
								difflist.redo
								updateMu(-learningRate)
								difflist.undo
								bWeightsUpdated = true
								bFalsePositive = true
							}
						if (bWeightsUpdated)
							{
								numUpdates += 1;
								updateSigma(learningRate);
								/*
						 //////////TEST
						 difflist.redo
						 val test:Double = difflist.scoreAndUndo
						 System.out.println("  after update: " + test+" before: " + modelRatio+" lam="+learningRate);
						 val td:Double = newTruthScore-oldTruthScore;
						 var agreeAft=true;
						 if(td<0 && test>0 || td>0 && test<0)
						 agreeAft=false;

						 var agree = true;
						 if(td<0 && modelRatio>0 || td>0 && modelRatio<0)
						 agree=false;
						 System.out.println("  agree before,aft: "+agree+"/"+agreeAft);
						 /////////////
						 */
								if (useAveraged)
									{
										model.templatesOf[CWLearning].foreach(t => {
											for (i <- 0 until t.weightsSum.size)
												t.weightsSum(i) += t.weights(i)
											t.weightsDivisor += 1
										})
									}
							}
						val logAccProb = (modelScoreRatio / temperature) + modelTransitionRatio
						if (logAccProb > Math.log(random.nextDouble))
							{
								if (modelRatio < 0)
									numNegativeMoves += 1
								numAcceptedMoves += 1
								proposalAccepted = true;
								difflist.redo
							}
						postProposalHook(difflist)
					}
				//
				//Put the weights average into each Factor's weights array
				if (useAveraged)
					{
						model.templatesOf[CWLearning].foreach(t => {
							var weightsDivisor = t.weightsDivisor
							for (i <- 0 until t.weights.size)
								t.weights(i) = t.weightsSum(i) / weightsDivisor
						})
					}
			}
	*/
 
 
 
	// TODO perhaps this should be re-factored to have more in common with GibbsPerceptronLearner?
	// At least it should offer more similar methods, such as
  // def sampleAndLearn[X](variables: Iterable[CoordinatedEnumVariable[X]], numIterations: Int): Unit
   /*
	class GibbsCWLearner[X](model:Model, val variables:Iterable[CoordinatedEnumVariable[X]]) extends MHCWLearner(model) {
	  var iter = variables.elements
    assert(iter.hasNext)
	  def propose(d:DiffList) : Double = {
	    import cc.factorie.util.Implicits._
	    if (!iter.hasNext) iter = variables.elements
	    val variable = iter.next
      val proposals = variable.multiPropose(model, d, false)
			val proposal = proposals.max(_.modelScore)
   		proposal.diff.redo
			d ++= proposal.diff
			0.0
	  }	
	}
  */

