package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap
import scalala.tensor.dense.DenseVector
import scalala.tensor.Vector
import scalala.tensor.sparse.SparseVector

//TODO: do not make this SampleRank specific
trait ConfidenceWeightedUpdates extends WeightUpdates with SampleRank {
  override type TemplatesToUpdate = DotTemplate
  def model : Model
  def learningMargin : Double
  var learningRate : Double = 1

    val epsilon = 0.0000001
    val eta = 0.95; //condifidence value, make sure eta>0.5 because probit(0.5)=0 and probit(x<0.5)<0
    
    /**Function of the confidence (it is more intuitive to express eta than the gaussian deviate directly). */
    val gaussDeviate = Maths.probit(eta);


  def updateWeights : Unit = {
    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    if(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore==0)
      return;
    val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
  	template.freezeDomains
  	val vector = new SparseVector(template.statsize)
  	this(template) = vector
  	vector
      }
    }
    addGradient(gradient,1.0)

    val learningRate = kktMultiplier(changeProposal,gradient)
    updateParameters(gradient)
    updateSigma(gradient)
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
 
  def kktMultiplier(proposal:Proposal,gradient:HashMap[TemplatesToUpdate,SparseVector]) : Double =
    {
      val marginMean = proposal.modelScore.abs
      val v = 1.0 + 2.0 * gaussDeviate * marginMean
      val marginVar = marginVariance(gradient)
      var lambda = 0.0
     if(marginMean >= gaussDeviate * marginVar)
	return 0.0
      if(marginVar > epsilon || marginVar < -epsilon)
	lambda = (-v + Math.sqrt(v*v - 8*gaussDeviate*(marginMean - gaussDeviate*marginVar))) / (4*gaussDeviate*marginVar);
      Math.max(0, lambda);
    }


  def marginVariance(gradient:HashMap[TemplatesToUpdate,SparseVector]):Double =
    {
      var result : Double = 0
      for((template,templateGradient)<-gradient)
	{
	  val templateSigma = sigma(template)
	  for((index,value)<-templateGradient.activeElements)
	      result += value*value*templateSigma(index)
	}
      result
    }

  def updateSigma(gradient: HashMap[DotTemplate,SparseVector]) : Unit =
    {
      for((template,templateGrad)<-gradient)
	{
	  val ratesTemplate = sigma(template)
	  for((index,value)<-templateGrad.activeElements)
	      ratesTemplate(index) = 1.0 / ((1.0 / ratesTemplate(index)) 
				  + 2*learningRate*gaussDeviate*value*value)
	}
    }

  def updateParameters(gradient:HashMap[TemplatesToUpdate,SparseVector]) : Unit =
    {
      for((template,templateGradient)<-gradient)
	{
	  val templateSigma = sigma(template)
	  for((index,value)<-templateGradient.activeElements)
	    template.weights(index) += 
	      templateGradient(index)*templateSigma(index)*learningRate
	}
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

