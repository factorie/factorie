package cc.factorie

import scalala.tensor.dense.DenseVector

	// extends Templates
	trait CWLearning extends GenericDensePerceptronLearning //extends MIRALearning
	{
		type TemplateType <: CWLearning
  
		override def learningMethod = "CWLearning"
  
  	/**Initialize the diagonal covariance matrix; this is the value in the diagonal elements */
  	val initialVariance = 0.1;

		//def weights : Vector //if we did it "loglinear learning" style
		//def scratch : Vector
		//lazy val sigma = new DenseVector(Array.make(suffsize,initialVariance));
		//lazy val scratch = new SparseVector(suffsize);
		lazy val sigma = {freezeDomains; new DenseVector(Array.make(statsize, initialVariance))}
		lazy val scratch = {freezeDomains; new DenseVector(statsize)}
	}

	// extends MHSampler
	abstract class MHCWLearner(model:Model, objective:Model) extends MHPerceptronLearner(model, objective)
	{
		/**Confidence value */
  	val epsilon = 0.0000001
  	val eta = 0.95; //it does not really make sense for this to be less than 0.75, since probit(0.5)=0 and probit(x<0.5)<0

  	/**Function of the confidence, precomputed because probit is expensive. */
  	val gaussDeviate = Maths.probit(eta);

		private def kktMultiplier(theModelScoreRatio: Double, fnu: Boolean): Double =
			{
				var marginMean = theModelScoreRatio;
				if (!fnu) marginMean = -marginMean;
				val v = 1 + 2 * gaussDeviate * marginMean;
				val marginVar = varianceOfMargin;
				var lambda = 0.0;
				if (marginMean >= gaussDeviate * marginVar)
					return 0.0;
				if (marginVar > 0 + epsilon || marginVar < 0 - epsilon)
					lambda = (-v + Math.sqrt(v * v - 8 * gaussDeviate * (marginMean - gaussDeviate * marginVar))) / (4 * gaussDeviate * marginVar);
				Math.max(0, lambda);
			}

		//puts a densevector difference on the scratch tape
		private def putDiffOnScratch: Unit = {
			model.templatesOf[CWLearning].foreach(t => t.scratch.zero)
			difflist.redo;
			model.factorsOf[CWLearning](difflist).foreach(factor => factor.template.scratch += factor.statistic.vector)
			difflist.undo;
			model.factorsOf[CWLearning](difflist).foreach(factor => factor.template.scratch -= factor.statistic.vector)
		}

		/**Returns variance of margin */
		private def varianceOfMargin: Double = {
			var result = 0.0
			putDiffOnScratch
			model.templatesOf[CWLearning].foreach(t => {
				for ((i, v) <- t.scratch.activeElements)
					result += v * v * t.sigma(i);
			})
			result
		}

		private def updateMu(incr: Double): Unit = {
			//System.out.println("inc="+incr);
			model.factorsOf[CWLearning](difflist).foreach(factor => {
				//factor.template.weights += factor.vector * factor.template.sigma * incr // Why doesn't this work?
				for (i <- factor.statistic.vector.activeDomain)
					factor.template.weights(i) += factor.statistic.vector(i) * factor.template.sigma(i) * incr
			})
		}

		def updateSigma(incr: Double): Unit = {
			putDiffOnScratch;
			// Square diff to obtain diag(\bf{x}_i)
			model.templatesOf[CWLearning].foreach(t =>
							for ((index, value) <- t.scratch.activeElements) {
								t.sigma(index) = 1.0 / ((1.0 / t.sigma(index)) + 2 * incr * gaussDeviate * value * value)
							})
		}

		override def sampleAndLearn(numIterations: Int): Unit =
			{
				for (iteration <- 0 until numIterations)
					{
						jumpAccepted = false;
						difflist = new DiffList
						// Jump until difflist has changes
						while (difflist.size <= 0) modelTransitionRatio = propose(model, difflist)
						newTruthScore = difflist.score(objective)
						modelScoreRatio = difflist.scoreAndUndo(model)
						oldTruthScore = difflist.score(objective)
						modelRatio = modelScoreRatio // + modelTransitionRatio
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
						logAccProb = (modelScoreRatio / temperature) + modelTransitionRatio
						if (logAccProb > Math.log(random.nextDouble))
							{
								if (modelRatio < 0)
									numNegativeMoves += 1
								numAcceptedMoves += 1
								jumpAccepted = true;
								difflist.redo
							}
						mhPerceptronPostProposalHook
						incrementIterations
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
	}
 
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

