package cc.factorie

import scalala.Scalala._
import scalala.tensor.dense.DenseVector
//import scalala.tensor.operators.TensorOp
//import scalala.tensor.operators.OperatorImplicits

	trait MIRALearning extends PerceptronLearning //extends template
	{
		type TemplateType <: MIRALearning
		lazy val denseDiff = {freezeDomains; new DenseVector(statsize)}
	}


	abstract class MHMIRALearner(model:Model, objective:Model) extends MHPerceptronLearner(model, objective)
	{
  	protected val epsilon: Double = 0.000000001;
		def kktMultiplier(loss: Double, fnu: Boolean): Double =
			{
				var logP = modelScoreRatio;
				if (!fnu)
					logP = -logP;
				//loss could be 0/1 or diff in f1
				val l2sqrd: Double = computeDenseDiffL2(fnu);
				val errorGradient: Double = loss - logP;
				var lambda: Double = 0;
				if (l2sqrd > 0 + epsilon || l2sqrd < 0 - epsilon)
					lambda = errorGradient / l2sqrd;
				if (lambda < 0) //no error (the passive part of passive-aggressive)
					lambda = 0;
				lambda;
			}

		/*
		 def computeSparseDiffL2() : Double =
		 {
		 HashMap[Int,Double] vec = new HashMap[Int,Double];
		 var l2n2:Double=0;
		 difflist.redo;
		 difflist.scoreFactors.foreach(factor =>
		 factor.incrementArray(factor.template.asInstanceOf[MIRALearning].denseDiff)(1))

}
def addToHashMap(vec:HashMap[Int,Double], t:Template)
{
val f = t.asInstanceOf[MIRALearning];

}
*/

		def computeDenseDiffL2(fnu: Boolean): Double =
			{
				//TODO: investigate why sign matters
				var sign = 1;
				if (fnu)
					sign = -1;
				var l2n2: Double = 0;
				//
				//zero the difference
				model.templatesOf[MIRALearning].foreach(f => {
					f.denseDiff.zero
				})
				//
				//compute modified config's contribution
				difflist.redo;
				model.factorsOf[MIRALearning](difflist).foreach(factor =>
								factor.template.asInstanceOf[MIRALearning].denseDiff += factor.statistic.vector * sign)
				//compute original config's contribution
				difflist.undo;
				model.factorsOf[MIRALearning](difflist).foreach(factor =>
								factor.template.denseDiff += factor.statistic.vector * -1)
				//
				//compute l2 squared
				model.templatesOf[MIRALearning].foreach(t => {
					for (i <- 0 until t.denseDiff.size) {
						val score = t.denseDiff(i)
						l2n2 += score * score;
					}
				})

				l2n2;
			}

		override def sampleAndLearn(numIterations: Int): Unit = {
			for (iteration <- 0 until numIterations) {
				difflist = new DiffList
				// Jump until difflist has changes
				while (difflist.size <= 0) modelTransitionRatio = propose(model, difflist)
				newTruthScore = difflist.score(objective)
				modelScoreRatio = difflist.scoreAndUndo(model)
				oldTruthScore = difflist.score(objective)
				modelRatio = modelScoreRatio + modelTransitionRatio
				bWeightsUpdated = false
				bFalsePositive = false;
				bFalseNegative = false;
				if (newTruthScore > oldTruthScore && modelRatio <= 0) {
					//          Console.println ("Learning from error: new actually better than old.  DiffList="+difflist.size)
					learningRate = kktMultiplier(1, true);
					model.factorsOf[MIRALearning](difflist).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
					difflist.redo
					model.factorsOf[MIRALearning](difflist).foreach(f => f.template.weights += f.statistic.vector * learningRate)
					difflist.undo
					bWeightsUpdated = true
					bFalseNegative = true
				}
				else if (newTruthScore < oldTruthScore && modelRatio >= 0) {
					learningRate = kktMultiplier(1, false);
					//          Console.println ("Learning from error: old actually better than new.  DiffList="+difflist.size)
					model.factorsOf[MIRALearning](difflist).foreach(f => f.template.weights += f.statistic.vector * learningRate)
					difflist.redo
					model.factorsOf[MIRALearning](difflist).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
					difflist.undo
					bWeightsUpdated = true
					bFalsePositive = true
				}
				if (bWeightsUpdated) {
					numUpdates += 1;
					/*
//////////TEST
difflist.redo
val test:Double =difflist.scoreAndUndo
System.out.println("  after update: " + test+" before: " + jumpLogPRatio);
/////////////
*/
					if (useAveraged) {
						// Sum current weights into the average
						model.templatesOf[MIRALearning].foreach(t => {
							for (i <- 0 until t.weightsSum.size)
								t.weightsSum(i) += t.weights(i)
							//f.weightsLastUpdated(i) += learningDiagnostic.numUpdates
							//modelTemplates.foreach(f=> {f.weights.foreach(w =>print(w+", ")); println})
							t.weightsDivisor += 1
						})
					}
				}
				//worldFactors.foreach(f => Console.println (f.toString+" weights = "+f.weights.toList))
				// Now simply sample according to the model, no matter how imperfect it is
				logAccProb = (modelScoreRatio / temperature) + modelTransitionRatio
				if (logAccProb > Math.log(random.nextDouble)) {
					if (modelRatio < 0) {
						//	    Console.print("\\")
						numNegativeMoves += 1
					}
					numAcceptedMoves += 1
					jumpAccepted = true;
					//	  Console.println("iteration: " + iteration + ", pRatio = " + pRatio);
					difflist.redo
				}
				incrementIterations
				mhPerceptronPostProposalHook
			}

			//Put the weights average into each Factor's weights array
			if (useAveraged) {
				model.templatesOf[PerceptronLearning].foreach(
					f => {
						var weightsDivisor = f.asInstanceOf[PerceptronLearning].weightsDivisor
						for (i <- 0 until f.weights.size)
							f.weights(i) = f.weightsSum(i) / weightsDivisor
					})
			}
		}
	}

