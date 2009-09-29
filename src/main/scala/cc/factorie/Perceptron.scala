package cc.factorie

import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector
import scala.reflect.Manifest


	trait GenericPerceptronLearning extends ExpTemplate with LogLinearScoring {
	  var gatherAverageWeights = false
	  var useAverageWeights = false

		// Various internal diagnostics


		type TemplateType <: GenericPerceptronLearning
		// lazy val weightsSum = { freezeDomains ; new DenseVector(suffsize) } // TODO Why doesn't this work on MacOS?
		def weightsSum: Vector
		// lazy val lastUpdateIteration = { freezeDomains ; new DenseVector(suffsize) } // TODO Why doesn't this work on MacOS?
		def lastUpdateIteration: Vector

		var weightsDivisor = 1.0

		def increment(f: GenericPerceptronLearning#Factor, rate: Double, perceptronIteration:Double) = {
			if (gatherAverageWeights) {
				for (i <- f.statistic.vector.activeDomain) {
					val iterationDiff = perceptronIteration - lastUpdateIteration(i)
					assert(iterationDiff >= 0)
					if (iterationDiff > 0) {
						weightsSum(i) += weights(i) * iterationDiff + f.statistic.vector(i) * rate
						lastUpdateIteration(i) = perceptronIteration
					} else
						weightsSum(i) += f.statistic.vector(i) * rate
				}
			}
			weights += f.statistic.vector * rate
			//Console.println("GenericPerceptronLearning increment weights.size="+weights.activeDomain.size)
		}

		def averageWeights = weightsSum :/ lastUpdateIteration

		override def score(s: StatType) =
			if (useAverageWeights)
				averageWeights dot s.vector
			else
				weights dot s.vector

		def setWeightsToAverage = weights := averageWeights
	}

	trait GenericDensePerceptronLearning extends DenseLogLinearScoring with GenericPerceptronLearning {
		type TemplateType <: GenericDensePerceptronLearning
		// lazy val weightsSum = { freezeDomains ; new DenseVector(suffsize) } // TODO Why doesn't this work on MacOS?
		private var _weightsSum: DenseVector = null

		def weightsSum: DenseVector = {
			if (_weightsSum == null) {
				freezeDomains
				_weightsSum = new DenseVector(statsize)
			}
			_weightsSum
		}
		// lazy val lastUpdateIteration = { freezeDomains ; new DenseVector(suffsize) }    TODO Why doesn't this work on MacOS?
		private var _lastUpdateIteration: DenseVector = null

		def lastUpdateIteration: DenseVector = {
			if (_lastUpdateIteration == null) {
				freezeDomains
				_lastUpdateIteration = new DenseVector(statsize)
			}
			_lastUpdateIteration
		}
	}


	trait GenericSparsePerceptronLearning extends SparseLogLinearScoring with GenericPerceptronLearning {
		type TemplateType <: GenericSparsePerceptronLearning
		// lazy val weightsSum = { freezeDomains ; new DenseVector(suffsize) } // TODO Why doesn't this work on MacOS?
		private var _weightsSum: SparseVector = null

		def weightsSum: SparseVector = {
			if (_weightsSum == null) {
				_weightsSum = new SparseVector(statsize)
			}
			_weightsSum
		}
		// lazy val lastUpdateIteration = { freezeDomains ; new SparseVector(suffsize) } // TODO Why doesn't this work on MacOS?
		private var _lastUpdateIteration: SparseVector = null

		def lastUpdateIteration: SparseVector = {
			if (_lastUpdateIteration == null) {
				_lastUpdateIteration = new SparseVector(statsize)
			}
			_lastUpdateIteration
		}
	}






 
 
 
	trait GibbsAbstractPerceptronLearning extends GenericPerceptronLearning 
	trait GibbsPerceptronLearning extends AbstractPerceptronLearning with GenericDensePerceptronLearning
	trait GibbsSparsePerceptronLearning extends AbstractPerceptronLearning with GenericSparsePerceptronLearning

  // TODO Consider changing name to just "PerceptronLearner", and similarly above
	class GibbsPerceptronLearner(model:Model, objective:Model) extends GibbsSampler(model) {
		// Meta-parameters for learning
		var learningRate = 1.0
		var learningMargin = 1.0

	  if (objective.length == 0) throw new IllegalArgumentException("Objective is empty.")

		/**Sample and learning over many variables for numIterations. */
		def sampleAndLearn(variables: Iterable[Variable with MultiProposer], numIterations: Int): Unit = {
			//Console.println ("GibbsPerceptronLearning sampleAndLearn #variables="+variables.toSeq.size)
			xsample(variables, numIterations, sampleAndLearn1 _)
		}

		/**Sample one variable once, and potentially train from the jump. */
		def sampleAndLearn1(variable: Variable with MultiProposer): Unit = {
			//case class Proposal(modelScore: Double, trueScore: Double, value: T, diff: DiffList)
			val proposals = variable.multiPropose(model, objective, new DiffList).toList // toList to make sure Projection doesn't make us eval twice 
   /*
			val proposals =
			for (value <- variable.domain toList) yield {
				val diff = new DiffList
				variable.set(value)(diff)
				var trueScore = objective.score(variable)
				val modelScore = diff.scoreAndUndo(model)
				trueScore -= objective.score(variable)
				Proposal(modelScore, trueScore, value, diff)
			}
                                                                   */
			val (bestScoring, bestScoring2) = proposals.max2(_ modelScore)
			val (bestTruth1, bestTruth2) = proposals.max2(_ trueScore)
			/*
				 proposals.foreach(p => println(p))
         println ("bestTruth1   trueScore = "+bestTruth1.trueScore)//+" value = "+bestTruth1.value)
         println ("bestScoring  trueScore = "+bestScoring.trueScore)//+" value = "+bestScoring.value)
         println ("bestTruth1  modelScore = "+bestTruth1.modelScore)
         println ("bestTruth2  modelScore = "+bestTruth2.modelScore)
         println ("bestScoring modelScore = "+bestScoring.modelScore)
         println ()
         */
			// Only do learning if the trueScore has a preference
			// It would not have a preference if the variable in question is unlabeled
			if (bestTruth1.trueScore != bestTruth2.trueScore) {
				// If the model doesn't score the truth highest, then update parameters
				if (bestScoring/*.value*/ != bestTruth1/*.value*/) {
					//def m[T](implicit m: Manifest[T]) = m.erasure
					//println ("Perceptron learning from error")
					//Console.println ("Model template assignable "+modelTemplates.map(t=>t.getClass.isAssignableFrom(m[PerceptronLearning])))
					//Console.println ("Model template assignable "+modelTemplates.map(t=>m[PerceptronLearning].isAssignableFrom(t.getClass)))
					//Console.println ("Model template assignable "+modelTemplates.map(t=>m[Variable].isAssignableFrom(t.getClass)))
					//Console.println ("Model templates "+modelTemplates.map(t=>t.toString+" "))
					//Console.println ("Model templates of PerceptronLearning "+modelTemplatesOf[PerceptronLearning].toSeq.size)
					//Console.println ("PerceptronLearning factors "+bestTruth1.diff.factorsOf[PerceptronLearning].toSeq.size)
					// ...update parameters by adding sufficient stats of truth, and subtracting error
					//
					bestTruth1.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestTruth1.diff).foreach(f => f.template.increment(f, learningRate, iterations))
					bestTruth1.diff.undo
					bestScoring.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestScoring.diff.undo
				}
				else if (bestScoring.modelScore - bestScoring2.modelScore < learningMargin) {
					//println ("Perceptron learning from margin")
					// ...update parameters by adding sufficient stats of truth, and subtracting runner-up
					bestTruth1.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestTruth1.diff).foreach(f => f.template.increment(f, learningRate, iterations))
					bestTruth1.diff.undo
					bestTruth2.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestTruth2.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestTruth2.diff.undo
				}
			} //else Console.println ("No preference unlabeled "+variable)
			// Set the variable to the value the model thinks is best
			bestScoring.diff.redo // TODO consider sampling here instead?
			// Populate and manage the size of the priority queue
			if (useQueue && maxQueueSize > 0) {
				queue ++= model.factors(bestScoring.diff)
				if (queue.size > maxQueueSize) queue.reduceToSize(maxQueueSize)
			}
		}

		/**Sample one variable once, and potentially train from the jump.  Assumes that we always start from the truth. */
		def contrastiveDivergenceSampleAndLearn1[T](variable: CoordinatedEnumVariable[T]): Unit = {
			val origIndex = variable.index
			val diff = this.sample1(variable)
			if (origIndex != variable.index) {
				// The sample wandered from truth
				model.factorsOf[AbstractPerceptronLearning](diff).foreach(f => f.template.increment(f, -learningRate, iterations))
				diff.undo
				model.factorsOf[AbstractPerceptronLearning](diff).foreach(f => f.template.increment(f, learningRate, iterations))
			}
		}
	}




	trait AbstractPerceptronLearning extends GenericPerceptronLearning 
	trait PerceptronLearning extends AbstractPerceptronLearning with GenericDensePerceptronLearning
	trait SparsePerceptronLearning extends AbstractPerceptronLearning with GenericSparsePerceptronLearning

	abstract class MHPerceptronLearner(model:Model, objective:Model) extends MHSampler(model) {
		var difflist: DiffList = null
		var modelScoreRatio = 0.0
		var modelTransitionRatio = 0.0

		// Meta-parameters for learning
		var useAveraged = true
		var learningRate = 1.0;

		// Various learning diagnostics
		var bWeightsUpdated = false;
		var bFalsePositive = false;
		var bFalseNegative = false;
		var numUpdates = 0; // accumulates

		def mhPerceptronPostProposalHook: Unit = {}

		def sampleAndLearn(numIterations: Int): Unit = {
			for (iteration <- 0 until numIterations)
				sampleAndLearn1
		}

		def sampleAndLearn1: Unit = {
			incrementIterations
			difflist = new DiffList
			// Jump until difflist has changes
			while (difflist.size <= 0) modelTransitionRatio = propose(model, difflist)
			newTruthScore = difflist.score(objective)
			modelScoreRatio = difflist.scoreAndUndo(model) // TODO Change this to use the new 2-arg version of scoreAndUndo
			oldTruthScore = difflist.score(objective)
			modelRatio = modelScoreRatio // + modelTransitionRatio
			bWeightsUpdated = false
			bFalsePositive = false;
			bFalseNegative = false;
			//        Console.println ("old truth score = "+oldTruthScore)
			//        Console.println ("new truth score = "+newTruthScore)
			//        Console.println ("modelScoreRatio = "+modelScoreRatio)
			if (newTruthScore > oldTruthScore && modelRatio <= 0) {
				//          Console.println ("Learning from error: new actually better than old.  DiffList="+difflist.size)
				model.factorsOf[AbstractPerceptronLearning](difflist).foreach(f => f.template.increment(f, -learningRate, iterations))
				difflist.redo
				model.factorsOf[AbstractPerceptronLearning](difflist).foreach(f => f.template.increment(f, learningRate, iterations))
				difflist.undo
				bWeightsUpdated = true
				bFalseNegative = true
			}
			else if (newTruthScore < oldTruthScore && modelRatio >= 0) {
				//          Console.println ("Learning from error: old actually better than new.  DiffList="+difflist.size)
				model.factorsOf[AbstractPerceptronLearning](difflist).foreach(f => f.template.increment(f, learningRate, iterations))
				difflist.redo
				model.factorsOf[AbstractPerceptronLearning](difflist).foreach(f => f.template.increment(f, -learningRate, iterations))
				difflist.undo
				bWeightsUpdated = true
				bFalsePositive = true
			}
			if (bWeightsUpdated) numUpdates += 1;
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
			mhPerceptronPostProposalHook
		}

		def sampleAndLearn(proposableVariables: Seq[Variable with MultiProposer]): Unit = {
			for (variable <- proposableVariables)
				sampleAndLearn1(variable)
		}

		def sampleAndLearn1(variable: Variable with MultiProposer): Unit = {
			incrementIterations
			difflist = new DiffList
			val proposals = variable.multiPropose(model, objective, difflist)
			if (proposals.size < 2) {
				// Don't bother when there is only one possible proposal
				// TODO is this right?  Yes, if it is common for multiPropose to also return a proposal for "no change
				assert(difflist.size == 0)
				return difflist
			}
			val (bestScoring, bestScoring2) = proposals.max2(_ modelScore)
			val (bestTruth1, bestTruth2) = proposals.max2(_ trueScore)
			/*
Console.println ("bestTruth1   trueScore = "+bestTruth1.trueScore+" value = "+bestTruth1.value)
Console.println ("bestScoring  trueScore = "+bestScoring.trueScore+" value = "+bestScoring.value)
Console.println ("bestTruth1  modelScore = "+bestTruth1.modelScore)
Console.println ("bestTruth2  modelScore = "+bestTruth2.modelScore)
Console.println ("bestScoring modelScore = "+bestScoring.modelScore)
*/
			// Only do learning if the trueScore has a preference
			if (bestTruth1.trueScore != bestTruth2.trueScore) {
				// There is a preference among trueScores
				if (!(bestScoring eq bestTruth1)) {
					// The model's best is not the same as the truth's best
					// ...update parameters by adding sufficient stats of truth, and subtracting error
					//Console.println ("Learning from error")
					bestTruth1.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestTruth1.diff).foreach(f => f.template.increment(f, learningRate, iterations))
					bestTruth1.diff.undo
					model.factorsOf[AbstractPerceptronLearning](bestTruth1.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestScoring.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestScoring.diff.undo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, +learningRate, iterations))
				}
				else if (bestScoring.modelScore - bestScoring2.modelScore < learningMargin) {
					// bestScore matches bestTruth1, but runner up is within the margin
					//Console.println ("Learning from margin")
					// ...update parameters by adding sufficient stats of truth, and subtracting runner-up
					bestScoring.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, learningRate, iterations))
					bestScoring.diff.undo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestScoring2.diff.redo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, -learningRate, iterations))
					bestScoring2.diff.undo
					model.factorsOf[AbstractPerceptronLearning](bestScoring.diff).foreach(f => f.template.increment(f, learningRate, iterations))
				}
			} // else println("No true preference.")

			//println("Chosen jump: " + bestScoring.diff)

			bestScoring.diff.redo // TODO consider sampling here instead; or sometimes picking bestTruth1

			//if (random.nextDouble < 0.3) bestTruth1.diff.redo
			//else if (random.nextDouble < 0.5) bestScoring.diff.redo
			//else proposals.sample(p => 1.0).diff.redo

			// Populate and manage the size of the priority queue
			if (useQueue && maxQueueSize > 0) {
				queue ++= model.factors(bestScoring.diff)
				if (queue.size > maxQueueSize) queue.reduceToSize(maxQueueSize)
			}
		}

	}



