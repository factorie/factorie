package cc.factorie

import scala.collection.mutable.{HashMap, HashSet, PriorityQueue}
import cc.factorie.util.{Log, ConsoleLogging}
import cc.factorie.util.Implicits._

	abstract class MHSampler(val model:Model) extends Proposer {
		private var _iterations = 0 // accumulates
		def iterations = _iterations
		def incrementIterations = _iterations += 1

		// A priority queue to help us revisit Factors that score low
		var useQueue = false
		val maxQueueSize = 500
		lazy val queue = new PriorityQueue[Factor]

		// For maintaining sufficient statistics counts for samples
		var sampleInterval = 0

		// Meta-parameters for learning
		var learningMargin = 1.0
		var temperature = 1.0

		// Various inference diagnostics
		var newTruthScore = 0.0;
		var jumpLogPRatio = 0.0;
		var oldTruthScore = 0.0;
		var modelRatio = 0.0;
		var logAccProb = 0.0;
		var jumpAccepted = false;
		var numAcceptedMoves = 0; // accumulates
		var numNegativeMoves = 0; // accumulates

		// track best config
		var maxModelScore = Math.MIN_DOUBLE
		var currModelScore = 0.0

		// ... feel free to add more diagnostic variables above...


		/** Called after each iteration of sampling the full list of variables.  Return true if you want sampling to stop early. */
		def mhPostIterationHook: Boolean = false

		/** Called just before each variable is sampled.  Return an alternative variable if you want that one sampled instead. */
		def mhPreVariableSampleHook(v: Variable): Variable = v

		/** Metropolis-Hastings jump function must be defined in subclass.  Return the ratio of backward/forward jump probabilities. If desired, you can access the queue of low scoring Factors from here. */
		/* Inherited from cc.factorie.Model.Proposer
			 def propose (difflist: DiffList): Double */ 
    // TODO consider changing this to have zero args and return (DiffList,Double)  
    // But no, because the proposal code might have to look at Diffs already in the DiffList to ensure no cycles

		/** Called everytime the current configuration is the best configuration found till now. */
		def mhBestConfigFound(): Unit = {}

		/** The jump function is given a (presumably empty) DiffList, and makes some changes to the configuration, returning the log-prob-ratio
		of forward and backward jump probabilities. */
		def sample(numIterations: Int): Unit = {
			//Console.println("\nWeights in MH.sample:");
			//modelFactors.foreach(f => Console.println (f.toString+" weights = "+f.weights.toList))
			//Console.println("end of weights in MH.sample\n");
			currModelScore = 0.0
			maxModelScore = 0.0 //the initial solution has relative score 0.0 (and this is thus the current max)
			mhBestConfigFound //the initial solution is also the current best config
			//maxModelScore = Math.MIN_DOUBLE
			for (iteration <- 0 until numIterations) {
				sample1
				if (mhPostIterationHook) return
			}
			//log(Log.DEBUG)("NumNegative: " + numNegativeMoves + ", NumAccepted: " + numAcceptedMoves)
		}


		def sample1: DiffList = {
			jumpAccepted = false
			val difflist = new DiffList
			var logAcceptanceProb: Double = propose(model, difflist)
			var modelRatio: Double = difflist.scoreAndUndo(model)
			logAcceptanceProb += (modelRatio / temperature)
			if (logAcceptanceProb > Math.log(random.nextDouble)) {
				jumpAccepted = true;
				//log(Log.INFO)("iteration: " + iteration + ", logAcceptanceProb = " + logAcceptanceProb);
				if (modelRatio < 0) numNegativeMoves += 1
				difflist.redo
				if (useQueue && maxQueueSize > 0) {
					queue ++= model.factors(difflist)
					if (queue.size > maxQueueSize) queue.reduceToSize(maxQueueSize)
				}
				currModelScore += modelRatio
				if (currModelScore > maxModelScore) {
					maxModelScore = currModelScore
					mhBestConfigFound()
				}
				numAcceptedMoves += 1
			} else {
				difflist.clear
			}
			incrementIterations
			difflist
		}


		val random = new scala.util.Random

		def sample(proposals: Seq[Proposal]): Proposal = {
			val max = proposals.max(p => p.modelScore).modelScore
			val sum: Double = proposals.sum(p => Math.exp((p.modelScore - max) * 1/temperature))
			val randomNumber = random.nextDouble * sum
			var total = 0.0
			for (proposal <- proposals) {
				total += Math.exp((proposal.modelScore - max) * 1/temperature)
				if (randomNumber < total) return proposal
			}
			proposals.first
		}


		def sample(proposableVariables: Seq[Variable with MultiProposer]): Unit = {
			for (variable <- proposableVariables)
				sample1(variable)
		}


		def sample1(variable: Variable with MultiProposer): Unit = {
			incrementIterations
			val difflist = new DiffList
			val proposals = variable.multiPropose(model, null, difflist)
			if (proposals.size < 2) {
				// Don't bother when there is only one possible proposal
				// TODO is this right?  Yes, if it is common for multiPropose to also return a proposal for "no change
				assert(difflist.size == 0)
				return difflist
			}

      val chosen = sample(proposals)
      //println("Chosen proposal: " + chosen)
      chosen.diff.redo
		}


	}
