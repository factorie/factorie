package cc.factorie

import scala.collection.mutable.{HashMap, HashSet, PriorityQueue}
import cc.factorie.util.Implicits._

trait GibbsSampling requires Model {
	//this : Model =>

	private var _iterations = 0 // accumulates
	def iterations = _iterations

	def incrementIterations = _iterations += 1

	class GibbsSampler {

		// A priority queue to help us revisit Factors that score low
		var useQueue = false
		val maxQueueSize = 500
		lazy val queue = new PriorityQueue[Factor]

		// For maintaining sufficient statistics counts for samples
		var keepSamples = false
		var sampleInterval = 100

		// Meta-parameters for learning
		var learningMargin = 1.0

		// Various internal diagnostics
		var temperature = 1.0
		// ... feel free to add more diagnostic variables here...

		/**Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
		def gibbsPostIterationHook: Boolean = true

		/**Called just before each variable is sampled.  Return an alternative variable if you want that one sampled instead. */
		//def gibbsPreVariableSampleHook[X](v:EnumVariable[X]) : EnumVariable[X] = v
		def gibbsPreVariableSampleHook[X](v: CoordinatedEnumVariable[X]): CoordinatedEnumVariable[X] = v

		/**Sample many variables for numIterations. */
		def sample[X](variables: Iterable[CoordinatedEnumVariable[X]], numIterations: Int): Unit =
			xsample(variables, numIterations, sample1 _)

		protected def xsample[X](variables: Iterable[CoordinatedEnumVariable[X]], numIterations: Int, singleSampler: CoordinatedEnumVariable[X] => Unit): Unit = {
			for (i <- 1 to numIterations) {
				//var j = 0
				for (variable <- variables.toSeq.shuffle) {
					//Console.print("."+j); Console.flush; j += 1
					// Sample each variable in turn
					singleSampler(variable)
					// Sometimes also work on a variable from the lowest scoring factor
					if (useQueue && random.nextDouble < 0.3 && queue.size > 1) { // TODO Why does > 0 here cause errors?  Bug in queue.size?
						// Pick the best factor on the priority queue
						val factor = queue.dequeue
						//              Console.println ("GibbsSamplingWithPriority " + factor.score + " " + factor)
						//              Console.println ("GibbsSamplingWithPriority " + queue(3).score + " another factor as queue position 2")
						// randomly pick a CoordinatedEnumVariable from among that factor's neighbors
						val variables = factor.variables.filter(v => classOf[CoordinatedEnumVariable[X]].isAssignableFrom(v.getClass)).toSeq
						// TODO consider intelligently picking the variable that is causing the problem
						singleSampler(variables(random.nextInt(variables.size)).asInstanceOf[CoordinatedEnumVariable[X]])
						0
					}
				}
				incrementIterations
				if (keepSamples && iterations % sampleInterval == 0) {
					// Gather sufficient stats by completely unrolling any Template with MarginalSamples
				}
				// Let subclasses to run some diagnostics, and allow them to ask that we stop early by returning false
				if (gibbsPostIterationHook == false) return
			}
		}


		/**Sample one variable once. */
		protected def sample1[X](variableToSample: CoordinatedEnumVariable[X]): DiffList = {
			val variable: CoordinatedEnumVariable[X] = gibbsPreVariableSampleHook(variableToSample)
			case class Proposal(var score: Double, diff: DiffList)
			val proposals =
			for (value <- variable.domain) yield {
				val diff = new DiffList
				variable.set(value)(diff)
				val score = diff.scoreAndUndo / temperature
				Proposal(score, diff)
			}
			// exponentiate scores and normalize to get a probability distribution
			val max: Double = proposals.max(_ score).score
			proposals.foreach(p => p.score = Math.exp(p.score - max))
			// Sample from it to get the new value for variable, and set it
			val proposal = proposals.sample(_ score)
			proposal.diff.redo
			// Populate and manage the size of the priority queue
			if (useQueue && maxQueueSize > 0) {
				queue ++= proposal.diff.factors
				if (queue.size > maxQueueSize) queue.reduceToSize(maxQueueSize)
			}
			proposal.diff
		}

		/**Set one variable to the max scoring value */
		protected def maximize1[X](variableToSample: CoordinatedEnumVariable[X]): DiffList = {
			val variable: CoordinatedEnumVariable[X] = gibbsPreVariableSampleHook(variableToSample)
			case class Proposal(var score: Double, diff: DiffList)
			val proposals =
			for (value <- variable.domain) yield {
				val diff = new DiffList
				variable.set(value)(diff)
				val score = diff.scoreAndUndo
				Proposal(score, diff)
			}
			// exponentiate scores and normalize to get a probability distribution
			val maxProposal = proposals.max(_ score)
			val proposal = proposals.max(_ score)
			proposal.diff.redo
			proposal.diff
		}

		/* Sample one variable, optimized for EnumVariable, which doesn't perform variable value coordination */
		def sample1b[X](variable: EnumVariable[X]): Unit = {
			case class IndexProposal(var score: Double, index: Int)
			var max = Math.NEG_INF_DOUBLE
			val proposals =
			for (index <- 0 until variable.domain.size) yield {
				variable.setByIndex(index)(null)
				val s = variable.factors.sum(_ score) / temperature
				if (s > max) max = s
				IndexProposal(s, index)
			}
			// proposals.foreach(p => p.score = Math.exp(p.score - max)) // TODO why isn't this working?  p.score is unchanged afterward!
			val proposals2 = proposals.map(p => IndexProposal(Math.exp(p.score - max), p.index))
			val proposal = proposals2.sample(_ score)
			variable.setByIndex(proposal.index)(null)
		}

		/**Sample a value for some variable of this factor.  Returns the variable whose value was changed. */
		def sampleFactor(factor: Factor): EnumVariable[_] = {
			case class IndexProposal(var score: Double, variable: EnumVariable[_], index: Int)
			var max = Math.NEG_INF_DOUBLE
			val proposals =
			for (variable <- factor.variables.filterByClass(classOf[EnumVariable[_]]); val oldIndex = variable.index; index <- 0 until variable.domain.size) yield {
				variable.setByIndex(index)(null)
				val s = variable.factors.sum(_ score) / temperature
				if (s > max) max = s
				variable.setByIndex(oldIndex)(null)
				IndexProposal(s, variable, index)
			}
			if (!proposals.isEmpty) {
				proposals.foreach(p => p.score = Math.exp(p.score - max))
				val proposal = proposals.sample(_ score)
				proposal.variable.setByIndex(proposal.index)(null)
				proposal.variable
			} else
				null
		}

		def sampleChain[X](v: EnumVariable[X], chainLength: Int): Unit = {
			sample1(v)
			val minScoringFactor = v.factors.min(_ score)
			if (chainLength > 1) {
				val v2 = sampleFactor(minScoringFactor) // TODO but this may already re-sample v
				if (v2 != v) // Don't just sample the same variable v again and again
					sampleChain(v2, chainLength - 1)
			}
		}


	}

}
