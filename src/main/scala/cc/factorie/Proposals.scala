package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging}
import cc.factorie.util.Implicits._

trait Proposals requires Model {

  	/**For representing a proposed change to a possible world. */
	trait Proposal {
		def modelScore: Double
		def trueScore: Double
		def diff: DiffList
	}

	/**A simple implementation of the Proposal trait as a case class. */
	case class CaseProposal(modelScore: Double, trueScore: Double, diff: DiffList) extends Proposal {
		def this(score: Double, difflist: DiffList) = this (score, 0.0, difflist)
	}

	/**A proposal that makes no change. */
	case class EmptyProposal() extends CaseProposal(0.0, 0.0, new DiffList)

	/**A Proposal that automatically populates its diff, trueScore and score fields given a closure that makes the proposed change. */
	class AutoProposal(change: (DiffList) => Unit) extends Proposal {
		implicit val diff = new DiffList
		//println("Calling change")
		change(diff)
		val (modelScore, trueScore) = {
			//println("AutoProposal before diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
			//println("AutoProposal diff = " + diff)
			var tmpTrueScore = diff.trueScore;
			//println("true score delta before undo: " + tmpTrueScore)
			val tmpModelScore = diff.scoreAndUndo;
			//println("tmpModelScore=" + tmpModelScore)
			//println("AutoProposal after  diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
			tmpTrueScore -= diff.trueScore;
			//println("true score delta after undo: " + tmpTrueScore)
			(tmpModelScore, tmpTrueScore)
		}
		//private var tmpTrueScore = diff.trueScore
		//val modelScore = diff.scoreAndUndo
		//tmpTrueScore -= diff.trueScore
		//val trueScore = tmpTrueScore
		// Now modelScore is the different in model scores before and after the proposal; likewise for trueScore
	}

	/**A variable that can propose changes to itself, and possibly also other variables through variable value coordination */
	trait Proposer {
		this: Variable =>

		/**Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
		def propose(implicit d: DiffList): Double
	}

	/**A variable that can propose a menu of changes to itself.  Useful for considering all choices and choosing the max, or for parameter estimation in which we check the ranking of the best choice. */
	trait MultiProposer extends Proposer {
		this: Variable =>

		/**Make all possible proposals.
		The argument is not implicit because we do not want it implicitly passed to other methods that change variables. Instead a new DiffList should be created for each Proposal.  The only reason to pass the DiffList argument to
		this method is so that implementations can check the DiffList for circular changes. */
		def multiPropose(d: DiffList): Seq[Proposal]

		def propose(implicit d: DiffList): Double = {
			val proposals = multiPropose(d)
			val maxModelScore = proposals.max(_.modelScore).modelScore
			val proposal = proposals.sample(p => Math.exp(p.modelScore - maxModelScore))
			proposal.diff.redo
			d ++= proposal.diff
			0.0 // TODO Make an API for putting ratio of q's into multiPropose
		}
	}
}
