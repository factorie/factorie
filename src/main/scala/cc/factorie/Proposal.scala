package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

// Proposals

/**For representing a proposed change to a possible world. */
trait Proposal {
	def model : Model
	def transitionRatio : Double // log(Q(x'|x)/Q(x|x'))
	def transitionRatio_=(r:Double)
	def modelScore: Double
	def modelScore_=(s:Double)
	def trueScore: Double
	def trueScore_=(s:Double)
	def diff: DiffList

  override def toString = "modelScore: " + modelScore + " trueScore:" + trueScore + " diff:" + diff

}

trait Proposal2 extends Proposal {
  // TODO Move trueScore members here??
}

/**A simple implementation of the Proposal trait as a case class. */
case class CaseProposal(val model:Model, var transitionRatio:Double, var modelScore:Double, var trueScore:Double, val diff:DiffList) extends Proposal {
	def this(model:Model, score: Double, difflist: DiffList) = this (model, 0.0, score, 0.0, difflist)
}

/**A proposal that makes no change. */
case class EmptyProposal() extends CaseProposal(null, 0.0, 0.0, 0.0, new DiffList)

/**A Proposal that automatically populates its diff, trueScore and score fields given a closure that makes the proposed change. */
class AutoProposal(val model:Model, val objective:Model, change: (DiffList) => Unit) extends Proposal {
	implicit val diff = new DiffList
	//println("Calling change")
	change(diff)
	var transitionRatio = 0.0 // TODO provide a way to set this with default value 0.0 when Scala 2.8 comes out.
	//println("AutoProposal before diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
	//println("AutoProposal diff = " + diff)
	var trueScore = if (objective != null) diff.score(objective) else 0.0
	//println("true score delta before undo: " + trueScore + " with objective " + objective + " and diff " + diff)
	var modelScore = diff.scoreAndUndo(model)
	assert (modelScore == modelScore) // check for NaN //java.lang.Double.isNaN(modelScore)
	//println("tmpModelScore=" + tmpModelScore)
	//println("AutoProposal after  diff #variables "+diff.map(_.variable).filter(_ != null).toSeq.length)
	trueScore -= (if (objective != null) diff.score(objective) else Math.NEG_INF_DOUBLE)
	//println("true score delta after undo: " + trueScore)
	//println("AutoProposal modelScore = "+this.modelScore)
}

/**An object (typically a variable or a world) that can propose changes to itself, and possibly also other variables through variable value coordination */
trait Proposer {
	/** Make a random proposal.  Return Metropolis-Hastings' log(q(old|new)/q(new|old)) */
	def propose(model:Model, d:DiffList): Double
}

/**An object (typically a variable or a world) that can propose a menu of changes to itself.  Useful for considering all choices and choosing the max, or for parameter estimation in which we check the ranking of the best choice. */
trait MultiProposer extends Proposer {
	/**Make all possible proposals.
	The argument is not implicit because we do not want it implicitly passed to other methods that change variables. Instead a new DiffList should be created for each Proposal.  The only reason to pass the DiffList argument to
	this method is so that implementations can check the DiffList for circular changes. */
	def multiPropose(model:Model, objective:Model, d:DiffList): Seq[Proposal]
  def propose(model:Model, d:DiffList): Double = {
    val proposals = multiPropose(model, null, d)
    val maxModelScore = proposals.max(_.modelScore).modelScore
    val proposal = proposals.sampleProportionally(p => Math.exp(p.modelScore - maxModelScore))
    proposal.diff.redo
    d ++= proposal.diff
    proposal.transitionRatio
  }	
  
  
  
}
