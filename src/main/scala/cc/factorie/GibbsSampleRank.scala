package cc.factorie
import scala.reflect.Manifest
import cc.factorie.util.Implicits._
import scalala.Scalala._

abstract class GibbsSampleRank[C<:Variable with IterableSettings](model:Model, override val objective:Model)(implicit mc:Manifest[C]) extends GibbsSampler1[C](model)(mc) {
  var learningMargin = 1.0
  
  def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit 

	override def proposalsHook(proposals:Seq[Proposal]) : Unit = {
  	val (bestModel1, bestModel2) = proposals.max2(_ modelScore)
  	val (bestObjective1, bestObjective2) = proposals.max2(_ objectiveScore)
  	updateWeights(bestModel1, bestModel2, bestObjective1, bestObjective2)
  }
  
}

