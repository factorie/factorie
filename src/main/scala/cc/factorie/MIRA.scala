package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
//import scalala.tensor.operators.TensorOp
//import scalala.tensor.operators.OperatorImplicits


trait MIRAUpdates extends PerceptronUpdates with AbstractMIRAUpdates 
trait AverageMIRAUpdates extends AveragePerceptronUpdates with AbstractMIRAUpdates 

trait AbstractMIRAUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
	def learningRate : Double
	def learningRate_=(x:Double) : Unit
  def model : Model
  def learningMargin : Double

  abstract override def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit = {
    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    learningRate = kktMultiplier(changeProposal, 1, true);
    super.updateWeights(bestModel1, bestModel2, bestObjective1, bestObjective2)
	}
  
  val denseDiff = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = { 
      template.freezeDomains
      val vector = new DenseVector(template.statsize)
      this(template) = vector
      vector
    }
  }

  protected val epsilon: Double = 0.000000001
  def kktMultiplier(changeProposal:Proposal, loss: Double, fnu: Boolean): Double = {
    val modelScoreRatio = changeProposal.modelScore
    var logP = modelScoreRatio;
    if (!fnu)	logP = -logP;
    //loss could be 0/1 or diff in f1
    val l2sqrd: Double = computeDenseDiffL2(fnu, changeProposal.diff);
    val errorGradient: Double = loss - logP;
    var lambda: Double = 0;
    if (l2sqrd > 0 + epsilon || l2sqrd < 0 - epsilon)
    	lambda = errorGradient / l2sqrd;
    if (lambda < 0) lambda = 0 //no error (the passive part of passive-aggressive)
    lambda;
  }

  def computeDenseDiffL2(fnu: Boolean, difflist:DiffList): Double = {
    //TODO: investigate why sign matters
    var sign = 1
    if (fnu) sign = -1
    var l2n2: Double = 0
    //zero the difference
    model.templatesOf[TemplatesToUpdate].foreach(t => denseDiff(t).zero)
    //compute modified config's contribution
    difflist.redo
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => denseDiff(f.template) += f.statistic.vector * sign)
    //compute original config's contribution
    difflist.undo;
    difflist.factorsOf[TemplatesToUpdate](model).foreach(f => denseDiff(f.template) += f.statistic.vector * -1)
    //compute l2 squared
    for (t <- model.templatesOf[TemplatesToUpdate]) {
      val templateDenseDiff = denseDiff(t)
    	for (i <- 0 until denseDiff(t).size) {
    		val score = templateDenseDiff(i)
    		l2n2 += score * score;
    	}
    }
    l2n2;
  }
}


