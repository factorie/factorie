package cc.factorie.app.nlp.xcoref

import scala.reflect.ClassTag
import cc.factorie.optimize.{SampleRankTrainer, ParameterAveraging, MIRA}
import cc.factorie.model.TupleTemplateWithStatistics2
import cc.factorie.app.nlp.hcoref.BagOfWordsVariable
import cc.factorie._
import cc.factorie.variable.BooleanValue

/**
 * @author John Sullivan
 */
trait TrainingObjective[Vars <: NodeVariables[Vars] with GroundTruth] {
  this:CorefSampler[Vars] with PairContextGenerator[Vars] =>

  override def objective = new CorefTrainerModel[Vars]

  val averager = new MIRA with ParameterAveraging
  val trainer = new SampleRankTrainer(this, averager)

  def train(numSteps:Int) {
    println("Starting %d training iterations".format(numSteps))
    (0 until numSteps).foreach { idx =>
      trainer.processContext(nextContext)
    }
    println("training complete")
    averager.setWeightsToAverage(model.asInstanceOf[Parameters].parameters)
  }
}

trait GroundTruth {
  this: NodeVariables[_] =>
  def truth:BagOfWordsVariable
}

class PairwiseTrainerFeature[Vars <: NodeVariables[Vars] with GroundTruth](val precisionDominated:Double = 0.95)(implicit ct:ClassTag[Vars]) extends TupleTemplateWithStatistics2[Vars, Node[Vars]#Exists] {
  def unroll1(vars:Vars) = if(vars.node.isRoot) Factor(vars, vars.node.existsVar) else Nil
  def unroll2(isEntity:Node[Vars]#Exists) = if(isEntity.node.isRoot) Factor(isEntity.node.variables, isEntity) else Nil
  override def score(vars:Vars, isEntity:BooleanValue):Double ={
    var result = 0.0
    //val bag = s._1
    val bagSeq = vars.truth.iterator.toSeq
    var i=0;var j=0
    var tp = 0.0
    var fp = 0.0
    while(i<bagSeq.size){
      //val e:AuthorEntity=null
      val (labeli,weighti) = bagSeq(i)
      j = i
      while(j<bagSeq.size){
        val (labelj,weightj) = bagSeq(j)
        if(labeli==labelj)
          tp += (weighti*(weighti-1))/2.0
        else
          fp += weighti*weightj
        j += 1
      }
      i += 1
    }
    val normalizer = tp+fp
    result = tp - fp
    result
  }
}

class CorefTrainerModel[Vars <: NodeVariables[Vars] with GroundTruth](implicit ct:ClassTag[Vars]) extends CombinedModel {
  this += new PairwiseTrainerFeature[Vars]
}
