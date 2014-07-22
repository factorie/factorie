/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.hcoref

import cc.factorie._
import scala.reflect.ClassTag
import cc.factorie.optimize.{SampleRankTrainer, ParameterAveraging, MIRA}
import cc.factorie.model.TupleTemplateWithStatistics2
import cc.factorie.variable.{BooleanValue, BagOfWordsVariable}

/**
 * @author John Sullivan
 */
trait TrainingObjective[Vars <: NodeVariables[Vars] with GroundTruth] {
  this:CorefSampler[Vars] with PairGenerator[Vars] =>

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
