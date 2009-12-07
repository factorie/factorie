/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
//import scalala.tensor.operators.TensorOp
//import scalala.tensor.operators.OperatorImplicits

// TODO Generalized this so that it doesn't require SampleRank

/** Changes parameters in the direction of the gradient, selecting jump size based on passive-aggressive MIRA method of Crammer and Singer. 
    @author Michael Wick
 */
trait MIRAUpdates extends GradientAscentUpdates with SampleRank {
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate
  def learningRate : Double
  def learningRate_=(x:Double) : Unit
  def model : Model
  def learningMargin : Double
  def useObjectiveDiffAsMargin : Boolean = true
  val boxConstraint : Double = 1.0//Math.POS_INF_DOUBLE


  //
  //TODO: not sure about learning margin violations
  abstract override def updateWeights : Unit = {
    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
    template.freezeDomains
    val vector = new SparseVector(template.statsize)
    this(template) = vector
    vector
      }
    }
    addGradient(gradient,1.0)
    if(useObjectiveDiffAsMargin)
      learningMargin = changeProposal.objectiveScore.abs else 1
    learningRate=Math.min(kktMultiplier(changeProposal,gradient),boxConstraint)
    super.updateWeights //let perceptron do the work and increment count
    //addGradient((template:Template) => template match {case t:TemplatesToUpdate => t.weights}, learningRate)
    //super.asInstanceOf[WeightUpdates].updateWeights
  }

  protected val epsilon: Double = 0.000000001

  def kktMultiplier(changeProposal:Proposal, gradient:HashMap[TemplatesToUpdate,SparseVector]): Double = {
    val modelScoreRatio = changeProposal.modelScore
    var margin = -(changeProposal.modelScore.abs)
    val l2sqrd : Double = computeL2Diff(gradient)
    val error: Double = learningMargin - margin;
    var lambda: Double = 0;
    //System.out.println("margin: " + margin)
    //System.out.println("l2 grad: " + l2sqrd)
    //System.out.println("err: " + error)
    
    if (l2sqrd > 0 + epsilon || l2sqrd < 0 - epsilon)
      lambda = error / l2sqrd;
    if (lambda < 0) lambda = 0 //no error (the passive part of passive-aggressive)
    lambda;
  }
  def computeL2Diff(nabla:HashMap[DotTemplate,SparseVector]) : Double =
    {
      var result : Double = 0
      for(t <- nabla.keySet)
        result += nabla(t) dot nabla(t)
      return result
    }
}


