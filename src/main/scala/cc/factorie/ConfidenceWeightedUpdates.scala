/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.HashMap
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.Vector
//import scalala.tensor.sparse.SparseVector
import cc.factorie.la._


//TODO: think about splitting this file up, and reorganize the inheritance hierarchy. implement second order gradient ascent, and have confidence weighting extend this. Optionally have a per-feature learning rate above this.

//TODO: don't require SampleRank. Let some class like MH or Gibbs control what the "changeProposal is. Really, all we need is the objective and model diff which should be governed by the behavioral class (MH or Gibbs)"
/** Given a gradient, change parameters according to approximate version outlined in "Confidence Weighted Learning", Dredze, Krammer, Pereira, ICML 2008.
    IMPORTANT NOTE:  for default settings, the confidence-weighted updates are small, resulting in a 'low-temperature' distribution. 
    For certain models, it may be necessary to compensate by changing the temperature parameter in both the learner and predictor. 
    For example:
    <code> 
    val learner = new GibbsSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {temperature=0.01}
    val predictor = new GibbsSampler(model) {temperature=0.01}
    </code>
    @author Michael Wick
    @see StdDevConfidenceWeightedUpdates
    @see AROWUpdates
    @see SecondOrderGradientAscentUpdates
    @see MIRAUpdates
    @WeightUpdates
    @since 0.8
 */
trait ConfidenceWeightedUpdates extends WeightUpdates /*with SampleRank*/ {
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate
  def templateClassToUpdate = classOf[DotTemplate]
  def model : Model
  def learningMargin : Double
  var numUpdates : Double = 0
  def processCount : Int

  def predictedScore : Double
  def targetScore : Double

  protected var learningRate : Double = 1 //adjusted automatically by CW
  val zeroEpsilon = 0.0000001 //used to detect when something is approximately zero
  var eta = 0.98; //condifidence value, make sure eta>0.5 because probit(0.5)=0 and probit(x<0.5)<0
    
  /**Function of the confidence (it is more intuitive to express eta than the gaussian deviate directly). */
  var gaussDeviate = Maths.probit(eta);

  def setConfidence(aeta:Double) : Unit = 
    {
      assert(aeta>0.5 && aeta<1.0)
      eta=aeta
      gaussDeviate = Maths.probit(eta)
    }

  /**Initialize the diagonal covariance matrix; this is the value in the diagonal elements */
  val initialVariance = 0.1;
  lazy val sigma = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = { 
      template.freezeDomains
      //val vector = DenseVector(template.statsize)(initialVariance)
      val vector = if (template.isInstanceOf[SparseWeights]) { val sv = new SparseVector(template.statsize); sv.default = initialVariance; sv } else DenseVector(template.statsize)(initialVariance)
      this(template) = vector
      vector
    }
  }

  override def updateWeights : Unit = {
//    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    //if (!shouldUpdate) return; //this should be determined outside this class
    numUpdates += 1
    val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
        template.freezeDomains
        val vector = new SparseVector(template.statsize)
        this(template) = vector
        vector
      }
    }
    addGradient(gradient, 1.0)
    learningRate = kktMultiplier(gradient)
    updateParameters(gradient)
    updateSigma(gradient)
    super.updateWeights //increments the updateCount
  }


 
  def kktMultiplier(gradient:HashMap[TemplatesToUpdate,SparseVector]) : Double =
    {
      val marginMean =predictedScore.abs
      val v = 1.0 + 2.0 * gaussDeviate * marginMean
      val marginVar = marginVariance(gradient)
      var lambda = 0.0
     if(marginMean >= gaussDeviate * marginVar)
       return 0.0
      if(marginVar > zeroEpsilon || marginVar < -zeroEpsilon)
  lambda = (-v + Math.sqrt(v*v - 8*gaussDeviate*(marginMean - gaussDeviate*marginVar))) / (4*gaussDeviate*marginVar);
      Math.max(0, lambda);
    }


  def marginVariance(gradient:HashMap[TemplatesToUpdate,SparseVector]):Double =
    {
      var result : Double = 0
      for((template,templateGradient)<-gradient)
  {
    val templateSigma = sigma(template)
    for((index,value)<-templateGradient.activeElements)
            result += value*value*templateSigma(index)
  }
      result
    }

  def updateSigma(gradient: HashMap[DotTemplate,SparseVector]) : Unit =
    {
      for((template,templateGrad)<-gradient)
  {
    val ratesTemplate = sigma(template)
    for((index,value)<-templateGrad.activeElements)
            ratesTemplate(index) = 1.0 / ((1.0 / ratesTemplate(index)) 
            + 2*learningRate*gaussDeviate*value*value)
  }
    }

  /**Cannot use the default 'addGradient' method because this update requires a separate learning rate for each parameter*/
  def updateParameters(gradient:HashMap[TemplatesToUpdate,SparseVector]) : Unit =
    {
      //TODO replace with elementwise product?
      for((template,templateGradient)<-gradient)
  {
    val templateSigma = sigma(template)
    for((index,value)<-templateGradient.activeElements)
      template.weights(index) += 
          templateGradient(index)*templateSigma(index)*learningRate
  }
    } 

}

/**Given a gradient, change the parameters according to the diagonal version of the standard deviation based confidence weighting, based on the paper: Exact Convex Confidence-Weighted Learning. Koby Crammer, Mark Dredze, Fernando Pereira. NIPS 2008.This is the 'diagonal' version of standard deviation based updates. 
    IMPORTANT NOTE:  for default settings, the confidence-weighted updates are small, resulting in a 'low-temperature' distribution. 
    For certain models, it may be necessary to compensate by changing the temperature parameter in both the learner and predictor. 
    For example:
    <code> 
    val learner = new GibbsSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {temperature=0.01}
    val predictor = new GibbsSampler(model) {temperature=0.01}
    </code>
    @author Michael Wick
    @see ConfidenceWeightedUpdates
    @see AROWUpdates
    @see SecondOrderGradientAscentUpdates
    @see MIRAUpdates
    @WeightUpdates
    @since 0.8
*/
trait StdDevConfidenceWeightedUpdates extends ConfidenceWeightedUpdates
{
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate
/*  
  override def kktMultiplier : Double =
    {
      var result : Double = 0
      var xi = 1+phi*phi
      result = 1/xi
    }
*/
}


  /**Given a gradient, change the parameters according to the diagonal version of AROW, a regularized version of confidence weighting based on the paper: "Adaptive Regularization of Weight Vectors. Koby Crammer, Alex  Kulesza, Mark Dredze. NIPS 2009. Introduces an extra parameter lambda that controls regularization.
    IMPORTANT NOTE:  for default settings, the confidence-weighted updates are small, resulting in a 'low-temperature' distribution. 
    For certain models, it may be necessary to compensate by changing the temperature parameter in both the learner and predictor. 
    For example:
    <code> 
    val learner = new GibbsSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {temperature=0.01}
    val predictor = new GibbsSampler(model) {temperature=0.01}
    </code>
    @author Michael Wick
    @see ConfidenceWeightedUpdates
    @see StdDevConfidenceWeightedUpdates
    @see SecondOrderGradientAscentUpdates
    @see MIRAUpdates
    @WeightUpdates
    @since 0.8
*/
trait AROWUpdates extends ConfidenceWeightedUpdates
{
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate

  //parameters specific to algorithm
  val lambda = 1.0
  //helpful misc vars/members
  var _beta : Double = -1.0 //always must be positive
  def r : Double = 2*lambda //for convenience
  def alpha(modelScore:Double,gradient:HashMap[DotTemplate,SparseVector]) : Double = Math.max(0,1-modelScore) * beta(gradient)
  def beta(gradient:HashMap[DotTemplate,SparseVector]) : Double = {if(_beta == -1.0)_beta = 1/(marginVariance(gradient) + r);_beta}
  
  override def updateWeights : Unit =
    {
      _beta = -1.0
      //val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      //if (!shouldUpdate) return;
      numUpdates += 1
      
      val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
        template.freezeDomains
        val vector = new SparseVector(template.statsize)
        this(template) = vector
        vector
      }
      }
      
      addGradient(gradient, 1.0)
      learningRate = alpha(predictedScore,gradient)
      updateParameters(gradient)
      updateSigma(gradient)
      super.updateWeights //increments the updateCount
    }
  
  //approximating outer product with inner product and justifying as a projection of covariance matrix onto the diagonal (may be grossly conservative)
  override def updateSigma(gradient:HashMap[DotTemplate,SparseVector]) : Unit =
    {
      for((template,templateGrad)<-gradient)
  {
    val ratesTemplate = sigma(template)
    for((index,value)<-templateGrad.activeElements)
      ratesTemplate(index) = ratesTemplate(index) - beta(gradient) * ratesTemplate(index)*ratesTemplate(index)*value*value
  }
    }
}

  /**Given a gradient, change the parameters according to the diagonal version of "second order perceptron", citation comoing soon.
    @author Michael Wick
    @see ConfidenceWeightedUpdates
    @see StdDevConfidenceWeightedUpdates
    @see AROWUpdates
    @see GradientAscentUpdates
    @WeightUpdates
    @since 0.8
*/
trait SecondOrderGradientAscentUpdates extends ConfidenceWeightedUpdates
{
  this: ProposalSampler[_] =>
    override type TemplatesToUpdate = DotTemplate
  override def updateWeights : Unit =
    {
      //val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      //if (!shouldUpdate) return;
      numUpdates += 1
      
      val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
        template.freezeDomains
        val vector = new SparseVector(template.statsize)
        this(template) = vector
        vector
      }
      }
      addGradient(gradient, 1.0)	 
      updateParameters(gradient)
      updateSigma(gradient)
      super.updateWeights //increments the updateCount
    }
}


