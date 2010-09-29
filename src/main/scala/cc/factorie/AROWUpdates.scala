/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie
import scala.collection.mutable.HashMap
import cc.factorie.la._

/** Given a gradient, change the parameters according to the diagonal version of AROW, 
    a regularized version of confidence weighting based on the paper:
    "Adaptive Regularization of Weight Vectors." Koby Crammer, Alex  Kulesza, Mark Dredze. NIPS 2009. 
    Introduces an extra parameter lambda that controls regularization.
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
trait AROWUpdates extends ConfidenceWeightedUpdates {
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate

  //parameters specific to algorithm
  val lambda = 1.0
  //helpful misc vars/members
  var _beta : Double = -1.0 //always must be positive
  def r : Double = 2*lambda //for convenience
  def alpha(modelScore:Double,gradient:HashMap[DotTemplate,SparseVector]) : Double = math.max(0,1-modelScore) * beta(gradient)
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

