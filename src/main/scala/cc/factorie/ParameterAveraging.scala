package cc.factorie

import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector
import scala.reflect.Manifest
import scala.collection.mutable.HashMap

/** Weight updates that keep a running average once per iteration.
    For example "Average Perceptron" can be implemented by
    <code>
    new StructuredPerceptron(model) with GradientAscentUpdates with ParameterAveraging
    </code>
    @author Michael Wick */
trait ParameterAveraging extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
  // To apply this learning to just a subset of the WeightedLinearTemplates, you can define "model" to be a subset of the original model.

  def model : Model
  def processCount : Int
  def perceptronIteration = updateCount //other options: updateCount, iterationCount
  val initialIteration = perceptronIteration
  val lastUpdateIteration = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = { 
      template.freezeDomains
      val vector = if (template.isInstanceOf[SparseWeights]) new SparseVector(template.statsize) else new DenseVector(template.statsize)
      vector += initialIteration // Make the default value be the iteration count at which we started learning
      this(template) = vector
      vector
    }
  }
  val weightsSum = new HashMap[TemplatesToUpdate,Vector] {
    override def default(template:TemplatesToUpdate) = {
      template.freezeDomains
      val vector = if (template.isInstanceOf[SparseWeights]) new SparseVector(template.statsize) else new DenseVector(template.statsize)
      vector += template.weights // Be sure to start the sum at the initial value of the weights, so we can re-train
      this(template) = vector
      vector
    }
  }


  //Store the model weights in here before accumulated weights are injected into model, this way they can be recovered if necessary
  var _backupWeights : HashMap[TemplatesToUpdate,Vector] = null
  def backupWeights : Unit =
    {
      _backupWeights = new HashMap[TemplatesToUpdate,Vector] {
  override def default(template:TemplatesToUpdate) = {
    template.freezeDomains
    val vector = if (template.isInstanceOf[SparseWeights]) new SparseVector(template.statsize) else new DenseVector(template.statsize)
    vector += template.weights // Be sure to start the sum at the initial value of the weights, so we can re-train
    this(template) = vector
    vector
  }
      }
    }

  def unsetWeightsToAverage : Unit =
    {
      if(_backupWeights==null)
  throw new Exception("Cannot 'unset' weights because 'setWeightsToAverage' has not been called")
      for (template <- model.templatesOf[TemplatesToUpdate]) 
  {
    if(_backupWeights.contains(template))
      {
        val backupWeightsTemplate = _backupWeights(template)
        for(i <- template.weights.activeDomain)
    template.weights(i) = backupWeightsTemplate(i)
      }
  }
    }

  // Make sure that weightsSum reflects the sum of all weights up to the current iterations
  def updateWeightsSum : Unit = {
    for (template <- model.templatesOf[TemplatesToUpdate]) {
      val templateLastUpdateIteration = lastUpdateIteration(template)
      val templateWeightsSum = weightsSum(template)
      val templateWeights = template.weights
      for (i <- template.weights.activeDomain) {
        val iterationDiff = perceptronIteration - templateLastUpdateIteration(i)
        assert(iterationDiff >= 0)
        if (iterationDiff > 0) {
          templateWeightsSum(i) += templateWeights(i) * iterationDiff
          templateLastUpdateIteration(i) = perceptronIteration
        }
      }
    }
  }

  // Before calling this the average weights are stored unnormalized in weightsSum.  After calling this the average weights are put in template.weights. 
  def setWeightsToAverage : Unit =
    {
      backupWeights
      updateWeightsSum
      var divisor : Double = perceptronIteration.asInstanceOf[Double]
      for (template <- model.templatesOf[TemplatesToUpdate]) 
  {
    if(weightsSum.contains(template))
      {
        val weightsSumTemplate = weightsSum(template)
        val lastUpdateIterationTemplate = lastUpdateIteration(template)
        for(i <- template.weights.activeDomain)
    template.weights(i) = weightsSumTemplate(i)/divisor///lastUpdateIterationTemplate(i)
      }
    //the following results in divide-by-zero errors when size of allocated domain is larger than size of active  domain:
    //if (weightsSum.contains(template))
          //  template.weights := weightsSum(template) :/ lastUpdateIteration(template)
  }
    }


  /**This method is agnostic to how the weights were originally updated**/
  abstract override def updateWeights: Unit = {
    //Get the gradient to identify the locations of weights changed by update
    val metaGradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
        template.freezeDomains
        val vector = new SparseVector(template.statsize)
        this(template) = vector
        vector
      }
    }
    addGradient(metaGradient, 1.0)
    //put on gradient the values of these weights before the update (negated)
    for((template,vector) <- metaGradient) {
      val templateWeights = template.weights
      for(i <- vector.activeDomain)
          vector(i) = -templateWeights(i)
    }
    //perform the update
    super.updateWeights
    //add the values of these weights after the update
    for((template,vector) <- metaGradient) {
      val templateWeights = template.weights
      for(i <- vector.activeDomain)
        vector(i) += templateWeights(i)
    }
    //determine if an update actually occurred
    if (l2Norm(metaGradient)==0.0)
      return
    //accumulate weights sparsely
    for((template,vector) <- metaGradient) {
      val templateWeightsSum = weightsSum(template)
      val templateLastUpdateIteration = lastUpdateIteration(template)
      val templateWeights = template.weights
      // First do it for the template's weights
      //templateWeights += vector //already done in super.updateWeights
      // Now maintain templateWeightsSum
      for (i <- vector.activeDomain) {
        val iterationDiff = perceptronIteration - templateLastUpdateIteration(i) // Note avoiding off-by-one relies on when iterationCount is incremented!
        assert(iterationDiff >= 0)
        if (iterationDiff > 0) {
          templateWeightsSum(i) += (templateWeights(i) * iterationDiff) + vector(i)
          templateLastUpdateIteration(i) = perceptronIteration
        } else  
          templateWeightsSum(i) += vector(i)
      } 
    }  
  } 

  def l2Norm(grad : HashMap[TemplatesToUpdate,SparseVector]) : Double = 
   {
      var result : Double = 0.0
      for((t,v) <- grad)
  result += v dot v
      result
    }
}
