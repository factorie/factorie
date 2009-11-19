package cc.factorie

import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector
import scala.reflect.Manifest
import scala.collection.mutable.HashMap


// TODO Consider renaming this?  It is really just GradientAscentUpdates.  But then we'll have "AverageGradientAscentUpdates"?  Yuck.
trait PerceptronUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
  var learningRate = 1.0
  def model : Model
  def learningMargin : Double
  override def updateWeights : Unit = {
    addGradient((template:Template) => template match {case t:TemplatesToUpdate => t.weights}, learningRate)
    super.updateWeights //increments the updateCount
  }
}



// Compiles, but not sure it is working properly.  Needs to be tested.
@deprecated
trait AveragePerceptronUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
  var learningRate = 1.0
  // To apply this learning to just a subset of the WeightedLinearTemplates, you can define "model" to be a subset of the original model.
  def model : Model
  def learningMargin : Double
  def iterationCount : Int
  def processCount : Int
  def perceptronIteration = processCount
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
      //println("AveragePerceptronUpdates weightsSum default "+template)
      template.freezeDomains
      val vector = if (template.isInstanceOf[SparseWeights]) new SparseVector(template.statsize) else new DenseVector(template.statsize)
      vector += template.weights // Be sure to start the sum at the initial value of the weights, so we can re-train
      this(template) = vector
      vector
    }
  }
  var weightsDivisor = 1.0

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
  def setWeightsToAverage : Unit = {
    updateWeightsSum
    for (template <- model.templatesOf[TemplatesToUpdate]) {
      //println("AveragePerceptronUpdates template="+template)
      //println(weightsSum.size)
      if (weightsSum.contains(template)) {
      	//println("AveragePerceptronUpdates iteration="+perceptronIteration+" "+template)
      	template.weights := weightsSum(template) :/ lastUpdateIteration(template)
      }
    }
  }
  

  

  override def updateWeights: Unit = {
  	val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
  		override def default(template:TemplatesToUpdate) = {
  			template.freezeDomains
  			val vector = new SparseVector(template.statsize)
  			this(template) = vector
  			vector
  		}
  	}
  	addGradient(gradient, 1.0)
  	for (template <- gradient.keys) {
  	  val vector = gradient(template)
  	  val templateWeightsSum = weightsSum(template)
  	  val templateLastUpdateIteration = lastUpdateIteration(template)
  	  val templateWeights = template.weights
  	  // First do it for the template's weights
  	  templateWeights += vector * learningRate
  	  // Now maintain templateWeightsSum
  	  for (i <- vector.activeDomain) {
  	  	val iterationDiff = perceptronIteration - templateLastUpdateIteration(i) // Note avoiding off-by-one relies on when iterationCount is incremented!
  	  	assert(iterationDiff >= 0)
  	  	if (iterationDiff > 0) {
  	  		templateWeightsSum(i) += (templateWeights(i) * iterationDiff) + (vector(i) * learningRate)
  	  		templateLastUpdateIteration(i) = perceptronIteration
  	  	} else	
  	  		templateWeightsSum(i) += vector(i) * learningRate
      }
  	}


	}

}




