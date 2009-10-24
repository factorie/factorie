package cc.factorie

import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector
import scala.reflect.Manifest
import scala.collection.mutable.HashMap

// TODO Move this to a more generic location
trait WeightUpdates {
  type TemplatesToUpdate = DotTemplate
  def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit
}


trait PerceptronUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
  var learningRate = 1.0
  def model : Model
  def learningMargin : Double
  
  def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit = { 
  	/*
  	List(bestModel1, bestModel2, bestObjective1, bestObjective2).foreach(p => println(p))
  	println ("bestObjective1 objectiveScore = "+bestObjective1.objectiveScore)//+" value = "+bestTruth1.value)
  	println ("bestObjective2 objectiveScore = "+bestObjective2.objectiveScore)//+" value = "+bestTruth1.value)
  	println ("bestModel1     objectiveScore = "+bestModel1.objectiveScore)//+" value = "+bestScoring.value)
  	println ("bestObjective1 modelScore = "+bestObjective1.modelScore)
  	println ("bestObjective2 modelScore = "+bestObjective2.modelScore)
  	println ("bestModel1     modelScore = "+bestModel1.modelScore)
  	println ()
  	*/
  	// Only do learning if the trueScore has a preference
  	// It would not have a preference if the variable in question is unlabeled
  	// TODO Is this the right way to test this though?  Could there be no preference at the top, but the model is selecting something else that is worse?
  	if (bestObjective1.objectiveScore != bestObjective2.objectiveScore) {
  		// If the model doesn't score the truth highest, then update parameters
  		if (bestModel1 ne bestObjective1) { // TODO  I changed != to "ne"  OK?
  			// ...update parameters by adding sufficient stats of truth, and subtracting error
  			//println ("Perceptron learning from error, learningRate="+learningRate)
  			//println (" Model #templates="+model.size)
  			//println (" Updating bestObjective1 "+(bestObjective1.diff.factorsOf[WeightedLinearTemplate](model).size)+" factors")
  			//println (" Updating bestModel1 "+(bestModel1.diff.factorsOf[WeightedLinearTemplate](model).size)+" factors")
  			bestObjective1.diff.redo
  			bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector *  learningRate)
  			bestObjective1.diff.undo
  			bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
  			bestModel1.diff.redo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
  			bestModel1.diff.undo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector *  learningRate)
  		}
  		else if (bestModel1.modelScore - bestModel2.modelScore < learningMargin) {
  			// ...update parameters by adding sufficient stats of truth, and subtracting runner-up
  			//println ("Perceptron learning from margin")
  			// TODO Note This is changed from previous version, where it was bestTruth.  Think again about this.
  			bestObjective1.diff.redo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector *  learningRate)
  			bestObjective1.diff.undo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
  			bestModel2.diff.redo
  			bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector * -learningRate)
  			bestModel2.diff.undo
  			bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => f.template.weights += f.statistic.vector *  learningRate)
  		}
  	} //else Console.println ("No preference unlabeled "+variable)
	}
  
}



// Compiles, but not sure it is working properly.  Needs to be tested.
@deprecated
trait AveragePerceptronUpdates extends WeightUpdates {
  override type TemplatesToUpdate = DotTemplate
  var learningRate = 1.0
  /** To apply this learning to just a subset of the WeightedLinearTemplates, you can define "model" to be a subset of the original model. */
  def model : Model
  def learningMargin : Double
  def iterationCount : Int
  def perceptronIteration = iterationCount
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

  /** Make sure that weightsSum reflects the sum of all weights up to the current iterations */
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

  /** Before calling this the average weights are stored unnormalized in weightsSum.  
   * After calling this the average weights are put in template.weights. */
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
  
  def updateWeights(bestModel1:Proposal, bestModel2:Proposal, bestObjective1:Proposal, bestObjective2:Proposal) : Unit = { 
  	// Only do learning if the trueScore has a preference
  	// It would not have a preference if the variable in question is unlabeled
  	// TODO Is this the right way to test this though?  Could there be no preference at the top, but the model is selecting something else that is worse?
  	if (bestObjective1.objectiveScore != bestObjective2.objectiveScore) {
  		// If the model doesn't score the truth highest, then update parameters
  		if (bestModel1 ne bestObjective1) { // TODO  I changed != to "ne"  OK?
  			// ...update parameters by adding sufficient stats of truth, and subtracting error
  			//println ("Perceptron learning from error, learningRate="+learningRate)
  			bestObjective1.diff.redo
  			bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, learningRate))
  			bestObjective1.diff.undo
  			bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, -learningRate))
  			bestModel1.diff.redo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, -learningRate))
  			bestModel1.diff.undo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, learningRate))
  		}
  		else if (bestModel1.modelScore - bestModel2.modelScore < learningMargin) {
  			// ...update parameters by adding sufficient stats of truth, and subtracting runner-up
  			//println ("Perceptron learning from margin")
  			// TODO Note This is changed from previous version, where it was bestTruth.  Think again about this.
  			bestObjective1.diff.redo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, learningRate))
  			bestObjective1.diff.undo
  			bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, -learningRate))
  			bestModel2.diff.redo
  			bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, -learningRate))
  			bestModel2.diff.undo
  			bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => increment(f, learningRate))
  		}
  	} //else Console.println ("No preference unlabeled "+variable)
	}
  
  /** Sparsely increment weights, accounting for all previous unchanged values in the average */
  protected def increment(f:TemplatesToUpdate#Factor, rate:Double) = {
    val template = f.template
    val templateWeightsSum = weightsSum(template)
    val templateLastUpdateIteration = lastUpdateIteration(template)
    val templateWeights = template.weights
    // First do it for the template's weights
    template.weights += f.statistic.vector * rate
    // Now gather weights into weightsSum 
  	for (i <- f.statistic.vector.activeDomain) {
  		val iterationDiff = perceptronIteration - templateLastUpdateIteration(i)
  		assert(iterationDiff >= 0)
  		if (iterationDiff > 0) {
  			templateWeightsSum(i) += templateWeights(i) * iterationDiff + f.statistic.vector(i) * rate
  			templateLastUpdateIteration(i) = perceptronIteration
  		} else	
  			templateWeightsSum(i) += f.statistic.vector(i) * rate
  	}
  }


}



