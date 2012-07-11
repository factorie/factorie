package cc.factorie
import cc.factorie.la._
import cc.factorie.optimize._

class DotMaximumLikelihood(val model:TemplateModel, val optimizer:GradientOptimizer) {
  def this(model:TemplateModel) = this(model, new ConjugateGradient2)
  var gaussianPriorVariance = 10.0
  var numRepeatConvergences = 2 // Number of times to re-run the optimizer to convergence
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  val weights = model.weightsTensor
  var logLikelihood: Double = Double.NaN
  // TODO For now, this only handles the case of IID DiscreteVars
  def process[V<:DiscreteVarWithTarget](variables: Iterable[V], numIterations:Int = Int.MaxValue): Unit = {
    val constraints = model.newDenseWeightsTensor
	// Gather constraints
	variables.foreach(_.setToTarget(null))
	model.factorsOfFamilies(variables, familiesToUpdate).foreach(f => {
	  constraints(f.family) += f.cachedStatistics.tensor 
	})

	var iterations = 0
	var convergences = 0
	while (iterations < numIterations && convergences < numRepeatConvergences) {
	  logLikelihood = 0.0 // log likelihood
	  val gradient = model.newDenseWeightsTensor
	  // Put -expectations into gradient
	  variables.foreach(v => {
	    val proportions = v.proportions(model) // use model scores to get a normalized distribution over values
        // TODO Think carefully about how we can pass in a generic Infer object and still get expectations
	    var i = 0; while (i < proportions.length) {
	      v := i
	      model.factorsOfFamilies(Seq(v), familiesToUpdate).foreach(f => gradient(f.family).+=(f.statistics.tensor, -proportions(i)))
	      i += 1
	    }
	    logLikelihood += math.log(proportions(v.targetIntValue))
	  })
	  // Put +constraints into gradient
	  gradient += constraints
	  // Put prior into gradient and value
	  gradient += (weights, -1.0/gaussianPriorVariance)
	  logLikelihood += -0.5 * 1.0/gaussianPriorVariance * weights.dot(weights)
	  // Use gradient and value to make a step of optimization
	  optimizer.step(weights, gradient, logLikelihood, Double.NaN)
	  if (optimizer.isConverged) { convergences += 1; optimizer.reset(); println("DotMaximumLikelihood converged in "+convergences + " iters with loglikelihood = " + logLikelihood) }
	  iterations += 1
	}
  }

}
