package cc.factorie
import cc.factorie.la._
import cc.factorie.optimize._
import scala.collection._

class DotMaximumLikelihood(val model: Model[Variable], val optimizer: GradientOptimizer) {
  def this(model: Model[Variable]) = this(model, new LimitedMemoryBFGS)
  var gaussianPriorVariance = 10.0
  var numRepeatConvergences = 2
  // Number of times to re-run the optimizer to convergence
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  val weights = model.weightsTensor
  var logLikelihood: Double = Double.NaN
  // TODO For now, this only handles the case of IID DiscreteVars
  // TODO Rename this to something indicating IID
  def processAll[V<:LabeledMutableDiscreteVar[_]](variables: Iterable[V], numIterations:Int = Int.MaxValue): Double = {
    val constraints = model.newDenseWeightsTensor
	  // Gather constraints
	  variables.foreach(_.setToTarget(null))
	  // TODO Why doesn't the implicit conversion Model[Variable] -> Model[Iterable[Variable]] work here?
	  //model.factorsOfFamilies(variables, familiesToUpdate).foreach(f => { constraints(f.family) += f.currentStatistics })
	  val factors = new collection.mutable.LinkedHashSet[Factor]
    variables.foreach(v => model.addFactors(v, factors))
    model.filterByFamilies(factors, familiesToUpdate).foreach(f => { constraints(f.family) += f.currentStatistics })

    var iterations = 0
    var convergences = 0
    while (iterations < numIterations && convergences < numRepeatConvergences) {
      logLikelihood = 0.0 // log likelihood
      val gradient = model.newDenseWeightsTensor
      // Put -expectations into gradient
      variables.foreach(v => {
        //val instanceWeight = if (instanceWeights == null) 1.0 else instanceWeights(v)
        val proportions = v.proportions(model) // use model scores to get a normalized distribution over values
        // TODO Think carefully about how we can pass in a generic Infer object and still get expectations
        var i = 0
        while (i < proportions.length) {
          v := i
          model.factorsOfFamilies(v, familiesToUpdate).foreach(f => {
            gradient(f.family) += (f.currentStatistics, -proportions(i) /* * instanceWeight*/)
          })
          i += 1
        }
        logLikelihood += math.log(proportions(v.targetIntValue)) // * instanceWeight
      })
      // Put +constraints into gradient
      gradient += constraints
      // Put prior into gradient and value
      gradient +=(weights, -1.0 / gaussianPriorVariance)
      logLikelihood += -0.5 * 1.0 / gaussianPriorVariance * weights.dot(weights)
      // Use gradient and value to make a step of optimization
      optimizer.step(weights, gradient, logLikelihood, Double.NaN)
      iterations += 1
      if (optimizer.isConverged) {convergences += 1; optimizer.reset(); System.err.println("DotMaximumLikelihood converged in " + iterations + " iterations with loglikelihood = " + logLikelihood)}
    }
    logLikelihood
  }

  // Consider making this not BP-specific by implementing something like addExpectationsInto with plain Factor and Summary?
  def processAllBP[V<:LabeledMutableDiscreteVar[_]](iidVariableSets: Iterable[Iterable[V]], inferencer:InferByBP, numIterations:Int = Int.MaxValue): Double = {
    val constraints = model.newDenseWeightsTensor
    // Gather constraints
    iidVariableSets.foreach(_.foreach(_.setToTarget(null)))
    for (variables <- iidVariableSets) {
      val factors = new collection.mutable.LinkedHashSet[Factor]
      variables.foreach(v => model.addFactors(v, factors))
      model.filterByFamilies(factors, familiesToUpdate).foreach(f => { constraints(f.family) += f.currentStatistics })
// TODO Why doesn't implicit conversion make the code below work?
//      model.factorsOfFamilies(variables, familiesToUpdate).foreach(f => {
//        println("1 " + constraints.dimensions.toSeq)
//        println("2 " + f.family)
//        println("3 " + constraints(f.family).dimensions.toSeq)
//        constraints(f.family) += f.currentStatistics 
//      })
    }
    var iterations = 0
    var convergences = 0
    while (iterations < numIterations && convergences < numRepeatConvergences) {
      logLikelihood = 0.0 // log likelihood
      val gradient = model.newDenseWeightsTensor
      // Put -expectations into gradient
      for (variables <- iidVariableSets) {
        val summary = inferencer.infer(variables, model).get
        val logZ = summary.bpFactors.head.calculateLogZ
        for (bpf <- summary.bpFactors) bpf.factor match {
          // TODO Check here instead for familiesToUpdate?  Or get rid of FamiliesToUpdate entirely, and just always use DotFamily? -akm
          case f: DotFamily#Factor if (f.family.isInstanceOf[DotFamily]) => {
            logLikelihood += (bpf match {
              //case _ => 1.0
              case bpf: BPFactor1 => bpf.calculateBeliefs.apply(bpf.edge1.variable.intValue) - logZ
              case bpf: BPFactor2 => bpf.calculateBeliefs.apply(bpf.edge1.variable.intValue, bpf.edge2.variable.intValue) - logZ
            })
            bpf.addExpectationInto(gradient(f.family), -1.0) // TODO But this adds the expectation over the neighbors, not the statistics!  When neighbors != statistics, this will not work.
          }
        }
      }
      // Put +constraints into gradient
      gradient += constraints
      // Put prior into gradient and value
      gradient += (weights, -1.0 / gaussianPriorVariance)
      logLikelihood += -0.5 * 1.0 / gaussianPriorVariance * weights.dot(weights)
      // Use gradient and value to make a step of optimization
      optimizer.step(weights, gradient, logLikelihood, Double.NaN)
      iterations += 1
      if (optimizer.isConverged) {convergences += 1; optimizer.reset(); println("DotMaximumLikelihood converged in " + iterations + " iterations with loglikelihood = " + logLikelihood)}
    }
    logLikelihood
  }
}
