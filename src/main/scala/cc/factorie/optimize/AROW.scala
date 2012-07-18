package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._
import cc.factorie.la.Tensor
import cc.factorie.maths
import cc.factorie.Model

class AROW(var learningMargin:Double = 1.0) extends GradientOptimizer {
  def reset(): Unit = throw new Error("Not yet implemented")
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    throw new Error("Not yet implemented.")
  }
  def isConverged = false // TODO What to put here?
}

class ConfidenceWeighting(val model:Model, var learningMargin:Double=1.0) extends GradientOptimizer {
  /**Initialize the diagonal covariance matrix; this is the value in the diagonal elements */
  var initialVariance = 0.1;
  var numUpdates : Double = 0
  protected var learningRate : Double = 1 //adjusted automatically by CW
  val zeroEpsilon = 0.0000001 //used to detect when something is approximately zero
  protected var eta = 0.98; //condifidence value, between 0.5 and 1
  var gaussDeviate = maths.probit(eta) //function of the confidence, it is more intuitive to express in terms of eta
  def isConverged=false
  def reset:Unit = {
    println("Resetting CW")
    gaussDeviate = maths.probit(eta)
    for(template <- sigma.families)sigma(template) := initialVariance
  }
  def setConfidence(aeta:Double) : Unit = {
    assert(aeta > 0.5 && aeta < 1.0)
    eta = aeta
    gaussDeviate = maths.probit(eta)
  }
  lazy val sigma:WeightsTensor = {
    //print("Initializing sigma...")
    val result:WeightsTensor = model.weightsTensor.copy.asInstanceOf[WeightsTensor]
    for(template <- result.families)result(template) := initialVariance
    //println(" done.")
    result
  }
  def step(weights2:Tensor, gradient2:Tensor, value:Double, margin:Double): Unit = {
    //println("Performing step")
    gradient2 match{
      case gradient:WeightsTensor => {
        weights2 match{
          case weights:WeightsTensor =>{
            learningRate = kktMultiplier(gradient,margin)
            //weights.+=(gradient,sigma) //update parameters, note sparse argument comes first for efficiency
            weights.+=(gradient,sigma,learningRate) //update parameters, note sparse argument comes first for efficiency
            for(template <- gradient.families){
              val templateWeights = weights(template)
              val templateGradient = gradient(template)
              val templateLearningRates = sigma(template)
//      val templateSigma = sigma(template)
//      for ((index, value) <- templateGradient.activeElements)
//        template.weights(index) +=
//              templateGradient(index) * templateSigma(index) * learningRate
//    }
//              templateGradient.foreachActiveElement((index,value) => {
//                templateWeights(index)
//              })
              //weights.+=(gradient,sigma*learningRate)
              templateGradient.foreachActiveElement((index,value) => {
                templateLearningRates(index) = 1.0/((1.0/templateLearningRates(index))
                  + 2*learningRate*gaussDeviate*value*value)
              })
            }
          }
        }
      }
      case _ => throw new Exception("Confidence weighting only implemented for WeightsTensor gradients.")
    }
  }
  def kktMultiplier(gradient: WeightsTensor,margin:Double): Double = {
    val marginMean = margin.abs
    val v = 1.0 + 2.0 * gaussDeviate * marginMean
    val marginVar = marginVariance(gradient)
    var lambda = 0.0
    if (marginMean >= gaussDeviate * marginVar)
      return 0.0
    if (marginVar > zeroEpsilon || marginVar < -zeroEpsilon)
      lambda = (-v + math.sqrt(v * v - 8 * gaussDeviate * (marginMean - gaussDeviate * marginVar))) / (4 * gaussDeviate * marginVar);
    math.max(0, lambda);
  }
  def marginVariance(gradient:WeightsTensor):Double ={
    var result = 0.0
    for(template <- gradient.families){
      val templateLearningRates=sigma(template)
      gradient(template).foreachActiveElement((index,value) => result += value*value*templateLearningRates(index))
    }
    result
  }
}