package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._
import cc.factorie.la.Tensor
import cc.factorie.maths

class AROW(model:Model[_], learningMargin:Double = 1.0, val lambdaAROW:Double=1.0) extends ConfidenceWeighting(model,learningMargin) {
  //parameters specific to the algorithm
  //
  protected def alpha(modelScore:Double,gradient:WeightsTensor) : Double = math.max(0,1-modelScore) * beta(gradient)
  protected def beta(gradient:WeightsTensor) : Double = 1/(marginVariance(gradient) + 2*lambdaAROW)
  override def adjustConfidence(weights:WeightsTensor, gradient:WeightsTensor):Unit ={
    val betaVal = beta(gradient)
    for(template <- gradient.families){ //Update per-parameter learning rates.
      val templateWeights = weights(template)
      val templateGradient = gradient(template)
      val templateLearningRates = sigma(template)
      templateGradient.foreachActiveElement((index,value) => {
        templateLearningRates(index)=
          templateLearningRates(index)
        - betaVal*templateLearningRates(index)*templateLearningRates(index)*value*value
      })
    }
  }
  override def calculateLearningRate(gradient: WeightsTensor,margin:Double): Double = alpha(margin,gradient)
}

class ConfidenceWeighting(val model:Model[_], var learningMargin:Double=1.0) extends GradientOptimizer {
  /**Initialize the diagonal covariance matrix; this is the value in the diagonal elements */
  var initialVariance = 0.1;
  var numUpdates : Double = 0
  protected var learningRate : Double = 1 //adjusted automatically by CW
  val zeroEpsilon = 0.0000001 //used to detect when something is approximately zero
  protected var eta = 0.98; //confidence value, between 0.5 and 1
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
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    //println("Performing step")
    gradient match{
      case gradient:WeightsTensor => {
        weights match{
          case weights:WeightsTensor =>{
            learningRate = calculateLearningRate(gradient,margin)
            weights.+=(gradient,sigma,learningRate) //Update parameters. Note: sparse argument comes first for efficiency.
            adjustConfidence(weights,gradient)
          }
          case _ => throw new Exception("Confidence weighting only implemented for weight vectors of type WeightsTensor.")
        }
      }
      case _ => throw new Exception("Confidence weighting only implemented for gradients of type WeightsTensor.")
    }
  }
  def adjustConfidence(weights:WeightsTensor, gradient:WeightsTensor):Unit ={
    for(template <- gradient.families){ //Update per-parameter learning rates.
      val templateWeights = weights(template)
      val templateGradient = gradient(template)
      val templateLearningRates = sigma(template)
      templateGradient.foreachActiveElement((index,value) => {
        templateLearningRates(index) = 1.0/((1.0/templateLearningRates(index))
          + 2*learningRate*gaussDeviate*value*value)
      })
    }
  }
  def calculateLearningRate(gradient: WeightsTensor,margin:Double): Double = {
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