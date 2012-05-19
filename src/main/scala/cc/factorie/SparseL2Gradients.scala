package cc.factorie

import collection.mutable.HashMap
import cc.factorie.la._


/**
This file contains a collection of classes to perform stochastic L2 regularization sparsely and efficiently (i.e., each update is linear in the number of active parameters in the gradient rather than linear in the number of parameters in the weight vector) by updating an implicit scalar value and computing the weight norm in a sparse fashion from the gradient alone. This code further utilizes the "PEGASOS trick" (Shalev-Shwartz, Singer, Srebro) of projecting onto the L2 ball of radius determined by the penalty term to obtain even faster convergence. This trick improves convergence from O(1/\epsilon^2) to O(1/epsilon). Conjecture: convergence may further be improved by using Polyjak parameter averaging. CAVEAT: use IterateAveraging instead of ParameterAveraging to achieve Polyjak averaging due to compatibility issues. Implement your own stochastic approximation learning rate schedule by overriding updateWeights (don't forget to call super.updateWeights), a common recommendation for a learning rate is 1/sqrt(t). For more usage details see example below.
    @author Michael Wick 
 */

/**
Trait that computes sparse gradients for doing L2 regularization. Requires simulating "L2 regularization" for efficiency, to perform the actual regularization, you must call "regularize". Combine this with SampleRank to get a max-margin varient of SampleRank. TODO: this method should extend "ProposalSampler0" not Sampler because we do not care about the contex (of generic type C). An example usage follows:

val trainer = new Proposer(myModel) 
        with SampleRank
        with GradientAscentUpdates
        with SparseL2Gradients[Null]
        with IterateAveraging
{
  penalty = 1.0
  temperature=0.001

  override def updateWeights : Unit ={
    learningMargin = targetScore.abs
    learningRate = 0.1/(scala.math.sqrt(upCount.asInstanceOf[Double]))
    super.updateWeights
  }
}
This code creates a new SampleRank trainer and applies SparseL2Gradients with Polyjak parameter averaging and a classical stochastic approximation learningRate schedule (given in the overriden update weights function). Note that the default "ParameterAveraging" class is not yet compatible with this code. For now, use "IterateAveraging". The low temperature (0.001) is recommended to compensate for the artificially high temperature resulting from regularization. The penalty variable is set to the default of 1.0.
@author Michael Wick
 */
trait SparseL2Gradients[C] extends Sampler[C] with WeightUpdates{ //Want ProposalSampler0 instead. These gradients shouldn't care about the context
  //type TemplatesToUpdate = DotFamily
  def familiesToUpdate: Seq[DotFamily]
  var penalty = 1.0
  var modelScale = 1.0
  def model : Model
  var upCount = 0
  def timeScale = upCount.asInstanceOf[Double]//iterationCount.asInstanceOf[Double]//upCount
  var weightNorm = 0.0//l2Norm
  /**The scaled difflist is needed to calculate the implicitly regularized scores*/
  override def newDiffList:DiffList = if(makeNewDiffList) new ScaledDiffList(modelScale) else null
  /**Make the implicit regularization explicit by scaling the actual weights. Do not call this method frequently because it is linear in the number of parameters. Instead, call this occasionally (e.g., after each epoch) or just once after learning has been completed.*/
  def regularize : Unit ={
    println("REGULARIZING WEIGHT VECTOR WITH NORM: " + weightNorm)
    println("  -exhaustive norm: "+l2Norm)
    println("  -scaling weights by a factor of " + modelScale)
    val normalizer = scala.math.min(1.0,1.0/(scala.math.sqrt(penalty)*weightNorm))
    for(template<-model.familiesOfClass[DotFamily])
      for(i<-template.weights.activeDomain)
        template.weights(i)*=modelScale
    modelScale=1.0
    weightNorm = l2Norm
    println("  -weight norm: " + weightNorm)
  }
  /**An exhaustive L2 norm calculation. Only needed upon initialization, or when reset by calls to "regularize"*/
  def l2Norm ={
    var result = 0.0
    for(template<-familiesToUpdate)
      result += template.weights dot template.weights
    scala.math.sqrt(result)
  }
  def shouldUpdate : Boolean
  var learningRate:Double
  /**Implicitly project onto the L2 ball with radius determined by the penalty term.*/
  def project : Unit = {modelScale=scala.math.min(1.0,1.0/(scala.math.sqrt(penalty)*weightNorm));if(modelScale!=modelScale)modelScale=1.0}//;println("modelScale: "+modelScale+" penalty: "+penalty+" weightNorm:"+weightNorm)}

  /**This method updates the weights using the following three steps. (1) implicitly perform a gradient step in the direction to minimize the L2 norm of the weights, (2) update the weights with respect to the actual loss function by calling super.update, (3) sparsely maintain the l2 Norm computation of the weight vector so that the implicit model scale is kept up to date.*/
  abstract override def updateWeights : Unit ={
    upCount += 1
    //
    //Perform gradient step to minimize L2 norm of weights
    modelScale = (1.0 - learningRate * 1/penalty) * modelScale
    //
    //Get the gradient to identify the locations of weights changed by update
    val metaGradient = new HashMap[DotFamily,Tensor] {
      override def default(template:DotFamily) = {
        val tensor = template.weights.blankCopy //new SparseVector(template.statisticsVectorLength)
        this(template) = tensor
        tensor
      }
    }
    //
    //Perform update and sparsely maintain value of weight norm.
    if(shouldUpdate){
      addGradient(metaGradient, 1.0)
      //
      //subtract off the old portion of the norm (before update)
      //println("0 EXH NORM: "+this.l2Norm)
      //println("0 WEIGHT NORM: "+weightNorm)
      weightNorm=weightNorm*weightNorm
      for((template,tensor)<-metaGradient)
        for(i<-tensor.activeDomain.asSeq)
          weightNorm -= template.weights(i) * template.weights(i)
      //
      //perform the update
      super.updateWeights
      //
      //add the new portion of the norm (after update)
      for((template,tensor)<-metaGradient)
        for(i<-tensor.activeDomain.asSeq)
          weightNorm += template.weights(i) * template.weights(i)
      weightNorm=scala.math.sqrt(weightNorm)
      //
      //Now project onto the L2 ball to speed up learning (ala PEGASOS)
      project
    }
  }
}

/*
trait GradientSnapshots extends  WeightUpdates{
  type TemplatesToUpdate = DotFamily
  var gradientSnapshot:HashMap[TemplatesToUpdate,SparseVector]
  def gradient = gradientSnapshot
  def updateWeightsExtras:Unit
  def beforeUpdateHook:Unit={}
  def afterUpdateHook:Unit={}
  abstract override def updateWeights: Unit = {
    gradientSnapshot = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
        //SR: is freezing necessary?
        //template.freezeDomains
        val vector = new SparseVector(template.statisticsVectorLength)
        this(template) = vector
        vector
      }
    }
    addGradient(gradientSnapshot,1.0)
    for((template,vector) <- gradientSnapshot) {
      val templateWeights = template.weights
      for(i <- vector.activeDomain)
        vector(i) = -templateWeights(i)
    }
    beforeUpdateHook
    //perform the update
    super.updateWeights
    //add the values of these weights after the update
    for((template,vector) <- gradientSnapshot) {
      val templateWeights = template.weights
      for(i <- vector.activeDomain)
        vector(i) += templateWeights(i)
    }
    afterUpdateHook
  }
}
*/

/**Automatically scales model scores by a specified factor*/
class ScaledDiffList(val scale:Double) extends DiffList{
  override def score(model:Model) = model.score(this)/scale
  override def scoreAndUndo(model:Model): Double ={
    if (this.length == 0) return 0.0  // short-cut the simple case
    var s = model.score(this)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo
    // We need to re-calculate the Factors list because the structure may have changed
    s -= model.score(this)
    //log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
    s/scale
  }
  /** For comparing the scores of two different models. */
  override def scoreAndUndo(model1:Model, model2:Model) : (Double, Double) ={
    if (this.length == 0) return (0.0, if (model2 == null) Double.NaN else 0.0) // short-cut the simple case
    var s1 = model1.score(this)
    var s2 = if (model2 == null) Double.NaN else model2.score(this)
    this.undo
    s1 -= model1.score(this)
    if (model2 != null) s2 -= model2.score(this)
    (s1/scale, s2)
  }
}

trait IterateAveraging extends WeightUpdates {
  class Weights extends HashMap[DotFamily,Tensor] {
    def this(initialValue:Double) = { this(); initialDouble = initialValue }
    def this(initialValues:Tensor) = { this(); initialTensor = initialValues }
    private var initialDouble = 0.0
    private var initialTensor: Tensor = null
    override def default(template:DotFamily) = {
      if (!template.isInstanceOf[SparseWeights]) template.freezeDomains
      val tensor = template.weights.blankCopy
      init(template, tensor)
      this(template) = tensor
      tensor
    }
    def init(template:DotFamily, t:Tensor): Unit = {
      if (initialTensor ne null) t += initialTensor else if (initialDouble != 0.0) t += initialDouble
    }
  }
  def familiesToUpdate: Seq[DotFamily]
  //type TemplatesToUpdate = DotFamily
  // To apply this learning to just a subset of the WeightedLinearTemplates, you can define "model" to be a subset of the original model.
  def model : Model
  def processCount : Int
  def perceptronIteration = updateCount //other options: updateCount, iterationCount
  def modelScale:Double
  var summedModelScale:Double = 0
  //def computeModelScale:Unit
  val initialIteration = perceptronIteration
  val lastUpdateIteration = new Weights(initialIteration) // Make the default value be the iteration count at which we started learning
  //stores the model scale at the time the param was last updated
  val lastModelScale = new Weights(1.0)  // Make the default value be the iteration count at which we started learning 
  val weightsSum = new Weights(initialIteration)  // Make the default value be the iteration count at which we started learning
  //Store the model weights in here before accumulated weights are injected into model, this way they can be recovered if necessary
  var _backupWeights : HashMap[DotFamily,Tensor] = null
  def backupWeights: Unit = {
    _backupWeights = new Weights { override def init(template:DotFamily, t:Tensor) = t += template.weights }
    for(template <- familiesToUpdate)
      _backupWeights(template)
  }

  def unsetWeightsToAverage : Unit ={
    if(_backupWeights==null)
      throw new Exception("Cannot 'unset' weights because 'setWeightsToAverage' has not been called")
    for (template <- familiesToUpdate){
      if(_backupWeights.contains(template)){
        val backupWeightsTemplate = _backupWeights(template)
        for(i <- template.weights.activeDomain)
          template.weights(i) = backupWeightsTemplate(i)
      }
    }
  }
  // Make sure that weightsSum reflects the sum of all weights up to the current iterations
  def updateWeightsSum : Unit = {
    for (template <- familiesToUpdate) {
      val templateLastUpdateIteration = lastUpdateIteration(template)
      val templateLastModelScale = lastModelScale(template)
      val templateWeightsSum = weightsSum(template)
      val templateWeights = template.weights
      for (i <- template.weights.activeDomain) {
        val iterationDiff = perceptronIteration - templateLastUpdateIteration(i)
        val summationDiff = summedModelScale - templateLastModelScale(i)
        assert(iterationDiff >= 0)
        if (iterationDiff > 0) {
          templateWeightsSum(i) += templateWeights(i) * summationDiff
          templateLastUpdateIteration(i) = perceptronIteration
          templateLastModelScale(i) = summedModelScale
        }
      }
    }
  }
  // Before calling this the average weights are stored unnormalized in weightsSum.  After calling this the average weights are put in template.weights.
  def setWeightsToAverage : Unit ={
    backupWeights
    updateWeightsSum
    var divisor : Double = perceptronIteration.asInstanceOf[Double]
    //      System.out.println("PA: " + perceptronIteration+" up: " + updateCount+" div="+divisor)
    for (template <- familiesToUpdate){
      if(weightsSum.contains(template)){
        val weightsSumTemplate = weightsSum(template)
        val lastUpdateIterationTemplate = lastUpdateIteration(template)
        for(i <- template.weights.activeDomain){
          template.weights(i) = weightsSumTemplate(i)/divisor///lastUpdateIterationTemplate(i)
        }
      }
      //the following results in divide-by-zero errors when size of allocated domain is larger than size of active  domain:
      //if (weightsSum.contains(template))
      //  template.weights := weightsSum(template) :/ lastUpdateIteration(template)
    }
    System.out.println("ITERATE AVERAGING")
    System.out.println("WEIGHtS DIVISOR: " + divisor)
  }
  /**This method is agnostic to how the weights were originally updated**/
  abstract override def updateWeights: Unit = {
    //Get the gradient to identify the locations of weights changed by update
    val metaGradient = new Weights 
    //computeModelScale
    addGradient(metaGradient, modelScale)
    //put on gradient the values of these weights before the update (negated)
    for((template,tensor) <- metaGradient) {
      val templateWeights = template.weights
      for(i <- tensor.activeDomain.asSeq)
        tensor(i) = -templateWeights(i)
    }
    //perform the update
    super.updateWeights
    //add the values of these weights after the update
    for((template,tensor) <- metaGradient) {
      val templateWeights = template.weights
      for(i <- tensor.activeDomain.asSeq)
        tensor(i) += templateWeights(i)
    }
    //at this point I have "modelScale" computed
    summedModelScale += modelScale
    //determine if an update actually occurred
    if (l2Norm(metaGradient)==0.0) //TODO figure this out by checking updateCount instead because it is much faster
      return
    //accumulate weights sparsely
    for((template,vector) <- metaGradient) {
      val templateWeightsSum = weightsSum(template)
      val templateLastUpdateIteration = lastUpdateIteration(template)
      val templateLastModelScale = lastModelScale(template)
      val templateWeights = template.weights
      for (i <- vector.activeDomain.asSeq) {
        val iterationDiff = perceptronIteration - templateLastUpdateIteration(i
        ) // Note avoiding off-by-one relies on when iterationCount is incremented!
        val summationDiff = summedModelScale - templateLastModelScale(i)
        assert(iterationDiff >= 0)
        if (iterationDiff > 0) {
          templateWeightsSum(i) += templateWeights(i) * summationDiff + vector(i)*modelScale
          templateLastUpdateIteration(i) = perceptronIteration
          templateLastModelScale(i) = summedModelScale
        } else
          templateWeightsSum(i) += vector(i)*modelScale
        //TODO: why not update iteration and model scale here?
      }
    }
  }
  def l2Norm(grad : HashMap[DotFamily,Tensor]) : Double = {
    var result : Double = 0.0
    for((t,v) <- grad)
      result += v dot v
    result
  }
}
