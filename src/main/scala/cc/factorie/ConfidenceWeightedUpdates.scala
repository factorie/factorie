package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap
import scalala.tensor.dense.DenseVector
import scalala.tensor.Vector
import scalala.tensor.sparse.SparseVector


//TODO: don't require SampleRank
/** Given a gradient, change parameters according to "Confidence Weighted Learning", Dredze, Crammar, Pereira, ICML 2008.
    IMPORTANT NOTE:  for default settings, the confidence-weighted updates are small, resulting in a 'low-temperature' distribution. 
    For certain models, it may be necessary to compensate by changing the temperature parameter in both the learner and predictor. 
    For example:
    <code> 
    val learner = new GibbsSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {temperature=0.01}
    val predictor = new GibbsSampler(model) {temperature=0.01}
    </code>
    @author Michael Wick
    @see MIRAUpdates
    @WeightUpdates
    @since 0.8
 */
trait ConfidenceWeightedUpdates extends WeightUpdates with SampleRank {
  this: ProposalSampler[_] =>
  override type TemplatesToUpdate = DotTemplate
  def model : Model
  def learningMargin : Double
  var numUpdates : Double = 0
  def processCount : Int

  protected var learningRate : Double = 1 //adjusted automatically by CW
  val epsilon = 0.0000001 //used to detect when something is approximately zero
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
      val vector = DenseVector(template.statsize)(initialVariance)
      this(template) = vector
      vector
    }
  }

  override def updateWeights : Unit = {
    numUpdates += 1
    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
    if(changeProposal.modelScore * changeProposal.objectiveScore > 0 
       || changeProposal.objectiveScore==0)
      return;
    val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
      override def default(template:TemplatesToUpdate) = {
  	template.freezeDomains
  	val vector = new SparseVector(template.statsize)
  	this(template) = vector
  	vector
      }
    }
    addGradient(gradient,1.0)
    learningRate = kktMultiplier(changeProposal,gradient)
    updateParameters(gradient)
    updateSigma(gradient)
    super.updateWeights //increments the updateCount
  }


 
  def kktMultiplier(proposal:Proposal,gradient:HashMap[TemplatesToUpdate,SparseVector]) : Double =
    {
      val marginMean = proposal.modelScore.abs
      val v = 1.0 + 2.0 * gaussDeviate * marginMean
      val marginVar = marginVariance(gradient)
      var lambda = 0.0
     if(marginMean >= gaussDeviate * marginVar)
	return 0.0
      if(marginVar > epsilon || marginVar < -epsilon)
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
      for((template,templateGradient)<-gradient)
	{
	  val templateSigma = sigma(template)
	  for((index,value)<-templateGradient.activeElements)
	    template.weights(index) += 
	      templateGradient(index)*templateSigma(index)*learningRate
	}
    } 
}
