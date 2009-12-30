/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import cc.factorie.util.Implicits._


/** A Metropolis-Hastings sampler.  
    The abstract method 'propose' should be defined to make a random proposal, putting changes in its implicit DiffList, 
    and returning the ratio of q/q' (backward to forward jump probabilities.) 
    @author Andrew McCallum
    @author Michael Wick
    @since 0.8
    @see ProposalSampler
    @see GibbsSampler
*/


//TODO: have an "action" class that contains a proposal
//TODO: add eligibility traces
//TODO: if we want to mixin gradient update types (eg mira,cw etc.) then we need to ensure that these methods do regression not classification: we need to generalize all update methods to regression, and have classification occur as an outside check (ie conditions checked outside)



//trait WatkinsQ[C] extends EGreedyPolicy[C] with QLambda
//trait QLambda extends QLearning with EligibilityTraces


/**Temporal difference QLearning designed for training factor graphs. Based on the framework described in:
  (1) Training Factor Graphs with Reinforcement Learning for Efficient MAP Inference. Michael Wick, Khashayar Rohanimanesh, Sameer Singh, Andrew McCallum. NIPS 2009.
  (2) Reinforcement Learning for MAP Inference in Large Factor Graphs. Khashayar Rohanimanesh, Michael Wick, Sameer Singh, Andrew McCallum. UMass Tech report #UM-CS-2008-040, 2008.

To use you must mixin an implemented policy as follows:

<code>
val trainer = new MHSampler[Label](model) with QLearning with EGreedyPolicy[Label]
</code>
    @author Michael Wick
    @see EligibilityTraces
    @see Policy
    @see SampledPolicy
    @see EGreedyPolicy
    @see BoltzmannPolicy
*/
trait QLearning extends ProposalSampler0 with SamplerOverSettings0 with Policy //with WeightUpdates .. I can't extend weights because of some odd subtleties in scala typing/erasure
{
  this: ProposalSampler[_] =>
  type TemplatesToUpdate <: DotTemplate
  //
  //parameters
  var gamma : Double = 0.9
  var rlLearningRate : Double = 0.00001
  //
  var currentAction : Proposal = null
  def shouldUpdate : Boolean = true
  var tdError : Double = 0.0
  def model:Model
  abstract override def objective = if (super.objective == null) Global.defaultObjective else super.objective
  //
  //for convenience
  def correctionMagnitude : Double = tdError * rlLearningRate 
  def currentReward=if(currentAction!=null)currentAction.objectiveScore else 0.0
  def currentQ=if(currentAction!=null)currentAction.modelScore else 0.0
  def nextReward=maxAction.objectiveScore
  def nextQ=maxAction.modelScore
  var count : Int = 0
  //
  //calculate the TD error
  abstract override def proposalsHook(proposals:Seq[Proposal]) : Unit =
    {
      tdError = currentReward - currentQ + gamma * nextQ
      updateWeights
      if(currentAction!=null)currentAction.diff.redo
      currentAction = nextAction
    }

  //TODO: if you want elegibility traces, this is the place to implement that
  def addGradient(accumulator:DotTemplate=>Vector, rate:Double): Unit =
    {
      if(currentAction==null)
	return
      currentAction.diff.redo
      currentAction.diff.factorsOf[TemplatesToUpdate](model)
	.foreach(f => accumulator(f.template) += f.statistic.vector * rate)
      currentAction.diff.undo
      currentAction.diff.factorsOf[TemplatesToUpdate](model)
	.foreach(f => accumulator(f.template) -= f.statistic.vector * rate)
    }
  //perform correction
  def updateWeights : Unit =
    {
      if(correctionMagnitude != 0) //scalala complains if multiplier is 0
	addGradient((template:Template) => template match {case t:DotTemplate => t.weights}, correctionMagnitude)
      //super.updateWeights
      if(count % 100 == 0)
	{
	  var norm : Double = 0.0
	  for(template<-model.templatesOf[DotTemplate])
	    {
	      norm += template.weights dot template.weights
	    }
	  System.out.println("UPDATE #"+count)
	  System.out.println("  |theta|: " + norm)
	  System.out.println("  delta: "+correctionMagnitude)
	}
      if(correctionMagnitude!=0)
	{
	  System.out.println("    iter:"+count+" mag: " + correctionMagnitude)
	}
      count += 1

    }
}


/**Adds(lambda) or Watkin's Q functionality to QLearning */
trait EligibilityTraces
{
  //this: ProposalSampler[_] =>
  type TemplatesToUpdate <: DotTemplate
  //
  //parameters
  var lambda : Double = 0.8

  def model : Model
  def currentAction : Proposal
  def resetTraceTrigger : Boolean
  var eligibilityTrace : HashMap[TemplatesToUpdate,SparseVector] = null
  resetTrace

  def resetTrace : Unit =
    {
      eligibilityTrace = new HashMap[TemplatesToUpdate,SparseVector] {
	override def default(template:TemplatesToUpdate) = {
	  template.freezeDomains
	  val vector = new SparseVector(template.statsize)
	  this(template) = vector
	  vector
	}
      }
    }

  def addGradient(accumulator:DotTemplate=>Vector, rate:Double): Unit =
    {
      currentAction.diff.redo
      currentAction.diff.factorsOf[TemplatesToUpdate](model)
	.foreach(f => accumulator(f.template) += f.statistic.vector * rate)
      currentAction.diff.undo
      currentAction.diff.factorsOf[TemplatesToUpdate](model)
	.foreach(f => accumulator(f.template) -= f.statistic.vector * rate)
    }
}

/*
  def getGradient : HashMap[TemplatesToUpdate,SparseVector] =
    {
      val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
    	override def default(template:TemplatesToUpdate) = {
    	  template.freezeDomains
    	  val vector = new SparseVector(template.statsize)
    	  this(template) = vector
    	  vector
    	}
      }
      addGradient(gradient, 1.0)
      gradient
    }
*/


trait Policy
{
  def maxAction : Proposal
  def nextAction : Proposal
  def prepareAction : Unit
}

//TODO: generalize these implementations to GibbsSampling... tried using proposal sampler, but tricky to get to work with MH and Gibbs (end up having to check conditions)


/**Represents a policy where max and next actions are found (or
approximated) by enumating or sampling a finite set of actions, and
then picking one. Max action is implemented here, but nextAction should be implemented in sub-classes. This is the type of policy used in UMASS08,NIPS09
on the ontology alignment problem. TODO: generalize to Gibbs.
    @author Michael Wick
    @see Policy
    @see EGreedyPolicy
    @see BoltzmannPolicy
*/
trait SampledPolicy[C] extends MHSampler[C] with Policy
{
  //
  //primary parameters
  var samplesPerAction : Int = 100
  var includeNoOpAction : Boolean = false
  //
  var candidateActions : ArrayBuffer[Proposal] = null
  var _nextAction : Proposal = null
  var _maxAction : Proposal = null
  var numActionSteps : Int = 0
  def prepareAction : Unit = {candidateActions = null;_nextAction = null}
  //
  //mixins
  def model : Model
  def objective : Model
  def temperature : Double
  def random : scala.util.Random

  def maxAction : Proposal =
    {
      if(_maxAction != null)
	return _maxAction
      _maxAction = candidateActions.max(_.modelScore);
      _maxAction
    }

  abstract override def process1(context:C) : DiffList = 
    {
      proposalsHook(proposals(context))
      //proposals(context)
      val result=nextAction.diff
      prepareAction
      result
    }
  abstract override def proposals(context:C) : Seq[Proposal] =
    {
      if(candidateActions == null)
	candidateActions = new ArrayBuffer[Proposal]
      else
	return candidateActions
      numActionSteps += 1
      var bfRatio = 0.0
      while(candidateActions.size<samplesPerAction)
      {
	var proposalAttemptCount = 0
	val difflist = new DiffList
	while (difflist.size == 0 && proposalAttemptCount < 10)
	{
	  bfRatio = propose(context)(difflist)
	  proposalAttemptCount += 1
	}
	if(difflist.size != 0)
	  {
	    val (modelScore, objectiveScore) = difflist.scoreAndUndo(model,objective)
	    val logAcceptanceScore = modelScore/temperature+bfRatio
	    candidateActions += new Proposal(difflist, modelScore, objectiveScore, logAcceptanceScore, bfRatio, temperature)
	  }
      }
      if(includeNoOpAction)
	{
	  val mirrorLogAcceptanceScore = Math.NEG_INF_DOUBLE //TODO: compute this properly taking into account all actions, requires sumLogProbs
	  //val mirrorLogAcceptanceScore = if (logAcceptanceScore>=0) Math.NEG_INF_DOUBLE else Math.log(1-Math.exp(logAcceptanceScore))
	  candidateActions += new Proposal(new DiffList,0.0,0.0,mirrorLogAcceptanceScore,Math.NaN_DOUBLE,0)
	}
      return candidateActions
    }
  
  /*To be implemented by a particular policy*/
  def nextAction:Proposal
}


/**The traditional epsilon-greedy policy, but based on a sample pool; <code>epsilon</code> controls the amount of exploration. This policy is used in UMASS08,NIPS09 on the ontology alignment problem.
    @author Michael Wick
    @see Policy
    @see SampledPolicy
    @see BoltzmannPolicy
*/
trait EGreedyPolicy[C] extends SampledPolicy[C]
{
  this: ProposalSampler[_] =>
  var resetTraceTrigger : Boolean = true //comes compatible with etraces
  //
  //primary parameters
  var epsilon : Double = 0.1
  
  override def nextAction : Proposal =
    {
      if(_nextAction != null)
	return _nextAction
      if(random.nextDouble>epsilon)
	  {
	    _nextAction = candidateActions.max(_.modelScore);
	    resetTraceTrigger=true
	  }
      else
	{
	  _nextAction = candidateActions(random.nextInt(candidateActions.size))
	  resetTraceTrigger=false
	}
      _nextAction
    }
}

/**The traditional Boltzmann policy where the next action is sampled according to the weight of each action (according to the Q-value).
    @author Michael Wick
    @see Policy
    @see SampledPolicy
    @see EGreedyPolicy
*/
/*

trait BoltzmannPolicy[C] extends SampledPolicy[C]
{
  //this: ProposalSampler[_] =>
  var resetTraceTrigger : Boolean = false //comes compatible with etraces
  override def nextAction : Proposal =
    {
      if(_nextAction == null)
	_nextAction = candidateActions.sampleExpProportionally(random, _.modelScore)
      super.nextAction //calls post proposal hook and returns
    }
}

//      postProposalHook(difflist)


*/

