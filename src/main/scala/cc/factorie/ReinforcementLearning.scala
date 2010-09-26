/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

///* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   This software is provided under the terms of the Eclipse Public License 1.0
//   as published by http://www.opensource.org.  For further information,
//   see the file `LICENSE.txt' included with this distribution. */
//
//package cc.factorie
//import scala.reflect.Manifest
//import scala.collection.mutable.HashMap
//import scala.collection.mutable.ArrayBuffer
//import scalala.Scalala._
//import scalala.tensor.Vector
//import scalala.tensor.dense.DenseVector
//import scalala.tensor.sparse.SparseVector
//
//
///**
//A class that ties all the RL code together. Based on the following papers that maps the problem of training factor graphs to reinforcement learning:
//
// (1) Training Factor Graphs with Reinforcement Learning for Efficient MAP Inference. Michael Wick, Khashayar Rohanimanesh, Sameer Singh, Andrew McCallum. NIPS 2009.
//  (2) Reinforcement Learning for MAP Inference in Large Factor Graphs. Khashayar Rohanimanesh, Michael Wick, Sameer Singh, Andrew McCallum. UMass Tech report #UM-CS-2008-040, 2008.
//
//<code>
//class Trainer = new MHSampler[C](model) with QLambda
//{
//   gamma = 0.9 //QLearning
//   lambda = 0.9 //EligibilityTraces
//   rlLearningRate = 0.0001 //QLearning
//}
//</code>
//    @author Michael Wick
//    @see QLearning
//    @see EligibilityTraces
//    @see EGreedyPolicy
//*/
////trait QLambda[C] extends QLearning with EligibilityTraces with EGreedyPolicy[C]
//
////========================TODO BOX========================
////CONCEPT OF "POCKETS" OF VARIABLES TO MAX OVER
////TODO: have an "action" class that contains a proposal
////TODO: debug eligibility traces: try with lambda=0.0
////TODO: if we want to mixin gradient update types (eg mira,cw etc.) then we need to ensure that these methods do regression not classification: we need to generalize all update methods to regression, and have classification occur as an outside check (ie conditions checked outside).
////trait WatkinsQ[C] extends EGreedyPolicy[C] with QLambda
////trait QLambda extends QLearning with EligibilityTraces
////========================================================
//
///**Temporal difference QLearning designed for training factor graphs. Based on the framework described in:
//  (1) Training Factor Graphs with Reinforcement Learning for Efficient MAP Inference. Michael Wick, Khashayar Rohanimanesh, Sameer Singh, Andrew McCallum. NIPS 2009.
//  (2) Reinforcement Learning for MAP Inference in Large Factor Graphs. Khashayar Rohanimanesh, Michael Wick, Sameer Singh, Andrew McCallum. UMass Tech report #UM-CS-2008-040, 2008.
//
//To use you must mixin an implemented policy and update method as follows:
//
//<code>
//val trainer = new MHSampler[Label](model) with QLearning with EGreedyPolicy[Label] with GradientAscentUpdates //with ParameterAveraging
//</code>
//    @author Michael Wick
//    @see EligibilityTraces
//    @see Policy
//    @see SampledPolicy
//    @see EGreedyPolicy
//    @see BoltzmannPolicy
//*/
//trait QLearning extends ProposalSampler0 with SamplerOverSettings0 with Policy //with WeightUpdates .. I can't extend weights because of some odd subtleties in scala typing/erasure
//{
//  this: ProposalSampler[_] =>
//  type TemplatesToUpdate = DotTemplate
//  //
//  //parameters
//  var gamma : Double = 0.9
//  var rlLearningRate : Double = 0.00001
//  //
//  def updateWeights : Unit
//  var currentAction : Proposal = null
//  def shouldUpdate : Boolean = true
//  var tdError : Double = 0.0
//  def model:Model
//  abstract override def objective = if (super.objective == null) cc.factorie.defaultObjective else super.objective
//  //
//  //mixed in
//  def predictedScore = currentQ
//  def targetScore = currentReward
//  def learningMargin = 0.0
//  //
//  //for convenience
//  def correctionMagnitude : Double = tdError * rlLearningRate 
//  def currentReward=if(currentAction!=null)currentAction.objectiveScore else 0.0
//  def currentQ=if(currentAction!=null)currentAction.modelScore else 0.0
//  def nextReward=maxAction.objectiveScore
//  def nextQ=maxAction.modelScore
//  var count : Int = 0
//  //
//  //calculate the TD error
//  abstract override def proposalsHook(proposals:Seq[Proposal]) : Unit =
//    {
//      tdError = currentReward - currentQ + gamma * nextQ
//      updateWeights
//
//      //<temporary>: for debugging purposes
//      if(count % 1000 == 0)
//	{
//	  var norm : Double = 0.0
//	  for(template<-model.templatesOf[DotTemplate])
//	    {
//	      norm += template.weights dot template.weights
//	    }
//	  System.out.println("UPDATE #"+count)
//	  System.out.println("  |theta|: " + norm)
//	  System.out.println("  delta: "+correctionMagnitude)
//	}
//      if(correctionMagnitude!=0)
//	{
//	  System.out.println("    iter:"+count+" mag: " + correctionMagnitude)
//	}
//      count += 1
//      //</temporary>
//
//
//      if(currentAction!=null)currentAction.diff.redo
//      currentAction = nextAction
//    }
//
//  /**td error factored into the update, rate adds additional flexibility*/
//  def addGradient(accumulator:DotTemplate=>Vector, rate:Double): Unit =
//    {
//      //ignore the rate, use correctionMagnitude instead.. maybe multiply them?
//      if(currentAction==null || correctionMagnitude==0 || rate==0)
//	return
//      //System.out.println("COR: " + correctionMagnitude+" rate="+rate+" tog="+(correctionMagnitude*rate))
//      currentAction.diff.redo
//      currentAction.diff.factorsOf[TemplatesToUpdate](model)
//	.foreach(f => accumulator(f.template) += f.statistics.vector * (correctionMagnitude*rate))
//      currentAction.diff.undo
//      currentAction.diff.factorsOf[TemplatesToUpdate](model)
//	.foreach(f => accumulator(f.template) -= f.statistics.vector * (correctionMagnitude*rate))
//    }
//}
//
//
///**Adds Q(lambda) or Watkin's Q functionality to QLearning */
////TODO: don't extend QLearning, make both of these extend WeightUpdates
//trait EligibilityTraces extends QLearning
//{
//  this: ProposalSampler[_] =>
//  type TemplatesToUpdate <: DotTemplate
//  //
//  //parameters
//  var lambda : Double = 0.8
//
//  def model : Model
//  def currentAction : Proposal
//  def resetTraceTrigger : Boolean
//  var eligibilityTrace : HashMap[DotTemplate,SparseVector] = null
//  resetTrace
//
//  def resetTrace : Unit =
//    {
//      System.out.println("RESETTING")
//      eligibilityTrace = new HashMap[DotTemplate,SparseVector] {
//	override def default(template:DotTemplate) = {
//	  template.freezeDomains
//	  val vector = new SparseVector(template.statsize)
//	  this(template) = vector
//	  vector
//	}
//      }
//    }
//
//  def updateTrace : Unit =
//    for((template,vector)<-eligibilityTrace)
//      vector *= lambda
//
//  //TODO: figure out why TemplatesToUpdate in the map is not accepted by addGradient
//  override def addGradient(accumulator:DotTemplate=>Vector, rate:Double): Unit =
//    {
//      super.addGradient(eligibilityTrace,1.0)
//
//      if(false)
//	{
//	  var norm : Double = 0.0
//	  for((template,vector)<-eligibilityTrace)
//	    {
//	      norm += vector dot vector
//	    }
//	  System.out.println("  |trace|: " + norm+" trig: " + resetTraceTrigger)
//	}
//
//      for((template,vector)<-eligibilityTrace)
//	template.weights += vector * rate
//      if(resetTraceTrigger)
//	resetTrace
//      else
//	updateTrace
//    }
//}
//
///*
//  def getGradient : HashMap[TemplatesToUpdate,SparseVector] =
//    {
//      val gradient = new HashMap[TemplatesToUpdate,SparseVector] {
//    	override def default(template:TemplatesToUpdate) = {
//    	  template.freezeDomains
//    	  val vector = new SparseVector(template.statsize)
//    	  this(template) = vector
//    	  vector
//    	}
//      }
//      addGradient(gradient, 1.0)
//      gradient
//    }
//*/
//
//
//trait Policy
//{
//  def maxAction : Proposal
//  def nextAction : Proposal
//  def prepareAction : Unit
//}
//
////TODO: generalize these implementations to GibbsSampling... tried using proposal sampler, but tricky to get to work with MH and Gibbs (end up having to check conditions)
//
//
///**Represents a policy where max and next actions are found (or
//approximated) by enumating or sampling a finite set of actions, and
//then picking one. Max action is implemented here, but nextAction should be implemented in sub-classes. This is the type of policy used in UMASS08,NIPS09
//on the ontology alignment problem. TODO: generalize to Gibbs.
//    @author Michael Wick
//    @see Policy
//    @see EGreedyPolicy
//    @see BoltzmannPolicy
//*/
//trait SampledPolicy[C] extends MHSampler[C] with Policy
//{
//  //
//  //primary parameters
//  var samplesPerAction : Int = 100
//  var includeNoOpAction : Boolean = false
//  //
//  var candidateActions : ArrayBuffer[Proposal] = null
//  var _nextAction : Proposal = null
//  var _maxAction : Proposal = null
//  var numActionSteps : Int = 0
//  def prepareAction : Unit = {candidateActions = null;_nextAction = null}
//  //
//  //mixins
//  def model : Model
//  def objective : Model
//  def temperature : Double
//  def random : scala.util.Random
//
//  def maxAction : Proposal =
//    {
//      if(_maxAction != null)
//	return _maxAction
//      _maxAction = candidateActions.maxByDouble(_.modelScore);
//      _maxAction
//    }
//
//  abstract override def process1(context:C) : DiffList = 
//    {
//      proposalsHook(proposals(context))
//      //proposals(context)
//      val result=nextAction.diff
//      prepareAction
//      result
//    }
//  abstract override def proposals(context:C) : Seq[Proposal] =
//    {
//      if(candidateActions == null)
//	candidateActions = new ArrayBuffer[Proposal]
//      else
//	return candidateActions
//      numActionSteps += 1
//      var bfRatio = 0.0
//      while(candidateActions.size<samplesPerAction)
//      {
//	var proposalAttemptCount = 0
//	val difflist = new DiffList
//	while (difflist.size == 0 && proposalAttemptCount < 10)
//	{
//	  bfRatio = propose(context)(difflist)
//	  proposalAttemptCount += 1
//	}
//	if(difflist.size != 0)
//	  {
//	    val (modelScore, objectiveScore) = difflist.scoreAndUndo(model,objective)
//	    val logAcceptanceScore = modelScore/temperature+bfRatio
//	    candidateActions += new Proposal(difflist, modelScore, objectiveScore, logAcceptanceScore, bfRatio, temperature)
//	  }
//      }
//      if(includeNoOpAction)
//	{
//	  val mirrorLogAcceptanceScore = Double.NegativeInfinity //TODO: compute this properly taking into account all actions, requires sumLogProbs
//	  //val mirrorLogAcceptanceScore = if (logAcceptanceScore>=0) Double.NegativeInfinity else math.log(1-math.exp(logAcceptanceScore))
//	  candidateActions += new Proposal(new DiffList,0.0,0.0,mirrorLogAcceptanceScore,Double.NaN,0)
//	}
//      return candidateActions
//    }
//  
//  /*To be implemented by a particular policy*/
//  def nextAction:Proposal
//}
//
//
///**The traditional epsilon-greedy policy, but based on a sample pool; <code>epsilon</code> controls the amount of exploration. This policy is used in UMASS08,NIPS09 on the ontology alignment problem.
//    @author Michael Wick
//    @see Policy
//    @see SampledPolicy
//    @see BoltzmannPolicy
//*/
//trait EGreedyPolicy[C] extends SampledPolicy[C]
//{
//  this: ProposalSampler[_] =>
//  var resetTraceTrigger : Boolean = true //comes compatible with etraces
//  //
//  //primary parameters
//  var epsilon : Double = 0.1
//  
//  override def nextAction : Proposal =
//    {
//      if(_nextAction != null)
//	return _nextAction
//      if(random.nextDouble>epsilon)
//	  {
//	    _nextAction = candidateActions.maxByDouble(_.modelScore);
//	    resetTraceTrigger=false
//	  }
//      else
//	{
//	  _nextAction = candidateActions(random.nextInt(candidateActions.size))
//	  resetTraceTrigger=true
//	}
//      _nextAction
//    }
//}
//
///**The traditional Boltzmann policy where the next action is sampled according to the weight of each action (according to the Q-value).
//    @author Michael Wick
//    @see Policy
//    @see SampledPolicy
//    @see EGreedyPolicy
//*/
///*
//
//trait BoltzmannPolicy[C] extends SampledPolicy[C]
//{
//  //this: ProposalSampler[_] =>
//  var resetTraceTrigger : Boolean = false //comes compatible with etraces
//  override def nextAction : Proposal =
//    {
//      if(_nextAction == null)
//	_nextAction = candidateActions.sampleExpProportionally(random, _.modelScore)
//      super.nextAction //calls post proposal hook and returns
//    }
//}
//
////      postProposalHook(difflist)
//
//
//*/

