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

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.{Hooks0,Hooks1,Hooks2}

/** A Metropolis-Hastings sampler.  
    The abstract method 'propose' should be defined to make a random proposal, putting changes in its implicit DiffList, 
    and returning the ratio of q/q' (backward to forward jump probabilities.) 
    @author Andrew McCallum
    @author Michael Wick
    @since 0.8
    @see ProposalSampler
    @see GibbsSampler
*/
abstract class MHSampler[C](val model:Model) extends ProposalSampler[C] {
  var random = cc.factorie.random
  
  // This method must be implemented in concrete subclasses
  def propose(context:C)(implicit d:DiffList) : Double

  /** If you want the Proposals to actually contain the objectiveScore, override this method appropriately.  Used for training. */
  def objective : Model = null

  // Various diagnostics
  var numProposedMoves = 0
  var numAcceptedMoves = 0
  var numNegativeMoves = 0
  var proposalAccepted = false
  // To track best configuration
  var maxModelScore = Double.MinValue
  var currentModelScore = 0.0

  // Hooks
  /** Called just before making the proposed change.  If you override, you must call super.preProposalHook! */
  val preProposalHooks = new Hooks0 // TODO And add these to the rest of the hooks below
  def preProposalHook : Unit = preProposalHooks.apply
  /** Called just after making the proposed change.  If you override, you must call super.postProposalHook! */
  def postProposalHook(d:DiffList) : Unit = {}
  /** Called just after undoing the proposed change.  If you override, you must call super.postUndoHook! */
  def postUndoHook(acceptScore:Double, d:DiffList) : Unit = {}
  /** Called after accepting the proposed change.  If you override, you must call super.postAcceptanceHook! */
  def postAcceptanceHook(logAcceptanceProb:Double, d:DiffList) : Unit = {}
  /** Called whenever we accept a proposal that results in the best configuration seen so far.  If you override, you must call super.bestConfigHook! */
  def bestConfigHook: Unit = {}
  
  /** Specialization of cc.factorie.Proposal that adds a MH forward-backward transition ratio, typically notated as a ratio of Qs. */
  case class Proposal(override val diff:DiffList, override val modelScore:Double, override val objectiveScore:Double, override val acceptanceScore:Double, val bfRatio:Double, val temperature:Double) extends cc.factorie.Proposal(diff, modelScore, objectiveScore,acceptanceScore)
  
  def proposals(context:C) : Seq[Proposal] = {
    numProposedMoves += 1
    proposalAccepted = false
    val difflist = new DiffList
    proposalAccepted = false
    preProposalHook
    // Make the proposed jump
    var bfRatio = 0.0
    var proposalAttemptCount = 0
    while (difflist.size == 0 && proposalAttemptCount < 10) {
      bfRatio = propose(context)(difflist)
      proposalAttemptCount += 1
    }
    if (difflist.size == 0) throw new Error("No proposal made changes in 10 tries.")
    postProposalHook(difflist)
    val (modelScore, objectiveScore) = difflist.scoreAndUndo(model, objective)
    //val goProposal = new Proposal(difflist, modelScore/temperature + bfRatio, objectiveScore, bfRatio)x
//    val goProposal = new Proposal(difflist,modelScore/temperature+bfRatio,objectiveScore,bfRatio,modelScore)
    //val stayProposal = new Proposal(new DiffList, 0.0, 0.0, Double.NaN,0.0)
    //List(goProposal,stayProposal)
    //System.out.println("MODEL: " + modelScore+" objSCORE:" + objectiveScore)
    val logAcceptanceScore = modelScore/temperature+bfRatio
    val mirrorLogAcceptanceScore = if (logAcceptanceScore>=0) Double.NegativeInfinity else math.log(1-math.exp(logAcceptanceScore))
    val goProposal = new Proposal(difflist,modelScore,objectiveScore,logAcceptanceScore,bfRatio,temperature)
    val stayProposal = new Proposal(new DiffList,0.0,0.0,mirrorLogAcceptanceScore,Double.NaN,0)
    List(goProposal,stayProposal)
  }
  
/*
  override def process1(context:C) : DiffList =
    {
      val props = proposals(context)
      val proposal = props.head
      //System.out.println("accp: " + (proposal.modelScore/proposal.temperature+proposal.bfRatio))
      if(proposal.modelScore * proposal.objectiveScore<=0 && proposal.objectiveScore!=0)
      proposalsHook(props)
      if(proposal.acceptanceScore >= math.log(cc.factorie.random.nextDouble()))
  {
    System.out.println("REDO")
    System.out.println("accept: " + proposal.acceptanceScore)
    proposal.diff.redo
  }
      //else
//  proposal.diff.undo
      proposal.diff
    }
*/


  override def proposalHook(proposal:cc.factorie.Proposal): Unit = {
    val p = proposal.asInstanceOf[Proposal]
    //if (p.bfRatio != Double.NaN) {
    if(!p.bfRatio.isNaN) {
      numAcceptedMoves += 1
      proposalAccepted = true
      val modelRatio = p.modelScore
      if (modelRatio < 0) numNegativeMoves += 1
      //log(Log.INFO)("iteration: " + iteration + ", logAcceptanceProb = " + logAcceptanceProb);
      // Maintain the running incremental change in model score
      currentModelScore += modelRatio
      if (currentModelScore > maxModelScore) {
        maxModelScore = currentModelScore
        bestConfigHook
      }
      postAcceptanceHook(p.modelScore/temperature+p.bfRatio, p.diff)
    } 
/*
    //mwick quick test of MIRA... if OBJ and MOD disagree on sign, then "UPD" should equal "OBJ" after the update
    if(p.objectiveScore!=0)
      {
  if(proposalAccepted)
    {
      val score = p.diff.scoreAndUndo(model)
      System.out.println("OBJ: " + p.objectiveScore)
      System.out.println("MOD: " + p.rawModelScore)
      System.out.println("UPD: " + score)
      p.diff.redo
    }
  else
    {
      p.diff.redo
      val score = p.diff.scoreAndUndo(model)
      System.out.println("99OBJ: " + p.objectiveScore)
      System.out.println("99MOD: " + p.rawModelScore)
      System.out.println("99UPD: " + score)
    }
      }
*/
    
  }

  
  def process1unused(context:C) : DiffList = {
    numProposedMoves += 1
    val difflist = new DiffList
    proposalAccepted = false
    preProposalHook
    // Make the proposed jump
    var logAcceptanceProb: Double = propose(context)(difflist)
    postProposalHook(difflist)
    var modelRatio: Double = difflist.scoreAndUndo(model)
    logAcceptanceProb += (modelRatio / temperature)
    System.out.println("log acc: " + logAcceptanceProb)
    postUndoHook(logAcceptanceProb, difflist)
    // TODO Change this to actually sample, but test to make sure that the commented code is correct !!!
    if (logAcceptanceProb > math.log(random.nextDouble)) {
      // Proposal accepted!  (Re)make the change.
      difflist.redo
      // Update diagnostics
      numAcceptedMoves += 1
      proposalAccepted = true
      if (modelRatio < 0) numNegativeMoves += 1
      //log(Log.INFO)("iteration: " + iteration + ", logAcceptanceProb = " + logAcceptanceProb);
      // Maintain the running incremental change in model score
      currentModelScore += modelRatio
      if (currentModelScore > maxModelScore) {
        maxModelScore = currentModelScore
        bestConfigHook
      }
      postAcceptanceHook(logAcceptanceProb, difflist)
    } else {
      difflist.clear
    }
    difflist
  }

  
}
