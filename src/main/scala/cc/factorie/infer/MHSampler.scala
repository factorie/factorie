/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.infer

import cc.factorie.model.Model
import cc.factorie.util.Hooks0
import cc.factorie.variable.DiffList

/** A Metropolis-Hastings sampler.  
    The abstract method 'propose' should be defined to make a random proposal, putting changes in its implicit DiffList, 
    and returning the log ratio of q/q' (backward to forward jump probabilities.)
    @author Andrew McCallum
    @author Michael Wick
    @since 0.8
    @see ProposalSampler
    @see GibbsSampler
*/
abstract class MHSampler[C](val model:Model)(implicit val random: scala.util.Random) extends ProposalSampler[C] {

  /** This method must be implemented in concrete subclasses.
      It should return the log of the ratio of backwards-to-forwards jump probabilities. */
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
  def preProposalHook(): Unit = preProposalHooks()
  /** Called just after making the proposed change.  If you override, you must call super.postProposalHook! */
  def postProposalHook(d:DiffList) : Unit = {}
  /** Called just after undoing the proposed change.  If you override, you must call super.postUndoHook! */
  def postUndoHook(acceptScore:Double, d:DiffList) : Unit = {}
  /** Called after accepting the proposed change.  If you override, you must call super.postAcceptanceHook! */
  def postAcceptanceHook(logAcceptanceProb:Double, d:DiffList) : Unit = {}
  /** Called whenever we accept a proposal that results in the best configuration seen so far.  If you override, you must call super.bestConfigHook! */
  def bestConfigHook(): Unit = {}
  
  /** Specialization of cc.factorie.Proposal that adds a MH forward-backward transition ratio, typically notated as a ratio of Qs. */
  case class Proposal[C](override val diff:DiffList, override val modelScore:Double, override val objectiveScore:Double, override val acceptanceScore:Double, bfRatio:Double, temperature:Double, override val context:C) extends cc.factorie.infer.Proposal(diff, modelScore, objectiveScore,acceptanceScore, context)
  
  var proposalsCount = 0
  def proposals(context:C): Seq[Proposal[C]] = {
    numProposedMoves += 1
    proposalAccepted = false
    val difflist = new DiffList
    proposalAccepted = false
    preProposalHook()
    // Make the proposed jump
    var bfRatio = 0.0
    var proposalAttemptCount = 0
    while (difflist.size == 0 && proposalAttemptCount < 10) {
      bfRatio = propose(context)(difflist)
      //println("MHSampler propose diff "+difflist)
      proposalAttemptCount += 1
    }
    if (difflist.size == 0) throw new Error("No proposal made changes in 10 tries.")
    proposalsCount += 1
    postProposalHook(difflist)
    val (modelScore, objectiveScore) = difflist.scoreAndUndo(model, objective)
    //val goProposal = new Proposal(difflist, modelScore/temperature + bfRatio, objectiveScore, bfRatio)x
//    val goProposal = new Proposal(difflist,modelScore/temperature+bfRatio,objectiveScore,bfRatio,modelScore)
    //val stayProposal = new Proposal(new DiffList, 0.0, 0.0, Double.NaN,0.0)
    //List(goProposal,stayProposal)
    //println("MHSampler modelScore="+modelScore+" objectiveScore="+objectiveScore)
    val logAcceptanceScore = modelScore/temperature+bfRatio
    val mirrorLogAcceptanceScore = if (logAcceptanceScore>=0) Double.NegativeInfinity else math.log(1-math.exp(logAcceptanceScore))
    val goProposal = new Proposal(difflist,modelScore,objectiveScore,logAcceptanceScore,bfRatio,temperature, context)
    val stayProposal = new Proposal(new DiffList,0.0,0.0,mirrorLogAcceptanceScore,Double.NaN,0, context)
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


  override def proposalHook(proposal:cc.factorie.infer.Proposal[C]): Unit = {
    super.proposalHook(proposal)
    val p = proposal.asInstanceOf[Proposal[C]]
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
        bestConfigHook()
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
    preProposalHook()
    // Make the proposed jump
    var logAcceptanceProb: Double = propose(context)(difflist)
    postProposalHook(difflist)
    var modelRatio: Double = difflist.scoreAndUndo(model)
    logAcceptanceProb += (modelRatio / temperature)
    System.out.println("log acc: " + logAcceptanceProb)
    postUndoHook(logAcceptanceProb, difflist)
    // TODO Change this to actually sample, but test to make sure that the commented code is correct !!!
    if (logAcceptanceProb > math.log(random.nextDouble())) {
      // Proposal accepted!  (Re)make the change.
      difflist.redo()
      // Update diagnostics
      numAcceptedMoves += 1
      proposalAccepted = true
      if (modelRatio < 0) numNegativeMoves += 1
      //log(Log.INFO)("iteration: " + iteration + ", logAcceptanceProb = " + logAcceptanceProb);
      // Maintain the running incremental change in model score
      currentModelScore += modelRatio
      if (currentModelScore > maxModelScore) {
        maxModelScore = currentModelScore
        bestConfigHook()
      }
      postAcceptanceHook(logAcceptanceProb, difflist)
    } else {
      difflist.clear()
    }
    difflist
  }

  
}
