package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.{Hooks0,Hooks1,Hooks2}

abstract class MHSampler[C](val model:Model) extends ProposalSampler[C] {
  var random = Global.random
  
  // This method must be implemented in concrete subclasses
  def propose(context:C)(implicit d:DiffList) : Double

  /** If you want the Proposals to actually contain the objectiveScore, override this method appropriately.  Used for training.	*/
  def objective : Model = null

  // Various diagnostics
  var numProposedMoves = 0
  var numAcceptedMoves = 0
  var numNegativeMoves = 0
  var proposalAccepted = false
  // To track best configuration
  var maxModelScore = Math.MIN_DOUBLE
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
  case class Proposal(override val diff:DiffList, override val modelScore:Double, override val objectiveScore:Double, fbRatio:Double) extends cc.factorie.Proposal(diff, modelScore, objectiveScore)
  
  def proposals(context:C) : Seq[Proposal] = {
    numProposedMoves += 1
    proposalAccepted = false
    val difflist = new DiffList
    proposalAccepted = false
    preProposalHook
    // Make the proposed jump
    var fbRatio = 0.0
    var proposalAttemptCount = 0
    while (difflist.size == 0 && proposalAttemptCount < 10) {
    	fbRatio = propose(context)(difflist)
    	proposalAttemptCount += 1
    }
    if (difflist.size == 0) throw new Error("No proposal made changes in 10 tries.")
    postProposalHook(difflist)
    val (modelScore, objectiveScore) = difflist.scoreAndUndo(model, objective)
    val goProposal = new Proposal(difflist, modelScore/temperature + fbRatio, objectiveScore, fbRatio)
    val stayProposal = new Proposal(new DiffList, 0.0, 0.0, Math.NaN_DOUBLE)
    List(goProposal,stayProposal)
  }
  
  override def proposalHook(proposal:cc.factorie.Proposal): Unit = {
    val p = proposal.asInstanceOf[Proposal]
    if (p.fbRatio != Math.NaN_DOUBLE) {
      numAcceptedMoves += 1
    	proposalAccepted = true
    	val modelRatio = p.modelScore - p.fbRatio
    	if (modelRatio < 0) numNegativeMoves += 1
    	//log(Log.INFO)("iteration: " + iteration + ", logAcceptanceProb = " + logAcceptanceProb);
      // Maintain the running incremental change in model score
      currentModelScore += modelRatio
    	if (currentModelScore > maxModelScore) {
    		maxModelScore = currentModelScore
    		bestConfigHook
    	}
      postAcceptanceHook(p.modelScore, p.diff)
    } 
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
    postUndoHook(logAcceptanceProb, difflist)
    // TODO Change this to actually sample, but test to make sure that the commented code is correct !!!
    if (logAcceptanceProb > 0.0 /*Math.log(random.nextDouble)*/ ) {
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
