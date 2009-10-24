package cc.factorie
import scala.reflect.Manifest
//import scala.collection.mutable.Publisher
import scala.collection.mutable.ArrayBuffer

abstract class MHSampler[C](val model:Model)(implicit mc:Manifest[C]) extends Sampler[C]()(mc) {
  var temperature = 1.0
  var random = Global.random
  
  // This method must be implemented in concrete subclasses
  def propose(context:C)(implicit d:DiffList) : Double

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
  def preProposalHook : Unit = {}
  /** Called just after making the proposed change.  If you override, you must call super.postProposalHook! */
  def postProposalHook(d:DiffList) : Unit = {}
  /** Called just after undoing the proposed change.  If you override, you must call super.postUndoHook! */
  def postUndoHook(acceptScore:Double, d:DiffList) : Unit = {}
  /** Called after accepting the proposed change.  If you override, you must call super.postAcceptanceHook! */
  def postAcceptanceHook(logAcceptanceProb:Double, d:DiffList) : Unit = {}
  /** Called whenever we accept a proposal that results in the best configuration seen so far.  If you override, you must call super.bestConfigHook! */
  def bestConfigHook: Unit = {}
  
  def process1(context:C) : DiffList = {
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
