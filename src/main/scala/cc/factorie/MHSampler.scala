package cc.factorie
import scala.reflect.Manifest
//import scala.collection.mutable.Publisher

abstract class MHSampler1[C](val model:Model)(implicit mc:Manifest[C]) extends Sampler[C]()(mc) {
  var temperature = 1.0
  var random = Global.random
  
  // This method must be implemented in concrete subclasses
  def propose(context:C, d:DiffList) : Double

  // Various diagnostics
  var numProposedMoves = 0
  var numAcceptedMoves = 0
  var numNegativeMoves = 0
  var proposalAccepted = false
  // To track best configuratio
  var maxModelScore = Math.MIN_DOUBLE
  var currentModelScore = 0.0
  
  def bestConfigFound : Unit = {}
  def postProposalHook : Unit = {}

  // TODO consider renaming this method
  def process(context:C) : DiffList = {
    numProposedMoves += 1
    val difflist = new DiffList
    proposalAccepted = true
    // Make the proposed jump
    var logAcceptanceProb: Double = propose(context, difflist)
    var modelRatio: Double = difflist.scoreAndUndo(model)
    logAcceptanceProb += (modelRatio / temperature)
    if (logAcceptanceProb > Math.log(random.nextDouble)) {
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
    		bestConfigFound
    	}
    } else {
    	difflist.clear
    }
    postProposalHook
    difflist
  }
  
}
