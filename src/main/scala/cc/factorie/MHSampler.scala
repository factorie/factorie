package cc.factorie
import scala.collection.mutable.Publisher

abstract class MHSampler1(val model:Model) extends Publisher[DiffList,MHSampler1] {
  var temperature = 1.0
  var random = Global.random
  
  // This method must be implemented in concrete subclasses
  def propose(d:DiffList) : Double

  // Various diagnostics
  var numProposedMoves = 0
  var numAcceptedMoves = 0
  var numNegativeMoves = 0
  var proposalAccepted = false
  // To track best configuratio
  var maxModelScore = Math.MIN_DOUBLE
  var currentModelScore = 0.0
  
  // Various messages we publish
  case class ProposalAccepted(difflist:DiffList)
  def bestConfigFound(s:MHSampler1) : Unit = {}
  val postIterationHook : MHSampler1=>Unit = null // TODO make this a member method?

  // TODO consider renaming this method
  def step : DiffList = {
    numProposedMoves += 1
    val difflist = new DiffList
    proposalAccepted = true
    // Make the proposed jump
    var logAcceptanceProb: Double = propose(difflist)
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
    		bestConfigFound(this)
    	}
    } else {
    	difflist.clear
    }
    difflist
  }
  
  def step(numIterations:Int): Unit = {
    for (iteration <- 0 until numIterations) {
      step
      if (postIterationHook != null) postIterationHook(this)
    }
  }
}
