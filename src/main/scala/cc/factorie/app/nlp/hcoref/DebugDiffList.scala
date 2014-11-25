package cc.factorie.app.nlp.hcoref

import cc.factorie.{TemplateModel, DiffList, Model}

/**
 * @author John Sullivan
 */
trait DebuggableTemplate {
  protected var _debug: Boolean = false
  def debugOn() = _debug = true
  def debugOff() = _debug = false
  def name: String

  /** methods implementing this trait need to call report manually during the scoring process
    * to print out debug results */
  def report(score:Double, weight:Double) {
    if(_debug) {
      println("\t%.4f = %.4f * %.4f (score * weight)  [%s]".format(score * weight, score, weight, name))
    }
  }
}

class DebugDiffList extends DiffList {
  override def scoreAndUndo(model:Model): Double = {
    model.asInstanceOf[TemplateModel].families.collect{case t:DebuggableTemplate => t.debugOn()}

    if (this.length == 0) return 0.0  // short-cut the simple case
    println("=====DEBUGGING MODEL SCORE=====")
    println("----NEXT WORLD----")
    var s = model.currentScore(this)
    println("  next: "+ s)
    //log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
    this.undo()
    // We need to re-calculate the Factors list because the structure may have changed
    println("----CURRENT WORLD----")
    val s2 = model.currentScore(this)
    println("  current: "+s2)
    s -= s2
    println("TOTAL SCORE: "+s)
    model.asInstanceOf[TemplateModel].families.collect{case t:DebuggableTemplate => t.debugOff()}
    s
  }}