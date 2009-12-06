package cc.factorie
import scalala.Scalala._
import scalala.tensor.Vector

/** Implements Geoff Hinton's Constrastive Divergence, obtaining a gradient after one step away from the true configuration.
    This implementation assumes that the initial configuration is the truth. 
    @author Andrew McCallum
    @since 0.8
 */
abstract class ContrastiveDivergence[C](model:Model) extends MHSampler[C](model) {
  type TemplatesToUpdate = DotTemplate
  def updateWeights: Unit
  var difflist : DiffList = null
  
  override def postProcessHook(context:C, d:DiffList) : Unit = {
    super.postProcessHook(context, d)
  	difflist = d
  	updateWeights // This will result in a call to addGradient
  }

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit = {
  	difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
  	difflist.undo
  	difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
  }  
}
