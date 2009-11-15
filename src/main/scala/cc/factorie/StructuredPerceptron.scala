package cc.factorie
import scala.reflect.Manifest
import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.Vector

/** Collins' structured-perceptron */
abstract class StructuredPerceptron(model:Model) extends PerceptronUpdates {
  type TemplatesToUpdate <: DotTemplate
  var learningMargin = 1.0 // TODO not currently used
  private var difflist: DiffList = null
  
  // Abstract method to be provided elsewhere
  def predict(vs:Seq[Variable with TrueSetting]): Unit
  
  def process(vs:Seq[Variable with TrueSetting]): Unit = {
    predict(vs)
    difflist = new DiffList
    vs.foreach(_.setToTruth(difflist))
    difflist.undo // TODO Consider commenting this out, but consider carefully.  Dangerous for "addGradient" to have side-effects.
    if (difflist.size > 0)
    	updateWeights // This will result in a call to "addGradient"
  }

  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit = {
    if (!difflist.done) difflist.redo
  	difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
  	difflist.undo
  	difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
  }

}


// TODO What would a MaximumMargin trait look like?
