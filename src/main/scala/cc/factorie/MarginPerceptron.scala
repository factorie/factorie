package cc.factorie
import cc.factorie.la._
import collection.mutable.HashMap

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 5/10/12
 * Time: 12:10 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class MarginPerceptron [V<:VarWithTargetValue] extends WeightUpdates {
  def model: TemplateModel
  var learningMargin = 1.0 // can be used by predictSecondBest
  protected[this] var difflist: DiffList = null
  var actualMargin = 0.0

  // Abstract methods to be provided elsewhere
  def predict(vs:Seq[V]): Double
  def predictSecondBest(vs: Seq[V]): Double

  def process(vs:Seq[V]): Unit = {
    val bestScore = predict(vs)
    difflist = new DiffList
    vs.foreach(_.setToTarget(difflist))
    val truthScore = model.score(vs)
    if (difflist.length > 0) {
      difflist.undo // TODO Consider commenting this out, but consider carefully.  Dangerous for "addGradient" to have side-effects.
      actualMargin = truthScore - bestScore
      updateWeights
    } else {
      val secondBestScore = predictSecondBest(vs)
      vs.foreach(_.setToTarget(difflist))
      if (truthScore - secondBestScore < learningMargin) {
        actualMargin = truthScore - secondBestScore
        difflist.undo
        updateWeights
      }
    }
  }

  def addGradient(accumulator:DotFamily=>Vector, rate:Double): Unit = {
    if (!difflist.done) difflist.redo
    model.factorsOfFamilies(difflist, familiesToUpdate).foreach(f => accumulator(f.family) += f.statistics.vector *  rate)
    //difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.family) += f.statistics.vector *  rate)
    difflist.undo
    model.factorsOfFamilies(difflist, familiesToUpdate).foreach(f => accumulator(f.family) += f.statistics.vector *  -rate)
    //difflist.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.family) += f.statistics.vector * -rate)
  }

}

abstract class StructuredSVMSGD[V <: VarWithTargetValue](lambda: Double) extends MarginPerceptron[V] with L2Regularization  {
  familiesToRegularize ++= familiesToUpdate
  lambdas ++= familiesToRegularize.map(f => lambda)

  def learningRate = 1.0/((updateCount + 1.0)* lambda)
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  //def learningMargin : Double
  override def updateWeights : Unit = {
    // Add the gradient directly to each relevant Template's weight vector, with scaling factor 'learningRate'
    addGradient((family:UpdateFamilyType) => family.weights, learningRate) //match {case t:TemplatesToUpdate => t.weights}, learningRate)
    // Call super to increment the updateCount
    super.updateWeights
  }


}