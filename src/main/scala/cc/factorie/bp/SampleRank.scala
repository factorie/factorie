package cc.factorie.bp
import cc.factorie._
import cc.factorie.la._
import cc.factorie.optimize._

//
//class Gradient extends scala.collection.mutable.HashMap[DotFamily,Vector]
//trait TensorMap extends scala.collection.Map[Any,Tensor]
//class TensorHashMap extends scala.collection.mutable.HashMap[Any,Tensor] with TensorMap
//class DotFamilyTensorHashMap extends scala.collection.mutable.HashMap[DotFamily,Tensor] {
//  override def default(f: DotFamily) = {
//    val t = Tensor.newSparse(null)
//    this(f) = t; t
//  }
//}
//
//
class SampleRank2[C](val model:Model, sampler:ProposalSampler[C], optimizer:GradientOptimizer) {
  def this(sampler:ProposalSampler[C], optimizer:GradientOptimizer) = this(sampler.model, sampler, optimizer)
  var learningMargin = 1.0
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  def gradientAndMargin(proposals:Seq[Proposal]): (Tensor,Double) = {
    var g: WeightsTensor = null // model.newSparseWeightsTensor
    //val g2 = new TensorMap; def gg(df:DotFamily): Tensor = g.getOrElseUpdate(df, Tensor.newSparse(df.weights)
    val bestModels = proposals.max2ByDouble(_ modelScore); val bestModel1 = bestModels._1; val bestModel2 = bestModels._2
    val bestObjectives = proposals.max2ByDouble(_ objectiveScore); val bestObjective1 = bestObjectives._1; val bestObjective2 = bestObjectives._2
    val margin = bestModel1.modelScore - bestModel2.modelScore
    if (bestModel1 ne bestObjective1) {
      // ...update parameters by adding sufficient stats of truth, and subtracting error
      g = model.newSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
    }
    else if (margin < learningMargin) {
      // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
      g = model.newSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel2.diff.redo
      model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, -1.0))
      bestModel2.diff.undo
      model.factorsOfFamilies(bestModel2.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.statistics.tensor, 1.0))
    }
    (g, margin)
  }
  def process(c:C): DiffList = {
    val proposals = sampler.proposals(c)
    val (gradient,margin) = gradientAndMargin(proposals)
    if (gradient ne null)
      optimizer.step(model.weightsTensor, gradient, Double.NaN, margin)
    val bestProposal = proposals.maxByDouble(_.modelScore)
    bestProposal.diff.redo
    bestProposal.diff
  }
  def processAll(cs:Iterable[C]): Unit = cs.foreach(process(_))
  def processAll(cs:Iterable[C], repeat:Int): Unit = for (i <- 0 until repeat) cs.foreach(process(_))
  // In the old SampleRank there was something like the following.  Do we need this for any reason?
  /* def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)      
    } else ... */

}

/*
  def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)      
    } else {
      // the objective function has some preference (e.g. we don't have an unlabeled example here)
      (bestObjective1.objectiveScore > bestObjective2.objectiveScore || bestObjective1.objectiveScore > bestModel1.objectiveScore) &&
      // the model got it wrong, or isn't confident enough about being right
      // TODO should this be based on acceptanceScore instead of modelScore?
      ((bestModel1 ne bestObjective1) || math.abs(bestModel1.modelScore - bestModel2.modelScore) < learningMargin)
    }
  }

 */


//
//// TODO Shouldn't really be called "Perceptron".  This is for any method that gets a gradient by a different of truth-predicted
//class Perceptron[C<:VarWithTargetValue](maximize:C=>Unit, updater:ChangeWeights) {
// def gradient(diff:DiffList): Gradient = throw new Error("Not yet implemented")
// def process(c:C): Unit = {
//   maximize(c)
//   val diff = new DiffList
//   c.setToTarget(diff)
//   val margin = 0.0 // TODO
//   val g = gradient(diff)
//   updater(g, Double.NaN, margin, 1.0)
// }
//}
//
//
//trait GradientOptimize {
//  def weights: Tensor
//  def apply(gradient:Tensor, value:Double): Unit
//  def isConverged: Boolean = false
//}
//
//class GradientAscent(val model:TemplateModel) extends GradientOptimize {
// var rate: Double = 1.0
// def apply(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
//   for (template <- model.familiesOfClass[DotFamily]) template.weights += gradient(template) * rate
// }
//}
//
//class AROW(val model:TemplateModel) extends GradientOptimize {
// var margin: Double
// def apply(weights:Tensor, gradient:Tensor, value:Double, margin:Double, rate:Double): Unit = {
//   throw new Error("Not yet implemented.")
// }
//}
//
//class LimitedMemoryBFGS(val model:TemplateModel) extends GradientOptimize {
//  var lineSearching = false
//  def apply(weights:Tensor, gradient:Tensor, value:Double, margin:Double, rate:Double): Unit = {
//    throw new Error("Not yet implemented.")
//  }
//}
//
//object UpdateTest {
//  
//}