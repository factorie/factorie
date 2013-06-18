package cc.factorie

import cc.factorie.la.{SparseIndexedTensor1, WeightsMapAccumulator, Tensor}
import cc.factorie.optimize.Example
import cc.factorie.util.DoubleAccumulator
import cc.factorie.app.nlp.Token
import cc.factorie.app.chain.ChainModel
import org.junit.Assert._

/**
 * User: apassos
 * Date: 6/15/13
 * Time: 7:38 AM
 */

trait WarmStartWeightedSummary {
  def summary: Summary
  def infer(): Unit
  def incrementWeights(v: DiscreteVar, t: Tensor, d: Double)
}

case class ModelWithInference[M,V](vars: V, model: M)(implicit val infer: (V,M) => WarmStartWeightedSummary) {
  def summary = infer(vars, model)
}

class DualDecomposition(stepSize: (Int,Int) => Double = DualDecomposition.LearningRates.expDualIncrease(1.0, 0.9)) extends Infer[Seq[WarmStartWeightedSummary], Seq[(Int,DiscreteVar,Int,DiscreteVar)]] with cc.factorie.util.GlobalLogging {
  def infer(summaries: Seq[WarmStartWeightedSummary], constraints: Seq[(Int, DiscreteVar, Int, DiscreteVar)]): MAPSummary = {
    var dual = summaries.map(_.summary.logZ).sum
    var updated = true
    var dualIncreases = 1
    var iterations = 1
    while (updated) {
      println("doing something")
      summaries.map(_.infer()) // summaries are responsible for caching inference results if they did not change
      println("a")
      val newDual = summaries.map(_.summary.logZ).sum
      println("a")
      if (newDual > dual) dualIncreases += 1
      println("a")
      dual = newDual
      println("a " + dual)
      logger.info(s"Dual: $dual}")
      println("a")
      updated = false
      println("a")
      for ((i1, v1, i2, v2) <- constraints) {
        println("a")
        val discreteMarginal1 = summaries(i1).summary.marginal(v1).asInstanceOf[DiscreteMarginal]
        println("a")
        val discreteMarginal2 = summaries(i2).summary.marginal(v2).asInstanceOf[DiscreteMarginal]
        println("a")
        println("a " + discreteMarginal1.proportions + " " + discreteMarginal2.proportions)
        if (discreteMarginal1.proportions.maxIndex != discreteMarginal2.proportions.maxIndex) {
          println("a")
          updated = true
          println("a")
          val t = new SparseIndexedTensor1(discreteMarginal1.proportions.length)
          println("a " + discreteMarginal1.proportions + " " + discreteMarginal2.proportions)
          t += (discreteMarginal1.proportions.maxIndex, -1.0)
          println("a")
          t += (discreteMarginal2.proportions.maxIndex, 1.0)
          println("a")
          val step = stepSize(iterations, dualIncreases)
          println("a")
          summaries(i1).incrementWeights(v1, t, step)
          println("a")
          summaries(i2).incrementWeights(v2, t, -step)
          println("a")
        }
      }
      iterations += 1
    }
    val as = new HashMapAssignment(ignoreNonPresent=false)
    for (summary <- summaries) {
      implicit val d = new DiffList
      summary.summary.setToMaximize(d)
      for (diff <- d; v = diff.variable)
        as.update[v.type,v.type#Value](v, v.value.asInstanceOf[v.type#Value])
      d.undo
    }
    new MAPSummary(as, summaries.flatMap(_.summary.factorMarginals.map(_.factor)).distinct)
  }
}

object InferByDualDecomposition extends DualDecomposition

object DualDecomposition {
  class WeightedSummaryWithBP(vars: Iterable[DiscreteVar], model: Model, baseInfer: MaximizeByBP) extends WarmStartWeightedSummary {
    val weightedModel = new ItemizedModel
    val combinedModel = new CombinedModel(model, weightedModel)
    var summary = baseInfer.infer(vars, combinedModel)
    def infer() { summary = baseInfer.infer(vars, combinedModel)}
    class WeightedFactor(v: DiscreteVar, t: Tensor, d: Double) extends Factor1[DiscreteVar](v) {
      def score(v1: DiscreteVar#Value) = d*(v1 dot t)
      override def valuesScore(v1: Tensor) = d*(v1 dot t)
    }
    def incrementWeights(v: DiscreteVar, t: Tensor, d: Double) { weightedModel += new WeightedFactor(v, t, d)}
  }
  def getBPInferChain(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPChain)
  def getBPInferTree(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPTree)
  def getBPInferLoopy(vars: Iterable[DiscreteVar], model: Model) = new WeightedSummaryWithBP(vars, model, MaximizeByBPLoopy)

  object LearningRates {
    def expDualIncrease(eta: Double, c: Double) = (iteration: Int, dualIncreases: Int) => eta*math.pow(c, -dualIncreases)
    def expT(eta: Double, c: Double) = (iteration: Int, dualIncreases: Int) => eta*math.pow(c, -iteration)

    def invDualIncrease(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/dualIncreases
    def invT(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/iteration

    def invSqrtDualIncrease(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/math.sqrt(dualIncreases)
    def invSqrtT(eta: Double) = (iteration: Int, dualIncreases: Int) => eta/math.sqrt(iteration)
  }
}

object DDTestMain {
  def main(args: Array[String]) {
    val random = new scala.util.Random(0)
    object cdomain extends CategoricalTensorDomain[String]()
    val features = new BinaryFeatureVectorVariable[String]() { def domain = cdomain }
    features += "asd"
    val ldomain = new CategoricalDomain[String]()
    val d = new app.nlp.Document("noname")
    val t0 = new Token(d, 0, 1)
    val t1 = new Token(d, 0, 1)
    val t2 = new Token(d, 0, 1)
    val t3 = new Token(d, 0, 1)
    class Label(t: String) extends LabeledCategoricalVariable[String](t) { def domain = ldomain}
    val l0 = new Label("1")
    val l1 = new Label("0")
    val l2 = new Label("2")
    val l3 = new Label("3")
    val lToT = Map(l0 -> t0, l1 -> t1, l2 -> t2, l3 -> t3)
    val tToL = Map(t0 -> l0, t1 -> l1, t2 -> l2, t3 -> l3)
    val model = new ChainModel[Label, BinaryFeatureVectorVariable[String], Token](ldomain, cdomain, l => features, lToT, tToL)
    model.parameters.tensors.foreach(t => t.foreachElement((i, v) => t(i) += random.nextDouble()))

    // testing dual decomposition
    val mapSummary = MaximizeByBPChain.infer(Seq(l0, l1, l2, l3), model)
    val model0 = DualDecomposition.getBPInferChain(Seq(l0, l1, l2), model)
    val model1 = DualDecomposition.getBPInferChain(Seq(l2, l3), model)
    val ddSummary = InferByDualDecomposition.infer(Seq(model0, model1), Seq((0, l2, 1, l2)))
    for (v <- Seq(l0, l1, l2, l3)) {
      val mfm = ddSummary.mapAssignment(v)
      val bpm = mapSummary.marginal(v)
      assert(bpm.proportions.maxIndex == mfm.intValue)
    }

  }
}