package cc.factorie

/**
 * @author sameer
 */

import cc.factorie._
import junit.framework._
import Assert._
import scala.util.Random
import collection.mutable.ArrayBuffer
import cc.factorie.variable.{LabeledCategoricalVariable, CategoricalDomain}
import cc.factorie.model.{ItemizedModel, TupleFactorWithStatistics2}
import cc.factorie.infer.VariablesSettingsSampler

/**
 * @author sameer
 * @since Sep 5, 2011
 */

class TestProposalSamplers extends TestCase with cc.factorie.util.FastLogging {

  val numLabels: Int = 3

  // a binary variable that takes values 0 or 1
  object LabelDomain extends CategoricalDomain[Int](0 until numLabels)

  class BinVar(i: Int) extends LabeledCategoricalVariable(i) {
    def domain = LabelDomain
  }

  import scala.language.existentials
  private def newFactor2(n1: BinVar, n2: BinVar, scoreEqual: Double, scoreUnequal: Double) =
    new TupleFactorWithStatistics2[BinVar, BinVar](n1, n2) {
      factor =>
      def score(s1:BinVar#Value, s2:BinVar#Value): Double = if (s1 == s2) scoreEqual else scoreUnequal
      override def equalityPrerequisite = this
    }

  // short for exponential
  private def e(num: Double) = math.exp(num)

  val eps = 1e-5

  override protected def setUp() {
    super.setUp()
    // initialize binary variables with two values
    new BinVar(0)
    new BinVar(1)
  }

  def testV2F1() = {
    implicit val random = new scala.util.Random(0)
    val samples = 10000
    val v1 = new BinVar(0)
    val v2 = new BinVar(0)
    val model = new ItemizedModel(newFactor2(v1, v2, 5, 1))
    val sampler = new VariablesSettingsSampler[BinVar](model)

    val origScore = model.currentScore(Seq(v1, v2))
    logger.debug("orig score: " + origScore)
    val assignCounts = Array.fill(numLabels, numLabels)(0)
    for (i <- 0 until samples) {
      sampler.process(Seq(v1, v2))
      assignCounts(v1.intValue)(v2.intValue) += 1
    }
    val totalCount = assignCounts.toSeq.foldLeft(0.0)((s, arr) => arr.toSeq.foldLeft(s)(_ + _))
    var Z = 0.0
    for (p <- sampler.proposals(Seq(v1, v2))) {
      p.diff.redo()
      val modelScore = model.currentScore(Seq(v1, v2))
      Z += e(modelScore)
      p.diff.undo()
    }
    for (p <- sampler.proposals(Seq(v1, v2))) {
      p.diff.redo()
      val modelScore = model.currentScore(Seq(v1, v2))
      val sampleProb = assignCounts(v1.intValue)(v2.intValue) / totalCount
      logger.debug("%d %d : true: %f, prop: %f, trueProb: %f, sample: %f".format(v1.intValue, v2.intValue, modelScore - origScore, p.modelScore, e(modelScore) / Z, sampleProb))
      assertEquals(modelScore - origScore, p.modelScore, eps)
      assertEquals(e(modelScore) / Z, sampleProb, 0.01)
      p.diff.undo()
    }
  }
}
