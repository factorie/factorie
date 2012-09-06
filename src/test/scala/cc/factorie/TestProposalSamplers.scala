package cc.factorie

/**
 * @author sameer
 * @date 2/7/12
 */

import cc.factorie._
import junit.framework._
import Assert._
import scala.util.Random
import collection.mutable.ArrayBuffer

/**
 * @author sameer
 * @since Sep 5, 2011
 */

class TestProposalSamplers extends TestCase {

  val numLabels: Int = 3

  // a binary variable that takes values 0 or 1
  object LabelDomain extends CategoricalDomain[Int](0 until numLabels)

  class BinVar(i: Int) extends LabelVariable(i) {
    def domain = LabelDomain
  }

  private def newFactor1(n1: BinVar, score0: Double, score1: Double) =
    new Factor1[BinVar] {
      factor =>
      def _1 = n1

      type StatisticsType = Stat

      final case class Stat(_1: BinVar#Value) extends Statistics {
        lazy val score: Double = factor.score(this)
      }

      def statistics(v1:BinVar#Value) = Stat(v1)

      def score(s: Stat): Double = if (s._1 == LabelDomain(0)) {
        score0
      } else {
        score1
      }

      override def equalityPrerequisite = this
    }

  private def newFactor2(n1: BinVar, n2: BinVar, scoreEqual: Double, scoreUnequal: Double) =
    new Factor2[BinVar, BinVar] {
      factor =>
      def _1 = n1

      def _2 = n2

      type StatisticsType = Stat

      final case class Stat(_1: BinVar#Value, _2: BinVar#Value) extends Statistics {
        lazy val score: Double = factor.score(this)
      }

      def statistics(v1:BinVar#Value, v2:BinVar#Value) = Stat(v1, v2)

      def score(s: Stat): Double = if (s._1 == s._2) scoreEqual else scoreUnequal

      override def equalityPrerequisite = this
    }

  // short for exponential
  private def e(num: Double) = math.exp(num)

  val eps = 1e-5

  override protected def setUp() {
    super.setUp
    // initialize binary variables with two values
    new BinVar(0)
    new BinVar(1)
  }

  def testV2F1 = {
    val samples = 10000
    val v1 = new BinVar(0)
    val v2 = new BinVar(0)
    val model = new FactorModel(newFactor2(v1, v2, 5, 1))
    val sampler = new VariablesSettingsSampler[BinVar](model)

    val origScore = model.score(Seq(v1, v2))
    println("orig score: " + origScore)
    val assignCounts = Array.fill(numLabels, numLabels)(0)
    for (i <- 0 until samples) {
      sampler.process(Seq(v1, v2))
      assignCounts(v1.intValue)(v2.intValue) += 1
    }
    val totalCount = assignCounts.toSeq.foldLeft(0.0)((s, arr) => arr.toSeq.foldLeft(s)(_ + _))
    var Z = 0.0
    for (p <- sampler.proposals(Seq(v1, v2))) {
      p.diff.redo
      val modelScore = model.score(Seq(v1, v2))
      Z += e(modelScore)
      p.diff.undo
    }
    for (p <- sampler.proposals(Seq(v1, v2))) {
      p.diff.redo
      val modelScore = model.score(Seq(v1, v2))
      val sampleProb = assignCounts(v1.intValue)(v2.intValue) / totalCount
      println("%d %d : true: %f, prop: %f, trueProb: %f, sample: %f".format(v1.intValue, v2.intValue, modelScore - origScore, p.modelScore, e(modelScore) / Z, sampleProb))
      assertEquals(modelScore - origScore, p.modelScore, eps)
      assertEquals(e(modelScore) / Z, sampleProb, 0.01)
      p.diff.undo
    }
  }
}
