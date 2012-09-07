package cc.factorie

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack
import org.junit.Assert.assertEquals
import scala.util.Random

/**
 * Test for the factorie-1.0 BP framework (that uses Tensors)
 * @author sameer, brian
 * @since Aug 7, 2012
 */


@RunWith(classOf[JUnitRunner])
class TestBP extends FunSuite with BeforeAndAfter {
  
  import BPTestUtils._

  val eps = 1e-4
  
  before {
    new BinVar(0)
    new BinVar(1)
  }
  
  after {
    
  }
  
  test("V1F1: equal potentials (sum-product)") {
    // one variable, one factor
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 1, 1))
    val fg = new BPSummary(Set(v), model)
    assert(fg.bpFactors.size === 1)
    assert(fg.bpVariables.size === 1)
    BP.inferLoopy(fg, 1)
    println(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), 0.5, eps)
  }
  
  test("V1F1: unequal potentials (sum-product)") {
    // one variable, one factor
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 2, 1))
    val fg = new BPSummary(Set(v), model)
    assert(fg.bpFactors.size === 1)
    assert(fg.bpVariables.size === 1)
    BP.inferLoopy(fg, 1)
    //println(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), e(2) / (e(2) + e(1)), eps)
  }
  
  test("V1F2: f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    val fg = new BPSummary(Set(v), model)
    assert(fg.bpFactors.size === 2)
    assert(fg.bpVariables.size === 1)
    BP.inferLoopy(fg, 1)
    //println(fg.marginal(v).proportions)
    assertEquals(0.5, fg.marginal(v).proportions(0), eps)
  }
    
  test("V1F2: f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    val fg = new BPSummary(Set(v), model)
    assert(fg.bpFactors.size === 2)
    assert(fg.bpVariables.size === 1)
    BP.inferLoopy(fg, 1)
    //println(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), e(0) / (e(0) + e(2)), eps)
  }
  
  test("V1F2MAP: f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    val fg = new BPSummary(Set(v), BPMaxProductRing, model) 
    BP.inferLoopy(fg, 2)
    //println(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), e(3) / (e(3) + e(3)), eps)
  }

  test("V1F2MAP: f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new FactorModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    val fg = new BPSummary(Set(v), BPMaxProductRing, model)
    BP.inferLoopy(fg, 1)
    //println(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), e(0 + 0) / (e(0) + e(2)), eps)
  }
  
  test("V2F1: varying both") {
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    val model = new FactorModel(newFactor2(v1, v2, 10, 0))
    val vars: Set[DiscreteVar] = Set(v1, v2)

    // vary both variables
    val fg = new BPSummary(vars, model)
    assert(fg.bpFactors.size === 1)
    assert(fg.bpVariables.size === 2)
    BP.inferLoopy(fg, 5)
    println("v1 : " + fg.marginal(v1).proportions)
    println("v2 : " + fg.marginal(v2).proportions)
    for (mfactor <- fg.bpFactors) {
      println(mfactor.proportions)
    }
    assertEquals(0.5, fg.marginal(v1).proportions(0), eps)
    assertEquals(0.5, fg.marginal(v2).proportions(0), eps)
  }
  
  test("V2F1: varying one") {
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    
    // create template between v1 and v2
    val model = new FactorModel(newFactor2(v1, v2, 10, 0))
    val vars: Set[Variable] = Set(v1, v2)
    val varying= Set(v1)
    
    val fg = new BPSummary(varying, model)
    assert(fg.bpFactors.size === 1)
    assert(fg.bpVariables.size === 1)
    BP.inferLoopy(fg, 5)
    println("v1 : " + fg.marginal(v1).proportions)
    for (mfactor <- fg.bpFactors) {
      println(mfactor.proportions)
    }
    
    val v1Marginal = fg.marginal(v1).proportions
    for ((_, i) <- v1.settings.zipWithIndex if v1.value == v2.value)
      assertEquals(v1Marginal(i), 0.0, eps)
    
  }  
  
  test("Loop2") {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val vars: Set[DiscreteVariable] = Set(v1, v2)

    val model = new FactorModel(
      // bias
      newFactor1(v1, 1, 0), 
      newFactor1(v2, 1, 0),
      // loop
      newFactor2(v1, v2, 1, 0),
      newFactor2(v1, v2, 3, -1)
    )
    
    var fg = new BPSummary(vars, model)
    BP.inferLoopy(fg, 1)
    println("v1 : " + fg.marginal(v1).proportions)
    println("v2 : " + fg.marginal(v2).proportions)
    
    fg.setToMaximize(null)
    
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    assert(v1.intValue === 0)
    assert(v2.intValue === 0)
  }

  test("Loop4") {
    println("Loop4")
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val v3 = new BinVar(1)
    val v4 = new BinVar(0)
    val vars: Set[DiscreteVariable] = Set(v1, v2, v3, v4)

    val model = new FactorModel(
      // loop of repulsion factors
      newFactor2(v1, v2, -5, 0), 
      newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, -5, 0), 
      newFactor2(v4, v1, -5, 0),
      // bias
      newFactor1(v4, 10, 0)
    )
    
    val fg = new BPSummary(vars, model)
    BP.inferLoopy(fg)
    fg.setToMaximize()
    
    println("v1 : " + fg.marginal(v1).proportions)
    println("v2 : " + fg.marginal(v2).proportions)
    println("v3 : " + fg.marginal(v3).proportions)
    println("v4 : " + fg.marginal(v4).proportions)
    
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    println("v3 val : " + v3.value)
    println("v4 val : " + v4.value)
    
    assert(v1.intValue === 0)
    assert(v2.intValue === 1)
    assert(v3.intValue === 1)
    assert(v4.intValue === 0)
  }

  test("ChainRandom") {
    println("ChainRandom")
    val numVars = 2
    val vars: Seq[BinVar] = (0 until numVars).map(new BinVar(_)).toSeq
    val varSet = vars.toSet[DiscreteVariable]
    for (seed <- (0 until 50)) {
      val random = new Random(seed * 1024)
      val model = new FactorModel
      for (i <- 0 until numVars) {
        model += newFactor1(vars(i), 0, random.nextDouble() * 4.0 - 2.0)
        if ((i + 1) != numVars) model += newFactor2(vars(i), vars(i + 1), 0, random.nextDouble() * 6.0 - 3.0)
      }
      // true marginals and the map
      val marginals: Array[Double] = Array.fill(numVars)(0.0)

      // go through all the configurations
      var Z = 0.0
      val scores = new ArrayBuffer[Double]
      var maxScore = Double.NegativeInfinity
      var mapAssignment: Int = -1
      for (bs <- 0 until math.pow(2, numVars).toInt) {
        for (i <- 0 until numVars) {
          vars(i).set((bs / math.pow(2, i)).toInt % 2)(null)
        }
        val score = model.score(vars.toIterable)
        scores += score
        Z += math.exp(score)
        for (i <- 0 until numVars) {
          if (vars(i).intValue == 0) {
            marginals(i) += math.exp(score)
          }
        }
        if (score > maxScore) {
          maxScore = score
          mapAssignment = bs
        }
      }
      println("map : " + mapAssignment)
      println("marginals : " + marginals.mkString(", "))
      println("marginals / Z: " + marginals.map(_ / Z).mkString(", "))
      // TODO: add this assertion back and check above code -brian
      // assertEquals(1.0, marginals.map(_ / Z).sum, eps)
      
      // test sum-product
      val fg = BP.inferChainSum(vars, model)
      for (i <- 0 until numVars) {
        println("v" + i + " : " + fg.marginal(vars(i)).proportions)
        assertEquals(marginals(i) / Z, fg.marginal(vars(i)).proportions(0), eps)
      }
      
      // TODO: add back logZ assertion
      //println("z : " + math.log(Z) + ", " + fg.logZ())
      //assertEquals(math.log(Z), fg.logZ(), eps)
      // max product
      
      val mfg = BP.inferChainMax(vars, model)
      println("probabilities : " + scores.map(math.exp(_) / Z).mkString(", "))
      for (i <- 0 until numVars) {
        println("v" + i + " : " + mfg.marginal(vars(i)).proportions)
        println("tv" + i + " : " + (mapAssignment / math.pow(2, i)).toInt % 2)
        assertEquals(vars(i).value.intValue, (mapAssignment / math.pow(2, i)).toInt % 2)
      }
    }
  }
  
  test("Tree3") {
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val vars: Set[DiscreteVar] = Set(v1, v2, v3)
    // v1 -- v3 -- v2
    val model = new FactorModel(
	    newFactor1(v1, 3, 0),
	    newFactor1(v2, 0, 3),
	    newFactor2(v1, v3, 3, 0),
	    newFactor2(v2, v3, 3, 0)
	  )
    
    val fg = BP.inferTreewiseSum(vars, model, root = v3)
    fg.setToMaximize()
    
    println("v1 : " + fg.marginal(v1).proportions)
    println("v2 : " + fg.marginal(v2).proportions)
    println("v3 : " + fg.marginal(v3).proportions)
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    println("v3 val : " + v3.value)
    
    assertEquals(0.5, fg.marginal(v3).proportions(0), eps)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
  }
  
  test("Tree7") {
    val v1 = new BinVar(0) { override def toString = "v1" }
    val v2 = new BinVar(1) { override def toString = "v2" }
    val v3 = new BinVar(0) { override def toString = "v3" }
    val v4 = new BinVar(0) { override def toString = "v4" }
    val v5 = new BinVar(0) { override def toString = "v5" }
    val v6 = new BinVar(0) { override def toString = "v6" }
    val v7 = new BinVar(0) { override def toString = "v7" }
    val vars: Set[DiscreteVar] = Set(v1, v2, v3, v4, v5, v6, v7)
    //        v4
    //    v3      v5
    //  v1  v2  v6  v7
    val model = new FactorModel(
      newFactor1(v1, 10, 0), //newFactor1(v7, 0, 3),
      newFactor2(v1, v3, 5, 0), newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, 5, 0), newFactor2(v5, v4, -5, 0),
      newFactor2(v6, v5, 5, 0), newFactor2(v7, v5, -5, 0)
    )
    val fg = BP.inferTreewiseSum(vars, model, v4)
    fg.setToMaximize()
    
    assert(fg.marginal(v7).proportions(0) > 0.95)
    
    println("v1 : " + fg.marginal(v1).proportions)
    println("v2 : " + fg.marginal(v2).proportions)
    println("v3 : " + fg.marginal(v3).proportions)
    println("v4 : " + fg.marginal(v4).proportions)
    println("v5 : " + fg.marginal(v5).proportions)
    println("v6 : " + fg.marginal(v6).proportions)
    println("v7 : " + fg.marginal(v7).proportions)
    println("      %2d".format(v4.intValue))
    println("  %2d      %2d".format(v3.intValue, v5.intValue))
    println("%2d  %2d  %2d  %2d".format(v1.intValue, v2.intValue, v6.intValue, v7.intValue))
    
    assert(v1.intValue === 0)
    assert(v2.intValue === 1)
    assert(v3.intValue === 0)
    assert(v4.intValue === 0)
    assert(v5.intValue === 1)
    assert(v6.intValue === 1)
    assert(v7.intValue === 0)
  }
  
}

object BPTestUtils {
  // a binary variable that takes values 0 or 1
  object BinDomain extends CategoricalDomain[Int](List(0, 1))

  class BinVar(i: Int) extends LabelVariable(i) {
    def domain = BinDomain
  }

  def newFactor1(n1: BinVar, score0: Double, score1: Double): Factor = {
    val family = new TemplateWithDotStatistics1[BinVar] {
      override def statisticsDomains = Tuple1(BinDomain)
    }
    family.weights(0) = score0
    family.weights(1) = score1
    n1.set(0)(null)
    println(family.score(n1))
    n1.set(1)(null)
    println(family.score(n1))
    family.factors(n1).head
  }

  def newFactor2(n1: BinVar, n2: BinVar, scoreEqual: Double, scoreUnequal: Double): Factor = {
    val family = new Template2[BinVar, BinVar] with DotStatistics1[BooleanValue] {
      override def statisticsDomains = Tuple1(BooleanDomain)
      def unroll1(v: BPTestUtils.this.type#BinVar) = if (v == n1) Factor(n1, n2) else Nil
      def unroll2(v: BPTestUtils.this.type#BinVar) = if (v == n2) Factor(n1, n2) else Nil
      def statistics(values: this.type#ValuesType) = Stat(BooleanDomain.value(values._1 == values._2))
    }
    family.weights(0) = scoreEqual
    family.weights(1) = scoreUnequal
    family.factors(n1).head
  }

  def newFactor3(n1: BinVar, n2: BinVar, n3: BinVar, scores: Seq[Double]) =
    new Factor3[BinVar, BinVar, BinVar] {
      factor =>
        
      def _1 = n1
      def _2 = n2
      def _3 = n3

      type StatisticsType = Stat

      final case class Stat(_1: BinVar#Value, _2: BinVar#Value, _3: BinVar#Value) extends Statistics {
        lazy val score: Double = factor.score(this)
      }

      def statistics(v: this.type#Values) = Stat(v._1, v._2, v._3)

      def score(s: Stat): Double = scores(s._1.category * 4 + s._2.category * 2 + s._3.category)

      override def equalityPrerequisite = this

      override def toString = "F(%s,%s,%s)".format(n1, n2, n3)
    }

  // short for exponential
  def e(num: Double) = math.exp(num)
  
}
