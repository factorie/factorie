package cc.factorie

import app.chain.ChainModel
import app.nlp.Token
import org.junit.Assert._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import org.junit.Test
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.infer._

/**
 * Test for the factorie-1.0 BP framework (that uses WeightsMap)
 * @author sameer, brian
 * @since Aug 7, 2012
 */


//@RunWith(classOf[JUnitRunner])
class TestBP extends util.FastLogging { //}extends FunSuite with BeforeAndAfter {
  
  import BPTestUtils._

  val eps = 1e-4
  
  @Test def v1f1Test() {
    // one variable, one factor
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 1, 1))
    val fg = BPSummary(Set(v), model)
    assert(fg.bpFactors.size == 1)
    assert(fg.bpVariables.size == 1)
    BP.inferLoopy(fg, 1)
    logger.debug(fg.marginal(v).proportions)
    assertEquals(0.5, fg.marginal(v).proportions(0), eps)
  }
  
  @Test def v1f1UnequalPotentialsSum() {
    // one variable, one factor
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 2, 1))
    val fg = BPSummary(Set(v), model)
    assert(fg.bpFactors.size == 1)
    assert(fg.bpVariables.size == 1)
    BP.inferLoopy(fg, 1)
    //logger.debug(fg.marginal(v).proportions)
    assertEquals(e(2) / (e(2) + e(1)), fg.marginal(v).proportions(0), eps)
  }
  
  @Test def v1f2Test1() {
    //f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    val fg = BPSummary(Set(v), model)
    assert(fg.bpFactors.size == 2)
    assert(fg.bpVariables.size == 1)
    BP.inferLoopy(fg, 1)
    //logger.debug(fg.marginal(v).proportions)
    assertEquals(0.5, fg.marginal(v).proportions(0), eps)
  }
    
  @Test def v1f2Test2() {
  // f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}") {
  // one variable, two factors
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    val fg = BPSummary(Set(v), model)
    assert(fg.bpFactors.size == 2)
    assert(fg.bpVariables.size == 1)
    BP.inferLoopy(fg, 1)
    //logger.debug(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions(0), e(0) / (e(0) + e(2)), eps)
  }
  
  @Test def v1f2MAP1() {
    // f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    val fg = BPSummary(Set(v), BPMaxProductRing, model) 
    BP.inferLoopy(fg, 2)
    //logger.debug(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions.maxIndex, 0)
  }

  @Test def v1f2MAP2() {
    // f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    val fg = BPSummary(Set(v), BPMaxProductRing, model)
    BP.inferLoopy(fg, 1)
    //logger.debug(fg.marginal(v).proportions)
    assertEquals(fg.marginal(v).proportions.maxIndex, 1)
  }

  @Test def v1f2ChainLogZ() {
    // f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}") {
    // one variable, two factors
    val v = new BinVar(0)
    val model = new ItemizedModel(newFactor1(v, 0.5, 1.3), newFactor1(v, -0.3, 4.0))
    val s = BP.inferChainMax(Seq(v), model)
    val s2 = BP.inferChainSum(Seq(v), model)
    // make sure all factors have the same logz
    //val szs = s.bpFactors.to[Vector].map(_.calculateLogZ)
    val s2zs = s2.bpFactors.to[Vector].map(_.calculateLogZ)
    // assert(szs.distinct.length == 1)
    assert(s2zs.distinct.length == 1)
  }

  @Test def v2f1VaryingBoth() {
    logger.debug("V2F1: varying both")
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    val model = newTemplate2(v1, v2, 10, 0)
    val vars: Set[DiscreteVar] = Set(v1, v2)
    
    val f = model.factors(v1).head
    logger.debug("f score unequal: " + f.currentScore)
    v2 := 1
    logger.debug("f score equal: " + f.currentScore)
    
    
    logger.debug(newTemplate2(v1, v2, 10.0, 0.0).neighborDomain2)
    logger.debug(model.asInstanceOf[FamilyWithNeighborDomains].neighborDomains)

    // vary both variables
    val fg = BPSummary(vars, model)
    assert(fg.bpFactors.size == 1)
    assert(fg.bpVariables.size == 2)
    BP.inferLoopy(fg, 5)
    logger.debug("v1 : " + fg.marginal(v1).proportions)
    logger.debug("v2 : " + fg.marginal(v2).proportions)
    assertEquals(0.5, fg.marginal(v1).proportions(0), eps)
    assertEquals(0.5, fg.marginal(v2).proportions(0), eps)

    assertEquals(math.log(2*math.exp(10) + 2*math.exp(0)), fg.logZ, 0.001)

    val fg2 = BP.inferChainSum(Seq(v1, v2), model)
    assertEquals(math.log(2*math.exp(10) + 2*math.exp(0)), fg2.logZ, 0.001)

    val fg3 = BP.inferTreeSum(Seq(v1, v2).toSet, model)
    assertEquals(math.log(2*math.exp(10) + 2*math.exp(0)), fg3.logZ, 0.001)
  }
  
  @Test def v2f2VaryingBoth() {
    logger.debug("V2F1: varying both")
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    val model = new ItemizedModel(newFactor2(v1, v2, 10, 0), newFactor1(v1, 2, 1))
    val vars: Set[DiscreteVar] = Set(v1, v2)

    val logZ = math.log(
      math.exp(10 + 2) // 0 0
      + math.exp(0 + 2)// 0 1
      + math.exp(0 + 1)// 1 0
      + math.exp(10 + 1)// 1 1
    )

    val fg2 = BP.inferChainSum(Seq(v1, v2), model)
    assertEquals(logZ, fg2.logZ, 0.001)

    val fg3 = BP.inferTreeSum(Seq(v1, v2).toSet, model)
    assertEquals(logZ, fg3.logZ, 0.001)

  }

  @Test def testLoopyLogZ() {
    val random = new scala.util.Random(0)
    object cdomain extends CategoricalVectorDomain[String]()
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
    val trueLogZ = InferByBPChain.infer(Seq(l0, l1, l2, l3), model).logZ
    val loopyLogZ = InferByBPLoopyTreewise.infer(Seq(l0, l1, l2, l3), model).logZ
    assertEquals(trueLogZ, loopyLogZ, 0.01)

    val fastSum = model.inferFast(Seq(l0, l1, l2, l3))
    val sum = InferByBPChain.infer(Seq(l0, l1, l2, l3), model)
    assertEquals(sum.logZ, fastSum.logZ, 0.001)
    for (label <- Seq(l0, l1, l2, l3)) {
      // assertArrayEquals(sum.marginal(label).proportions.toArray, fastSum.marginal(label).asInstanceOf[DiscreteMarginal1[DiscreteVar]].proportions.toArray, 0.001)
    }
    for (factor <- sum.factors.get) {
      // assertArrayEquals(sum.marginal(factor).tensorStatistics.toArray, fastSum.marginal(factor).tensorStatistics.toArray, 0.001)
    }

    val meanFieldSummary = InferByMeanField.apply(Seq(l0, l1, l2, l3), model)
    val BPSummary = InferByBPChain(Seq(l0, l1, l2, l3), model)
    for (v <- meanFieldSummary.variables) {
      val mfm = meanFieldSummary.marginal(v)
      val bpm = BPSummary.marginal(v)
      for (i <- 0 until mfm.proportions.length) {
        assertEquals(mfm.proportions(i), bpm.proportions(i), 0.1)
      }
    }

    // Testing MPLP
    val mplpSummary = MaximizeByMPLP.infer(Seq(l0, l1, l2, l3), model)
    val mapSummary = MaximizeByBPChain.infer(Seq(l0, l1, l2, l3), model)
    for (v <- Seq(l0, l1, l2, l3)) {
      val mfm = mplpSummary.mapAssignment(v)
      val bpm = mapSummary.mapAssignment(v)
      assertEquals(bpm.intValue, mfm.intValue)
    }

    // testing dual decomposition
//    val model0 = DualDecomposition.getBPInferChain(Seq(l0, l1, l2), model)
//    val model1 = DualDecomposition.getBPInferChain(Seq(l2, l3), model)
//    val ddSummary = InferByDualDecomposition.infer(Seq(model0, model1), Seq((0, l2, 1, l2)))
//    for (v <- Seq(l0, l1, l2, l3)) {
//      val mfm = ddSummary.mapAssignment(v)
//      val bpm = mapSummary.mapAssignment(v)
//      assertEquals(bpm.intValue, mfm.intValue)
//    }

    val samplingSummary = InferByGibbsSampling.infer(Seq(l0, l1, l2, l3), model)
    for ((variable, marginal) <- samplingSummary.variableMap) {
      variable.value
      marginal.proportions
    }
  }

  @Test def v2f1VaryingOne() {
    logger.debug("V2F1: varying one")
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    
    // create template between v1 and v2
    val model = newTemplate2(v1, v2, -10, 0)
    val vars: Set[Var] = Set(v1, v2)
    val varying = Set(v1)
    
    val fg = BPSummary(varying, model)
    assert(fg.bpFactors.size == 1)
    assert(fg.bpVariables.size == 1)
    BP.inferLoopy(fg, 5)
    logger.debug("v1 : " + fg.marginal(v1).proportions)

    val v1Marginal = fg.marginal(v1).proportions
    for ((_, i) <- v1.settings.zipWithIndex if v1.value == v2.value)
      assertEquals(v1Marginal(i), 0.0, eps)
    
  }  
  
  @Test def loop2() {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val vars: Set[DiscreteVar] = Set(v1, v2)

    val model = new ItemizedModel(
      // bias
      newFactor1(v1, 1, 0), 
      newFactor1(v2, 1, 0),
      // loop
      newFactor2(v1, v2, 1, 0),
      newFactor2(v1, v2, 3, -1)
    )
    
    var fg = BPSummary(vars, model)
    BP.inferLoopy(fg, 1)
    logger.debug("v1 : " + fg.marginal(v1).proportions)
    logger.debug("v2 : " + fg.marginal(v2).proportions)
    
    fg.setToMaximize(null)
    
    logger.debug("v1 val : " + v1.value)
    logger.debug("v2 val : " + v2.value)
    assert(v1.intValue == 0)
    assert(v2.intValue == 0)
  }

  @Test def loop4() {
    logger.debug("Loop4")
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val v3 = new BinVar(1)
    val v4 = new BinVar(0)
    val vars: Set[DiscreteVar] = Set(v1, v2, v3, v4)

    val model = new ItemizedModel(
      // loop of repulsion factors, with v4 having an extra factor
      // pegging its value to 0
      newFactor2(v1, v2, -5, 0), 
      newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, -5, 0), 
      newFactor2(v4, v1, -5, 0),
      // bias
      newFactor1(v4, 10, 0)
    )
    
    val fg = BPSummary(vars, model)
    BP.inferLoopy(fg, 4)
    fg.setToMaximize()
    
    assertEquals(fg.marginal(v1).proportions(0), 0.0, 0.1)
    assertEquals(fg.marginal(v1).proportions(1), 1.0, 0.1)
    assertEquals(fg.marginal(v2).proportions(0), 1.0, 0.1)
    assertEquals(fg.marginal(v2).proportions(1), 0.0, 0.1)
    assertEquals(fg.marginal(v3).proportions(0), 0.0, 0.1)
    assertEquals(fg.marginal(v3).proportions(1), 1.0, 0.1)
    assertEquals(fg.marginal(v4).proportions(0), 1.0, 0.1)
    assertEquals(fg.marginal(v4).proportions(1), 0.0, 0.1)

    assertEquals(v1.intValue, 1)
    assertEquals(v2.intValue, 0)
    assertEquals(v3.intValue, 1)
    assertEquals(v4.intValue, 0)
  }

  @Test def chainRandom() {
    logger.debug("ChainRandom")
    val numVars = 2
    val vars: Seq[BinVar] = (0 until numVars).map(new BinVar(_)).toSeq
    val varSet = vars.toSet[DiscreteVar]
    for (seed <- 0 until 50) {
      val random = new Random(seed * 1024)
      val model = new ItemizedModel
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
        val score = model.currentScore(vars.toIterable)
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
      logger.debug("map : " + mapAssignment)
      logger.debug("marginals : " + marginals.map(_ / Z).mkString(", "))
      
      // test sum-product
      val fg = BP.inferChainSum(vars, model)
      for (i <- 0 until numVars) {
        logger.debug("v" + i + " : " + fg.marginal(vars(i)).proportions)
        assertEquals(marginals(i) / Z, fg.marginal(vars(i)).proportions(0), eps)
      }

      assertEquals(fg.bpFactors.head.calculateLogZ, fg.bpFactors.last.calculateLogZ, 0.1)

      // TODO: add back logZ assertion
      //logger.debug("z : " + math.log(Z) + ", " + fg.logZ())
      //assertEquals(math.log(Z), fg.logZ(), eps)
      // max product
      
      val mfg = BP.inferChainMax(vars, model)
      val mfg2 = BP.inferTreeMarginalMax(vars, model)
      assertEquals(mfg.logZ, mfg2.logZ, 0.001)
      for (v <- vars) {
        assertEquals(mfg.mapAssignment(v).intValue, mfg2.mapAssignment(v).intValue)
      }
      mfg.setToMaximize(null)
      logger.debug("probabilities : " + scores.map(math.exp(_) / Z).mkString(", "))
      for (i <- 0 until numVars) {
        // logger.debug("v" + i + " : " + mfg.marginal(vars(i)).proportions)
        logger.debug("tv" + i + " : " + (mapAssignment / math.pow(2, i)).toInt % 2)
        assertEquals(vars(i).value.intValue, (mapAssignment / math.pow(2, i)).toInt % 2)
      }
    }
  }
  
  @Test def tree3() {
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val vars: Set[DiscreteVar] = Set(v1, v2, v3)
    // v1 -- v3 -- v2
    val model = new ItemizedModel(
	    newFactor1(v1, 3, 0),
	    newFactor1(v2, 0, 3),
	    newFactor2(v1, v3, 3, 0),
	    newFactor2(v3, v2, 3, 0)
	  )
    
    val fg = BP.inferTreeSum(vars, model, root = v3)
    fg.setToMaximize()
    
    logger.debug("v1 : " + fg.marginal(v1).proportions)
    logger.debug("v2 : " + fg.marginal(v2).proportions)
    logger.debug("v3 : " + fg.marginal(v3).proportions)
    logger.debug("v1 val : " + v1.value)
    logger.debug("v2 val : " + v2.value)
    logger.debug("v3 val : " + v3.value)
    
    assertEquals(0.5, fg.marginal(v3).proportions(0), eps)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)

    var z = 0.0
    for (i <- Seq(0, 1); j <- Seq(0, 1); k <- Seq(0, 1)) {
      v1.set(i)(null)
      v2.set(j)(null)
      v3.set(k)(null)
      z += math.exp(model.currentScore(Seq(v1, v2, v3)))
    }

    assertEquals(fg.logZ, math.log(z), 0.001)

    val fg2 = BP.inferChainSum(vars.toSeq, model)
    assertEquals(fg2.logZ, math.log(z), 0.001)

    val vars2 = Seq(v1, v3)
    var z2 = 0.0
    for (i <- Seq(0, 1); j <- Seq(0, 1)) {
      v1.set(i)(null)
      v3.set(j)(null)
      z2 += math.exp(model.currentScore(vars2))
    }

    assertEquals(math.log(z2), BP.inferChainSum(vars2, model).logZ, 0.001)
    assertEquals(math.log(z2), BP.inferTreeSum(vars2.toSet, model).logZ, 0.001)
  }
  
  @Test def tree7() {
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
    val model = new ItemizedModel(
      newFactor1(v1, 10, 0), //newFactor1(v7, 0, 3),
      newFactor2(v1, v3, 5, 0), newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, 5, 0), newFactor2(v5, v4, -5, 0),
      newFactor2(v6, v5, 5, 0), newFactor2(v7, v5, -5, 0)
    )
    val fg = BP.inferTreeSum(vars, model, v4)
    fg.setToMaximize()
    
    assert(fg.marginal(v7).proportions(0) > 0.95)
    
    logger.debug("v1 : " + fg.marginal(v1).proportions)
    logger.debug("v2 : " + fg.marginal(v2).proportions)
    logger.debug("v3 : " + fg.marginal(v3).proportions)
    logger.debug("v4 : " + fg.marginal(v4).proportions)
    logger.debug("v5 : " + fg.marginal(v5).proportions)
    logger.debug("v6 : " + fg.marginal(v6).proportions)
    logger.debug("v7 : " + fg.marginal(v7).proportions)
    logger.debug("      %2d".format(v4.intValue))
    logger.debug("  %2d      %2d".format(v3.intValue, v5.intValue))
    logger.debug("%2d  %2d  %2d  %2d".format(v1.intValue, v2.intValue, v6.intValue, v7.intValue))
    
    assert(v1.intValue == 0)
    assert(v2.intValue == 1)
    assert(v3.intValue == 0)
    assert(v4.intValue == 0)
    assert(v5.intValue == 1)
    assert(v6.intValue == 1)
    assert(v7.intValue == 0)
  }
  
}

object BPTestUtils {
  // a binary variable that takes values 0 or 1
  object BinDomain extends DiscreteDomain(2)

  class BinVar(i: Int) extends DiscreteVariable(i) {
    def domain = BinDomain
  }
  

  def newFactor1(n1: BinVar, score0: Double, score1: Double): Factor = {
    val family = new DotTemplateWithStatistics1[BinVar] with Parameters {
      val weights = Weights(new la.DenseTensor1(BinDomain.size))
    }
    assert(family.weights.value ne null)
    family.weights.value(0) = score0
    family.weights.value(1) = score1
    n1.set(0)(null)
    n1.set(1)(null)
    family.factors(n1).head
  }

  def newFactor2(n1: BinVar, n2: BinVar, scoreEqual: Double, scoreUnequal: Double): Factor = {
    val family = new DotTemplate2[BinVar, BinVar] with Parameters {
      override def neighborDomain1 = BinDomain
      override def neighborDomain2 = BinDomain
      val weights = Weights(new la.DenseTensor1(BinDomain.size))
      def unroll1(v: BinVar) = if (v == n1) Factor(n1, n2) else Nil
      def unroll2(v: BinVar) = if (v == n2) Factor(n1, n2) else Nil
      override def statistics(value1: BinVar#Value, value2: BinVar#Value) = 
        BinDomain(if (value1.intValue == value2.intValue) 0 else 1)
    }
    assert(!family.statisticsAreValues)
    family.weights.value(0) = scoreEqual
    family.weights.value(1) = scoreUnequal
    family.factors(n1).head
  }
  
  def newTemplate2(n1: BinVar, n2: BinVar, scoreEqual: Double, scoreUnequal: Double) = {
    new TupleTemplateWithStatistics2[BinVar, BinVar] {
      override def neighborDomain1 = BinDomain
      override def neighborDomain2 = BinDomain
      def unroll1(v: BPTestUtils.this.type#BinVar) = if (v == n1) Factor(n1, n2) else Nil
      def unroll2(v: BPTestUtils.this.type#BinVar) = if (v == n2) Factor(n1, n2) else Nil
      def score(v1:BinVar#Value, v2:BinVar#Value): Double = if (v1 == v2) scoreEqual else scoreUnequal
    }
  }

  import scala.language.existentials
  def newFactor3(n1: BinVar, n2: BinVar, n3: BinVar, scores: Seq[Double]) =
    new TupleFactorWithStatistics3[BinVar, BinVar, BinVar](n1, n2, n3) {
      factor =>
      def score(v1:BinVar#Value, v2:BinVar#Value, v3:BinVar#Value): Double = scores(v1.intValue * 4 + v2.intValue * 2 + v3.intValue)
      override def equalityPrerequisite = this
      override def toString = "F(%s,%s,%s)".format(n1, n2, n3)
    }

  // short for exponential
  def e(num: Double) = math.exp(num)
  
}
