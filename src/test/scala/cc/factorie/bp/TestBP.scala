package cc.factorie.bp

/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

import cc.factorie._
import junit.framework._
import Assert._
import scala.util.Random
import collection.mutable.ArrayBuffer

/**
 * @author sameer
 * @since Sep 5, 2011
 */

class TestBP extends TestCase {

  // a binary variable that takes values 0 or 1
  object BinDomain extends CategoricalDomain[Int](List(0, 1))

  class BinVar(i: Int) extends LabelVariable(i) {
    def domain = BinDomain
  }

  private def newFactor1(n1: BinVar, score0: Double, score1: Double) =
    new Factor1[BinVar] {
      factor =>
      def _1 = n1

      type StatisticsType = Stat

      final case class Stat(_1: BinVar#Value) extends Statistics {
        lazy val score: Double = factor.score(this)
      }

      def statistics(v: this.type#Values) = Stat(v._1)

      def score(s: Stat): Double = if (s._1 == BinDomain(0)) {
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

      def statistics(v: this.type#Values) = Stat(v._1, v._2)

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

  def testV1F1 = {
    // one variable, one factor
    val v = new BinVar(0)
    var fg: FG = null
    // 1) equal potentials
    //    a) sum-product
    val model1 = new FactorModel(newFactor1(v, 1, 1))
    fg = new FG(Set(v)) with SumProductFG
    fg.createUnrolled(model1)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg.nodes.size))
    fg.inferLoopyBP(1)
    println(fg.node(v).marginal)
    //println("domain: %s".format(marginal.domain))
    //println("indexEq: %s".format(marginal.domain(0) == 0))
    //println("index: %d".format(marginal.domain.indexOf(marginal.domain(0))))
    //println("score: %f".format(marginal.score(marginal.domain(0))))
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), 0.5, eps)
    // 2) unequal potentials
    //    a) sum-product
    val model2 = new FactorModel(newFactor1(v, 2, 1))
    fg = new FG(Set(v)) with SumProductFG
    fg.createUnrolled(model2)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg.nodes.size))
    fg.inferLoopyBP(1)
    println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), e(1) / (1 + e(1)), eps)
    for (factor <- fg.factors) {
      for (value: Values <- factor.valuesIterator(Set(v))) {
        println(value.index(Set(v)))
      }
    }
  }

  def testV1F2 = {
    // one variable, two factors
    val v = new BinVar(0)
    var fg: FG = null
    // 1) f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}
    val model1 = new FactorModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    fg = new FG(Set(v)) with SumProductFG
    fg.createUnrolled(model1)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg.nodes.size))
    fg.inferLoopyBP(2)
    println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), 0.5, eps)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    val model2 = new FactorModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    fg = new FG(Set(v)) with SumProductFG
    fg.createUnrolled(model2)
    fg.inferLoopyBP(1)
    println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), 1.0 / (1 + e(2)), eps)
  }

  def testV1F2MAP = {
    // one variable, two factors
    // println("Testing MAP BP")
    val v = new BinVar(0)
    var fg: FG = null
    // 1) f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}
    val model1 = new FactorModel(newFactor1(v, 1, 2), newFactor1(v, 2, 1))
    fg = new FG(Set(v)) with MaxProductFG
    fg.createUnrolled(model1)
    // println("num Factors = %d".format(fg.factors.size))
    // println("num Variables = %d".format(fg.nodes.size))
    fg.inferLoopyBP(2)
    // println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.score(BinDomain(0)), 3.0, eps)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    val model2 = new FactorModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    fg = new FG(Set(v)) with MaxProductFG
    fg.createUnrolled(model2)
    fg.inferLoopyBP(1)
    // println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), 1.0 / (1 + e(2)), eps)
  }

  def testV2F1 = {
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    var fg: FG = null
    val model = new FactorModel(newFactor2(v1, v2, 10, 0))
    val vars: Set[Variable] = Set(v1, v2)

    // vary both variables
    fg = new FG(vars) with SumProductFG
    fg.createUnrolled(model)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg._nodes.size))
    fg.inferLoopyBP(1)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    for (mfactor <- fg.mfactors) {
      for (values <- mfactor.factor.valuesIterator(vars)) {
        println(values + " : " + mfactor.marginal(values))
        //if(values(v1)==values(v2))
        //  assertEquals(mfactor.marginal(values), 0.5, eps)
        //else assertEquals(mfactor.marginal(values), 0.0, eps)
      }
    }
    assertEquals(fg.node(v1).marginal.probability(BinDomain(0)), 0.5, eps)
    assertEquals(fg.node(v2).marginal.probability(BinDomain(0)), 0.5, eps)
    // vary just one variable
    fg = new FG(Set(v2)) with SumProductFG
    fg.createUnrolled(model)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg._nodes.size))
    fg.inferLoopyBP(1)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    for (mfactor <- fg.mfactors) {
      for (values <- mfactor.factor.valuesIterator(Set(v2))) {
        println(values + " : " + mfactor.marginal(values))
        if (values(v1) == values(v2))
          assertEquals(1.0 - 5e-5, mfactor.marginal(values), eps)
        else assertEquals(5e-5, mfactor.marginal(values), eps)

      }
    }
    assertEquals(0.0, fg.node(v1).marginal.probability(BinDomain(0)), eps)
    assertEquals(5e-5, fg.node(v2).marginal.probability(BinDomain(0)), eps)
  }

  def testV2F1MAP = {
    // a sequence of two variables, one factor
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    // println("Testing MAP V2F1")
    // create template between v1 and v2
    var fg: FG = null
    val model = new FactorModel(newFactor2(v1, v2, 10, 0))
    val vars: Set[Variable] = Set(v1, v2)

    // vary both variables
    fg = new FG(vars) with MaxProductFG
    fg.createUnrolled(model)
    // println("num Factors = %d".format(fg.factors.size))
    // println("num Variables = %d".format(fg._nodes.size))
    fg.inferLoopyBP(2)
    // println("v1 : " + fg.node(v1).marginal)
    // println("v2 : " + fg.node(v2).marginal)
    for (mfactor <- fg.mfactors) {
      for (values <- mfactor.factor.valuesIterator(vars)) {
        println(values + " : " + mfactor.marginal(values))
      }
    }
    assertEquals(fg.node(v1).marginal.score(BinDomain(0)), 10, eps)
    assertEquals(fg.node(v2).marginal.score(BinDomain(0)), 10, eps)
    assertEquals(fg.node(v1).marginal.score(BinDomain(1)), 10, eps)
    assertEquals(fg.node(v2).marginal.score(BinDomain(1)), 10, eps)
    // vary just one variable
  }


  // Same as V2F1 above, just use a much larger potential
  def testV2F1Hard = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    var fg: FG = null
    val model = new FactorModel(newFactor2(v1, v2, 1000, 0))
    val vars: Set[Variable] = Set(v1, v2)

    // vary both variables
    fg = new FG(vars) with SumProductFG
    fg.createUnrolled(model)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg._nodes.size))
    fg.inferLoopyBP(1)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    for (mfactor <- fg.mfactors) {
      for (values <- mfactor.factor.valuesIterator(vars)) {
        println(values + " : " + mfactor.marginal(values))
        if (values(v1) == values(v2))
          assertEquals(mfactor.marginal(values), 0.5, eps)
        else assertEquals(mfactor.marginal(values), 0.0, eps)

      }
    }
    assertEquals(fg.node(v1).marginal.probability(BinDomain(0)), 0.5, eps)
    assertEquals(fg.node(v2).marginal.probability(BinDomain(0)), 0.5, eps)
    // vary just one variable
    fg = new FG(Set(v2)) with SumProductFG
    fg.createUnrolled(model)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg._nodes.size))
    fg.inferLoopyBP(1)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    for (mfactor <- fg.mfactors) {
      for (values <- mfactor.factor.valuesIterator(Set(v2))) {
        println(values + " : " + mfactor.marginal(values))
        if (values(v1) == values(v2))
          assertEquals(mfactor.marginal(values), 1.0, eps)
        else assertEquals(mfactor.marginal(values), 0.0, eps)

      }
    }
    assertEquals(fg.node(v1).marginal.probability(BinDomain(0)), 0.0, eps)
    assertEquals(fg.node(v2).marginal.probability(BinDomain(0)), 0.0, eps)
  }

  def testTwoChain = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(1)

    val model = new FactorModel(newFactor1(v1, 1, 0), newFactor1(v2, 1, 0), newFactor2(v1, v2, 2, 0))
    val fg = new FG(Set(v1, v2)) with SumProductFG
    fg.createUnrolled(model)
    fg.inferLoopyBP(2)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    println("tv1 : " + ((e(4) + e(1)) / (e(4) + e(1) + e(1) + e(2))))
    println("tv2 : " + ((e(4) + e(1)) / (e(4) + e(1) + e(1) + e(2))))
    fg.setToMaxMarginal(Set(v1, v2))
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 0)
  }

  def testLoop2 = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2)

    val model = new FactorModel(
      newFactor1(v1, 1, 0), newFactor1(v2, 1, 0),
      newFactor2(v1, v2, 1, 0), newFactor2(v1, v2, 3, -1))
    var fg = new FG(model, vars) with SumProductFG
    fg.inferLoopyBP()
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    fg.setToMaxMarginal(Set(v1, v2))
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 0)
  }

  def testLoop4 = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val v3 = new BinVar(1)
    val v4 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3, v4)

    val model = new FactorModel(
      newFactor1(v4, 10, 0),
      newFactor2(v1, v2, -5, 0), newFactor2(v1, v3, -5, 0),
      newFactor2(v2, v4, -5, 0), newFactor2(v3, v4, -5, 0)
    )
    var fg = new FG(model, vars) with SumProductFG
    fg.inferLoopyBP(4)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    println("v3 : " + fg.node(v3).marginal)
    println("v4 : " + fg.node(v4).marginal)
    fg.setToMaxMarginal()
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    println("v3 val : " + v3.value)
    println("v4 val : " + v4.value)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
    assertEquals(v3.intValue, 1)
    assertEquals(v4.intValue, 0)
  }

  def testLoop4MAP = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val v3 = new BinVar(1)
    val v4 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3, v4)

    val model = new FactorModel(
      newFactor1(v4, 10, 0),
      newFactor2(v1, v2, -5, 0), newFactor2(v1, v3, -5, 0),
      newFactor2(v2, v4, -5, 0), newFactor2(v3, v4, -5, 0)
    )
    // println("Testing loopy map")
    val fg = new FG(model, vars) with MaxProductFG
    fg.inferLoopyBP(4)
    // println("v1 : " + fg.node(v1).marginal)
    // println("v2 : " + fg.node(v2).marginal)
    // println("v3 : " + fg.node(v3).marginal)
    // println("v4 : " + fg.node(v4).marginal)
    fg.setToMaxMarginal()
    // println("v1 val : " + v1.value)
    // println("v2 val : " + v2.value)
    // println("v3 val : " + v3.value)
    // println("v4 val : " + v4.value)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
    assertEquals(v3.intValue, 1)
    assertEquals(v4.intValue, 0)
  }

  def testTree3 = {
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3)
    // v1 -- v3 -- v2
    val model = new FactorModel(
      newFactor1(v1, 3, 0), newFactor1(v2, 0, 3),
      newFactor2(v1, v3, 3, 0), newFactor2(v2, v3, 3, 0))
    var fg = new FG(model, vars) with SumProductFG
    fg.inferUpDown(v1, false)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    println("v3 : " + fg.node(v3).marginal)
    fg.setToMaxMarginal()
    println("v1 val : " + v1.value)
    println("v2 val : " + v2.value)
    println("v3 val : " + v3.value)
    assertEquals(fg.node(v3).marginal.probability(BinDomain(0)), 0.5, eps)
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
  }

  def testTree7 = {
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val v4 = new BinVar(0)
    val v5 = new BinVar(0)
    val v6 = new BinVar(0)
    val v7 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3, v4, v5, v6, v7)
    //        v4
    //    v3      v5
    //  v1  v2  v6  v7
    val model = new FactorModel(
      newFactor1(v1, 10, 0), //newFactor1(v7, 0, 3),
      newFactor2(v1, v3, 5, 0), newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, 5, 0), newFactor2(v5, v4, -5, 0),
      newFactor2(v6, v5, 5, 0), newFactor2(v7, v5, -5, 0)
    )
    var fg = new FG(model, vars) with SumProductFG
    fg.inferUpDown(v1, false)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    println("v3 : " + fg.node(v3).marginal)
    println("v4 : " + fg.node(v4).marginal)
    println("v5 : " + fg.node(v5).marginal)
    println("v6 : " + fg.node(v6).marginal)
    println("v7 : " + fg.node(v7).marginal)
    assertTrue(fg.node(v7).marginal.probability(BinDomain(0)) > 0.95)
    fg.setToMaxMarginal()
    println("      %2d".format(v4.intValue))
    println("  %2d      %2d".format(v3.intValue, v5.intValue))
    println("%2d  %2d  %2d  %2d".format(v1.intValue, v2.intValue, v6.intValue, v7.intValue))
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
    assertEquals(v3.intValue, 0)
    assertEquals(v4.intValue, 0)
    assertEquals(v5.intValue, 1)
    assertEquals(v6.intValue, 1)
    assertEquals(v7.intValue, 0)
  }


  def testTree7MAP = {
    println(" -- Testing MAP tree")
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val v4 = new BinVar(0)
    val v5 = new BinVar(0)
    val v6 = new BinVar(0)
    val v7 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3, v4, v5, v6, v7)
    //        v4
    //    v3      v5
    //  v1  v2  v6  v7
    val model = new FactorModel(
      newFactor1(v1, 10, 0), //newFactor1(v7, 0, 3),
      newFactor2(v1, v3, 5, 0), newFactor2(v2, v3, -5, 0),
      newFactor2(v3, v4, 5, 0), newFactor2(v5, v4, -5, 0),
      newFactor2(v6, v5, 5, 0), newFactor2(v7, v5, -5, 0)
    )
    val fg = new FG(model, vars) with MaxProductFG
    fg.inferUpDown(v1, false)
    println("v1 : " + fg.node(v1).marginal)
    println("v2 : " + fg.node(v2).marginal)
    println("v3 : " + fg.node(v3).marginal)
    println("v4 : " + fg.node(v4).marginal)
    println("v5 : " + fg.node(v5).marginal)
    println("v6 : " + fg.node(v6).marginal)
    println("v7 : " + fg.node(v7).marginal)
    assertTrue(fg.node(v7).marginal.score(BinDomain(0)) > 0)
    fg.setToMaxMarginal()
    println("      %2d".format(v4.intValue))
    println("  %2d      %2d".format(v3.intValue, v5.intValue))
    println("%2d  %2d  %2d  %2d".format(v1.intValue, v2.intValue, v6.intValue, v7.intValue))
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 1)
    assertEquals(v3.intValue, 0)
    assertEquals(v4.intValue, 0)
    assertEquals(v5.intValue, 1)
    assertEquals(v6.intValue, 1)
    assertEquals(v7.intValue, 0)
  }

  def testChainRandom = {
    println(" -- Testing Random Inference")
    val numVars = 2
    val vars: Seq[BinVar] = (0 until numVars).map(new BinVar(_)).toSeq
    val varSet = vars.toSet[Variable]
    for (seed <- (3 until 33)) {
      val random = new Random(seed)
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
        //println(bs + " -> " + score)
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
      println("marginals : " + marginals.map(_ / Z).mkString(", "))
      // test sum-product
      val fg = new FG(model, varSet) with SumProductFG
      fg.inferUpDown(vars.sampleUniformly, false)
      for (i <- 0 until numVars) {
        println("v" + i + " : " + fg.node(vars(i)).marginal)
        assertEquals(marginals(i) / Z, fg.node(vars(i)).marginal.probability(BinDomain(0)), eps)
      }
      println("z : " + math.log(Z) + ", " + fg.logZ)
      // max product
      val mfg = new FG(model, varSet) with MaxProductFG
      mfg.inferUpDown(vars.sampleUniformly, false)
      mfg.inferUpDown(vars.sampleUniformly, false)
      //mfg.inferLoopyBP(numVars*2) //UpDown(vars.sampleUniformly, false)
      mfg.setToMaxMarginal()
      println("probabilities : " + scores.map(math.exp(_) / Z).mkString(", "))
      for (i <- 0 until numVars) {
        //println("v" + i + " : " + mfg.node(vars(i)).marginal)
        assertEquals(vars(i).value.intValue, (mapAssignment / math.pow(2, i)).toInt % 2)
      }
    }
  }


}
