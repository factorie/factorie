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

/**
 * @author sameer
 * @since Sep 5, 2011
 */

class TestBP extends TestCase {

  // a binary variable that takes values 0 or 1
  object BinDomain extends CategoricalDomain[Int](List(0, 1))

  class BinVar(i: Int) extends LabelVariable(i) with VarInSeq[BinVar] {
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

  val eps = 0.001

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
    fg = new FG(Set(v))
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
    fg = new FG(Set(v))
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
    fg = new FG(Set(v))
    fg.createUnrolled(model1)
    println("num Factors = %d".format(fg.factors.size))
    println("num Variables = %d".format(fg.nodes.size))
    fg.inferLoopyBP(2)
    println(fg.node(v).marginal)
    assertEquals(fg.node(v).marginal.probability(BinDomain(0)), 0.5, eps)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    val model2 = new FactorModel(newFactor1(v, 0, 1), newFactor1(v, 0, 1))
    fg = new FG(Set(v))
    fg.createUnrolled(model2)
    fg.inferLoopyBP(1)
    println(fg.node(v).marginal)
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
    fg = new FG(vars)
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
    fg = new FG(Set(v2))
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

  // Same as above, just use a much larger potential
  def testV2F1Hard = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)

    // create template between v1 and v2
    var fg: FG = null
    val model = new FactorModel(newFactor2(v1, v2, 1000, 0))
    val vars: Set[Variable] = Set(v1, v2)

    // vary both variables
    fg = new FG(vars)
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
    fg = new FG(Set(v2))
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

  /*
  def testTwoChain = {
    // two variables, three factors: two unary, one binary
    var vseq = new BinVarSeq

    val v1 = new BinVar(1)
    vseq += v1

    val v2 = new BinVar(1)
    vseq += v2

    var lattice: BPLattice[BinVar] = null
    val model = new TemplateModel(newTemplate1(1, 0), newTemplate2(9, 0))
    lattice = new BPLattice(Array(v1, v2), model)
    lattice.updateTreewise()
    // print factor marginal
    model.factors(Seq(v2)).foreach {
      f =>
        if (f.variables.size == 1) {
          val marginals = lattice.marginalMap(f)
          assertEquals(marginals(List(0)), (1 + e(10)) / (2 + e(8) + e(10)), eps)
          assertEquals(marginals(List(1)), (1 + e(8)) / (2 + e(8) + e(10)), eps)
        } else {
          val marginals = lattice.marginalMap(f)
          assertEquals(marginals(List(0, 0)), e(10) / (2 + e(8) + e(10)), eps)
          assertEquals(marginals(List(0, 1)), 1.0 / (2 + e(8) + e(10)), eps)
          assertEquals(marginals(List(1, 0)), 1.0 / (2 + e(8) + e(10)), eps)
          assertEquals(marginals(List(1, 1)), e(8) / (2 + e(8) + e(10)), eps)
        }
    }
    assertEquals(lattice.marginal(v1).get(0), (1 + e(10)) / (2 + e(8) + e(10)), eps)
    assertEquals(lattice.sumLogZ, 11.127, eps)
    // check that max configuration is (v1=0, v2=0)
    lattice = new BPLattice(Array(v1, v2), model)
    lattice.updateTreewiseMax()
    lattice.setVariablesToMax(Array(v1, v2))
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 0)
  }*/

  def testLoop2 = {
    val v1 = new BinVar(1)
    val v2 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2)

    val model = new FactorModel(
      newFactor1(v1, 1, 0), newFactor1(v2, 1, 0),
      newFactor2(v1, v2, 1, 0), newFactor2(v1, v2, 3, -1))
    var fg = new FG(model, vars)
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
    var fg = new FG(model, vars)
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

  def testTree3 = {
    val v1 = new BinVar(0)
    val v2 = new BinVar(1)
    val v3 = new BinVar(0)
    val vars: Set[Variable] = Set(v1, v2, v3)
    // v1 -- v3 -- v2
    val model = new FactorModel(
      newFactor1(v1, 3, 0), newFactor1(v2, 0, 3),
      newFactor2(v1, v3, 3, 0), newFactor2(v2, v3, 3, 0))
    var fg = new FG(model, vars)
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
    var fg = new FG(model, vars)
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
}
