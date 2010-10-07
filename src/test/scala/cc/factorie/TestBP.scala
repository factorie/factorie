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

package cc.factorie

import cc.factorie._
import junit.framework._
import Assert._

/**
 * JUnit test party! Tests BP for various factor graphs.
 *
 * @author kedarb
 * @since Sep 30, 2010
 */

class TestBP extends TestCase {
  // a binary variable that takes values 0 or 1
  class BinVar(i: Int) extends LabelVariable(i) with VarInSeq[BinVar]
  // a sequence of binary variables
  class BinVarSeq extends VariableSeq[BinVar]

  // a factor template that scores a single variable's value (if v==0 then score0 else score1)
  private def newTemplate1(score0: Double, score1: Double) =
    new InitializedTemplate(new TemplateWithVectorStatistics1[BinVar] {
      def score(s: Stat) = {
        if (s._1.intValue == 0) score0 else score1
      }
    })

  // a factor template that scores successive variables (if v1==v2 then scoreEqual else scoreUnequal)
  private def newTemplate2(scoreEqual: Double, scoreUnequal: Double) =
    new InitializedTemplate(new TemplateWithVectorStatistics2[BinVar, BinVar] {
      def unroll1(v1: BinVar) = if (v1.hasNext) Factor(v1, v1.next) else Nil

      def unroll2(v2: BinVar) = if (v2.hasPrev) Factor(v2.prev, v2) else Nil

      def score(s: Stat) = if (s._1.intValue == s._2.intValue) scoreEqual else scoreUnequal
    })

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
    var lattice: BPLattice[BinVar] = null
    // 1) equal potentials
    //    a) sum-product
    val model1 = new Model(newTemplate1(1, 1))
    val varying = Array(v)
    lattice = new BPLattice[BinVar](varying, model1)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v).get(0), 0.5, eps)
    //    b) max-product
    lattice = new BPLattice[BinVar](varying, model1)
    lattice.updateTreewiseMax()
    assertEquals(lattice.marginal(v).get(0), 0.5, eps)
    // 2) unequal potentials
    //    a) sum-product
    val model2 = new Model(newTemplate1(2, 1))
    lattice = new BPLattice(varying, model2)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v).get(0), e(1) / (1 + e(1)), eps)
    //    b) max-product
    lattice = new BPLattice[BinVar](varying, model2)
    lattice.updateTreewiseMax()
    assertEquals(lattice.marginal(v).get(0), e(1) / (1 + e(1)), eps)
  }

  def testV1F2 = {
    // one variable, two factors
    val v = new BinVar(0)
    var lattice: BPLattice[BinVar] = null
    // 1) f1 = {0: 2, 1: 1}, f2 = {0: 1, 1: 2}
    val model1 = new Model(newTemplate1(2, 1), newTemplate1(1, 2))
    val varying = Array(v)
    lattice = new BPLattice[BinVar](varying, model1)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v).get(0), 0.5, eps)
    // 2) f1 = {0: 0, 1: 1}, f2 = {0: 0, 1: 1}
    val model2 = new Model(newTemplate1(0, 1), newTemplate1(0, 1))
    lattice = new BPLattice(varying, model2)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v).get(0), 1.0 / (1 + e(2)), eps)
  }

  def testV2F1 = {
    // a sequence of two variables, one factor
    var vseq = new BinVarSeq

    val v1 = new BinVar(1)
    vseq += v1

    val v2 = new BinVar(0)
    vseq += v2

    var lattice: BPLattice[BinVar] = null
    val model1 = new Model(newTemplate2(1000, 0))
    val varying = Array(v1, v2)
    // create template between v1 and v2
    lattice = new BPLattice(varying, model1)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v1).get(0), 0.5, eps)
    assertEquals(lattice.marginal(v2).get(0), 0.5, eps)
    // vary just one variable
    lattice = new BPLattice(Array(v2), model1)
    lattice.updateTreewise()
    assertEquals(lattice.marginal(v2).get(0), 0.0, eps)
    // get the factor
    val factor = model1.factors(v2).head
    val marginals = lattice.marginalMap(factor)
    assertEquals(marginals(List(0)), 0.0, eps)
    assertEquals(marginals(List(1)), 1.0, eps)
  }

  def testTwoChain = {
    // two variables, three factors: two unary, one binary
    var vseq = new BinVarSeq

    val v1 = new BinVar(1)
    vseq += v1

    val v2 = new BinVar(1)
    vseq += v2

    var lattice: BPLattice[BinVar] = null
    val model = new Model(newTemplate1(1, 0), newTemplate2(9, 0))
    lattice = new BPLattice(Array(v1, v2), model)
    lattice.updateTreewise()
    // print factor marginal
    model.factors(v2).foreach {
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
  }

  def testLoop = {
    // two variables, two factors in a loop
    var vseq = new BinVarSeq

    val v1 = new BinVar(1)
    vseq += v1

    val v2 = new BinVar(0)
    vseq += v2

    val model = new Model(newTemplate1(1, 0), newTemplate2(1, 0), newTemplate2(3, -1))
    var lattice = new BPLattice(Array(v1, v2), model)
    var foundLoop = false
    try {
      lattice.updateTreewise()
    } catch {
      case e: Exception => foundLoop = true
    }
    assertTrue("Undirected model should have a loop!", foundLoop)
    // do simple update for loopy case
    lattice = new BPLattice(Array(v1, v2), model)
    forIndex(10)(i => {
      println("max-product marginal(v1) before iteration=" + i + ": " + lattice.marginal(v1))
      lattice.updateMax
    })
    println("max-product marginal(v1) finally: " + lattice.marginal(v1))
    lattice.setVariablesToMax(Array(v1, v2))
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 0)
    // do sum-product update
    println
    lattice = new BPLattice(Array(v1, v2), model)
    forIndex(5)(i => {
      println("sum-product marginal(v1) before iteration=" + i + ":" + lattice.marginal(v1))
      lattice.update
    })
    println("sum-product marginal(v1) finally: " + lattice.marginal(v1))
    lattice.setVariablesToMax(Array(v1, v2))
    assertEquals(v1.intValue, 0)
    assertEquals(v2.intValue, 0)
  }
}
