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

import cc.factorie.la._
import cc.factorie.optimize._
import scala.collection.mutable.HashMap

class SuffStats extends HashMap[DotFamily, Vector] {
  override def default(template: DotFamily) = {
    template.freezeDomains
    val vector: Vector = template.weights match {
      case w: SparseVector => new SparseVector(w.length)
      case w: DenseVector => new DenseVector(w.length)
      case w: SparseOuter1DenseVector1 => new SparseOuter1DenseVector1(w.length1, w.length2)
      case w: SparseOuter2DenseVector1 => new SparseOuter2DenseVector1(w.length1, w.length2, w.length3)
    }
    this(template) = vector
    vector
  }
  // To help make sure sort order of vectors matches
  def sortedKeys = keys.toSeq.sortWith(_.hashCode > _.hashCode)
}

class LogLinearOptimizable[V <: DiscreteVarWithTarget with NoVariableCoordination](val templates: Seq[DotFamily],
                                                                                   val model: Model,
                                                                                   val variableSets: Seq[Seq[V]],
                                                                                   val gaussianPriorVariance: Double,
                                                                                   val familiesToUpdate: Seq[DotFamily],
                                                                                   val constraints: SuffStats)
          extends OptimizableFamilies(templates) with OptimizableByValueAndGradient {
  // Cached values
  private var oValue = Double.NaN
  private var oGradient: Array[Double] = new Array[Double](numOptimizableParameters)
  // Flush cache when parameters change
  override def setOptimizableParameters(a: Array[Double]): Unit = {
    oValue = Double.NaN
    model.families.foreach(_.clearCachedStatistics) // Parameter changing, so cache no longer valid
    super.setOptimizableParameters(a)
  }

  override def optimizableParameter_=(index: Int, d: Double): Unit = {oValue = Double.NaN; super.optimizableParameter_=(index, d)}
  // Calculation of value and gradient

  val doSetOptimizableValueAndGradient = if (variableSets.forall(_.size == 1)) { () => setOptimizableValueAndGradientIID  } else { () => setOptimizableValueAndGradientBP}

  def setOptimizableValueAndGradient: Unit = doSetOptimizableValueAndGradient()

  def updateOValueBP(expectations: SuffStats) {
    variableSets.foreach(variables => {
      if (variables.size > 0) {
        val lattice = new BPLattice(variables, model)
        // Do inference on the tree
        lattice.updateTreewise(expectations)
        // For all factors // TODO Here skip factors that would have been left out in the TRP spanning tree of a loopy graph
        // TODO Note that this will only work for variables with TrueSetting.  Where to enforce this?
        variables.foreach(_.asInstanceOf[VarWithTargetValue].setToTarget(null))
        // oValue += model.factors(variables).foldLeft(0.0)(_+_.cachedStatistics.score) - logZ
        for (bpfactor <- lattice.bpFactors.values) oValue += bpfactor.factor.cachedStatistics.score
        oValue -= lattice.sumLogZ
      }
    })
  }

  val invVariance = -1.0 / gaussianPriorVariance
  def updateExpectations(expectations: SuffStats, constraints: SuffStats) {
    familiesToUpdate.foreach {
      t =>
        oValue += 0.5 * t.weights.dot(t.weights) * invVariance
        // sum positive constraints into (previously negated) expectations
        expectations(t) += constraints(t)
        //vecPlusEq(expectations(t), constraints(t), 1.0)
        // subtract weights due to regularization
        expectations(t) += t.weights * invVariance
    }
  }

  def setOptimizableValueAndGradientBP: Unit = {
    val expectations = new SuffStats
    oValue = 0.0
    //java.util.Arrays.fill(oGradient, 0.0)
    updateOValueBP(expectations)
    updateExpectations(expectations, constraints)
    // constraints.keys.foreach(t => expectations(t) += constraints(t))
    oGradient = (new ArrayFromVectors(expectations.sortedKeys.map(expectations(_)))).getVectorsInArray(oGradient)
  }

  var distribution = Array.fill(0)(0.0)
  def doOValueIID(expectations: SuffStats) {
    variableSets.foreach(_.foreach(v => {
      if (v.domain.size != distribution.length) distribution = new Array[Double](v.domain.size)
      var i = 0
      while (i < distribution.length) {
        v.set(i)(null)
        distribution(i) = model.score(Seq(v))
        i += 1
      }

      maths.expNormalize(distribution)
      i = 0
      while (i < distribution.length) {
        v.set(i)(null)
        // put negative expectations into 'expectations' StatMap
        model.factorsOfFamilies(Seq(v), familiesToUpdate).foreach(f => expectations(f.family) +=  f.statistics.vector *(-distribution(i)))
        i += 1
      }

      oValue += math.log(distribution(v.targetIntValue))
    }))
  }

  def setOptimizableValueAndGradientIID: Unit = {
    val expectations = new SuffStats
    oValue = 0.0
    java.util.Arrays.fill(oGradient, 0.0)
    doOValueIID(expectations)
    updateExpectations(expectations, constraints)
    // constraints.keys.foreach(t => expectations(t) += constraints(t))
    oGradient = (new ArrayFromVectors(expectations.sortedKeys.map(expectations(_)))).getVectorsInArray(oGradient)
  }

  def optimizableValue: Double = {
    if (oValue.isNaN) setOptimizableValueAndGradient
    oValue
  }

  def getOptimizableGradient(a: Array[Double]) = {
    if (oValue.isNaN) setOptimizableValueAndGradient
    Array.copy(oGradient, 0, a, 0, oGradient.length)
  }
}

// TODO preliminary: currently only supports trivial inference on IID discrete variables
// and inference on tree-shaped graphs of discrete variables.
// In the future there will be a choice of different inference methods over arbitrary graphical model structures

/**
 * Maximum likelihood parameter estimation for the weights of DotTemplate.
 * @author Andrew McCallum, Kedar Bellare, Gregory Druck
 */
class LogLinearMaximumLikelihood(model: Model, modelFile: String = null) {
  //type TemplatesToUpdate = DotTemplate
  var gaussianPriorVariance = 10.0
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])

  //def process[V <: DiscreteVariableWithTrueSetting with NoVariableCoordination](variables: Seq[V], numIterations: Int): Unit = process(List(variables), numIterations)
  // TODO Figure out how to reinstate something like this.
  //def process[V <: DiscreteVariableWithTrueSetting with NoVariableCoordination](variables: Seq[V]): Unit = process(List(variables), Int.MaxValue)

  def process[V <: DiscreteVarWithTarget with NoVariableCoordination](variableSet: Seq[V], numIterations: Int = Int.MaxValue): Unit = 
    processAll(List(variableSet), numIterations)

  /**First argument is a collection of collections-of-variables.  The former are considered iid.  The later may have dependencies.  */
  def processAll[V <: DiscreteVarWithTarget with NoVariableCoordination](variableSets: Seq[Seq[V]], numIterations: Int = Int.MaxValue): Unit = {
    // Data structure for holding per-template constraints and expectations

    println("free memory: " + scala.sys.runtime.freeMemory())
    println("used memory: " + (scala.sys.runtime.totalMemory() - scala.sys.runtime.freeMemory()))
    println("total memory: " + scala.sys.runtime.totalMemory())

    val constraints = new SuffStats
    // Add all model dot templates to constraints
    //familiesToUpdate.foreach(t => constraints(t) = constraints.default(t)) // TODO Why is this line necessary? Delete it? -akm
    // Gather constraints
    variableSets.foreach(_.foreach(_.setToTarget(null)))
    variableSets.foreach(vars => model.factorsOfFamilies(vars, familiesToUpdate).foreach(f => { val vector = f.cachedStatistics.vector ; constraints(f.family) += vector }))

    def templates = constraints.sortedKeys

    println("added all constraints")
    println("free memory: " + scala.sys.runtime.freeMemory())
    println("used memory: " + (scala.sys.runtime.totalMemory() - scala.sys.runtime.freeMemory()))
    println("total memory: " + scala.sys.runtime.totalMemory())

    // Currently only supports iid single DiscreteVariables
    val optimizable = new LogLinearOptimizable(templates, model, variableSets, gaussianPriorVariance, familiesToUpdate, constraints)
    
    def runOptimizer() {
      try {
        val optimizer = new LimitedMemoryBFGS(optimizable) {
          override def postIteration(i: Int) {
            if (model.isInstanceOf[TemplateModel] && (modelFile ne null))
              model.asInstanceOf[TemplateModel].save(modelFile + "-iter=" + i, gzip = true)
          }
        }
        optimizer.optimize(numIterations)
      }
      catch {
        case e : Error => e.printStackTrace
      }
    }

    // Do the gradient-climbing optimization!
    runOptimizer()
    // Resetting and running again sometimes improves results
    runOptimizer()
  }
}

