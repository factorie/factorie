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

// TODO preliminary: currently only supports trivial inference on IID discrete variables
// and inference on tree-shaped graphs of discrete variables.
// In the future there will be a choice of different inference methods over arbitrary graphical model structures

/** Maximum likelihood parameter estimation for the weights of DotTemplate. 
    @author Andrew McCallum, Kedar Bellare, Gregory Druck */
class LogLinearMaximumLikelihood(model: Model) {
  type TemplatesToUpdate = DotTemplate
  var gaussianPriorVariance = 10.0

  //def process[V <: DiscreteVariableWithTrueSetting with NoVariableCoordination](variables: Seq[V], numIterations: Int): Unit = process(List(variables), numIterations)
  // TODO Figure out how to reinstate something like this.
  //def process[V <: DiscreteVariableWithTrueSetting with NoVariableCoordination](variables: Seq[V]): Unit = process(List(variables), Int.MaxValue)
  /** First argument is a collection of collections-of-variables.  The former are considered iid.  The later may have dependencies.  */
  def process[V <: DiscreteVariableWithTrueSetting with NoVariableCoordination](variableSets: Seq[Seq[V]], numIterations: Int = Int.MaxValue): Unit = {
    // Data structure for holding per-template constraints and expectations
    class SuffStats extends HashMap[TemplatesToUpdate, Vector] {
      override def default(template: TemplatesToUpdate) = {
        template.freezeDomains
        val vector: Vector = template.weights match {
          case w: SparseVector => new SparseVector(w.domainSize)
          case w: DenseVector => new DenseVector(w.domainSize)
        }
        this(template) = vector
        vector
      }
      // To help make sure sort order of vectors matches
      def sortedKeys = keys.toSeq.sortWith(_.hashCode > _.hashCode)
    }
    val constraints = new SuffStats
    // Add all model dot templates to constraints
    model.templatesOf[TemplatesToUpdate].foreach(t => constraints(t) = constraints.default(t)) // TODO Why is this line necessary? Delete it? -akm
    // Gather constraints
    variableSets.foreach(_.foreach(_.setToTruth(null)))
    variableSets.foreach(vars => model.factorsOf[TemplatesToUpdate](vars).foreach(f => constraints(f.template) += f.statistic.vector))

    def templates = constraints.sortedKeys

    // Currently only supports iid single DiscreteVariables
    val optimizable = new OptimizableTemplates(templates) with OptimizableByValueAndGradient {
      // Cached values
      private var oValue = Double.NaN
      private var oGradient: Array[Double] = new Array[Double](numOptimizableParameters)
      // Flush cache when parameters change
      override def setOptimizableParameters(a: Array[Double]): Unit = {oValue = Double.NaN; super.setOptimizableParameters(a)}

      override def optimizableParameter_=(index: Int, d: Double): Unit = {oValue = Double.NaN; super.optimizableParameter_=(index, d)}
      // Calculation of value and gradient
      def setOptimizableValueAndGradient: Unit = {
        if (variableSets.forall(_.size == 1)) setOptimizableValueAndGradientIID
        else setOptimizableValueAndGradientBP
      }
      def setOptimizableValueAndGradientBP: Unit = {
        val expectations = new SuffStats
        oValue = 0.0
        java.util.Arrays.fill(oGradient, 0.0)
        variableSets.foreach(variables => {
          if (variables.size > 0) {
          val lattice = new BPLattice(variables, model)
          // Do inference on the tree
          lattice.updateTreewise()
          // For all factors // TODO Here skip factors that would have been left out in the TRP spanning tree of a loopy graph
          for (bpfactor <- lattice.bpFactors.values; if (bpfactor.factor.template.isInstanceOf[TemplatesToUpdate])) {
            val factor = bpfactor.factor.asInstanceOf[TemplatesToUpdate#Factor]
            val marginalMap = bpfactor.marginalMap
            // For all value settings of neighbors of that factor
            for ((values,prob) <- marginalMap) {
              // Set to those values
              bpfactor.variables.zip(values).foreach({ case(variable,intValue) => variable.set(intValue)(null) })
             // put negative expectations into 'expectations' StatMap
             expectations(factor.template.asInstanceOf[TemplatesToUpdate]) += factor.statistic.vector * -prob
            }
          }
          // TODO Note that this will only work for variables with TrueSetting.  Where to enforceme this?
          variables.foreach(_.asInstanceOf[TrueSetting].setToTruth(null))
          oValue += model.score(variables) - lattice.sumLogZ
          }
        })
        val invVariance = -1.0 / gaussianPriorVariance
        model.templatesOf[TemplatesToUpdate].foreach {
          t =>
            oValue += 0.5 * t.weights.dot(t.weights) * invVariance
            // sum positive constraints into (previously negated) expectations
            expectations(t) += constraints(t)
            // subtract weights due to regularization
            expectations(t) += t.weights * invVariance
        }
        // constraints.keys.foreach(t => expectations(t) += constraints(t))
        oGradient = (new ArrayFromVectors(expectations.sortedKeys.map(expectations(_)))).getVectorsInArray(oGradient)
      }
      def setOptimizableValueAndGradientIID: Unit = {
        val expectations = new SuffStats
        oValue = 0.0
        java.util.Arrays.fill(oGradient, 0.0)
        variableSets.foreach(_.foreach(v => {
          val distribution = new Array[Double](v.domainSize) // TODO Are we concerned about all this garbage collection?
          forIndex(distribution.length)(i => {
            v.set(i)(null)
            // compute score of variable with value 'i'
            distribution(i) = model.score(v)
          })

          Maths.expNormalize(distribution)

          forIndex(distribution.length)(i => {
            v.set(i)(null)
            // put negative expectations into 'expectations' StatMap
            model.factorsOf[TemplatesToUpdate](v).foreach(f => expectations(f.template) += f.statistic.vector * -distribution(i))
          })

          oValue += math.log(distribution(v.trueIntValue))
        }))
        val invVariance = -1.0 / gaussianPriorVariance
        model.templatesOf[TemplatesToUpdate].foreach {
          t =>
            oValue += 0.5 * t.weights.dot(t.weights) * invVariance
            // sum positive constraints into (previously negated) expectations
            expectations(t) += constraints(t)
            // subtract weights due to regularization
            expectations(t) += t.weights * invVariance
        }
        // constraints.keys.foreach(t => expectations(t) += constraints(t))
        oGradient = (new ArrayFromVectors(expectations.sortedKeys.map(expectations(_)))).getVectorsInArray(oGradient)
      }

      def optimizableValue: Double = {
        if (oValue.isNaN) setOptimizableValueAndGradient
        oValue
      }

      def getOptimizableGradient(a: Array[Double] = null): Array[Double] = {
        if (oValue.isNaN) setOptimizableValueAndGradient
        if (a == null) {
          var b = new Array[Double](numOptimizableParameters);
          Array.copy(oGradient, 0, b, 0, oGradient.length);
          b
        }
        else {Array.copy(oGradient, 0, a, 0, oGradient.length); a}
      }
    }

    // Do the gradient-climbing optimization!
    var optimizer = new LimitedMemoryBFGS(optimizable)
    optimizer.optimize(numIterations)
    // Resetting and running again sometimes helps
    optimizer = new LimitedMemoryBFGS(optimizable)
    optimizer.optimize(numIterations)
  }
}

