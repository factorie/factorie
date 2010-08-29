/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import cc.factorie.la._
import cc.factorie.optimize._
import scala.collection.mutable.HashMap

/**Maximum likelihood parameter estimation. */
// TODO Very preliminary: currently only supports trivial inference on IID discrete variables
// In the future there will be a choice of different inference methods over arbitrary graphical model structures

class LogLinearMaximumLikelihood(model: Model) {
  type TemplatesToUpdate = DotTemplate
  var gaussianPriorVariance = 1.0

  def process[V <: DiscreteVariableWithTrueSetting](variables: Seq[V], numIterations: Int = Math.MAX_INT): Unit = {
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
    model.templatesOf[TemplatesToUpdate].foreach(t => constraints(t) = constraints.default(t))
    // Gather constraints
    variables.foreach(_.setToTruth(null))
    model.factorsOf[TemplatesToUpdate](variables).foreach(f => constraints(f.template) += f.statistic.vector)

    def templates = constraints.sortedKeys

    // Currently only supports iid single DiscreteVariables
    val optimizable = new OptimizableTemplates(templates) with OptimizableByValueAndGradient {
      // Cached values
      private var oValue = Math.NaN_DOUBLE
      private var oGradient: Array[Double] = new Array[Double](numOptimizableParameters)
      // Flush cache when parameters change
      override def setOptimizableParameters(a: Array[Double]): Unit = {oValue = Math.NaN_DOUBLE; super.setOptimizableParameters(a)}

      override def optimizableParameter_=(index: Int, d: Double): Unit = {oValue = Math.NaN_DOUBLE; super.optimizableParameter_=(index, d)}
      // Calculation of value and gradient
      def setOptimizableValueAndGradient: Unit = {
        val expectations = new SuffStats
        oValue = 0.0
        java.util.Arrays.fill(oGradient, 0.0)
        variables.foreach(v => {
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

          oValue += Math.log(distribution(v.trueIntValue))
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

    val optimizer = new LimitedMemoryBFGS(optimizable)
    optimizer.optimize(numIterations)
  }
}

