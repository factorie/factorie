/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.infer

import cc.factorie.la.DenseTensor1
import cc.factorie.model.{Factor, Model}
import cc.factorie.variable._

import scala.collection.mutable.ArrayBuffer

/**
 * User: apassos
 * Date: 5/29/13
 * Time: 8:46 AM
 */
class MPLP(variables: Seq[DiscreteVar], model: Model, maxIterations: Int = 100) extends cc.factorie.util.GlobalLogging {
  val varying = variables.toSet

  class MPLPFactor(val factor: Factor) {
    val thisVariables = factor.variables.toSet
    val varyingVariables = thisVariables.filter(v => v.isInstanceOf[DiscreteVar]).map(_.asInstanceOf[DiscreteVar]).filter(varying.contains).toSet
    val lambdas = varyingVariables.map(v => v -> new DenseTensor1(v.domain.size)).toMap
    def mapScore: Double = getMaxMarginals(varyingVariables.head).max
    def getMaxMarginals(v: DiscreteVar): DenseTensor1 = {
      assert(varyingVariables.contains(v))
      val marginals = new DenseTensor1(v.domain.size)
      varyingVariables.size match {
        case 1 =>
          // we're the only varying neighbor, get the score
          val assignment = new Assignment1[v.type](v, v.domain(0).asInstanceOf[v.Value])
          for (i <- 0 until v.domain.size) {
            assignment.value1 = v.domain(i).asInstanceOf[v.Value]
            marginals(i) = factor.assignmentScore(assignment)
          }
        case 2 =>
          // there is one other varying neighbor we have to max over
          val other = if (varyingVariables.head eq v) varyingVariables.drop(1).head else varyingVariables.head
          val otherLambda = lambdas(other)
          val assignment = new Assignment2[v.type,other.type](v, v.domain(0).asInstanceOf[v.Value], other, other.domain(0).asInstanceOf[other.Value])
          for (value <- 0 until v.domain.size) {
            assignment.value1 = v.domain(value).asInstanceOf[v.Value]
            var maxScore = Double.NegativeInfinity
            for (otherValue <- 0 until other.domain.size) {
              assignment.value2 = other.domain(otherValue).asInstanceOf[other.Value]
              val s = factor.assignmentScore(assignment) + otherLambda(otherValue)
              if (s > maxScore) maxScore = s
            }
            marginals(value) = maxScore
          }
        case _ =>
          def increment(a: Array[Int], domains: Array[Int]): Boolean = {
            var i = a.length-1
            var done = false
            while (i >= 0 && !done) {
              a(i) = (a(i) + 1) % domains(i)
              if (a(i) != 0) done = true
              i -= 1
            }
            !done
          }
          for (value <- 0 until v.domain.size) {
            val others = varyingVariables.filterNot(_ eq v).map(_.asInstanceOf[MutableDiscreteVar]).toSeq
            val domainSizes = others.map(_.domain.size).toArray
            val values = (0 until others.size).map(i => 0).toArray
            var maxScore = Double.NegativeInfinity
            do {
              implicit val d = new DiffList
              for (i <- 0 until others.length) others(i).set(others(i).domain(values(i)).intValue)(d)
              var score = factor.currentScore
              d.undo()
              for (i <- 0 until others.length) score += lambdas(others(i))(values(i))
              if (score > maxScore) maxScore = score
            } while (increment(values, domainSizes))
            marginals(value) = maxScore
          }
      }
      marginals += lambdas(v)
      marginals
    }
  }
  @inline final def near(a: Double, b: Double, eps: Double = 0.000001): Boolean = math.abs(a - b) < (math.abs(a)*eps + eps)

  def isConverged(maxMarginals: Seq[DenseTensor1]): Boolean = {
    val maxIndex0 = maxMarginals.head.maxIndex
    for (i <- 1 until maxMarginals.length) {
      val maxIndex = maxMarginals(i).maxIndex
      if (maxIndex0 != maxIndex && !near(maxMarginals(i)(maxIndex), maxMarginals(i)(maxIndex0))) {
        return false
      }
    }
    true
  }

  def updateMessages(v: DiscreteVar, factors: Seq[MPLPFactor]): Boolean = {
    val maxMarginals = factors.map(_.getMaxMarginals(v))
    val converged = isConverged(maxMarginals)
    if (!converged) {
      val sumMarginals = new DenseTensor1(maxMarginals.head.length)
      maxMarginals.foreach(sumMarginals += _)
      sumMarginals *= 1.0/maxMarginals.length
      for (i <- 0 until factors.length) {
        val lambda = factors(i).lambdas(v)
        for (j <- 0 until lambda.length)
          lambda(j) += sumMarginals(j) - maxMarginals(i)(j)
      }
      assert(isConverged(factors.map(_.getMaxMarginals(v))))
    }
    converged
  }

  def infer: MAPSummary = {
    val factors = model.factors(variables).map(new MPLPFactor(_))
    val variableFactors = collection.mutable.LinkedHashMap[Var,ArrayBuffer[MPLPFactor]]()
    for (f <- factors) {
      for (v <- f.varyingVariables) {
        val list = variableFactors.getOrElseUpdate(v, ArrayBuffer[MPLPFactor]())
        list += f
      }
    }
    var converged = true
    var i = 0
    do {
      converged = true
      for (v <- variables) converged = converged && updateMessages(v.asInstanceOf[DiscreteVar], variableFactors(v))
      logger.debug("Dual is: " + factors.map(_.mapScore).sum)
      i += 1
    } while (!converged && i < maxIterations)
    val assignment = new HashMapAssignment(ignoreNonPresent=false)
    for (v <- variables) {
      val value = v.domain(variableFactors(v).head.getMaxMarginals(v).maxIndex).asInstanceOf[DiscreteVar#Value]
      assignment.update(v, value)
      // go over the factors and "set" this variable to this value. This will avoid bad behavior when
      // the LP relaxation is not tight
      for (factor <- variableFactors(v)) {
        val lambda = factor.lambdas(v)
        for (i <- 0 until lambda.length)
          lambda(i) = Double.NegativeInfinity
        lambda(value.intValue) = 0
      }
    }
    new MAPSummary(assignment, factors.map(_.factor).toSeq)
  }
}

object MaximizeByMPLP extends Maximize[Iterable[DiscreteVar],Model] {
  override def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary = null): MAPSummary = {
    if (marginalizing ne null) throw new Error("Multivariate case yet implemented.")
    new MPLP(variables.toSeq, model).infer
  }
}

class MaximizeByMPLP(maxIterations: Int) extends Maximize[Iterable[DiscreteVar],Model] {
  override def infer(variables:Iterable[DiscreteVar], model:Model, marginalizing:Summary = null) = {
    if (marginalizing ne null) throw new Error("Multivariate case yet implemented.")
    new MPLP(variables.toSeq, model, maxIterations=maxIterations).infer
  }
}
