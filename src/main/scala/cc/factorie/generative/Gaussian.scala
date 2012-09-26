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

package cc.factorie.generative
import cc.factorie._
import cc.factorie.util.DoubleSeq

object Gaussian extends GenerativeFamily3[DoubleVar,DoubleVar,DoubleVar] {
  self =>
  def logpr(value:Double, mean:Double, variance:Double): Double = {
      val diff = value - mean
      return - diff * diff / (2 * variance) - 0.5 * math.log(2.0 * math.Pi * variance)
  } 
  def pr(value:Double, mean:Double, variance:Double): Double = math.exp(logpr(value, mean, variance))
  def sampledValue(mean:Double, variance:Double): Double = maths.nextGaussian(mean, variance)(cc.factorie.random)
  case class Factor(override val _1:DoubleVar, override val _2:DoubleVar, override val _3:DoubleVar) extends super.Factor(_1, _2, _3) {
    override def logpr(child:Double, mean:Double, variance:Double): Double = self.logpr(child, mean, variance)
    override def logpr: Double = self.logpr(_1.value, _2.value, _3.value)
    def pr(child:Double, mean:Double, variance:Double) = math.exp(logpr(child, mean, variance))
    override def pr: Double = self.pr(_1.value, _2.value, _3.value)
    def sampledValue(mean:Double, variance:Double): Double = self.sampledValue(mean, variance)
    //override def sampledValue: Double = self.sampledValue(_2.value, _3.value)
  }
  def newFactor(a:DoubleVar, b:DoubleVar, c:DoubleVar) = Factor(a, b, c)
}

// TODO Complete something like this
//object PlatedGaussian extends GenerativeFamilyWithStatistics3[DoubleSeqVar,DoubleVar,DoubleVar] 



object MaximizeGaussianMean extends Maximize {
  var debug = false
  def maxMean(meanVar:MutableDoubleVar, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]): Double = {
    var mean = 0.0
    var sum = 0.0
    //println("MaximizeGaussianMean var="+meanVar)
    for (factor <- model.extendedChildFactors(meanVar)) {
      //println(" MaximizeGaussianMean factor="+factor)
      //println(" MaximizeGaussianMean mean="+mean)
      factor match {
      	case g:Gaussian.Factor if (g._2 == meanVar) => { mean += g._1.doubleValue; sum += 1.0 }
      	case gm:GaussianMixture.Factor if (gm._2.contains(meanVar)) => {
          val gate = gm._4
          val gateMarginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal1(gate) 
          val mixtureIndex = gm._2.indexOf(meanVar) // Yipes!  Linear search.  And we are doing it twice because of "contains" above
          if (gateMarginal eq null) {
            if (gm._4.intValue == mixtureIndex) { mean += gm._1.doubleValue; sum += 1.0 }
          } else {
            val p = gateMarginal.proportions(mixtureIndex)
            assert(p == p); assert(p >= 0.0); assert(p <= 1.0)
            mean += p * gm._1.doubleValue; sum += p
            //println("MaximizeGaussianMean mean="+mean+" sum="+sum+" p="+p+" proportions="+gateMarginal.proportions)
          }
        }
        case m:Mixture.Factor => {}
        case f:Factor => { if (debug) println("MaximizeGaussianMean can't handle factor "+f.getClass.getName+"="+f); return Double.NaN }
      }
      if (mean != mean) return Double.NaN
    }
    //println("MaximizeGaussianMean mean="+(mean/sum))
    mean / sum
  }
  def apply(meanVar:MutableDoubleVar, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar] = null): Unit = {
    meanVar.set(maxMean(meanVar, model, summary))(null)
  }
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[AssignmentSummary] = {
    val gModel = model match { case m:GenerativeModel => m ; case _ => return None }
    val dSummary = summary match { case s:DiscreteSummary1[DiscreteVar] => s ; case null => null ; case _ => return None }
    lazy val assignment = new HashMapAssignment
    for (v <- variables) v match {
      case r:MutableDoubleVar => { val m = maxMean(r, gModel, dSummary); if (m.isNaN) return None else assignment.update[MutableDoubleVar](r, m) } 
      case _ => return None
    }
    //println("MaximizeGaussianMean assignment "+assignment(variables.head))
    Option(new AssignmentSummary(assignment))
  }
}

object MaximizeGaussianVariance extends Maximize {
  var debug = false
  def minSamplesForVarianceEstimate = 5
  def maxVariance(varianceVar:MutableDoubleVar, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]): Double = {
    var mean = 0.0
    var sum = 0.0
    val factors = model.extendedChildFactors(varianceVar)
    if (factors.size < minSamplesForVarianceEstimate) return 1.0
    for (factor <- factors) factor match {
      case g:Gaussian.Factor if (g._3 == varianceVar) => { mean += g._1.doubleValue; sum += 1.0 }
      case gm:GaussianMixture.Factor if (gm._3.contains(varianceVar)) => {
        val gate = gm._4
        val gateMarginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal1(gate) 
        val mixtureIndex = gm._3.indexOf(varianceVar) // Yipes!  Linear search.  And we are doing it twice, because of "contains" above
        if (gateMarginal eq null) {
          if (gm._4.intValue == mixtureIndex) { mean += gm._1.doubleValue; sum += 1.0 }
        } else {
          val p = gateMarginal.proportions(mixtureIndex)
          mean += p * gm._1.doubleValue; sum += p
        }
       }
      case m:Mixture.Factor => {}
      case f:Factor => { if (debug) println("MaximizeGaussianVariance can't handle factor "+f.getClass.getName+"="+f); return Double.NaN }
    }
    if (sum == 0.0) { if (debug) println("MaximizeGaussianVariance found no child factors"); return Double.NaN }
    mean /= sum
    //println("MaximizeGaussianVariance mean="+mean)
    var v = 0.0 // accumulates the variance
    sum = 0.0
    for (factor <- factors) factor match {
      case g:Gaussian.Factor if (g._3 == varianceVar) => { val diff = mean - g._1.doubleValue; v += diff * diff; sum += 1 }
      case gm:GaussianMixture.Factor if (gm._3.contains(varianceVar)) => {
        val gate = gm._4
        val gateMarginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal1(gate) 
        val mixtureIndex = gm._3.indexOf(varianceVar) // Yipes!  Linear search.  And we are doing it twice, because of "contains" above
        if (gateMarginal eq null) {
          if (gm._4.intValue == mixtureIndex) { val diff = mean - gm._1.doubleValue; v += diff * diff; sum += 1 }
        } else {
          val p = gateMarginal.proportions(mixtureIndex)
          val diff = mean - gm._1.doubleValue; v += diff * diff * p; sum += p
        }
      }
      case _ => { return Double.NaN }
    }
    assert(sum >= minSamplesForVarianceEstimate)
    // TODO Does this work for weighted children?
    math.sqrt(v / (sum - 1))
  }
  def apply(varianceVar:MutableDoubleVar, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar] = null): Unit = {
    varianceVar.set(maxVariance(varianceVar, model, summary))(null)
  }
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[AssignmentSummary] = {
    val gModel = model match { case m:GenerativeModel => m ; case _ => return None }
    val dSummary = summary match { case s:DiscreteSummary1[DiscreteVar] => s ; case null => null ; case _ => return None }
    lazy val assignment = new HashMapAssignment
    for (v <- variables) v match {
      case r:MutableDoubleVar => { val va = maxVariance(r, gModel, dSummary); if (va.isNaN) return None else assignment.update[MutableDoubleVar](r, va) } 
      case _ => return None
    }
    Option(new AssignmentSummary(assignment))
  }
}

// More efficient to maximize them all at once.
object MaximizeGaussianMixture extends Maximize {
  def minSamplesForVarianceEstimate = 5
  def maxMeanMixture(mixture:Mixture[MutableDoubleVar], model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]): Double = {
    throw new Error("Not yet implemented")
    0.0
  }
}