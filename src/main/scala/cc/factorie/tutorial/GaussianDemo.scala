/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.tutorial

import cc.factorie._
import cc.factorie.directed._
import la.{DenseTensor2, Tensor2, DenseTensor1, Tensor1}
import cc.factorie.directed.{MaximizeMultivariateGaussianCovariance, MaximizeMultivariateGaussianMean, MultivariateGaussian, Gaussian}
import cc.factorie.variable.{TensorVariable, DoubleVariable}
import cc.factorie.infer.Maximize

object GaussianDemo {
  def main(args: Array[String]): Unit = {
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)
    val mean = new DoubleVariable(10)
    val variance = new DoubleVariable(1.0)

    val data = for (i <- 1 to 1000) yield new DoubleVariable :~ Gaussian(mean, variance)
    // data.take(50).foreach(println(_))

    val origMean = mean.value
    val origVariance = variance.value
    // println("Original mean=" + origMean)
    // println("Original variance=" + origVariance)

    Maximize(mean)
    Maximize(variance)
    // Or alternatively:
    //MaximizeGaussianMean(mean, model)
    //MaximizeGaussianVariance(variance, model)

    //println("Estimated mean=" + mean.value)
    //println("Estimated variance=" + variance.value)
    assert(math.abs((mean.value / origMean) - 1.0) < .05, "Mean estimate failed")
    assert(math.abs((variance.value / origVariance) - 1.0) < .05, "Variance estimate failed")
  }
}

object MultivariateGaussianDemo {
  def main(args:Array[String]): Unit = {
    implicit val model = DirectedModel()
    implicit val random = new scala.util.Random(0)
    val mean = new TensorVariable[Tensor1](new DenseTensor1(10, 0.0))
    val variance = new TensorVariable[Tensor2](new DenseTensor2(Array.tabulate(10, 10)((i, j) => if (i == j) 1.0 else 0.0)))

    val data = for (i <- 1 to 1000) yield new TensorVariable[Tensor1] :~ MultivariateGaussian(mean, variance)
//    data.take(50).foreach(println(_))

    //println("Original mean="+mean.value)
    //println("Original variance="+variance.value)

    MaximizeMultivariateGaussianMean(mean, model)
    MaximizeMultivariateGaussianCovariance(variance, model)
    // Or alternatively:

    //println("Estimated mean="+mean.value)
    //println("Estimated variance="+variance.value)
  }
}