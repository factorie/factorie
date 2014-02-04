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

package cc.factorie.tutorial

import cc.factorie._
import cc.factorie.directed._
import cc.factorie.la.{DenseTensor2, Tensor2, DenseTensor1, Tensor1}
import cc.factorie.directed._
import cc.factorie.variable._
import cc.factorie.infer.InferByMeanField
import cc.factorie.directed.DirectedTypeHelpers.{MutableTensorVarTensor2, MutableTensorVarTensor1}


object GaussianMixtureDemo {
  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val numComponents = 2
    implicit val model = DirectedModel()
    object ZDomain extends DiscreteDomain(numComponents)
    class Z extends DiscreteVariable(random.nextInt(numComponents)) { def domain = ZDomain }
    val meanComponents = Mixture(numComponents)(new DoubleVariable(random.nextDouble * 10))
    val varianceComponents = Mixture(numComponents)(new DoubleVariable(1.0))
    val mixtureProportions = ProportionsVariable.uniform(numComponents)
    // Generate some data
    val data = for (i <- 1 to 100) yield {
      val z = new Z :~ Discrete(mixtureProportions)
      new DoubleVariable() :~ GaussianMixture(meanComponents, varianceComponents, z)
    }
    // A convenience function for getting the Z for a particular DoubleVar data variable x
    def z(x:DoubleVar): Z = model.parentFactor(x).asInstanceOf[GaussianMixture.Factor]._4.asInstanceOf[Z]
    // Get the list of Z variables, so we can pass it into the EMInferencer
    val zs = data.map(z(_))

    // Show a little example data

    // Now randomly re-assign variable values so we can do the work of re-estimating them
    zs.foreach(_.set(random.nextInt(numComponents))(null))
    meanComponents.foreach(_.set(random.nextDouble())(null))

    // Estimate means and zs by EM
    val em = new EMInferencer(meanComponents, zs, model, InferByMeanField, MaximizeGaussianMean)
    for (i <- 1 to 10) {
      em.process(1)
    }
  }
}  

// TODO: this is a really slow test, I should profile this -luke
object MultivariateGaussianMixtureDemo {
  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val numComponents = 2
    implicit val model = DirectedModel()
    object ZDomain extends DiscreteDomain(numComponents)
    class Z extends DiscreteVariable(random.nextInt(numComponents)) { def domain = ZDomain }
    val meanComponents = Mixture[MutableTensorVarTensor1](numComponents)(new TensorVariable[Tensor1](new DenseTensor1(10, random.nextDouble() * 10)))
    val varianceComponents = Mixture[MutableTensorVarTensor2](numComponents)(new TensorVariable[Tensor2](
      new DenseTensor2(Array.tabulate(10, 10)((i, j) => if (i == j) 10.0 else random.nextDouble() * 0.5))))
    val mixtureProportions = ProportionsVariable.uniform(numComponents)
    // Generate some data
    val data = for (i <- 1 to 1000) yield {
      val z = new Z :~ directed.Discrete(mixtureProportions)
      new TensorVariable[Tensor1] :~ MultivariateGaussianMixture(meanComponents, varianceComponents, z)
    }
    // A convenience function for getting the Z for a particular DoubleVar data variable x
    def z(x: MutableTensorVarTensor1): Z = model.parentFactor(x).asInstanceOf[MultivariateGaussianMixture.Factor]._4.asInstanceOf[Z]
    // Get the list of Z variables, so we can pass it into the EMInferencer
    val zs = data.map(z(_))

    // Show a little example data
    val origMeans = meanComponents.map(_.value)

    // Now randomly re-assign variable values so we can do the work of re-estimating them
    zs.foreach(_.set(random.nextInt(numComponents))(null))
    meanComponents.foreach(_.set(new DenseTensor1(10, random.nextDouble()))(null))
    varianceComponents.foreach(_.set(new DenseTensor2(Array.tabulate(10, 10)((i, j) => if (i == j) 10.0 else random.nextDouble() * 0.5)))(null))

    // Estimate means and zs by EM
    val em = new EMInferencer(meanComponents.toIterable, zs, model, InferByMeanField, MaximizeMultivariateGaussianMean)
    for (i <- 1 to 10) {
      em.process(1)
    }

    meanComponents.foreach(m1 => assert(origMeans.exists(m2 => (m1.value - m2).twoNorm / m2.twoNorm < .1), "Inferred means were not within tolerance of true means!"))
  }
}