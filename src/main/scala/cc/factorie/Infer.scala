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
import cc.factorie.generative._

// BPInfer is a factory for BPInferencers
// BPInferencer is specific to a model and some variables

// Infer.apply() does the work and returns a Lattice on success or null on failure. 
//  if the inference is complicated and may be incrementally, it may create a Inferencer 
// An Inferencer is specific to a model and some variables and may be run incrementally;
//  it may also hold on to or be initialized with a Lattice (which is a type of Model)

// TODO What methods should go in a generic Lattice?
// If none, then delete this and just use "Model" instead.
trait Lattice2 extends Model

trait Infer[-V1<:Variable,-V2<:Variable] {
  type LatticeType <: Lattice2
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def apply(variables:Iterable[V1], varying:Iterable[V2], model:Model, qModel:Model): LatticeType // Abstract implemented in subclasses
  def apply(variables:Iterable[V1], varying:Iterable[V2], model:Model): LatticeType = apply(variables, varying, model, null)
  def apply(variables:Iterable[V1], model:Model, qModel:Model): LatticeType = apply(variables, model, qModel)
  def apply(variables:Iterable[V1], model:Model): LatticeType = apply(variables, Nil, model, null)
  /** Called by generic inference engines that manages a suite of Infer objects, allowing each to attempt an inference request.
      If you want your Infer subclass to support such usage by a suite, override this method to check types as a appropriate
      and return a non-null Lattice on success, or null on failure. */
  def attempt(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): LatticeType = null.asInstanceOf[LatticeType]
}

class InferVariables extends Infer[Variable,Variable] {
  def defaultSuite = List(InferIndependentDiscrete,InferByGibbsSampling)
  val suite = new scala.collection.mutable.ArrayBuffer[Infer[Variable,Variable]] ++ defaultSuite
  def apply(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): LatticeType = throw new Error
}
object Infer extends InferVariables


class IndependentDiscreteLattice extends GenerativeFactorModel with Lattice2 {
  def marginal(d:DiscreteVariable): Proportions = this.parentFactor(d) match {
    case f:Discrete.Factor => f._2
  }
}

object InferIndependentDiscrete extends Infer[DiscreteVariable,Nothing] {
  type LatticeType = IndependentDiscreteLattice
  def array(d:DiscreteVariable, model:Model): Array[Double] = {
    val distribution = new Array[Double](d.domain.size)
    val origValue = d.intValue
    val factors = model.factors1(d)
    for (i <- 0 until distribution.size) {
      d := i
      factors.foreach(f => distribution(i) += f.score)
    }
    maths.expNormalize(distribution)
    d := origValue
    distribution
  }
  def proportions(d:DiscreteVariable, model:Model): Proportions = new DenseProportions(array(d, model))
  def apply(d:DiscreteVariable, model:Model): IndependentDiscreteLattice = {
    implicit val lattice = new IndependentDiscreteLattice
    d ~ Discrete(proportions(d, model))
    lattice
  }
  def apply(variables:Iterable[DiscreteVariable], varying:Iterable[Nothing], model:Model, qModel:Model): IndependentDiscreteLattice = {
    if (varying.size > 0) return null
    if (qModel ne null) return null
    implicit val lattice = new IndependentDiscreteLattice
    for (d <- variables) d ~ Discrete(proportions(d, model))
    lattice
  }
  override def attempt(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): IndependentDiscreteLattice = {
    if (varying.size > 0) return null
    if (qModel ne null) return null
    implicit val lattice = new IndependentDiscreteLattice
    for (d <- variables) d match {
      case d:DiscreteVariable => d ~ Discrete(proportions(d, model))
      case _ => return null
    }
    lattice
  }
}


class DiscreteSamplingLattice(variables:Iterable[DiscreteVariable]) extends IndependentDiscreteLattice {
  for (d <- variables) d.~(Discrete(new DenseCountsProportions(d.domain.size)))(this)
  def proportions(d:DiscreteVariable): DenseCountsProportions = this.parentFactor(d) match {
    case df: Discrete.Factor => df._2.asInstanceOf[DenseCountsProportions]
  }
  def increment(d:DiscreteVariable): Unit = proportions(d).increment(d.intValue, 1.0)(null) 
}

class SamplingInferencer2[C](val sampler:Sampler[C]) {
  def infer(targets:Iterable[DiscreteVariable], contexts:Iterable[C], iterations:Int = 500, burnIn:Int = 100, thinning:Int = 20): DiscreteSamplingLattice = {
    val lattice = new DiscreteSamplingLattice(targets)
    sampler.processAll(contexts, burnIn)
    for (i <- 0 until iterations/thinning) {
      sampler.processAll(contexts, thinning)
      for (d <- targets) lattice.increment(d)
      //postSample(targets)
    }
    lattice
  }
}

object InferByGibbsSampling extends Infer[DiscreteVariable,Variable with IterableSettings] {
  type LatticeType = DiscreteSamplingLattice
  def apply(variables:Iterable[DiscreteVariable], varying:Iterable[Variable with IterableSettings], model:Model, qModel:Model): DiscreteSamplingLattice = {
    val inferencer = new SamplingInferencer2(new VariableSettingsSampler[Variable with IterableSettings](model, null))
    inferencer.infer(variables, varying)
  }
  override def attempt(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): DiscreteSamplingLattice = throw new Error("Not yet implemented.")
}
