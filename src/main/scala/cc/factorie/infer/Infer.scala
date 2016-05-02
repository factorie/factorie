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

import cc.factorie.maths
import cc.factorie.model.Model
import cc.factorie.variable.{DenseProportions1, DiscreteVariable, Proportions1, Var}

// Inference naming conventions:

// Inference requests can be made to objects with class name "Infer*"
// Typically these requests will be made through an "apply" method, with type signature that is specific to the particular inference method. 
// Some of these objects are complex enough to have ongoing internal state may create an *Inferencer to do the work.
// Some of these objects may inherit from the Infer trait, indicating that they have an "infer" method suitable for calling by generic inference suites

// The result of inference is a "Summary" object, which is basically a container for a bunch of Marginal objects.

// For example
// BPInferencer is specific to a model and some variables; its stores messages and can do incremental work.
// InferByBP is a factory for BPInferencers
// InferByBP.apply takes DiscreteVariables as an argument creates an inferencer and runs it to convergence
// InferByBP.infer takes Variables as an argument, and if able to handle the arguments returns Some[Summary]; otherwise None.

// Infer.apply() does the work and returns a Summary on success or null on failure. 
//  if the inference is complicated and may be incrementally, it may create a Inferencer 
// An Inferencer is specific to a model and some variables and may be run incrementally;
//  it may also hold on to or be initialized with a Summary (which is a container for Marginals)


trait Infer[-A<:Iterable[Var], -B<:Model] {
  def infer(variables:A, model:B, marginalizing:Summary = null): Summary
  //def infer(variables:A, model:B): Summary = infer(variables, model, null)
//  def infer(variables:A, model:B): Summary
}

//trait InferMarginalizing[-A<:Iterable[Var], -B<:Model, -C<:Summary] extends Infer[A,B] {
//  def infer(variables:A, model:B, marginalizing:C): Summary
//  def infer(variables:A, model:B) = infer(variables, model, null.asInstanceOf[C])
//}

// TODO Rename simply InferDiscrete?  Multiple DiscreteVariables could be handled by a "InferDiscretes" // Yes, I think so.  Note DiscreteSummary1 => DiscretesSummary also. -akm
object InferDiscrete1 extends Infer[Iterable[DiscreteVariable],Model] {
  // TODO Consider renaming this "asArray"
  def array(d:DiscreteVariable, model:Model): Array[Double] = {
    val distribution = new Array[Double](d.domain.size)
    val origValue = d.intValue
    val factors = model.factors(d) // Note that this doens't handle value-specific factor unrolling
    for (i <- 0 until distribution.size) {
      d := i // Note that this doesn't handle variable-value coordination, and if this is present, undo'ing won't happen properly.
      factors.foreach(f => distribution(i) += f.currentScore)
    }
    maths.ArrayOps.expNormalize(distribution)
    d := origValue
    distribution
  }
  def proportions(d:DiscreteVariable, model:Model): Proportions1 = new DenseProportions1(array(d, model))
  def marginal[V<:DiscreteVariable](d:V, model:Model): DiscreteMarginal1[V] = new SimpleDiscreteMarginal1(d, proportions(d, model))
  def apply[V<:DiscreteVariable](varying:Iterable[V], model:Model): DiscreteSummary1[V] = {
    val summary = new DiscreteSummary1[V]
    for (v <- varying) summary += new SimpleDiscreteMarginal1(v, proportions(v, model))
    summary
  }
  def apply[V<:DiscreteVariable](varying:V, model:Model): DiscreteSummary1[V] = {
    val summary = new DiscreteSummary1[V]
    summary += new SimpleDiscreteMarginal1(varying, proportions(varying, model))
    summary
  }
  def infer(variables:Iterable[DiscreteVariable], model:Model, marginalizing:Summary): DiscreteSummary1[DiscreteVariable] = {
    if (marginalizing ne null) throw new Error("Marginalizing case not yet implemented.")
    apply(variables, model)
  }
}



class SamplingInferencer[C,S<:IncrementableSummary](val sampler:Sampler[C], val summary:S) {
  var iterationCount = 0
  //def this(sampler:Sampler[C], discreteVars:Iterable[DiscreteVariable]) = this(sampler, new DiscreteSummary1(discreteVars));
  def process(contexts:Iterable[C], iterations:Int = 500, burnIn:Int = 100, thinning:Int = 20): Unit = {
    iterationCount += 1
    sampler.processAll(contexts, burnIn)
    for (i <- 0 until iterations/thinning) {
      sampler.processAll(contexts, thinning)
      summary.incrementCurrentValues(1.0)
      //postSample(targets)
    }
  }
}
