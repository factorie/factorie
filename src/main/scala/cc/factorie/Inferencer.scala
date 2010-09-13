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
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}

// Preliminary steps toward generic interfaces to inference
// Eventually we will have marginals over factors instead of variables


// Generic over sampling-based inference and variational inference
// TODO Not yet sure what general interfaces should go here.
trait Marginal
trait Lattice

/** Generic infrastructure for inference, both for calculating various marginals and maxima.
    @author Andrew McCallum
    @since 0.8 */
trait Inferencer[V<:Variable,C] {
  type LatticeType <: Lattice
  /** Infer the target 'variables' using 'varying' to drive the exploration of changes to the configuration.
      For example, in SamplingInference, 'varying' are Sampler 'contexts' which do not necessarily have to be Variables;
      they are arbitrary keys to the sampler that may drive exploration, which will eventually result in changes to the 'target' variables.
      If for you this separation of target variables and sampling contexts is unnecessary or confusing, consider VariableInferencer instead.  */
  def infer(variables:Iterable[V], varying:Iterable[C]): LatticeType
  //def infer(factors:TemplateList[VectorTemplate], variables:Iterable[V]): LatticeType
}

/** An Inferencer in which the context arguments are Variables.
    @author Andrew McCallum
    @since 0.8
 */
trait VariableInferencer[V<:Variable] extends Inferencer[V,V] {
  /** Infer the 'variables', changing their values, but not other variables' (except perhaps through variable-value coordination). */
  def infer(variables:Iterable[V]): LatticeType = infer(variables, variables)
  /** Infer the 'targets' variables, considering not only changes their their values, but also changes to the 'marginalizing' variables. */
  def inferMarginalizing(targets:Iterable[V], marginalizing:Iterable[V]) = infer(targets, targets ++ marginalizing)
}

//trait Maximizer[V<:Variable] extends Inferencer[V,V] // Include something like this?
// 'infer' here would actually change state to the maximum found
// 'infer' in Inferencer would leave it in some random state, with the results really in the Marginal objects



/** Perform inference according to belief propagation.
    @author Andrew McCallum, Kedar Bellare, Tim Vieira
    @since 0.8
 */
class BPInferencer[V<:BeliefPropagation.BPVariable](model:Model) extends VariableInferencer[V] {
  override type LatticeType = BPLattice
  def infer(variables:Iterable[V], varying:Iterable[V]): LatticeType = infer(variables, varying, 4) // TODO Make a more sensible default
  def infer(variables:Iterable[V], varying:Iterable[V], numIterations:Int): LatticeType = {
    val result = new BPLattice(varying, model)
    result.update(numIterations) // TODO Of course make this smarter later
    result.setVariablesToMax(variables) // For now, just inference my marginal maximization
    // NOTE the above line requires that 'variables' is a subset of varying, of course!
    result
  }
  def infer(variables:Iterable[V], numIterations:Int): LatticeType = infer(variables, variables, numIterations)
  // waiting for Scala 2.8 default parameters...
  def inferTreewise(variables:Iterable[V], varying:Iterable[V]): LatticeType = inferTreewise(variables, varying, 1)
  def inferTreewise(variables:Iterable[V], varying:Iterable[V], maxiterations:Int): LatticeType = {
    // NOTE: 'variables' must be a subset of varying, of course!
    val result = new BPLattice(varying, model)
    result.updateTreewise()
    result.setVariablesToMax(variables)
    result
  }
  def inferTreewise(variables:Iterable[V]): LatticeType = inferTreewise(variables, variables, 1)
  def inferTreewise(variables:Iterable[V], maxiterations:Int): LatticeType = inferTreewise(variables, variables, maxiterations)
  def inferTreewiseMax(variables: Iterable[V], varying: Iterable[V], maxiterations: Int): LatticeType = {
    val result = new BPLattice(varying, model)
    result.updateTreewiseMax()
    result.setVariablesToMax(variables)
    result
  }
  def inferTreewiseMax(variables: Iterable[V]): LatticeType = inferTreewiseMax(variables, variables, 1)
  def inferTreewiseMax(variables: Iterable[V], maxIterations: Int): LatticeType = inferTreewiseMax(variables, variables, maxIterations)
}



/** BruteForce searches for the optimal configuration of a Collection of Variables
    by doing exhaustive enumeration or all possible configurations.
    @author Tim Vieira */
@deprecated("This class is not yet integrated into the general Inference framework.")
class BruteForce[V<:DiscreteVariable with NoVariableCoordination](model:Model) {

  // TODO: make this conform to some of the existing Inferencer interfaces.
  // extends VariableInferencer[V]?

  def infer(variables:Seq[V]): Unit = {
    // Argmax over all combinations of variable values
    val iterators = variables.map(v2 => v2.asInstanceOf[V].settings).toList
    iterators.foreach(setting => {setting.reset; setting.next}) // reset each iterator and advance to first setting.
    var score = 0.0
    var best_score = Double.NegativeInfinity
    var best = null.asInstanceOf[Config]
    do {
      score = model.score(variables)
      if (score > best_score || best == null) {
        best = new Config(variables) // snapshot the variable configuration
        best_score = score
      }
    } while (nextValues(iterators))
    best.load // leave the variables in the best configuration found.
  }

  class Config(val variables:Seq[V]) {
    // remember the settings of each variable
    val settings = new Array[Int](variables.size)
    for ((v,i) <- variables.toList.zipWithIndex) { settings(i) = v.intValue }
    // set the values of each variable to that in this configuration
    def load = for ((v,k) <- variables.toList.zip(settings.toList)) v.set(k)(null)
    override def toString = ("Config(" + settings.toList.toString + ")")
  }

  /** Iterate through all combinations of values in Variables given their `SettingIterators */
  private def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.head.hasNext) {vs.head.next; true}
    else if (vs.tail != Nil) {vs.head.reset; vs.head.next; nextValues(vs.tail)}
    else false
  }
}
