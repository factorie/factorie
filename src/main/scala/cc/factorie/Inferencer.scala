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

// TODO Not yet sure what general interfaces should go here.
trait Marginal extends Variable
trait Lattice[V<:Variable] {
  type VariableMarginalType <: Marginal
  type FactorMarginalType <: Marginal
  def marginal(v:V): Option[VariableMarginalType] = None
  //def marginal(v:V*): Option[MarginalType] = None // ??
  def marginal(f:Factor): Option[FactorMarginalType] = None
}

/** Generic infrastructure for inference, both for calculating marginals and maxima.
    @author Andrew McCallum
    @since 0.8 */
trait Inferencer[V<:Variable,C] {
  type LatticeType <: Lattice[V]
  /** Infer the target 'variables' using 'varying' to drive the exploration of changes to the configuration.
      For example, in SamplingInference, 'varying' are Sampler 'contexts' which do not necessarily have to be Variables;
      they are arbitrary keys to the sampler that may drive exploration, which will eventually result in changes to the 'target' variables.
      If for you this separation of target variables and sampling contexts is unnecessary or confusing, consider VariableInferencer instead.  */
  def infer(variables:Iterable[V], varying:Iterable[C]): LatticeType
  //def infer(templates:Seq[VectorTemplate], variables:Iterable[V]): LatticeType
  //def infer(factors:Seq[VectorTemplate#Factor], variables:Iterable[V]): LatticeType
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

class IIDDiscreteInferencer[V<:DiscreteVariable](model:Model) extends VariableInferencer[V] {
  type LatticeType = IIDDiscreteLattice
  class IIDDiscreteLattice extends HashMap[V,DiscreteMarginal[V]] with Lattice[V] {
    type VariableMarginalType = DiscreteMarginal[V]
    override def marginal(v:V) = this.get(v)
  }
  def infer(variables:Iterable[V], varying:Iterable[V]): LatticeType = {
    val lattice = new IIDDiscreteLattice
    for (v <- variables) {
      val a = new Array[Double](v.domain.size)
      forIndex(a.length)(i => {
        v.set(i)(null)
        a(i) = model.score(v)
      })
      maths.expNormalize(a)
      lattice(v) = new DiscreteMarginal[V](v, a)
    }
    lattice
  }
}


class DiscreteMarginal[V<:DiscreteVars](val variable:V, proportions:Seq[Double] = Nil) extends cc.factorie.generative.DenseCountsProportions(variable.domain.size) with Marginal {
  // TODO Consider also including "val inferencer:Inferencer" in constructor arguments
  if (proportions != Nil) this.set(proportions)(null)
  override def keepChildren = false
  def incrementCurrentValue : Unit = variable match {
    case v:DiscreteVar => increment(v.intValue, 1.0)(null)
    case v:BinaryFeatureVectorVariable[_] => { for (index <- v.activeDomain) increment(index, 1.0)(null) } // throw new Error // TODO Put this code back in: v.incrementInto(this)
  }
}
// TODO Not yet in its final form
class DiscreteFactorMarginal(val factor:Factor, val value:Array[Double]) extends Marginal {
  def length = value.length
  def apply(i:Int) = value(i)
  /**The settings of each of the N variables that together yield the highest probability. */
  def maxEntry: Array[Int] = throw new Error("Not yet implemented")
}


//trait Maximizer[V<:Variable] extends Inferencer[V,V] // Include something like this?  No.  Regular Inferencer can also do Maximization, or a mixture of Marginalization and Maximization.
// 'infer' here would actually change state to the maximum found
// 'infer' in Inferencer would leave it in some random state, with the results really in the Marginal objects





/** BruteForce searches for the optimal configuration of a Collection of Variables
    by doing exhaustive enumeration or all possible configurations.
    @author Tim Vieira */
@deprecated("This class is not yet integrated into the general Inference framework, and may be removed in the future.")
class BruteForceInferencer[V<:DiscreteVariable with NoVariableCoordination](model:Model) {

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
