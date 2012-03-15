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
import cc.factorie.la._

/** */

// TODO Call this "Variables"??
trait Neighbors {
  /** The ordered sequence of neighboring variables. */
  def variables: Seq[Variable]
  /** The number of variables neighboring this factor.  
      Alternative to "variables.length" that is likely faster because "variables" may need to construct the Seq. */
  def numVariables: Int
  /** Return one of the neighboring variables in the sequence of neighbors. */
  def variable(index: Int): Variable
}

trait Neighbors1[N1<:Variable] extends Neighbors {
  def _1: N1
  def numVariables = 1
  override def variables = IndexedSeq(_1)
  def variable(i:Int) = i match { case 0 => _1; case _ => throw new IndexOutOfBoundsException(i.toString) }
}
trait Neighbors2[N1<:Variable,N2<:Variable] extends Neighbors {
  def _1: N1
  def _2: N2
  def numVariables = 2
  override def variables = IndexedSeq(_1, _2)
  def variable(i:Int) = i match { case 0 => _1; case 1 => _2; case _ => throw new IndexOutOfBoundsException(i.toString) }
}

trait NewLattice {
  def marginal(v:Variable*): Expectations // TODO ???
  def marginal(values:Values): Double // ???
}

trait Expectations {
  // What goes here?
}

// TODO Will there be DenseDiscreteExpectations1 and SparseDiscreteExpectations1, etc?
// or perhaps 
//  DiscreteExpectations1 with DenseCountsProportions1
//  DiscreteExpectations1 with SparseCountsProportions1
class DiscreteExpectations1(val _1:DiscreteVar) extends DenseCountsProportions(_1.domain.size) with FactorWithStatistics1[DiscreteVar] {
  def this(f:Factor1[DiscreteVar]) = this (f._1)
  def score(s:Statistics) = this.apply(s._1.intValue)
}
//class DiscreteExpectations2(val domain1:DiscreteDomain, val domain2:DiscreteDomain) extends DenseCountsProportions2(domain1.size, domain2.size) with Neighbors2[DiscreteVar,DiscreteVar]

abstract class GaussianExpectations1 extends Neighbors1[RealVar] {
  def mean = 0.0
  def variance = 0.0
  def pr(x:Double): Double = 0.0
}


