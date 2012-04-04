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
import cc.factorie.la._


@deprecated("Will be removed, because I think Expectations can be Factors.")
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


trait DiscreteExpectations extends Factor with Expectations {
  def proportions: Proportions
}
class DiscreteExpectations1[V1<:DiscreteVar](val _1:V1, proportions1:Proportions1 = null) extends FactorWithStatistics1[V1] with DiscreteExpectations {
  def this(f:Factor1[V1]) = this (f._1)
  val proportions = if (proportions1 eq null) new DenseProportions1(_1.domain.size) else proportions1 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.intValue))
  final def pr(i:Int): Double = proportions(i) // Just a short-cut alias
}
class DiscreteExpectations2[V1<:DiscreteVar,V2<:DiscreteVar](val _1:V1, val _2:V2, proportions2:Proportions2 = null) extends FactorWithStatistics2[V1,V2] with DiscreteExpectations {
  def this(f:Factor2[V1,V2]) = this (f._1, f._2)
  val proportions = if (proportions2 eq null) new DenseProportions2(_1.domain.size, _2.domain.size) else proportions2 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.intValue, s._2.intValue))
  final def pr(i:Int, j:Int): Double = proportions(i, j) // Just a short-cut alias
}
class DiscreteExpectations3[V1<:DiscreteVar,V2<:DiscreteVar,V3<:DiscreteVar](val _1:V1, val _2:V2, val _3:V3, proportions3:Proportions3 = null) extends FactorWithStatistics3[V1,V2,V3] with DiscreteExpectations {
  def this(f:Factor3[V1,V2,V3]) = this (f._1, f._2, f._3)
  val proportions: Proportions3 = if (proportions3 eq null) new DenseProportions3(_1.domain.size, _2.domain.size, _3.domain.size) else proportions3 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.intValue, s._2.intValue, s._3.intValue))
  final def pr(i:Int, j:Int, k:Int): Double = proportions(i, j, k) // Just a short-cut alias
}
class DiscreteExpectations4[V1<:DiscreteVar,V2<:DiscreteVar,V3<:DiscreteVar,V4<:DiscreteVar](val _1:V1, val _2:V2, val _3:V3, val _4:V4, proportions4:Proportions4 = null) extends FactorWithStatistics4[V1,V2,V3,V4] with DiscreteExpectations {
  def this(f:Factor4[V1,V2,V3,V4]) = this (f._1, f._2, f._3, f._4)
  val proportions: Proportions4 = if (proportions4 eq null) new DenseProportions4(_1.domain.size, _2.domain.size, _3.domain.size, _4.domain.size) else proportions4 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.intValue, s._2.intValue, s._3.intValue, s._4.intValue))
  final def pr(i:Int, j:Int, k:Int, l:Int): Double = proportions(i, j, k, l) // Just a short-cut alias
}
object DiscreteExpectations {
  def apply[V1<:DiscreteVar](f:Factor1[V1]): DiscreteExpectations1[V1] = new DiscreteExpectations1(f)
  def apply[V1<:DiscreteVar,V2<:DiscreteVar](f:Factor2[V1,V2]): DiscreteExpectations2[V1,V2] = new DiscreteExpectations2(f)
  def apply[V1<:DiscreteVar,V2<:DiscreteVar,V3<:DiscreteVar](f:Factor3[V1,V2,V3]): DiscreteExpectations3[V1,V2,V3] = new DiscreteExpectations3(f)
  def apply[V1<:DiscreteVar,V2<:DiscreteVar,V3<:DiscreteVar,V4<:DiscreteVar](f:Factor4[V1,V2,V3,V4]): DiscreteExpectations4[V1,V2,V3,V4] = new DiscreteExpectations4(f)
}


// Gaussian expectations

class GaussianExpectations1[V1<:RealVar](val _1:V1) extends FactorWithStatistics1[V1] with Expectations {
  // TODO Set this up better for incremental estimation
  var mean = 0.0
  var variance = 1.0
  def pr(x:Double): Double = cc.factorie.generative.Gaussian.pr(x, mean, variance)
  def score(s:Statistics) = cc.factorie.generative.Gaussian.pr(s._1, mean, variance)
}


