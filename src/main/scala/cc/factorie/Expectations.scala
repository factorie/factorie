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


trait Expectations {
  // What goes here?
}
trait TensorExpectations extends Expectations {
  def tensor: Tensor
}


trait DiscreteExpectations extends Factor with Expectations {
  def proportions: Proportions
}
class DiscreteExpectations1[V1<:DiscreteVectorVar](val _1:V1, proportions1:Proportions1 = null) extends FactorWithStatistics1[V1] with DiscreteExpectations {
  def this(f:Factor1[V1]) = this (f._1, null)
  val proportions = if (proportions1 eq null) new DenseProportions1(_1.domain.dimensionDomain.size) else proportions1 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = s._1 match { case d:DiscreteVar => math.log(proportions(d.intValue)); case d:DiscreteVectorVar => throw new Error("Not yet implemented") }
  def incrementCurrentValue: Unit = _1 match { case d:DiscreteVar => proportions.+=(d.intValue, 1.0); case d:DiscreteVectorVar => throw new Error("Not yet implemented") }
}
class DiscreteExpectations2[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar](val _1:V1, val _2:V2, proportions2:Proportions2 = null) extends FactorWithStatistics2[V1,V2] with DiscreteExpectations {
  def this(f:Factor2[V1,V2]) = this (f._1, f._2, null)
  val proportions = if (proportions2 eq null) new DenseProportions2(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size) else proportions2 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.asInstanceOf[DiscreteVar].intValue, s._2.asInstanceOf[DiscreteVar].intValue)) // Generalize to DiscreteVectorVar
}
class DiscreteExpectations3[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar,V3<:DiscreteVectorVar](val _1:V1, val _2:V2, val _3:V3, proportions3:Proportions3 = null) extends FactorWithStatistics3[V1,V2,V3] with DiscreteExpectations {
  def this(f:Factor3[V1,V2,V3]) = this (f._1, f._2, f._3, null)
  val proportions: Proportions3 = if (proportions3 eq null) new DenseProportions3(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size) else proportions3 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.asInstanceOf[DiscreteVar].intValue, s._2.asInstanceOf[DiscreteVar].intValue, s._3.asInstanceOf[DiscreteVar].intValue))
}
class DiscreteExpectations4[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar,V3<:DiscreteVectorVar,V4<:DiscreteVectorVar](val _1:V1, val _2:V2, val _3:V3, val _4:V4, proportions4:Proportions4 = null) extends FactorWithStatistics4[V1,V2,V3,V4] with DiscreteExpectations {
  def this(f:Factor4[V1,V2,V3,V4]) = this (f._1, f._2, f._3, f._4, null)
  val proportions: Proportions4 = if (proportions4 eq null) new DenseProportions4(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size, _4.domain.dimensionDomain.size) else proportions4 // must do this here because no access to _1 in default argument values
  def score(s:Statistics) = math.log(proportions(s._1.asInstanceOf[DiscreteVar].intValue, s._2.asInstanceOf[DiscreteVar].intValue, s._3.asInstanceOf[DiscreteVar].intValue, s._4.asInstanceOf[DiscreteVar].intValue))
}
object DiscreteExpectations {
  def apply[V1<:DiscreteVectorVar](f:Factor1[V1]): DiscreteExpectations1[V1] = new DiscreteExpectations1(f)
  def apply[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar](f:Factor2[V1,V2]): DiscreteExpectations2[V1,V2] = new DiscreteExpectations2(f)
  def apply[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar,V3<:DiscreteVectorVar](f:Factor3[V1,V2,V3]): DiscreteExpectations3[V1,V2,V3] = new DiscreteExpectations3(f)
  def apply[V1<:DiscreteVectorVar,V2<:DiscreteVectorVar,V3<:DiscreteVectorVar,V4<:DiscreteVectorVar](f:Factor4[V1,V2,V3,V4]): DiscreteExpectations4[V1,V2,V3,V4] = new DiscreteExpectations4(f)
  def apply(f:Factor): DiscreteExpectations = f match {
    case f:Factor1[DiscreteVectorVar] => apply(f)
    case f:Factor2[DiscreteVectorVar,DiscreteVectorVar] => apply(f)
    case f:Factor3[DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar] => apply(f)
    case f:Factor4[DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar] => apply(f)
  }
  def apply(f:Factor, p:Proportions): DiscreteExpectations = f match {
    case f:Factor1[DiscreteVectorVar] => new DiscreteExpectations1(f._1, p.asInstanceOf[Proportions1])
    case f:Factor2[DiscreteVectorVar,DiscreteVectorVar] => new DiscreteExpectations2(f._1, f._2, p.asInstanceOf[Proportions2])
    case f:Factor3[DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar] => new DiscreteExpectations3(f._1, f._2, f._3, p.asInstanceOf[Proportions3])
    case f:Factor4[DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar,DiscreteVectorVar] => new DiscreteExpectations4(f._1, f._2, f._3, f._4, p.asInstanceOf[Proportions4])
  }
}


// Gaussian expectations

class GaussianExpectations1[V1<:RealVar](val _1:V1) extends FactorWithStatistics1[V1] with Expectations {
  // TODO Set this up better for incremental estimation
  var mean = 0.0
  var variance = 1.0
  def pr(x:Double): Double = cc.factorie.generative.Gaussian.pr(x, mean, variance)
  def score(s:Statistics) = cc.factorie.generative.Gaussian.pr(s._1, mean, variance)
}


