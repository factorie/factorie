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
import cc.factorie.directed.{MultivariateGaussian, Gaussian}

/** Stores a marginal distribution containing a joint distribution over a set of variables.
    See also Summary, which stores a collection of Marginals.
    @author Andrew McCallum */
trait Marginal {
  def variables: Iterable[Var]
  def setToMaximize(implicit d:DiffList): Unit
}

trait Marginal1 extends Marginal {
  def variables: Iterable[Var] = Seq(_1)
  def _1:Var
}
trait Marginal2 extends Marginal {
  def variables: Iterable[Var] = Seq(_1, _2)
  def _1:Var
  def _2:Var
}

/** A Marginal associated with a Factor.
    The marginal distribution may be over all the neighboring variables of the factor,
    or, in the case in which inference varied some but not all of the neighbors,
    the marginal distribution will be over a subset of the neighboring variables of the factor.
    The tensorStatistics always covers all the neighboring variables of the factor.  */
trait FactorMarginal {
  def factor: Factor
  def tensorStatistics: Tensor
}


// Marginals over discrete variables

/** A Marginal in which all the variables are discrete (either singleton or tensors).
    @author Andrew McCallum */
trait DiscreteMarginal extends Marginal {
  def variables: Iterable[DiscreteTensorVar]
  def proportions: Proportions
}

// TODO Do we need a trait version of these? -akm
//trait DiscreteMar1[V1<:DiscreteVectorVar] extends DiscreteMar { def _1: V1; def proportions: Proportions1 }
trait DiscreteMarginal1[V1<:DiscreteTensorVar] extends Marginal1 with DiscreteMarginal {
  override def _1: V1
  override def variables = Seq(_1)
  def proportions: Proportions1
  //def variables = Seq(_1) // This is already inherited from AbstractAssignment1
  def value1: V1#Value = _1.domain.dimensionDomain(proportions.maxIndex).asInstanceOf[V1#Value]
  def setToMaximize(implicit d:DiffList): Unit = _1 match { case v:MutableDiscreteVar[_] => v.set(proportions.maxIndex); case _ => throw new Error }
}

class SimpleDiscreteMarginal1[V1<: DiscreteTensorVar](val _1: V1, initialProportions1: Proportions1=null) extends DiscreteMarginal1[V1] {
  private val _proportions = if (initialProportions1 ne null) initialProportions1 else new DenseProportions1(_1.domain.dimensionSize)
  def proportions = _proportions
  def incrementCurrentValue(w:Double): Unit = _1 match { case d:DiscreteVar => proportions.masses.+=(d.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
}

// This can't be a class because different Factor arity versions of this need to be mixed into BPFactor1.
trait DiscreteMarginal1Factor1[V1<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal1[V1] =>
  override def factor: Factor1[V1]
  def tensorStatistics = proportions
}

trait DiscreteMarginal1Factor2[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal1[V1] =>
  def _1: V1
  override def factor: Factor2[V1,V2]
  def tensorStatistics = {
    if (_1 eq factor._1)
      proportions outer factor._2.value
    else
      factor._1.value outer proportions
  }
}

trait DiscreteMarginal1Factor3[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal1[V1] =>
  def _1: V1
  override def factor: Factor3[V1,V2,V3]
  def tensorStatistics = {
    if (_1 eq factor._1)
      proportions outer (factor._2.asInstanceOf[TensorVar].value outer factor._3.asInstanceOf[TensorVar].value)
    else if (_1 eq factor._2)
      factor._1.asInstanceOf[TensorVar].value outer (proportions outer factor._3.asInstanceOf[TensorVar].value)
    else
      factor._1.asInstanceOf[TensorVar].value outer (factor._2.asInstanceOf[TensorVar].value outer proportions)
  }
}


class DiscreteMarginal2[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar](val _1:V1, val _2:V2, proportions2:Proportions2 = null) extends DiscreteMarginal with Marginal2 {
  def this(f:Factor2[V1,V2]) = this (f._1, f._2, null)
  override def variables = Seq(_1, _2)
  protected var _proportions: Proportions2 = if (proportions2 eq null) new DenseProportions2(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size) else proportions2 // must do this here because no access to _1 in default argument values
  def proportions: Proportions2 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2) match { case (d1:DiscreteVar,d2:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
}

trait DiscreteMarginal2Factor2[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar] extends DiscreteMarginal2[V1,V2] with FactorMarginal {
  override def factor: Factor2[V1,V2]
  def tensorStatistics = proportions
}

trait DiscreteMarginal2Factor3[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal2[V1,V2] =>
  val factor: Factor3[V1,V2,V3]
  def _1: V1
  def _2: V2
  def tensorStatistics = {
    if ((factor._1 eq _1) && (factor._2 eq _2))
      proportions outer factor._3.asInstanceOf[TensorVar].value
    else if ((factor._2 eq _1) && (factor._3 eq _2))
      factor._1.asInstanceOf[TensorVar].value outer proportions
    else throw new Error("As of 2013-05-10 the la package doesn't support the kind of outer and transpose we need")
  }
}

class DiscreteMarginal3[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar](val _1:V1, val _2:V2, val _3:V3, proportions3:Proportions3 = null) extends DiscreteMarginal {
  def this(f:Factor3[V1,V2,V3]) = this (f._1, f._2, f._3, null)
  def variables = Seq(_1, _2, _3)
  protected var _proportions: Proportions3 = if (proportions3 eq null) new DenseProportions3(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size) else proportions3 // must do this here because no access to _1 in default argument values
  def proportions: Proportions3 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2,_3) match { case (d1:DiscreteVar,d2:DiscreteVar,d3:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, d3.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
}

trait DiscreteMarginal3Factor3[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal3[V1,V2,V3] =>
  override def factor: Factor3[V1,V2,V3]
  def tensorStatistics = proportions
}

class DiscreteMarginal4[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar,V4<:DiscreteTensorVar](val _1:V1, val _2:V2, val _3:V3, val _4:V4, proportions4:Proportions4 = null) extends DiscreteMarginal {
  def this(f:Factor4[V1,V2,V3,V4]) = this(f._1, f._2, f._3, f._4, null)
  def variables = Seq(_1, _2, _3, _4)
  protected var _proportions: Proportions4 = if (proportions4 eq null) new DenseProportions4(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size, _4.domain.dimensionDomain.size) else proportions4 // must do this here because no access to _1 in default argument values
  def proportions: Proportions4 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2,_3,_4) match { case (d1:DiscreteVar,d2:DiscreteVar,d3:DiscreteVar,d4:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, d3.intValue, d4.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
}

trait DiscreteMarginal4Factor4[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar,V4<:DiscreteTensorVar] extends FactorMarginal {
  this: DiscreteMarginal4[V1,V2,V3,V4] =>
  override def factor: Factor4[V1,V2,V3,V4]
  def tensorStatistics = proportions
}


object DiscreteMarginal {
  def apply[V1<:DiscreteTensorVar](f:Factor1[V1]): DiscreteMarginal1[V1] = new SimpleDiscreteMarginal1(f._1)
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar](f:Factor2[V1,V2]): DiscreteMarginal2[V1,V2] = new DiscreteMarginal2(f) with DiscreteMarginal2Factor2[V1,V2] { val factor = f }
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar](f:Factor3[V1,V2,V3]): DiscreteMarginal3[V1,V2,V3] = new DiscreteMarginal3(f) with DiscreteMarginal3Factor3[V1,V2,V3] { val factor = f }
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar,V4<:DiscreteTensorVar](f:Factor4[V1,V2,V3,V4]): DiscreteMarginal4[V1,V2,V3,V4] = new DiscreteMarginal4(f) with DiscreteMarginal4Factor4[V1,V2,V3,V4] { val factor = f }
  def apply(f:Factor): DiscreteMarginal = f match {
    case f:Factor1[DiscreteTensorVar] => apply(f)
    case f:Factor2[DiscreteTensorVar,DiscreteTensorVar] => apply(f)
    case f:Factor3[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => apply(f)
    case f:Factor4[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => apply(f)
  }
  def apply(f:Factor, p:Proportions): DiscreteMarginal = f match {
    case f:Factor1[DiscreteTensorVar] => new SimpleDiscreteMarginal1(f._1, p.asInstanceOf[Proportions1])
    case f:Factor2[DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal2(f._1, f._2, p.asInstanceOf[Proportions2]) with DiscreteMarginal2Factor2[DiscreteTensorVar,DiscreteTensorVar] { val factor = f }
    case f:Factor3[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal3(f._1, f._2, f._3, p.asInstanceOf[Proportions3]) with DiscreteMarginal3Factor3[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] { val factor = f }
    case f:Factor4[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal4(f._1, f._2, f._3, f._4, p.asInstanceOf[Proportions4]) with DiscreteMarginal4Factor4[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] { val factor = f }
  }
}

class DiscreteSeqMarginal[V<:DiscreteSeqVariable](val _1:V, val proportionsSeq:Seq[Proportions1]) extends Marginal1 {
  override def variables = Seq(_1)
  def setToMaximize(implicit d:DiffList): Unit = {
    var i = 0
    while (i < _1.length) {
      _1.set(i, proportionsSeq(i).maxIndex)(d)
      i += 1
    }
  }
}


// Marginals over Proportions

class ProportionsDirichletMarginal1[V<:ProportionsVar](_1:V, val masses:Masses1) extends Marginal {
  def variables = Seq(_1)
  def setToMaximize(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Handling of DiffList here not yet implemented.")
    _1.tensor := masses
  }
}


// Marginals over Reals
// TODO Should there also be a Marginal over DoubleVar?  Should they be unified?  e.g. ScalarMarginal?

trait RealMarginal1[V1<:RealVar] extends Marginal {
  val _1:V1
  def mean:Double
  def pr(x:Double): Double
}

class RealSpikeMarginal1[V1<:RealVar](val _1:V1, val mean:Double) extends AbstractAssignment1[V1] with Marginal {
  def pr(x:Double): Double = if (x == mean) 1.0 else 0.0
  override def globalize(implicit d:DiffList): Unit = _1 match { case v:RealVariable => v.set(mean) }
  final def value1: V1#Value = mean.asInstanceOf[V1#Value] // For AbstractAssignment1
}

// Gaussian Marginal

class RealGaussianMarginal1[V1<:RealVar](val _1:V1) extends Marginal {
  def variables = Seq(_1)
  // TODO Set this up better for incremental estimation
  var mean = 0.0
  var variance = 1.0
  def pr(x:Double): Double = Gaussian.pr(x, mean, variance)
  def setToMaximize(implicit d:DiffList): Unit = _1 match { case v:RealVariable => v.set(mean) }
}

class MultivariateGaussianMarginal1[V1 <: MutableTensorVar[Tensor1]](val _1: V1) extends Marginal {
  def variables = Seq(_1)
  // TODO Set this up better for incremental estimation
  var mean = new DenseTensor1(_1.value.length, 0.0)
  var variance = new DenseTensor2(Array.tabulate(_1.value.length, _1.value.length)((i, j) => if (i == j) 1.0 else 0.0))
  def pr(x: Tensor1): Double = MultivariateGaussian.pr(x, mean, variance)
  def setToMaximize(implicit d: DiffList): Unit = _1.set(mean)
}