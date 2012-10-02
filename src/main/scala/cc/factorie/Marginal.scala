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
//import scala.actors.Actor
//import scala.actors.Actor._

/** Stores a marginal distribution containing a joint distribution over a set of variables.
    See also Summary, which stores a collection of Marginals. */
trait Marginal {
  def variables: Iterable[Variable]
  def setToMaximize(implicit d:DiffList): Unit
}

// Marginals over discrete variables

trait DiscreteMarginal extends Marginal {
  def _1: DiscreteTensorVar // TODO Consider removing this.
  def variables: Iterable[DiscreteTensorVar]
  def proportions: Proportions
}
// TODO Do we need a trait version of these? -akm
//trait DiscreteMar1[V1<:DiscreteVectorVar] extends DiscreteMar { def _1: V1; def proportions: Proportions1 }
class DiscreteMarginal1[V1<:DiscreteTensorVar](val _1:V1, proportions1:Proportions1 = null) extends DiscreteMarginal with AbstractAssignment1[V1] {
  def this(f:Factor1[V1]) = this (f._1, null)
  //def variables = Seq(_1)
  def value1: V1#Value = _1.domain.dimensionDomain(proportions.maxIndex).asInstanceOf[V1#Value]
  protected var _proportions = proportions1 // Cannot use default arguments to create because no access to _1 in default argument values
  def proportions: Proportions1 = {
    if (proportions1 eq null) _proportions = new DenseProportions1(_1.domain.dimensionDomain.size)
    _proportions
  }
  def incrementCurrentValue(w:Double): Unit = _1 match { case d:DiscreteVar => proportions.masses.+=(d.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  override def globalize(implicit d:DiffList): Unit = _1 match { case v:MutableDiscreteVar[_] => v.set(proportions.maxIndex); case _ => throw new Error }
//  /** An Actor that can receive increment requests in a thread-safe manner. */
//  // TODO How/when does this get garbage collected?  See http://thread.gmane.org/gmane.comp.lang.scala.user/20255/focus=20391
//  lazy val incrementer = actor {
//    loop {
//      react {
//        case dv:DiscreteValue => proportions.masses.+=(dv.intValue, 1.0)
//        case t:Tensor1 => proportions.masses.+=(t)
//        case i:Int => proportions.masses.+=(i, 1.0)
//        case (i:Int, d:Double) => proportions.masses.+=(i, d)
//      }
//    }
//  }
}
class DiscreteMarginal2[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar](val _1:V1, val _2:V2, proportions2:Proportions2 = null) extends DiscreteMarginal {
  def this(f:Factor2[V1,V2]) = this (f._1, f._2, null)
  def variables = Seq(_1, _2)
  protected var _proportions = if (proportions2 eq null) new DenseProportions2(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size) else proportions2 // must do this here because no access to _1 in default argument values
  def proportions: Proportions2 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2) match { case (d1:DiscreteVar,d2:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
//  /** An Actor that can receive increment requests in a thread-safe manner. */
//  // TODO How/when does this get garbage collected?  See http://thread.gmane.org/gmane.comp.lang.scala.user/20255/focus=20391
//  lazy val incrementer = actor {
//    loop {
//      react {
//        case t:Tensor2 => proportions.masses.+=(t)
//        case (i:Int, j:Int, d:Double) => proportions.masses.+=(i, j, d)
//      }
//    }
//  }
}
class DiscreteMarginal3[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar](val _1:V1, val _2:V2, val _3:V3, proportions3:Proportions3 = null) extends DiscreteMarginal {
  def this(f:Factor3[V1,V2,V3]) = this (f._1, f._2, f._3, null)
  def variables = Seq(_1, _2, _3)
  protected var _proportions: Proportions3 = if (proportions3 eq null) new DenseProportions3(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size) else proportions3 // must do this here because no access to _1 in default argument values
  def proportions: Proportions3 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2,_3) match { case (d1:DiscreteVar,d2:DiscreteVar,d3:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, d3.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
}
class DiscreteMarginal4[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar,V4<:DiscreteTensorVar](val _1:V1, val _2:V2, val _3:V3, val _4:V4, proportions4:Proportions4 = null) extends DiscreteMarginal {
  def this(f:Factor4[V1,V2,V3,V4]) = this (f._1, f._2, f._3, f._4, null)
  def variables = Seq(_1, _2, _3, _4)
  protected var _proportions: Proportions4 = if (proportions4 eq null) new DenseProportions4(_1.domain.dimensionDomain.size, _2.domain.dimensionDomain.size, _3.domain.dimensionDomain.size, _4.domain.dimensionDomain.size) else proportions4 // must do this here because no access to _1 in default argument values
  def proportions: Proportions4 = _proportions
  def incrementCurrentValue(w:Double): Unit = (_1,_2,_3,_4) match { case (d1:DiscreteVar,d2:DiscreteVar,d3:DiscreteVar,d4:DiscreteVar) => proportions.masses.+=(d1.intValue, d2.intValue, d3.intValue, d4.intValue, w); case d:DiscreteTensorVar => throw new Error("Not yet implemented") }
  def setToMaximize(implicit d:DiffList): Unit = throw new Error("Not yet implemented")
}


object DiscreteMarginal {
  def apply[V1<:DiscreteTensorVar](f:Factor1[V1]): DiscreteMarginal1[V1] = new DiscreteMarginal1(f)
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar](f:Factor2[V1,V2]): DiscreteMarginal2[V1,V2] = new DiscreteMarginal2(f)
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar](f:Factor3[V1,V2,V3]): DiscreteMarginal3[V1,V2,V3] = new DiscreteMarginal3(f)
  def apply[V1<:DiscreteTensorVar,V2<:DiscreteTensorVar,V3<:DiscreteTensorVar,V4<:DiscreteTensorVar](f:Factor4[V1,V2,V3,V4]): DiscreteMarginal4[V1,V2,V3,V4] = new DiscreteMarginal4(f)
  def apply(f:Factor): DiscreteMarginal = f match {
    case f:Factor1[DiscreteTensorVar] => apply(f)
    case f:Factor2[DiscreteTensorVar,DiscreteTensorVar] => apply(f)
    case f:Factor3[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => apply(f)
    case f:Factor4[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => apply(f)
  }
  def apply(f:Factor, p:Proportions): DiscreteMarginal = f match {
    case f:Factor1[DiscreteTensorVar] => new DiscreteMarginal1(f._1, p.asInstanceOf[Proportions1])
    case f:Factor2[DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal2(f._1, f._2, p.asInstanceOf[Proportions2])
    case f:Factor3[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal3(f._1, f._2, f._3, p.asInstanceOf[Proportions3])
    case f:Factor4[DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar,DiscreteTensorVar] => new DiscreteMarginal4(f._1, f._2, f._3, f._4, p.asInstanceOf[Proportions4])
  }
}

class DiscreteSeqMarginal[V<:DiscreteSeqVariable](val _1:V, val proportionsSeq:Seq[Proportions1]) extends Marginal {
  def variables = Seq(_1)
  def setToMaximize(implicit d:DiffList): Unit = {
    var i = 0;
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

class RealSpikeMarginal1[V1<:RealVar](val _1:V1, val mean:Double) extends AbstractAssignment1[V1] {
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
  def pr(x:Double): Double = cc.factorie.generative.Gaussian.pr(x, mean, variance)
  def setToMaximize(implicit d:DiffList): Unit = _1 match { case v:RealVariable => v.set(mean) }
}


