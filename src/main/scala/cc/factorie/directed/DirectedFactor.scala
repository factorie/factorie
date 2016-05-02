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

package cc.factorie.directed

import cc.factorie.model._
import cc.factorie.variable.{MutableVar, Var}

trait DirectedFactor extends Factor {
  type ChildType <: Var
  def child: ChildType
  def parents: Seq[Var]
  //def pr(s:StatisticsType): Double
  def pr: Double // = pr(statistics)
  //def logpr(s:StatisticsType): Double = math.log(pr(s))
  def logpr: Double = math.log(pr) // logpr(statistics)
  //def score: Double = logpr
  //def sampledValue(s:StatisticsType): Any
  def sampledValue(implicit random: scala.util.Random): Any // = sampledValue(statistics)
  // TODO Consider removing these methods because we'd have specialized code in the inference recipes.
  /** Update sufficient statistics in collapsed parents, using current value of child, with weight.  Return false on failure. */
  // TODO Consider passing a second argument which is the value of the child to use in the update
  def updateCollapsedParents(weight:Double): Boolean = throw new Error(factorName+": Collapsing parent not implemented in " + this.getClass.getName)
  def updateCollapsedChild(): Boolean = throw new Error(factorName+": Collapsing child not implemented.")
  def resetCollapsedChild(): Boolean = throw new Error(factorName+": Resetting child not implemented.")
}

class GeneratedVarWrapper[V<:Var](val v:V) {
  /** Create a new DirectedFactor, make it the "parent" generating factor for this variable,
      and add this new factor to the given model. */
  def ~[V2<:Var](partialFactor: V2 => DirectedFactor)(implicit model:MutableDirectedModel): V = {
    model += partialFactor(v.asInstanceOf[V2])
    v
  }
}

class GeneratedMutableVarWrapper[V<:MutableVar](val v:V) {
  /** Create a new DirectedFactor, make it the "parent" generating factor for this variable,
      add this new factor to the given model, 
      and also assign the variable a new value randomly drawn from this factor. */
  def :~[V2<:Var](partialFactor: V2 => DirectedFactor)(implicit model:MutableDirectedModel, random: scala.util.Random): V = {
    model += partialFactor(v.asInstanceOf[V2])
    v.set(model.parentFactor(v).sampledValue.asInstanceOf[v.Value])(null)
    v
  }
}



trait RealGeneratingFactor extends DirectedFactor {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}

trait IntGeneratingFactor extends DirectedFactor {
  def sampleInt: Int
  def pr(x:Int): Double
  def logpr(x:Int): Double
}

abstract class DirectedFactorWithStatistics1[C<:Var](override val _1:C) extends FactorWithStatistics1[C](_1) with DirectedFactor {
  type ChildType = C
  def child = _1
  def parents: Seq[Var] = Nil
  def score(v1:C#Value): Double = logpr(v1:C#Value)
  def pr(v1:C#Value): Double
  def logpr(v1:C#Value): Double = math.log(pr(v1))
  def pr: Double = pr(_1.value.asInstanceOf[C#Value])
  override def sampledValue(implicit random: scala.util.Random): C#Value
}

abstract class DirectedFactorWithStatistics2[C<:Var,P1<:Var](override val _1:C, override val _2:P1) extends TupleFactorWithStatistics2[C,P1](_1, _2) with DirectedFactor {
  type ChildType = C
  def child = _1
  def parents = Seq(_2)
  def score(v1:C#Value, v2:P1#Value): Double = logpr(v1, v2)
  def pr(v1:C#Value, v2:P1#Value): Double
  def logpr(v1:C#Value, v2:P1#Value): Double = math.log(pr(v1, v2))
  def pr: Double = pr(_1.value.asInstanceOf[C#Value], _2.value.asInstanceOf[P1#Value])
  def sampledValue(p1:P1#Value)(implicit random: scala.util.Random): C#Value
  def sampledValue(implicit random: scala.util.Random): C#Value = sampledValue(_2.value.asInstanceOf[P1#Value])
  // TODO Consider this:
  //def parents = _2 match { case vars:Vars[Parameter] => vars; case _ => Seq(_2) }
}

abstract class DirectedFactorWithStatistics3[C<:Var,P1<:Var,P2<:Var](override val _1:C, override val _2:P1, override val _3:P2) extends TupleFactorWithStatistics3[C,P1,P2](_1, _2, _3) with DirectedFactor {
  type ChildType = C
  def child = _1
  def parents = Seq(_2, _3)
  def score(v1:C#Value, v2:P1#Value, v3:P2#Value): Double = logpr(v1, v2, v3)
  def pr(v1:C#Value, v2:P1#Value, v3:P2#Value): Double
  def logpr(v1:C#Value, v2:P1#Value, v3:P2#Value): Double = math.log(pr(v1, v2, v3))
  def pr: Double = pr(_1.value.asInstanceOf[C#Value], _2.value.asInstanceOf[P1#Value], _3.value.asInstanceOf[P2#Value])
  def sampledValue(p1:P1#Value, p2:P2#Value)(implicit random: scala.util.Random): C#Value
  def sampledValue(implicit random: scala.util.Random): C#Value = sampledValue(_2.value.asInstanceOf[P1#Value], _3.value.asInstanceOf[P2#Value])
}

abstract class DirectedFactorWithStatistics4[C<:Var,P1<:Var,P2<:Var,P3<:Var](override val _1:C, override val _2:P1, override val _3:P2, override val _4:P3) extends TupleFactorWithStatistics4[C,P1,P2,P3](_1, _2, _3, _4) with DirectedFactor {
  type ChildType = C
  def child = _1
  def parents = Seq(_2, _3, _4)
  def score(v1:C#Value, v2:P1#Value, v3:P2#Value, v4:P3#Value): Double = logpr(v1, v2, v3, v4)
  def pr(v1:C#Value, v2:P1#Value, v3:P2#Value, v4:P3#Value): Double
  def logpr(v1:C#Value, v2:P1#Value, v3:P2#Value, v4:P3#Value): Double = math.log(pr(v1, v2, v3, v4))
  def pr: Double = pr(_1.value.asInstanceOf[C#Value], _2.value.asInstanceOf[P1#Value], _3.value.asInstanceOf[P2#Value], _4.value.asInstanceOf[P3#Value])
  def sampledValue(p1:P1#Value, p2:P2#Value, p3:P3#Value)(implicit random: scala.util.Random): C#Value
  def sampledValue(implicit random: scala.util.Random): C#Value = sampledValue(_2.value.asInstanceOf[P1#Value], _3.value.asInstanceOf[P2#Value], _4.value.asInstanceOf[P3#Value])
}

trait DirectedFamily1[Child<:Var] {
  type C = Child
  abstract class Factor(override val _1:Child) extends DirectedFactorWithStatistics1[C](_1)
  def newFactor(c:C): Factor
  def apply(): C => Factor = newFactor(_)
}

trait DirectedFamily2[Child<:Var,Parent1<:Var] {
  type C = Child
  type P1 = Parent1
  abstract class Factor(override val _1:Child, override val _2:Parent1) extends DirectedFactorWithStatistics2[C,P1](_1, _2)
  def newFactor(c:C, p1:P1): Factor
  def apply(p1: P1): C => Factor = newFactor(_, p1)
}

trait DirectedFamily3[Child<:Var,Parent1<:Var,Parent2<:Var] {
  type C = Child
  type P1 = Parent1
  type P2 = Parent2
  abstract class Factor(override val _1:Child, override val _2:Parent1, override val _3:Parent2) extends DirectedFactorWithStatistics3[C,P1,P2](_1, _2, _3)
  def newFactor(c:C, p1:P1, p2:P2): Factor
  def apply(p1: P1, p2: P2): C => Factor = newFactor(_, p1, p2)
}

trait DirectedFamily4[Child<:Var,Parent1<:Var,Parent2<:Var,Parent3<:Var] {
  type C = Child
  type P1 = Parent1
  type P2 = Parent2
  type P3 = Parent3
  abstract class Factor(override val _1:Child, override val _2:Parent1, override val _3:Parent2, override val _4:Parent3) extends DirectedFactorWithStatistics4[C,P1,P2,P3](_1, _2, _3, _4)
  def newFactor(c:C, p1:P1, p2:P2, p3:P3): Factor
  def apply(p1: P1, p2: P2, p3: P3): C => Factor = newFactor(_, p1, p2, p3)
}

