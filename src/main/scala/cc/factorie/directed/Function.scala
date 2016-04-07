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

import cc.factorie.variable.{DoubleVar, VarWithDeterministicValue}

trait DeterministicFunction extends VarWithDeterministicValue {
  //overrride var parentFactor: DirectedFactor = null
  //override def isDeterministic = true
}
trait DoubleFunction extends DeterministicFunction with DoubleVar

class DoubleSum(a:DoubleVar, b:DoubleVar)(implicit val model: MutableDirectedModel) extends DeterministicFunction with DoubleVar {
  this ~ DoubleSum2(a, b)
  def doubleValue = a.doubleValue + b.doubleValue
}

trait FunctionFactor extends DirectedFactor {
  type ValueType 
  /*override*/ def isDeterministic = true
  def value: ValueType
}
trait DoubleFunctionFactor extends FunctionFactor { type ValueType = Double }
object DoubleSum2 extends DirectedFamily3[DoubleFunction,DoubleVar,DoubleVar] {
  case class Factor(override val _1:DoubleFunction, override val _2:DoubleVar, override val _3:DoubleVar) extends super.Factor(_1, _2, _3) {
    def pr(child:Double, p1:Double, p2:Double): Double = if (child == p1 + p2) 1.0 else 0.0
    def sampledValue(p1:Double, p2:Double)(implicit random: scala.util.Random=null): Double = p1 + p2
    def value: Double = _2.doubleValue + _3.doubleValue 
  }
  def newFactor(_1:DoubleFunction, _2:DoubleVar, _3:DoubleVar) = Factor(_1, _2, _3)
}
// val a = new RealFunction ~ RealSum2(b, c)
