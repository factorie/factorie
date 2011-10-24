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

package cc.factorie.generative
import cc.factorie._

trait DeterministicFunction extends Variable {
  //overrride var parentFactor: GenerativeFactor = null
  override def isDeterministic = true
}
trait RealFunction extends DeterministicFunction with RealVar

class RealSum(a:RealVar, b:RealVar) extends DeterministicFunction with RealVar {
  this ~ RealSum2(a, b)
  def doubleValue = a.doubleValue + b.doubleValue
}

trait FunctionFactor extends GenerativeFactor {
  type ValueType 
  /*override*/ def isDeterministic = true
  def value: ValueType
}
trait RealFunctionFactor extends FunctionFactor { type ValueType = Double }
object RealSum2 extends GenerativeFamily3[RealFunction,RealVar,RealVar] {
  case class Factor(_1:RealFunction, _2:RealVar, _3:RealVar) extends super.Factor {
    def pr(s:Statistics): Double = if (_1.doubleValue == _2.doubleValue + _3.doubleValue) 1.0 else 0.0
    def sampledValue(s:Statistics): Double = s._2 + s._3
    def value: Double = _2.doubleValue + _3.doubleValue 
  }
  def newFactor(_1:RealFunction, _2:RealVar, _3:RealVar) = Factor(_1, _2, _3)
}
// val a = new RealFunction ~ RealSum2(b, c)
