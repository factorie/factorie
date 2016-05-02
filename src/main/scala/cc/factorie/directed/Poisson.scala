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

import cc.factorie._
import cc.factorie.variable.{DoubleVar, IntegerVar}

object Poisson extends DirectedFamily2[IntegerVar,DoubleVar] {
  case class Factor(override val _1:IntegerVar, override val _2:DoubleVar) extends super.Factor(_1, _2) {
    def pr(k:Int, mean:Double): Double = math.pow(mean, k) * math.exp(-mean) / maths.factorial(k)
    //def pr(s:Statistics): Double = pr(s._1, s._2)
    def sampledValue(mean:Double)(implicit random: scala.util.Random): Int = maths.nextPoisson(mean)(random).toInt
    //def sampledValue(s:Statistics): Int = sampledValue(s._2)
  }
  def newFactor(a:IntegerVar, b:DoubleVar) = Factor(a, b)
}
