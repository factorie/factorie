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

import cc.factorie.variable._

object DirichletMixture extends DirectedFamily3[ProportionsVariable,Mixture[MassesVariable],DiscreteVariable] {
  case class Factor(override val _1:ProportionsVariable, override val _2:Mixture[MassesVariable], override val _3:DiscreteVariable) extends super.Factor(_1, _2, _3) with MixtureFactor {
    def gate = _3
    def pr(child:Proportions,  mixture:Seq[Masses], z:DiscreteValue) = Dirichlet.pr(child, mixture(z.intValue))
    def sampledValue(mixture:Seq[Masses], z:DiscreteValue)(implicit random: scala.util.Random): Proportions = Dirichlet.sampledValue(mixture(z.intValue))
    def prChoosing(child:Proportions,  mixture:Seq[Masses], mixtureIndex:Int): Double = Dirichlet.pr(child, mixture(mixtureIndex))
    def sampledValueChoosing(mixture:Seq[Masses], mixtureIndex:Int)(implicit random: scala.util.Random): Proportions = Dirichlet.sampledValue(mixture(mixtureIndex)) //.asInstanceOf[ProportionsVar#Value]
  }
  def newFactor(a:ProportionsVariable, b:Mixture[MassesVariable], c:DiscreteVariable) = Factor(a, b, c)
}

