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

object DirichletMixture extends GenerativeFamily3[Proportions,Mixture[Masses],GateVariable] {
  case class Factor(_1:Proportions, _2:Mixture[Masses], _3:GateVariable) extends super.Factor with MixtureFactor {
    def gate = _3
    def pr(s:StatisticsType) = Dirichlet.pr(s._1, s._2(s._3.intValue))
    def sampledValue(s:StatisticsType): ProportionsValue = Dirichlet.sampledValue(s._2(s._3.intValue))
    def prChoosing(s:StatisticsType, mixtureIndex:Int): Double = Dirichlet.pr(s._1, s._2(mixtureIndex))
    def sampledValueChoosing(s:Statistics, mixtureIndex:Int): ProportionsValue = Dirichlet.sampledValue(s._2(mixtureIndex))
  }
  def newFactor(a:Proportions, b:Mixture[Masses], c:GateVariable) = Factor(a, b, c)
}

