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
import cc.factorie._

package object generative {

  type GenerativeFactor = GenerativeTemplate#Factor

  val defaultGenerativeModel = new TemplateModel(
      new DiscreteTemplate, 
      new DiscreteMixtureTemplate,
      new PlatedDiscreteTemplate,
      new PlatedDiscreteMixtureTemplate,
      new GaussianTemplate)

  implicit def seqDouble2ProportionsValue(s:Seq[Double]): ProportionsValue = new ProportionsValue {
    val value: IndexedSeq[Double] = s.toIndexedSeq
    def apply(i:Int) = value(i)
    def length = value.length
    //def sampleInt = maths.nextDiscrete(value)(cc.factorie.random)
  }
  
  //implicit val denseDirichletEstimator = new DenseDirichletEstimator
  //implicit val mutableProportionsEstimator = new MutableProportionsEstimator

}
