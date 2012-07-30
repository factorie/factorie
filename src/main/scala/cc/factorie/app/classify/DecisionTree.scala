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

package cc.factorie.app.classify
import cc.factorie._
import cc.factorie.optimize._
//import cc.factorie.la.Tensor

class ID3DecisionTreeTrainer extends ClassifierTrainer {
  var iterations = 10
  var learningRateDecay = 0.9
//  class ID3DecisionTreeTemplate[L<:LabelVariable[_],F<:DiscreteTensorVar](val labelToFeatures:L=>F, val labelDomain:DiscreteDomain /*with Domain[L#ValueType]*/, val featureDomain:DiscreteTensorDomain)(implicit m1:Manifest[L], m2:Manifest[F]) extends DecisionTreeTemplateWithStatistics2[L,F]()(m1,m2) with ID3DecisionTreeStatistics2[L#ValueType,F#ValueType] {
//    def statisticsDomains = Tuple(labelDomain, featureDomain)
//    def unroll1(label:L) = Factor(label, labelToFeatures(label))
//    def unroll2(features:F) = throw new Error("Cannot unroll from feature variables.")
//  }

  def train[L<:LabelVariable[_],F<:DiscreteTensorVar](il:LabelList[L,F])(implicit lm:Manifest[L], fm:Manifest[F]): Classifier[L] = {
    //val dmodel = new ID3DecisionTreeTemplate[L](il.labelToFeatures, il.labelDomain, il.instanceDomain)
    //dmodel.train(il)
    //new Classifier[L] { val model = dmodel; val labelDomain = il.head.domain }
    null
  }
}
