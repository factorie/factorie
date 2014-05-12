/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.relation

import cc.factorie._
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2}

object RelationModel {
  import RelationVariables._

  class LocalTemplate(val model: Parameters) extends DotTemplateWithStatistics2[RelationLabel, Features] {
    //override def statisticsDomains = ((RelationLabelDomain, RelationFeaturesDomain))
    val weights = model.Weights(new cc.factorie.la.DenseTensor2(RelationLabelDomain.size, RelationFeaturesDomain.dimensionSize))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.features)
    def unroll2(v: Features) = throw new Error("features don't change")
  }

  class Arg1Template(val model: Parameters) extends DotTemplateWithStatistics2[RelationLabel, ArgFeatures] {
    //override def statisticsDomains = ((RelationLabelDomain, RelationArgFeaturesDomain))
    val weights = model.Weights(new cc.factorie.la.DenseTensor2(RelationLabelDomain.size, RelationArgFeaturesDomain.dimensionSize))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.arg1Features)
    def unroll2(v: ArgFeatures) = throw new Error("features don't change")
  }

  class Arg2Template(val model: Parameters) extends DotTemplateWithStatistics2[RelationLabel, ArgFeatures] {
    //override def statisticsDomains = ((RelationLabelDomain, RelationArgFeaturesDomain))
    val weights = model.Weights(new cc.factorie.la.DenseTensor2(RelationLabelDomain.size, RelationArgFeaturesDomain.dimensionSize))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.arg2Features)
    def unroll2(v: ArgFeatures) = throw new Error("features don't change")
  }
}