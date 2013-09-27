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