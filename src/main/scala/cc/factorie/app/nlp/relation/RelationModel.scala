package cc.factorie.app.nlp.relation

import cc.factorie.TemplateWithDotStatistics2

/**
 * @author sameer
 * @date 12/30/11
 */

object RelationModel {
  import RelationVariables._

  class LocalTemplate extends TemplateWithDotStatistics2[RelationLabel, Features] {
    override def statisticsDomains = ((RelationLabelDomain, RelationFeaturesDomain))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.features)
    def unroll2(v: Features) = throw new Error("features don't change")
  }

  class Arg1Template extends TemplateWithDotStatistics2[RelationLabel, ArgFeatures] {
    override def statisticsDomains = ((RelationLabelDomain, RelationArgFeaturesDomain))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.arg1Features)
    def unroll2(v: ArgFeatures) = throw new Error("features don't change")
  }

  class Arg2Template extends TemplateWithDotStatistics2[RelationLabel, ArgFeatures] {
    override def statisticsDomains = ((RelationLabelDomain, RelationArgFeaturesDomain))
    def unroll1(v: RelationLabel) = Factor(v, v.mention.arg2Features)
    def unroll2(v: ArgFeatures) = throw new Error("features don't change")
  }
}