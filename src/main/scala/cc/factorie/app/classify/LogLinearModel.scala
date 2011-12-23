package cc.factorie.app.classify
import cc.factorie._

// Bias weights
class LogLinearTemplate1[L<:DiscreteVar](implicit lm:Manifest[L]) extends TemplateWithDotStatistics1[L]()

// Label-Feature weights
class LogLinearTemplate2[L<:DiscreteVar,F<:DiscreteVectorVar](lf:L=>F, fl:F=>L)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateWithDotStatistics2[L,F]() {
  def this(lf:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."))
  def unroll1(label: L) = Factor(label, lf(label))
  def unroll2(features: F) = Factor(fl(features), features)
}


class LogLinearModel[L<:DiscreteVar,F<:DiscreteVectorVar](lf:L=>F, fl:F=>L)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateModel {
  def this(lf:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."))
  val biasTemplate = new LogLinearTemplate1[L]
  val evidenceTemplate = new LogLinearTemplate2[L,F](lf, fl)
  this += biasTemplate
  this += evidenceTemplate
}