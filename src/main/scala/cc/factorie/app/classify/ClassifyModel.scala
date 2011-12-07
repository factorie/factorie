package cc.factorie.app.classify
import cc.factorie._

class ClassifyTemplate[L<:DiscreteVar,F<:DiscreteVectorVar](lf:L=>F, fl:F=>L)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateWithDotStatistics2[L,F]() {
  def unroll1(label: L) = Factor(label, lf(label))
  def unroll2(features: F) = Factor(fl(features), features)
}

class ClassifyModel[L<:DiscreteVar,F<:DiscreteVectorVar](lf:L=>F, fl:F=>L)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateModel(
  new ClassifyTemplate[L,F](lf, fl)
)