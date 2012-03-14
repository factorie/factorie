package cc.factorie.app.classify
import cc.factorie._
import collection.mutable.HashMap

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
  val factorCache = HashMap[Variable, Seq[Factor]]()
  override def factors(vs:Iterable[Variable]): Seq[Factor] =  {
    assert(vs.size == 1)
    val v = vs.head
    if (!factorCache.contains(v)) {
      factorCache(v) = super.factors(vs)
    }
    factorCache(v)
  }
  override def factors1(v:Variable): Seq[Factor] = {
    if (!factorCache.contains(v)) {
      factorCache(v) = super.factors1(v)
    }
    factorCache(v)
  }
}

// TODO consider requiring the statisticsDomains for label and features when creating the model.
// otherwise we can get error "You must override statisticsDomains if you want..."
