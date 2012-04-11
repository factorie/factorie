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
  var __statisticsDomains = Seq.empty[DiscreteVectorDomain]
  override def statisticsDomains = {
    if (__statisticsDomains.isEmpty)
      super.statisticsDomains
    else
      __statisticsDomains
  }
}


class LogLinearModel[L<:DiscreteVar,F<:DiscreteVectorVar](lf:L=>F, fl:F=>L)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateModel {
  def this(lf:L=>F)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."))
  val biasTemplate = new LogLinearTemplate1[L]
  val evidenceTemplate = new LogLinearTemplate2[L,F](lf, fl)
  this += biasTemplate
  this += evidenceTemplate
  val factorCache = HashMap[Variable, Seq[Factor]]()
  override def factors(vs:Iterable[Variable]): Seq[Factor] =  {
    if (vs.size == 1) factors1(vs.head)
    else vs.flatMap(factors1).toSeq
  }
  override def factors1(v:Variable): Seq[Factor] = {
    if (!factorCache.contains(v)) {
      factorCache(v) = super.factors1(v)
    }
    factorCache(v)
  }
  override def factorsOfFamilies[F<:Family](variables:Iterable[Variable], families:Seq[F]): Seq[F#Factor] = {
    if ((families.length == 2) &&
        (families(0) == biasTemplate || families(1) == biasTemplate) &&
        (families(0) == evidenceTemplate || families(1) == evidenceTemplate))
      factors(variables).asInstanceOf[Seq[F#Factor]]
    else {
      filterByFamilies(factors(variables), families)
    }
  }
  override def score(variables:Iterable[Variable]): Double = {
    val f = factors(variables)
    var s = 0.0
    var i = 0
    while (i < f.length) {
      s += f(i).score
      i += 1
    }
    s
  }

}

// TODO consider requiring the statisticsDomains for label and features when creating the model.
// otherwise we can get error "You must override statisticsDomains if you want..."
