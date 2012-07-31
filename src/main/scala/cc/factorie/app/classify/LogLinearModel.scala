package cc.factorie.app.classify
import cc.factorie._
import collection.mutable.HashMap

// Bias weights
class LogLinearTemplate1[L<:DiscreteVar](val labelStatisticsDomain:DiscreteDomain)(implicit lm:Manifest[L]) extends TemplateWithDotStatistics1[L]() {
  def statisticsDomains = Tuple(labelStatisticsDomain)
}

// Label-Feature weights
class LogLinearTemplate2[L<:DiscreteVar,F<:DiscreteTensorVar](lf:L=>F, fl:F=>L, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateWithDotStatistics2[L,F]() {
  def this(lf:L=>F, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."), labelStatisticsDomain, featureStatisticsDomain)
  def statisticsDomains = Tuple(labelStatisticsDomain, featureStatisticsDomain)
  def unroll1(label: L) = Factor(label, lf(label))
  def unroll2(features: F) = Factor(fl(features), features)
}

// TODO Consider renaming this DotModel, like DotMaximumLikelihood
class LogLinearModel[L<:DiscreteVar,F<:DiscreteTensorVar](lf:L=>F, fl:F=>L, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateModel {
  def this(lf:L=>F, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."), labelStatisticsDomain, featureStatisticsDomain)
  val biasTemplate = new LogLinearTemplate1[L](labelStatisticsDomain)
  val evidenceTemplate = new LogLinearTemplate2[L,F](lf, fl, labelStatisticsDomain, featureStatisticsDomain)
  this += biasTemplate
  this += evidenceTemplate
  val factorCache = HashMap[Variable, Iterable[Factor]]()
  override def factors(vs:Iterable[Variable]): Iterable[Factor] =  {
    if (vs.size == 1) factors(vs.head)
    else vs.flatMap(factors).toSeq
  }
  override def factors(v:Variable): Iterable[Factor] = {
    if (!factorCache.contains(v)) {
      factorCache(v) = super.factors(v)
    }
    factorCache(v)
  }
  override def factorsOfFamilies[F<:Family](variables:Iterable[Variable], families:Seq[F]): Iterable[F#Factor] = {
    if ((families.length == 2) &&
        (families(0) == biasTemplate || families(1) == biasTemplate) &&
        (families(0) == evidenceTemplate || families(1) == evidenceTemplate))
      factors(variables).asInstanceOf[Seq[F#Factor]]
    else {
      filterByFamilies(factors(variables), families)
    }
  }
  override def score(variables:Iterable[Variable]): Double = {
    var s = 0.0
    // TODO We can make this more efficient
    for (f <- factors(variables)) s += f.score
    s
  }

}

// TODO consider requiring the statisticsDomains for label and features when creating the model.
// otherwise we can get error "You must override statisticsDomains if you want..."
