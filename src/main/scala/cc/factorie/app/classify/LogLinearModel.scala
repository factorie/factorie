package cc.factorie.app.classify
import cc.factorie._
import collection.mutable

// Bias weightsSet
class LogLinearTemplate1[L<:DiscreteVar](val model: Parameters, val labelStatisticsDomain:DiscreteDomain)(implicit lm:Manifest[L]) extends DotTemplateWithStatistics1[L]() {
  //def statisticsDomains = Tuple1(labelStatisticsDomain.asInstanceOf[DiscreteDomain with Domain[L#Value]])
  val weights = model.Weights(new la.DenseTensor1(labelStatisticsDomain.size))
}

// Label-Feature weightsSet
class LogLinearTemplate2[L<:DiscreteVar,F<:DiscreteTensorVar](val model: Parameters, lf:L=>F, fl:F=>L, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) extends DotTemplateWithStatistics2[L,F]() {
  def this(model: Parameters, lf:L=>F, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) = this(model, lf, (f:F) => throw new Error("Function from classify features to label not provided."), labelStatisticsDomain, featureStatisticsDomain)
  //def statisticsDomains = ((labelStatisticsDomain.asInstanceOf[DiscreteDomain with Domain[L#Value]], featureStatisticsDomain.asInstanceOf[DiscreteTensorDomain with Domain[F#Value]]))
  val weights = model.Weights(new la.DenseTensor2(labelStatisticsDomain.size, featureStatisticsDomain.dimensionSize))
  def unroll1(label: L) = Factor(label, lf(label))
  def unroll2(features: F) = Factor(fl(features), features)
}

// TODO Consider renaming this DotModel, like DotMaximumLikelihood
class LogLinearModel[L<:DiscreteVar,F<:DiscreteTensorVar](lf:L=>F, fl:F=>L, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) extends TemplateModel with Parameters {
  def this(lf:L=>F, labelStatisticsDomain:DiscreteDomain, featureStatisticsDomain:DiscreteTensorDomain)(implicit lm:Manifest[L], fm:Manifest[F]) = this(lf, (f:F) => throw new Error("Function from classify features to label not provided."), labelStatisticsDomain, featureStatisticsDomain)
  val biasTemplate = new LogLinearTemplate1[L](this, labelStatisticsDomain)
  val evidenceTemplate = new LogLinearTemplate2[L,F](this, lf, fl, labelStatisticsDomain, featureStatisticsDomain)
  this += biasTemplate
  this += evidenceTemplate
  val factorCache = mutable.HashMap[Var, Iterable[Factor]]()
  override def factors(vs:Iterable[Var]): Iterable[Factor] =  {
    if (vs.size == 1) factors(vs.head)
    else vs.flatMap(factors).toSeq
  }
  override def families: Seq[DotFamily with Template] = Seq(biasTemplate, evidenceTemplate)
  override def factors(v:Var): Iterable[Factor] = {
    v match {
      case v:L if lm.erasure.isAssignableFrom(v.getClass) => Seq(biasTemplate.Factor(v), evidenceTemplate.Factor(v, lf(v)))
      case _ => Nil
    }
    //if (!factorCache.contains(v)) factorCache(v) = super.factors(v); factorCache(v)
  }
//  override def factorsOfFamilies[F<:Family](variables:Iterable[Variable], families:Seq[F]): Iterable[F#Factor] = {
//    if ((families.length == 2) &&
//        (families(0) == biasTemplate || families(1) == biasTemplate) &&
//        (families(0) == evidenceTemplate || families(1) == evidenceTemplate))
//      factors(variables).asInstanceOf[Iterable[F#Factor]]
//    else {
//      filterByFamilies(factors(variables), families)
//    }
//  }
}

// TODO consider requiring the statisticsDomains for label and features when creating the model.
// otherwise we can get error "You must override statisticsDomains if you want..."
