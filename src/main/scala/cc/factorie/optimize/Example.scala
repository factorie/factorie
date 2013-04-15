package cc.factorie.optimize

import cc.factorie._
import cc.factorie.util._
import app.classify
import cc.factorie.la._

/**
 * Created by IntelliJ IDEA.
 * User: Alexandre Passos, Luke Vilnis
 * Date: 10/5/12
 * Time: 11:13 AM
 * To change this template use File | Settings | File Templates.
 */
//trait ExampleState { def merge(other: ExampleState): ExampleState }

// TODO Rename Example to "Grade" or "Example"
// Lately I like "Grade" better.  "Example" can mean the data, not the gradient/value evaluation.  "Grade" has one syllable.
// "Training" also associated with education, for which there are "grades".

// Examples must be thread safe? -alex  Really? Must they be? -akm
trait Example[-M<:Model] {
  // gradient or value can be null if they don't need to be computed.
  def accumulateExampleInto(model:M, gradient:WeightsTensorAccumulator, value:DoubleAccumulator): Unit
  // TODO Consider this too.  It would accumulate the "expectations" part, but not the constraints, which presumably would have been added elsewhere.
  //def accumulateValueAndExpectations(model: Model[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit
}

/** Treats a few examples as a single example. creating minibatches which can be fed to the stochastic trainers */
class MiniBatchExample[-M<:Model](val baseExamples: Seq[Example[M]]) extends Example[M] {
  def accumulateExampleInto(model: M, gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    baseExamples.foreach(_.accumulateExampleInto(model, gradient, value))
  }
}

object MiniBatchExample {
  def apply[M <: Model](batchSize: Int, examples: Seq[Example[M]]): Seq[MiniBatchExample[M]] = {
    examples.grouped(batchSize).map(e => new MiniBatchExample[M](e.toSeq)).toSeq
  }
}

// A locking example wraps an example with knowledge on how to
// lock the structure it needs at runtime to ensure
// that there are no race conditions
trait LockingExample  {
  def lockExample()
  def unlockExample()
}

// An ExampleLocker is an implicit that knows how to create locking examples from examples
trait ExampleLocker {
  def getLockingExample[M<:Model](e: Example[M], model: M): LockingExample
}

/** Calculates value by log-likelihood and gradient by maximum likelihood (that is difference of constraints - expectations). */
class LikelihoodExample[V<:LabeledVar](val labels:Iterable[V], val infer:Infer) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val summary = infer.infer(labels, model).get
    if (value != null)
      value.accumulate(-summary.logZ)
    val factors = summary.usedFactors.getOrElse(model.factors(labels))
    for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
      val aStat = factor.assignmentStatistics(TargetAssignment)
      if (value != null) value.accumulate(factor.statisticsScore(aStat))
      if (gradient != null) {
        gradient.accumulate(factor.family, aStat)
        gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)
      }
    }
    if (value != null) // add in the score from non-DotFamilies
      for (factor <- model.filterNotByFamilyClass(factors, classOf[DotFamily]))
        value.accumulate(factor.assignmentScore(TargetAssignment))
  }
}

class LikelihoodExample2[C<:Iterable[LabeledVar]](val labels:C, val infer:Infer) extends Example[ModelWithContext[C]] {
  def accumulateExampleInto(model: ModelWithContext[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val summary = infer.infer(labels, model).get
    if (value != null)
      value.accumulate(-summary.logZ)
    val factors = summary.usedFactors.getOrElse(model.factors(labels))
    for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
      val aStat = factor.assignmentStatistics(TargetAssignment)
      if (value != null) value.accumulate(factor.statisticsScore(aStat))
      if (gradient != null) {
        gradient.accumulate(factor.family, aStat)
        gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)
      }
    }
    if (value != null) // add in the score from non-DotFamilies
      for (factor <- model.filterNotByFamilyClass(factors, classOf[DotFamily]))
        value.accumulate(factor.assignmentScore(TargetAssignment))
  }
}

// This is just like DiscreteLikelihoodExample but it loops over all variables passed in
class PseudolikelihoodExample[V <: LabeledDiscreteVar](val labels: Iterable[V]) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    for (label <- labels) {
      val factors = model.factorsOfFamilyClass[DotFamily](label) // should this be just DotFamily factors?
      val proportions = label.proportions(factors)
      if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
      if (gradient ne null) {
        val assignment = new DiscreteAssignment1(label, 0)
        var i = 0
        while (i < proportions.length) {
          assignment.intValue1 = i
          val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
          factors.foreach(f => gradient.accumulate(f.family, f.assignmentStatistics(assignment), p)) // TODO Consider instance weights here also?
          i += 1
        }
      }
    }
  }
}

/** A gradient from a collection of IID DiscreteVars, where the set of factors should remain the same as the DiscreteVar value changes. */
class DiscreteLikelihoodExample[V<:LabeledDiscreteVar](val label:V) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    val factors = model.factorsOfFamilyClass[DotFamily](label)
    val proportions = label.proportions(factors)
    if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
    if (gradient ne null) {
      val assignment = new DiscreteAssignment1(label, 0)
      var i = 0
      while (i < proportions.length) {
        assignment.intValue1 = i
        val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
        factors.foreach(f => gradient.accumulate(f.family, f.assignmentStatistics(assignment), p)) // TODO Consider instance weights here also?
        i += 1
      }
    }
  }
}

/** A gradient from a collection of IID DiscreteVars, where the set of factors is allowed to change based on the DiscreteVar value. */
class CaseFactorDiscreteLikelihoodExample[V<:LabeledMutableDiscreteVar[_]](val label:V) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    val proportions = label.caseFactorProportions(model)
    if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
    if (gradient ne null) {
      var i = 0
      while (i < proportions.length) {
        label := i
        val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
        // Note that label must be mutable here because there is no way to get different factors with an Assignment.  A little sad. -akm
        model.factorsOfFamilyClass[DotFamily](label).foreach(f => {
          gradient.accumulate(f.family, f.currentStatistics, p) // TODO Consider instance weights here also?
        })
        i += 1
      }
    }
  }
}

// The following trait has convenience methods for adding to an accumulator the
// factors that touch a pair of Good/Bad variables
object GoodBadExample {
  def addGoodBad(gradient: WeightsTensorAccumulator, model: Model, good:Var, bad:Var) {
    model.factors(good).foreach({
      case f: DotFamily#Factor => gradient.accumulate(f.family, f.currentStatistics)
      case _ => sys.error("Domination loss requires DotFamily")
    })
    model.factors(bad).foreach({
      case f: DotFamily#Factor => gradient.accumulate(f.family, f.currentStatistics, -1.0)
      case _ => sys.error("Domination loss requires DotFamily")
    })
  }
}

// The following Example implements the domination loss function: it penalizes models that rank any of
// the badCandates above any of the goodCandidates.
// The actual loss used in this version is the maximum (margin-augmented) difference between
// goodCandidates and badCandidates.
// See DominationLossExampleAllGood for one that outputs a gradient for all goodCandidates
class DominationLossExample(goodCandidates: Seq[Var], badCandidates: Seq[Var]) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    require(gradient != null, "The DominationLossExample needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val worstGoodIndex = goodScores.zipWithIndex.maxBy(i => -i._1)._2
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    if (goodScores(worstGoodIndex) < badScores(bestBadIndex) + 1) {
      if (value != null) value.accumulate(goodScores(worstGoodIndex) - badScores(bestBadIndex) - 1)
      GoodBadExample.addGoodBad(gradient, model, goodCandidates(worstGoodIndex), badCandidates(bestBadIndex))
    }
  }
}

class DominationLossExampleAllGood(goodCandidates: Seq[Var], badCandidates: Seq[Var]) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    require(gradient != null, "The DominationLossExampleAllGood needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    for (i <- 0 until goodScores.length) {
      if (goodScores(i) < badScores(bestBadIndex) + 1) {
        if (value != null) value.accumulate(goodScores(i) - badScores(bestBadIndex) - 1)
        GoodBadExample.addGoodBad(gradient, model, goodCandidates(i), badCandidates(bestBadIndex))
      }
    }
  }
}

// StructuredPerceptron is just Likelihood with a Maximize ("value" will be correct as long as the marginals returned by
// Maximize have the max model score as "logZ")
class StructuredPerceptronExample[V <: LabeledVar](labels: Iterable[V], infer: Maximize = MaximizeByBPLoopy) extends LikelihoodExample(labels, infer)

// NOTE: a "loss" is a negated objective - so higher score is worse - otherwise this won't work since there is no way to make a
// CombinedModel that subtracts one model's score from another
// USE: make sure that loss overrides neighborDomain1 or valuesScore (inference needs this to score values)
// NOTE: For structured SVM with specialized inference, just use StructuredPerceptron and pass in a loss-augmented Infer object
class StructuredSVMExample[V <: LabeledVar](labels: Iterable[V], loss: Model = HammingLoss, infer: Maximize = MaximizeByBPLoopy)
  extends LikelihoodExample(labels, infer) {
  override def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    if (value != null) {
      val valueAcc = new LocalDoubleAccumulator(0.0)
      super.accumulateExampleInto(new CombinedModel(model, loss), gradient, valueAcc)
      // get a margin from LikelihoodExample (which equals value since value is the penalty of the most violated constraint)
      if (value != null) value.accumulate(valueAcc.value)
    }
  }
}

class SemiSupervisedExample[V<:LabeledVar](val labels:Iterable[V], val inferConstrained:Infer, val inferUnconstrained: Infer) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val constrainedSummary = inferConstrained.infer(labels, model).get
    val unconstrainedSummary = inferUnconstrained.infer(labels, model).get
    if (value != null)
      value.accumulate(constrainedSummary.logZ - unconstrainedSummary.logZ)
    val factors = unconstrainedSummary.usedFactors.getOrElse(model.factors(labels))
    if (gradient != null) {
      for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
        gradient.accumulate(factor.family, constrainedSummary.marginalTensorStatistics(factor), 1.0)
        gradient.accumulate(factor.family, unconstrainedSummary.marginalTensorStatistics(factor), -1.0)
      }
    }
  }
}
