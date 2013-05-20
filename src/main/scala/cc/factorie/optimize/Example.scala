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

trait Example {
  // gradient or value can be null if they don't need to be computed.
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit
}

/** Treats a few examples as a single example. creating minibatches which can be fed to the stochastic trainers */
class MiniBatchExample(val baseExamples: Seq[Example]) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator) {
    baseExamples.foreach(_.accumulateExampleInto(gradient, value))
  }
}

object MiniBatchExample {
  def apply(batchSize: Int, examples: Seq[Example]): Seq[MiniBatchExample] = {
    examples.grouped(batchSize).map(e => new MiniBatchExample(e.toSeq)).toSeq
  }
}

/** Calculates value by log-likelihood and gradient by maximum likelihood (that is difference of constraints - expectations). */
class LikelihoodExample(labels: Iterable[LabeledVar], model: Model with Parameters, val infer: Infer) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val summary = infer.infer(labels, model).get
    if (value != null)
      value.accumulate(-summary.logZ)
    val factors = summary.usedFactors.getOrElse(model.factors(labels))
    for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
      val aStat = factor.assignmentStatistics(TargetAssignment)
      if (value != null) value.accumulate(factor.statisticsScore(aStat))
      if (gradient != null) {
        gradient.accumulate(factor.family.weights, aStat)
        gradient.accumulate(factor.family.weights, summary.marginalTensorStatistics(factor), -1.0)
      }
    }
    if (value != null) // add in the score from non-DotFamilies
      for (factor <- model.filterByNotFamilyClass(factors, classOf[DotFamily]))
        value.accumulate(factor.assignmentScore(TargetAssignment))
  }
}

// NOTE: this doesn't do anything right now because our infer objects don't call factorsWithContext -luke

//class LikelihoodExample2[C <: Iterable[LabeledVar]](model: ModelWithContext[C] with Parameters, val labels: C, val infer: Infer) extends Example {
//  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
//    if (labels.size == 0) return
//    val summary = infer.infer(labels, model).get
//    if (value != null)
//      value.accumulate(-summary.logZ)
//    val factors = summary.usedFactors.getOrElse(model.factors(labels))
//    for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
//      val aStat = factor.assignmentStatistics(TargetAssignment)
//      if (value != null) value.accumulate(factor.statisticsScore(aStat))
//      if (gradient != null) {
//        gradient.accumulate(factor.family.weights, aStat)
//        gradient.accumulate(factor.family.weights, summary.marginalTensorStatistics(factor), -1.0)
//      }
//    }
//    if (value != null) // add in the score from non-DotFamilies
//      for (factor <- model.filterByNotFamilyClass(factors, classOf[DotFamily]))
//        value.accumulate(factor.assignmentScore(TargetAssignment))
//  }
//}

// This is just like DiscreteLikelihoodExample but it loops over all variables passed in
class PseudolikelihoodExample(labels: Iterable[LabeledDiscreteVar], model: Model with Parameters) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    for (label <- labels) {
      val factors = model.factors(label)
      val proportions = label.proportions(factors)
      if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
      if (gradient ne null) {
        val assignment = new DiscreteAssignment1(label, 0)
        var i = 0
        while (i < proportions.length) {
          assignment.intValue1 = i
          val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
          for (f <- model.filterByFamilyClass[DotFamily](factors, classOf[DotFamily]))
            gradient.accumulate(f.family.weights, f.assignmentStatistics(assignment), p) // TODO Consider instance weights here also?
          i += 1
        }
      }
    }
  }
}

// Generalization of Pseudo likelihood to sets of variables, instead of a single one
class CompositeLikelihoodExample(components: Iterable[Iterable[LabeledDiscreteVar]], model: Model with Parameters) extends Example {
  def computeProportions(labels: Iterable[LabeledDiscreteVar], factors:Iterable[Factor]): (Proportions1, Int) = {
    val iterator = AssignmentIterator.assignments(labels.toSeq)
    val l = iterator.length
    val distribution = new DenseTensor1(l)
    var score = 0.0
    var i = 0
    var targetIndex = -1
    for (assignment <- iterator) {
      score = 0.0; factors.foreach(f => score += f.assignmentScore(assignment))   // compute score of variable with value 'i'
      distribution(i) = score
      if(labels.forall(v => assignment(v).intValue == v.targetIntValue)) targetIndex = i
      i += 1
    }
    distribution.expNormalize()
    (new NormalizedTensorProportions1(distribution, checkNormalization=false), targetIndex)
  }
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    for (component <- components) {
      val factors = model.factors(component) // should this be just DotFamily factors?
      val (proportions, targetIndex) = computeProportions(component, factors)
      if (value ne null) value.accumulate(math.log(proportions(targetIndex)))
      if (gradient ne null) {
        var i = 0
        for(assignment <- AssignmentIterator.assignments(component.toSeq)) {
          val p = if (i == targetIndex) 1.0 - proportions(i) else -proportions(i)
          for (f <- model.filterByFamilyClass[DotFamily](factors, classOf[DotFamily]))
            gradient.accumulate(f.family.weights, f.assignmentStatistics(assignment), p) // TODO Consider instance weights here also?
          i += 1
        }
      }
    }
  }
}

/** A gradient from a collection of IID DiscreteVars, where the set of factors should remain the same as the DiscreteVar value changes. */
class DiscreteLikelihoodExample(label: LabeledDiscreteVar, model: Model with Parameters) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    val factors = model.factors(label)
    val proportions = label.proportions(factors)
    if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
    if (gradient ne null) {
      val assignment = new DiscreteAssignment1(label, 0)
      var i = 0
      while (i < proportions.length) {
        assignment.intValue1 = i
        val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
        for (f <- model.filterByFamilyClass[DotFamily](factors, classOf[DotFamily]))
          gradient.accumulate(f.family.weights, f.assignmentStatistics(assignment), p) // TODO Consider instance weights here also?
        i += 1
      }
    }
  }
}

/** A gradient from a collection of IID DiscreteVars, where the set of factors is allowed to change based on the DiscreteVar value. */
class CaseFactorDiscreteLikelihoodExample(label: LabeledMutableDiscreteVar[_], model: Model with Parameters) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    val proportions = label.caseFactorProportions(model)
    if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
    if (gradient ne null) {
      var i = 0
      while (i < proportions.length) {
        label := i
        val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
        // Note that label must be mutable here because there is no way to get different factors with an Assignment.  A little sad. -akm
        model.factorsOfFamilyClass[DotFamily](label).foreach(f => {
          gradient.accumulate(f.family.weights, f.currentStatistics, p) // TODO Consider instance weightsSet here also?
        })
        i += 1
      }
    }
  }
}

// The following trait has convenience methods for adding to an accumulator the
// factors that touch a pair of Good/Bad variables
object GoodBadExample {
  def addGoodBad(gradient: WeightsMapAccumulator, model: Model with Parameters, good:Var, bad:Var) {
    model.factors(good).foreach({
      case f: DotFamily#Factor => gradient.accumulate(f.family.weights, f.currentStatistics)
      case _ => sys.error("Domination loss requires DotFamily")
    })
    model.factors(bad).foreach({
      case f: DotFamily#Factor => gradient.accumulate(f.family.weights, f.currentStatistics, -1.0)
      case _ => sys.error("Domination loss requires DotFamily")
    })
  }
}

// The following Example implements the domination loss function: it penalizes models that rank any of
// the badCandates above any of the goodCandidates.
// The actual loss used in this version is the maximum (margin-augmented) difference between
// goodCandidates and badCandidates.
// See DominationLossExampleAllGood for one that outputs a gradient for all goodCandidates
// In FACTORIE we don't minimize a "loss" we maximize an "objective".  I think this class should be renamed. -akm 
class DominationLossExample(goodCandidates: Seq[Var], badCandidates: Seq[Var], model: Model with Parameters) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator) {
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

class DominationLossExampleAllGood(model: Model with Parameters, goodCandidates: Seq[Var], badCandidates: Seq[Var]) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator) {
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
class StructuredPerceptronExample(labels: Iterable[LabeledVar], model: Model with Parameters, infer: Maximize = MaximizeByBPLoopy) extends LikelihoodExample(labels, model, infer)

// NOTE: a "loss" is a negated objective - so higher score is worse - otherwise this won't work since there is no way to make a
// CombinedModel that subtracts one model's score from another
// USE: make sure that loss overrides neighborDomain1 or valuesScore (inference needs this to score values)
// NOTE: For structured SVM with specialized inference, just use StructuredPerceptron and pass in a loss-augmented Infer object
class StructuredSVMExample(labels: Iterable[LabeledVar], model: Model with Parameters, objective: Model = HammingLoss, infer: Maximize = MaximizeByBPLoopy)
  extends StructuredPerceptronExample(labels, new CombinedModel(model, objective) with Parameters { override val parameters = model.parameters }, infer) {
  override def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    if (value != null) {
      val valueAcc = new LocalDoubleAccumulator(0.0)
      super.accumulateExampleInto(gradient, valueAcc)
      // get a margin from LikelihoodExample (which equals value since value is the penalty of the most violated constraint)
      if (value != null) value.accumulate(valueAcc.value)
    }
  }
}

class SemiSupervisedLikelihoodExample(labels: Iterable[LabeledVar], model: Model with Parameters, inferConstrained: Infer, inferUnconstrained: Infer) extends Example {
  def accumulateExampleInto(gradient: WeightsMapAccumulator, value: DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val constrainedSummary = inferConstrained.infer(labels, model).get
    val unconstrainedSummary = inferUnconstrained.infer(labels, model).get
    if (value != null)
      value.accumulate(constrainedSummary.logZ - unconstrainedSummary.logZ)
    val factors = unconstrainedSummary.usedFactors.getOrElse(model.factors(labels))
    if (gradient != null) {
      for (factor <- model.filterByFamilyClass(factors, classOf[DotFamily])) {
        gradient.accumulate(factor.family.weights, constrainedSummary.marginalTensorStatistics(factor), 1.0)
        gradient.accumulate(factor.family.weights, unconstrainedSummary.marginalTensorStatistics(factor), -1.0)
      }
    }
  }
}
