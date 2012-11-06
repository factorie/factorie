package cc.factorie.optimize

import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.util._
import app.classify
import cc.factorie.la._
import classify.{ModelBasedClassifier, LogLinearModel}
import collection.parallel.mutable.ParSeq
import collection.GenSeq
import java.io.File
import io.Source


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
  // gradient or value or margin can be null if they don't need to be computed.
  def accumulateExampleInto(model:M, gradient:WeightsTensorAccumulator, value:DoubleAccumulator, margin:DoubleAccumulator): Unit
  // TODO Consider this too.  It would accumulate the "expectations" part, but not the constraints, which presumably would have been added elsewhere.
  //def accumulateValueAndExpectations(model: Model[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit
}

/** Calculates value by log-likelihood and gradient by maximum likelihood (that is difference of constraints - expectations). */
class LikelihoodExample[V<:LabeledVar](val labels:Iterable[V], val infer:Infer) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val summary = infer.infer(labels, model).get

    if (value != null)
      value.accumulate(model.assignmentScore(labels, TargetAssignment) - summary.logZ)
    // TODO Note that this unrolls the model twice.  We could consider ways to avoid this.

    if (gradient != null) {
      model.factorsOfFamilyClass[DotFamily](labels, classOf[DotFamily]).foreach(factor => {
        gradient.accumulate(factor.family, factor.assignmentStatistics(TargetAssignment))
        gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)
      })
    }
  }
}

class LikelihoodExample2[C<:Iterable[LabeledVar]](val labels:C, val infer:Infer) extends Example[ModelWithContext[C]] {
  def accumulateExampleInto(model: ModelWithContext[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
    if (labels.size == 0) return
    val summary = infer.infer(labels, model).get
    if (value != null)
      value.accumulate(model.assignmentScore(labels, TargetAssignment) - summary.logZ)
    // TODO Note that this unrolls the model twice.  We could consider ways to avoid this.
    if (gradient != null) {
      model.factorsOfFamilyClass[DotFamily](labels, classOf[DotFamily]).foreach(factor => {
        gradient.accumulate(factor.family, factor.assignmentStatistics(TargetAssignment))
        gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)
      })
    }
  }
}


/** A gradient from a collection of IID DiscreteVars, where the set of factors should remain the same as the DiscreteVar value changes. */
class DiscreteLikelihoodExample[V<:LabeledDiscreteVar](val label:V) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
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
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
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
  def addGoodBad(gradient: WeightsTensorAccumulator, model: Model, good:Variable, bad:Variable) {
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
class DominationLossExample(goodCandidates: Seq[Variable], badCandidates: Seq[Variable]) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator) {
    require(gradient != null, "The DominationLossExample needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val worstGoodIndex = goodScores.zipWithIndex.maxBy(i => -i._1)._2
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    if (goodScores(worstGoodIndex) < badScores(bestBadIndex) + 1) {
      value.accumulate(goodScores(worstGoodIndex) - badScores(bestBadIndex) - 1)
      GoodBadExample.addGoodBad(gradient, model, goodCandidates(worstGoodIndex), badCandidates(bestBadIndex))
    }
  }
}

class DominationLossExampleAllGood(goodCandidates: Seq[Variable], badCandidates: Seq[Variable]) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator) {
    require(gradient != null, "The DominationLossExampleAllGood needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    for (i <- 0 until goodScores.length) {
      val goodIndex = goodScores.zipWithIndex.maxBy(i => -i._1)._2
      if (goodScores(goodIndex) < badScores(bestBadIndex) + 1) {
        value.accumulate(goodScores(goodIndex) - badScores(bestBadIndex) - 1)
        GoodBadExample.addGoodBad(gradient, model, goodCandidates(goodIndex), badCandidates(bestBadIndex))
      }
    }
  }
}

