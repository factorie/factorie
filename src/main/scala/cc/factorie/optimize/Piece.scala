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
//trait PieceState { def merge(other: PieceState): PieceState }

// Pieces are thread safe
trait Piece[C] {
  // gradient or value can be null if they don't need to be computed.
  // TODO Rename accumulateValueAndGradientInto
  def accumulateValueAndGradient(model: Model[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit
  // TODO Consider this too.  It would accumulate the "expectations" part, but not the constraints, which presumably would have been added elsewhere.
  //def accumulateValueAndExpectations(model: Model[C], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit
}


class BPMaxLikelihoodPiece[V<:LabeledMutableDiscreteVar[_]](labels:Iterable[V], infer:InferByBP) extends Piece[Variable] {
  labels.foreach(_.setToTarget(null)) // TODO What if someone else changes these values after Piece construction!
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    val summary = infer.infer(labels, model).get
    // The log loss is - score + log Z
    if (value != null)
      value.accumulate(modelElement2Iterable(model).currentScore(labels) - summary.logZ)

    if (gradient != null) {
      summary.bpFactors.foreach(f => {
        val factor = f.factor.asInstanceOf[DotFamily#Factor]
        gradient.accumulate(factor.family, factor.currentStatistics)
        f.accumulateExpectedStatisticsInto(gradient.accumulator(factor.family), -1.0)
        //gradient.accumulate(factor.family, f.calculateMarginal * -1) // TODO No this is wrong, because the BPFactor may have lower rank than its Factor; e.g. BPFactor2Factor3
      })
    }
  }
}

//class MaxLikelihoodPiece[V<:LabeledMutableDiscreteVar[_]](labels:Seq[V], infer:Infer) extends Piece[Variable] {
//  private var _model: Model[Variable] = null
//  private var _constraints: WeightsTensor = null
//  // Use cached constraints Tensor if the model id has not changed since the last call
//  def constraints(model:Model[Variable]): WeightsTensor = if (_model eq model) _constraints else {
//    _model = model
//    _constraints = model.newDenseWeightsTensor // TODO Can't afford to keep something this big for every Piece :-(
//    labels.foreach(_.setToTarget(null))
//    modelVariable2Variables(model).factorsOfFamilyClass(labels, classOf[DotFamily]).foreach(f => { constraints(f.family) += f.currentStatistics })
//    _constraints
//  }
//  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
//    val summary = infer.infer(labels, model).get
//    if (value ne null)
//      value.accumulate(model.currentScore(labels) - summary.logZ)
//    if (gradient ne null) summary.marginals.foreach(m => {
//      throw new Error("Implementation not yet finished") // Need to be able to get factors from Marginals?
//    })
//  }
//}

/** A gradient from a collection of IID DiscreteVars, where the set of factors should remain the same as the DiscreteVar value changes. */
class DiscretePiece[V<:LabeledMutableDiscreteVar[_]](labels:Iterable[V]) extends Piece[Variable] {
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    val familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
    labels.foreach(_.setToTarget(null)) // TODO But what if someone changes the values after this construction?
    for (label <- labels) {
      // TODO This could still be made more efficient, because we unroll for label.proportions, and then again below. 
      val proportions = label.proportions(model)
      if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
      if (gradient ne null) {
        val factors = model.factorsOfFamilies(label, familiesToUpdate)
        var i = 0
        while (i < proportions.length) {
          label := i
          val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
          factors.foreach(f => {
            gradient.accumulate(f.family, f.currentStatistics, p) // TODO Consider instance weights here also?
          })
          i += 1
        }
      }
    }
  }
}

/** A gradient from a collection of IID DiscreteVars, where the set of factors is allowed to change based on the DiscreteVar value. */
class CaseFactorDiscretePiece[V<:LabeledMutableDiscreteVar[_]](labels:Iterable[V]) extends Piece[Variable] {
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator): Unit = {
    val familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
    labels.foreach(_.setToTarget(null)) // TODO But what if someone changes the values after this construction?
    for (label <- labels) {
      val proportions = label.proportions(model)
      if (value ne null) value.accumulate(math.log(proportions(label.targetIntValue)))
      if (gradient ne null) {
        var i = 0
        while (i < proportions.length) {
          label := i
          val p = if (i == label.targetIntValue) 1.0 - proportions(i) else -proportions(i)
          model.factorsOfFamilies(label, familiesToUpdate).foreach(f => {
            gradient.accumulate(f.family, f.currentStatistics, p) // TODO Consider instance weights here also?
          })
          i += 1
        }
      }
    }
  }
}

// The following trait has convenience methods for adding to an accumulator the
// factors that touch a pair of Good/Bad variables
object GoodBadPiece {
  def addGoodBad[C](gradient: WeightsTensorAccumulator, model: Model[C], good: C, bad: C) {
    model.factors(good).foreach(f => {
      f match {
        case f: DotFamily#Factor => gradient.accumulate(f.family, f.currentStatistics)
        case _ => Nil
      }
    })
    model.factors(bad).foreach(f => {
      f match {
        case f: DotFamily#Factor => gradient.accumulate(f.family, f.currentStatistics * -1.0)
        case _ => Nil
      }
    })
  }
}

// The following piece implements the domination loss function: it penalizes models that rank any of
// the badCandates above any of the goodCandidates.
// The actual loss used in this version is the maximum (margin-augmented) difference between
// goodCandidates and badCandidates.
// See DominationLossPieceAllGood for one that outputs a gradient for all goodCandidates
class DominationLossPiece(goodCandidates: Seq[Variable], badCandidates: Seq[Variable]) extends Piece[Variable] {
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    assert(gradient != null, "The DominationLossPiece needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val worstGoodIndex = goodScores.zipWithIndex.maxBy(i => -i._1)._2
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    if (goodScores(worstGoodIndex) < badScores(bestBadIndex) + 1) {
      value.accumulate(goodScores(worstGoodIndex) - badScores(bestBadIndex) - 1)
      GoodBadPiece.addGoodBad(gradient, model, goodCandidates(worstGoodIndex), badCandidates(bestBadIndex))
    }
  }
}

class DominationLossPieceAllGood(goodCandidates: Seq[Variable], badCandidates: Seq[Variable]) extends Piece[Variable] {
  def accumulateValueAndGradient(model: Model[Variable], gradient: WeightsTensorAccumulator, value: DoubleAccumulator) {
    assert(gradient != null, "The DominationLossPieceAllGood needs a gradient accumulator")
    val goodScores = goodCandidates.map(model.currentScore(_))
    val badScores = badCandidates.map(model.currentScore(_))
    val bestBadIndex = badScores.zipWithIndex.maxBy(i => i._1)._2
    for (i <- 0 until goodScores.length) {
      val goodIndex = goodScores.zipWithIndex.maxBy(i => -i._1)._2
      if (goodScores(goodIndex) < badScores(bestBadIndex) + 1) {
        value.accumulate(goodScores(goodIndex) - badScores(bestBadIndex) - 1)
        GoodBadPiece.addGoodBad(gradient, model, goodCandidates(goodIndex), badCandidates(bestBadIndex))
      }
    }
  }
}

