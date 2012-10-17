package cc.factorie.optimize

import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.util._
import app.classify
import cc.factorie.la._
import classify.{ModelBasedClassifier, LogLinearModel}
import scala.collection.mutable._
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
  def accumulateValueAndGradient(model: Model[C], gradient: TensorAccumulator, value: DoubleAccumulator): Unit
  def accumulateGradient(model: Model[C], gradient: TensorAccumulator): Unit = accumulateValueAndGradient(model, gradient, NoopDoubleAccumulator)
  def accumulateValue(model: Model[C], value: DoubleAccumulator): Unit = accumulateValueAndGradient(model, NoopTensorAccumulator, value)
  //def state: PieceState
  //def updateState(state: PieceState)
}


class BPMaxLikelihoodPiece[V <: LabeledCategoricalVariable[C],C](labels: collection.Seq[V]) extends Piece[Variable] {
  def state = null

  labels.foreach(_.setToTarget(null))

  def accumulateValueAndGradient(model: Model[Variable], gradient: TensorAccumulator, value: DoubleAccumulator) {
    val fg = BP.inferTreewiseSum(labels.toSet, model)
    // The log loss is - score + log Z
    value.accumulate(new Variable2IterableModel[Variable](model).currentScore(labels) - fg.bpFactors.head.calculateLogZ)

    fg.bpFactors.foreach(f => {
      val factor = f.factor.asInstanceOf[DotFamily#Factor]
      gradient.accumulate(factor.family, factor.currentStatistics)
      gradient.accumulate(factor.family, f.calculateMarginal * -1)
    })
  }
}

// The following trait has convenience methods for adding to an accumulator the
// factors that touch a pair of Good/Bad variables
object GoodBadPiece {
  def addGoodBad[C](gradient: TensorAccumulator, model: Model[C], good: C, bad: C) {
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
  def accumulateValueAndGradient(model: Model[Variable], gradient: TensorAccumulator, value: DoubleAccumulator) {
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
  def accumulateValueAndGradient(model: Model[Variable], gradient: TensorAccumulator, value: DoubleAccumulator) {
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

