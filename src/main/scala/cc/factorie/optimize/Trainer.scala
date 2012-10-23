package cc.factorie.optimize

import cc.factorie.Model
import cc.factorie.util.LocalDoubleAccumulator
import cc.factorie.la.{Tensor, WeightsTensor, LocalWeightsTensorAccumulator}
import cc.factorie.util.FastLogging

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/17/12
 * Time: 7:29 PM
 * To change this template use File | Settings | File Templates.
 */

/** Learns the parameters of a Model by processing the gradients and values from a collection of Examples. */
trait Trainer[C] {
  def model: Model[C]
  def processAll(pieces: Iterable[Piece[C]]): Unit
  // TODO Rename processExamples
}

// TODO Re-order the constructor arguments to Trainer: model, optimizer

/** Learns the parameters of a Model by summing the gradients and values of all Examples, 
    and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS). */
class BatchTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  def processAll(pieces: Iterable[Piece[C]]): Unit = {
    if (isConverged) return
    gradientAccumulator.tensor.zero()
    valueAccumulator.value = 0.0
    pieces.foreach(piece => piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator))
    logger.info("Gradient Norm: " + gradientAccumulator.tensor.oneNorm + "\nLoss: " + valueAccumulator.value)
    optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, 0)
  }
  def isConverged = optimizer.isConverged
}

class ParallelBatchTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] with FastLogging {
  def processAll(pieces: Iterable[Piece[C]]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] { override def initialValue = new LocalWeightsTensorAccumulator(model.newWeightsTensor.asInstanceOf[WeightsTensor]) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { override def initialValue = new LocalDoubleAccumulator }
    pieces.par.foreach(piece => piece.accumulateValueAndGradient(model, gradientAccumulator.get, valueAccumulator.get))
    throw new Error("Not yet implemented.  Now need to gather all gradientAccumulator from each thread, combine them and pass to optimizer.")
    optimizer.step(model.weightsTensor, gradientAccumulator.get.tensor, valueAccumulator.get.value, 0)
  }
  def isConverged = optimizer.isConverged
}

// Hacky proof of concept
class InlineSGDTrainer[C](val optimizer: GradientOptimizer, val model: Model[C], val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[C] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newSparseWeightsTensor)
  override def processAll(pieces: Iterable[Piece[C]]): Unit = {
    val weights = model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily)
    pieces.foreach(piece => {
      val glmPiece = piece.asInstanceOf[GLMPiece]
      val oldWeight = glmPiece.weight
      glmPiece.weight *= learningRate
      piece.accumulateValueAndGradient(model, gradientAccumulator, null)
      glmPiece.weight = oldWeight
    })
    weights *= (1.0 - l2)
  }
  def isConverged = false
}

class SGDTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newSparseWeightsTensor)
  val valueAccumulator = new LocalDoubleAccumulator
  override def processAll(pieces: Iterable[Piece[C]]): Unit = {
    pieces.foreach(piece => {
      gradientAccumulator.tensor.zero()
      piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
      optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, 0)
    })
  }
  def isConverged = false
}

class HogwildTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] {
  val gradient = new ThreadLocal[Tensor] {override def initialValue = model.newSparseWeightsTensor }
  val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] {override def initialValue = new LocalWeightsTensorAccumulator(gradient.get.asInstanceOf[WeightsTensor])}
  override def processAll(pieces: Iterable[Piece[C]]): Unit = {
    pieces.toSeq.par.foreach(piece => {
      gradient.get.zero()
      piece.accumulateValueAndGradient(model, gradientAccumulator.get, null)
      throw new Error("Not implemented: Next step isn't thread safe.")
      optimizer.step(model.weightsTensor, gradient.get, 0, 0) // TODO But this isn't thread-safe!
    })
  }
  def isConverged = false
}

// Hacky proof of concept
class SGDThenBatchTrainer[C](val optimizer: GradientOptimizer, val model: Model[C], val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[C] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.weightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  val batchLearner = new BatchTrainer[C](optimizer, model)
  var sgdPasses = 5
  override def processAll(pieces: Iterable[Piece[C]]): Unit = {
    if (sgdPasses > 0) {
      valueAccumulator.value = 0.0
      pieces.foreach(piece => {
        val glmPiece = piece.asInstanceOf[GLMPiece]
        val oldWeight = glmPiece.weight
        glmPiece.weight *= learningRate
        piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
        glmPiece.weight = oldWeight
      })
      model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily) *= math.pow(1.0 - learningRate * l2, math.sqrt(pieces.size))
      valueAccumulator.value += -l2 * (model.weightsTensor dot model.weightsTensor)
      logger.info("Loss: " + valueAccumulator.value)
      sgdPasses -= 1
    }
    else
      batchLearner.processAll(pieces)
  }
  def isConverged = optimizer.isConverged
}
