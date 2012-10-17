package cc.factorie.optimize

import cc.factorie.Model
import collection.GenSeq
import cc.factorie.util.LocalDoubleAccumulator
import cc.factorie.la.{Tensor, WeightsTensor, LocalTensorAccumulator}

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/17/12
 * Time: 7:29 PM
 * To change this template use File | Settings | File Templates.
 */


trait Trainer[C] {
  def model: Model[C]
  def process(pieces: GenSeq[Piece[C]]): Unit
}

class BatchTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] {
  val gradient = model.weightsTensor.copy
  val gradientAccumulator = new LocalTensorAccumulator(gradient.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  def process(pieces: GenSeq[Piece[C]]): Unit = {
    if (isConverged) return
    gradient.zero()
    valueAccumulator.value = 0.0
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit...
    pieces /*.par */ .foreach(piece => piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator))
    println("Gradient: " + gradient + "\nLoss: " + valueAccumulator.value)
    optimizer.step(model.weightsTensor, gradient, valueAccumulator.value, 0)
  }
  def isConverged = optimizer.isConverged
}

// Hacky proof of concept
class InlineSGDTrainer[C](val optimizer: GradientOptimizer, val model: Model[C], val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[C] {
  val gradientAccumulator = new LocalTensorAccumulator(model.weightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  override def process(pieces: GenSeq[Piece[C]]): Unit = {
    val weights = model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily)
    valueAccumulator.value = 0.0
    pieces.foreach(piece => {
      val glmPiece = piece.asInstanceOf[MultiClassGLMPiece]
      val oldWeight = glmPiece.weight
      glmPiece.weight *= learningRate
      piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
      glmPiece.weight = oldWeight
    })
    weights *= (1.0 - l2)
    valueAccumulator.value += -l2 * (weights dot weights)
    println("Loss: " + valueAccumulator.value)
  }
  def isConverged = false
}

class SGDTrainer[C](val optimizer: GradientOptimizer, val model: Model[C]) extends Trainer[C] {
  val gradient = new ThreadLocal[Tensor] {override def initialValue = model.weightsTensor.asInstanceOf[WeightsTensor].copy}
  val gradientAccumulator = new ThreadLocal[LocalTensorAccumulator] {override def initialValue = new LocalTensorAccumulator(gradient.get.asInstanceOf[WeightsTensor])}
  val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] {override def initialValue = new LocalDoubleAccumulator(0.0)}

  override def process(pieces: GenSeq[Piece[C]]): Unit = {
    // Note that nothing stops us from computing the gradients in parallel if the machine is 64-bit
    pieces.foreach(piece => {
      gradient.get.zero()
      valueAccumulator.get.value = 0.0
      piece.accumulateValueAndGradient(model, gradientAccumulator.get, valueAccumulator.get)
      optimizer.step(model.weightsTensor, gradient.get, valueAccumulator.get.value, 0)
      //      println("Step!")
    })
  }

  def isConverged = false
}

class HogwildTrainer[C](optimizer: GradientOptimizer, model: Model[C]) extends SGDTrainer[C](optimizer, model) {
  override def process(pieces: GenSeq[Piece[C]]) = super.process(pieces.par)
}

// Hacky proof of concept
class SGDThenBatchTrainer[C](val optimizer: GradientOptimizer, val model: Model[C], val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[C] {
  val gradientAccumulator = new LocalTensorAccumulator(model.weightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  val batchLearner = new BatchTrainer[C](optimizer, model)
  var sgdPasses = 5
  override def process(pieces: GenSeq[Piece[C]]): Unit = {
    if (sgdPasses > 0) {
      valueAccumulator.value = 0.0
      pieces.foreach(piece => {
        val glmPiece = piece.asInstanceOf[MultiClassGLMPiece]
        val oldWeight = glmPiece.weight
        glmPiece.weight *= learningRate
        piece.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
        glmPiece.weight = oldWeight
      })
      model.weightsTensor.asInstanceOf[WeightsTensor](DummyFamily) *= math.pow(1.0 - learningRate * l2, math.sqrt(pieces.length))
      valueAccumulator.value += -l2 * (model.weightsTensor dot model.weightsTensor)
      println("Loss: " + valueAccumulator.value)
      sgdPasses -= 1
    }
    else
      batchLearner.process(pieces)
  }
  def isConverged = optimizer.isConverged
}
