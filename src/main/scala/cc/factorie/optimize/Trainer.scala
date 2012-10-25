package cc.factorie.optimize

import cc.factorie.Model
import cc.factorie.util.LocalDoubleAccumulator
import cc.factorie.la.{Tensor, WeightsTensor, LocalWeightsTensorAccumulator}
import cc.factorie.util.FastLogging
import cc.factorie.app.classify.LogLinearModel

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/17/12
 * Time: 7:29 PM
 * To change this template use File | Settings | File Templates.
 */

/** Learns the parameters of a Model by processing the gradients and values from a collection of Examples. */
trait Trainer[M<:Model[_]] {
  def model: M
  def processAll(examples: Iterable[Example[M]]): Unit
  // TODO Rename processExamples
}

// TODO Re-order the constructor arguments to Trainer: model, optimizer

/** Learns the parameters of a Model by summing the gradients and values of all Examples, 
    and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS). */
class BatchTrainer[M<:Model[_]](val optimizer:GradientOptimizer, val model:M) extends Trainer[M] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  def processAll(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    gradientAccumulator.tensor.zero()
    valueAccumulator.value = 0.0
    examples.foreach(example => example.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator))
    logger.info("Gradient Norm: " + gradientAccumulator.tensor.oneNorm + "\nLoss: " + valueAccumulator.value)
    optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, 0)
  }
  def isConverged = optimizer.isConverged
}

class ParallelBatchTrainer[M<:Model[_]](val optimizer: GradientOptimizer, val model: M) extends Trainer[M] with FastLogging {
  def processAll(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] { override def initialValue = new LocalWeightsTensorAccumulator(model.newWeightsTensor.asInstanceOf[WeightsTensor]) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { override def initialValue = new LocalDoubleAccumulator }
    examples.par.foreach(example => example.accumulateValueAndGradient(model, gradientAccumulator.get, valueAccumulator.get))
    throw new Error("Not yet implemented.  Now need to gather all gradientAccumulator from each thread, combine them and pass to optimizer.")
    optimizer.step(model.weightsTensor, gradientAccumulator.get.tensor, valueAccumulator.get.value, 0)
  }
  def isConverged = optimizer.isConverged
}

// Hacky proof of concept
class InlineSGDTrainer(val optimizer: GradientOptimizer, val model: LogLinearModel[_,_], val learningRate: Double = 0.01, val l2: Double = 0.1) extends Trainer[LogLinearModel[_,_]] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newSparseWeightsTensor)
  override def processAll(examples: Iterable[Example[LogLinearModel[_,_]]]): Unit = {
    val weights = model.evidenceTemplate.weightsTensor // weightsTensor.asInstanceOf[WeightsTensor](DummyFamily)
    examples.foreach(example => {
      val glmExample = example.asInstanceOf[GLMExample]
      val oldWeight = glmExample.weight
      glmExample.weight *= learningRate
      example.accumulateValueAndGradient(model, gradientAccumulator, null)
      glmExample.weight = oldWeight
    })
    weights *= (1.0 - l2)
  }
  def isConverged = false
}

class SGDTrainer[M<:Model[_]](val optimizer: GradientOptimizer, val model:M) extends Trainer[M] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator
  override def processAll(examples: Iterable[Example[M]]): Unit = {
    examples.foreach(example => {
      gradientAccumulator.tensor.zero()
      example.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
      optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, 0)
    })
  }
  def isConverged = false
}

class HogwildTrainer[M<:Model[_]](val optimizer: GradientOptimizer, val model: M) extends Trainer[M] {
  val gradient = new ThreadLocal[Tensor] {override def initialValue = model.newWeightsTensor }
  val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] {override def initialValue = new LocalWeightsTensorAccumulator(gradient.get.asInstanceOf[WeightsTensor])}
  override def processAll(examples: Iterable[Example[M]]): Unit = {
    examples.toSeq.par.foreach(example => {
      gradient.get.zero()
      example.accumulateValueAndGradient(model, gradientAccumulator.get, null)
      throw new Error("Not implemented: Next step isn't thread safe.")
      optimizer.step(model.weightsTensor, gradient.get, 0, 0) // TODO But this isn't thread-safe!
    })
  }
  def isConverged = false
}

// Hacky proof of concept
class SGDThenBatchTrainer[M<:Model[_]](val optimizer:GradientOptimizer, val model:M, val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[M] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.weightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  val batchLearner = new BatchTrainer(optimizer, model)
  var sgdPasses = 5
  override def processAll(examples: Iterable[Example[M]]): Unit = {
    if (sgdPasses > 0) {
      valueAccumulator.value = 0.0
      examples.foreach(example => {
        val glmExample = example.asInstanceOf[GLMExample]
        val oldWeight = glmExample.weight
        glmExample.weight *= learningRate
        example.accumulateValueAndGradient(model, gradientAccumulator, valueAccumulator)
        glmExample.weight = oldWeight
      })
      model.asInstanceOf[cc.factorie.app.classify.LogLinearModel[_,_]].evidenceTemplate.weightsTensor *= math.pow(1.0 - learningRate * l2, math.sqrt(examples.size))
      valueAccumulator.value += -l2 * (model.weightsTensor dot model.weightsTensor)
      logger.info("Loss: " + valueAccumulator.value)
      sgdPasses -= 1
    }
    else
      batchLearner.processAll(examples)
  }
  def isConverged = optimizer.isConverged
}
