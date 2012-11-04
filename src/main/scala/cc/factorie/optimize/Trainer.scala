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
trait Trainer[M<:Model] {
  /** The Model that is being trained. */
  def model: M
  /** Use all these Examples once to make progress towards training */
  def processExamples(examples:Iterable[Example[M]]): Unit
  /** Would more training help? */
  def isConverged: Boolean
  /** Repeatedly process the examples until training has converged. */
  def trainFromExamples(examples:Iterable[Example[M]]): Unit = while (!isConverged) processExamples(examples)
}

trait OnlineTrainer[M<:Model] extends Trainer[M] {
  /** Use this single Example to make progress towards training. */
  def processExample(example:Example[M]): Unit
}


/** Learns the parameters of a Model by summing the gradients and values of all Examples, 
    and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS). */
class BatchTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  val marginAccumulator = new LocalDoubleAccumulator(0.0)
  // TODO This is sad:  The optimizer determines which of gradient/value/margin it needs, but we don't know here
  // so we create them all, possibly causing the Example to do more work.
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    gradientAccumulator.tensor.zero()
    valueAccumulator.value = 0.0
    marginAccumulator.value = 0.0
    val startTime = System.currentTimeMillis
    examples.foreach(example => example.accumulateExampleInto(model, gradientAccumulator, valueAccumulator, marginAccumulator))
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(gradientAccumulator.tensor.oneNorm, valueAccumulator.value, timeString))
    optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, marginAccumulator.value)
  }
  def isConverged = optimizer.isConverged
}

class ParallelBatchTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] with FastLogging {
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] { override def initialValue = new LocalWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor]) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { override def initialValue = new LocalDoubleAccumulator }
    val marginAccumulator = new ThreadLocal[LocalDoubleAccumulator] { override def initialValue = new LocalDoubleAccumulator }
    examples.par.foreach(example => example.accumulateExampleInto(model, gradientAccumulator.get, valueAccumulator.get, marginAccumulator.get))
    throw new Error("Not yet implemented.  Now need to gather all gradientAccumulator from each thread, combine them and pass to optimizer.")
    optimizer.step(model.weightsTensor, gradientAccumulator.get.tensor, valueAccumulator.get.value, marginAccumulator.get.value)
  }
  def isConverged = optimizer.isConverged
}

// Hacky proof of concept
class InlineSGDTrainer(val model:LogLinearModel[_,_], val optimizer:GradientOptimizer = new MIRA, val learningRate:Double = 0.01, val l2:Double = 0.1) extends Trainer[LogLinearModel[_,_]] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor])
  override def processExamples(examples: Iterable[Example[LogLinearModel[_,_]]]): Unit = {
    val weights = model.evidenceTemplate.weightsTensor
    examples.foreach(example => {
      val glmExample = example.asInstanceOf[GLMExample]
      val oldWeight = glmExample.weight
      glmExample.weight *= learningRate
      example.accumulateExampleInto(model, gradientAccumulator, null, null)
      glmExample.weight = oldWeight
    })
    weights *= (1.0 - l2)
  }
  def isConverged = false
}

class SGDTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer = new MIRA) extends Trainer[M] {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator
  val marginAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    examples.foreach(example => {
      gradientAccumulator.tensor.zero()
      example.accumulateExampleInto(model, gradientAccumulator, valueAccumulator, marginAccumulator)
      optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, marginAccumulator.value)
    })
  }
  def isConverged = false
}

class HogwildTrainer[M<:Model](val model: M, val optimizer: GradientOptimizer) extends Trainer[M] {
  val gradient = new ThreadLocal[Tensor] {override def initialValue = model.newBlankWeightsTensor }
  val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] {override def initialValue = new LocalWeightsTensorAccumulator(gradient.get.asInstanceOf[WeightsTensor])}
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    examples.toSeq.par.foreach(example => {
      gradient.get.zero()
      example.accumulateExampleInto(model, gradientAccumulator.get, null, null)
      throw new Error("Not implemented: Next step isn't thread safe.")
      optimizer.step(model.weightsTensor, gradient.get, 0, 0) // TODO But this isn't thread-safe!
    })
  }
  def isConverged = false
}

// Hacky proof of concept
class SGDThenBatchTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer, val learningRate: Double = 0.01, val l2: Double = 0.1)
  extends Trainer[M] with FastLogging {
  val gradientAccumulator = new LocalWeightsTensorAccumulator(model.weightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator
  val marginAccumulator = new LocalDoubleAccumulator
  val batchLearner = new BatchTrainer(model, optimizer)
  var sgdPasses = 5
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (sgdPasses > 0) {
      valueAccumulator.value = 0.0
      examples.foreach(example => {
        val glmExample = example.asInstanceOf[GLMExample]
        val oldWeight = glmExample.weight
        glmExample.weight *= learningRate
        example.accumulateExampleInto(model, gradientAccumulator, valueAccumulator, marginAccumulator)
        glmExample.weight = oldWeight
      })
      model.asInstanceOf[cc.factorie.app.classify.LogLinearModel[_,_]].evidenceTemplate.weightsTensor *= math.pow(1.0 - learningRate * l2, math.sqrt(examples.size))
      valueAccumulator.value += -l2 * (model.weightsTensor dot model.weightsTensor)
      logger.info("Loss: " + valueAccumulator.value)
      sgdPasses -= 1
    }
    else
      batchLearner.processExamples(examples)
  }
  def isConverged = optimizer.isConverged
}
