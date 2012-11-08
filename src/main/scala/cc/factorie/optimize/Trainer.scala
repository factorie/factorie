package cc.factorie.optimize

import cc.factorie.app.classify.LogLinearModel
import cc.factorie.{DotFamily, Family, Model}
import cc.factorie.la._
import cc.factorie.util.{Accumulator, LocalDoubleAccumulator, FastLogging}

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

trait AccumulatorMaximizer extends WeightsTensorAccumulator {
  acc : AccumulatorMaximizer =>
  def accumulator(family: DotFamily) = new TensorAccumulator {
    def accumulate(t: Tensor) { acc.accumulate(family, t, 1.0)}
    def accumulate(index: Int, value: Double) { acc.accumulate(family, index, value)}
    def accumulate(t: Tensor, factor: Double) { acc.accumulate(family, t, factor)}
    def combine(ta: Accumulator[Tensor]) {throw new Error("Not implemented")}
  }

  def accumulate(family: DotFamily, t: Tensor) { accumulate(family, t, 1.0)}
  def combine(ta: Accumulator[Tensor]) { throw new Error("Not implemented")}
  def accumulate(t: Tensor, factor: Double) { throw new Error("Not implemented")}
  def accumulate(index: Int, value: Double) { throw new Error("Not implemented")}
  def accumulate(t: Tensor) {accumulate(t, 1.0)}
}

class GradientAccumulatorMaximizer(val weights: WeightsTensor, learningRate: Double = 0.1) extends AccumulatorMaximizer {
  def lrate : Double = learningRate
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) { throw new Error("Not implemented") }

  def accumulate(family: DotFamily, t: Tensor, factor: Double) { weights(family) += (t, lrate * factor) }

  def accumulate(family: DotFamily, index: Int, value: Double) { weights(family)(index) += lrate*value }
}

class AdagradAccumulatorMaximizer(val model: Model, learningRate: Double = 0.1) extends AccumulatorMaximizer {
  val weights = model.weightsTensor.asInstanceOf[WeightsTensor]
  val sumGradientsSquared = model.newBlankDenseWeightsTensor
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) {throw new Error("Not implemented")}

  def accumulate(family: DotFamily, t: Tensor, factor: Double) {
    val w = weights(family)
    val g = sumGradientsSquared(family)
    (w,g) match {
      case (w:DenseTensor,t:SparseIndexedTensor) =>
        val indices = t._indices
        val values = t._values
        var i = 0
        while (i < t.activeDomainSize) {
          g(indices(i)) += values(i)*values(i)
          w(indices(i)) += values(i)*learningRate/math.sqrt(g(indices(i)))
          i += 1
        }
    }
  }

  def accumulate(family: DotFamily, index: Int, value: Double) {
    sumGradientsSquared(family)(index) += value * value
    weights(family)(index) += value * learningRate / math.sqrt(sumGradientsSquared(family)(index))
  }
}

class InlineSGDTrainer[M<:Model](val model: M, val lrate : Double = 0.01, var optimizer : AccumulatorMaximizer = null) extends Trainer[M] {
  if (optimizer == null) optimizer = new GradientAccumulatorMaximizer(model.weightsTensor.asInstanceOf[WeightsTensor], lrate)

  def processExamples(examples: Iterable[Example[M]]) {
    examples.foreach(e => e.accumulateExampleInto(model, optimizer, null, null))
  }

  def isConverged = false
}

class SGDTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer = new MIRA) extends Trainer[M] {
  var gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankSparseWeightsTensor.asInstanceOf[WeightsTensor])

  val marginAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    examples.foreach(example => {
      // FIXME: creating a new one every go round is infinitely faster than zero-ing the old tensor - should this be?
      // also, the answers given are different - which means there's some bug somewhere, probably using old values
      // in the gradient sparse tensor even tho _npos is set to 0! Need to investigate this further -luke
//      gradientAccumulator.tensor.zero()
      gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankSparseWeightsTensor.asInstanceOf[WeightsTensor])
      example.accumulateExampleInto(model, gradientAccumulator, null, marginAccumulator)
      optimizer.step(model.weightsTensor, gradientAccumulator.tensor, Double.NaN, marginAccumulator.value)
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
