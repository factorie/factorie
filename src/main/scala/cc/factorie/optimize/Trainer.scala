package cc.factorie.optimize

import cc.factorie.app.classify.LogLinearModel
import cc.factorie.la._
import util._
import cc.factorie._
import java.util.concurrent.Callable
import util._
import util.FastLogging

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/17/12
 * Time: 7:29 PM
 * To change this template use File | Settings | File Templates.
 */

/** Learns the parameters of a Model by processing the gradients and values from a collection of Examples. */
trait Trainer[M<:WeightsDef] {
  /** The Model that is being trained. */
  def model: M
  /** Use all these Examples once to make progress towards training */
  def processExamples(examples:Iterable[Example[M]]): Unit
  /** Would more training help? */
  def isConverged: Boolean
  /** Repeatedly process the examples until training has converged. */
  def trainFromExamples(examples:Iterable[Example[M]]): Unit = while (!isConverged) processExamples(examples)
}


/** Learns the parameters of a Model by summing the gradients and values of all Examples, 
    and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS). */
class BatchTrainer[M<:WeightsDef](val model:M, val optimizer:GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] with FastLogging {
  val gradientAccumulator = new LocalTensorsAccumulator(model.weightsSet.blankDenseCopy)
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  // TODO This is sad:  The optimizer determines which of gradient/value/margin it needs, but we don't know here
  // so we create them all, possibly causing the Example to do more work.
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    gradientAccumulator.tensor.zero()
    valueAccumulator.value = 0.0
    val startTime = System.currentTimeMillis
    examples.foreach(example => example.accumulateExampleInto(model, gradientAccumulator, valueAccumulator))
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(gradientAccumulator.tensor.oneNorm, valueAccumulator.value, timeString))
    optimizer.step(model.weightsSet, gradientAccumulator.tensor, valueAccumulator.value)
  }
  def isConverged = optimizer.isConverged
}

class ParallelBatchTrainer[M<:WeightsDef](val model: M, val optimizer: GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] with FastLogging {
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalTensorsAccumulator] { def initialValue = new LocalTensorsAccumulator(model.weightsSet.blankDenseCopy) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { def initialValue = new LocalDoubleAccumulator }
    val startTime = System.currentTimeMillis
    examples.par.foreach(example => example.accumulateExampleInto(model, gradientAccumulator.get, valueAccumulator.get))
    val grad = gradientAccumulator.instances.reduce((l, r) => { l.combine(r); l }).tensor
    val value = valueAccumulator.instances.reduce((l, r) => { l.combine(r); l }).value
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(grad.oneNorm, value, timeString))
    optimizer.step(model.weightsSet, grad, value)
  }
  def isConverged = optimizer.isConverged
}

class SynchronizedBatchTrainer[M<:WeightsDef](val model: M, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, val nThreads: Int = Runtime.getRuntime.availableProcessors()) extends Trainer[M] with FastLogging {
  import collection.JavaConversions._
  def examplesToRunnables[M<: WeightsDef](es: Iterable[Example[M]], model: M, grad: TensorsAccumulator, va: DoubleAccumulator): Seq[Callable[Object]] =
    es.map(e => new Callable[Object] { def call() = { e.accumulateExampleInto(model, grad, va); null.asInstanceOf[Object] } }).toSeq

  val gradientAccumulator = new SynchronizedTensorsAccumulator(model.weightsSet.blankDenseCopy)
  val valueAccumulator = new SynchronizedDoubleAccumulator
  var runnables = null.asInstanceOf[java.util.Collection[Callable[Object]]]
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (runnables eq null) {
      runnables = examplesToRunnables[M](examples, model, gradientAccumulator, valueAccumulator)
    }
    gradientAccumulator.l.tensor.zero()
    valueAccumulator.l.value = 0
    val startTime = System.currentTimeMillis
    if (isConverged) return
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(gradientAccumulator.tensor.oneNorm, valueAccumulator.l.value, timeString))
    optimizer.step(model.weightsSet, gradientAccumulator.tensor, valueAccumulator.l.value)
  }
  def isConverged = optimizer.isConverged
}

class HogwildTrainer[M<:WeightsDef](val model: M, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3, var logEveryN : Int = -1) extends Trainer[M] with FastLogging {
  import collection.JavaConversions._
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = System.currentTimeMillis()
  def examplesToRunnables[M<: WeightsDef](es: Iterable[Example[M]], model: M): Seq[Callable[Object]] = es.map(e => {
    new Callable[Object] { 
      def call() = {
        val gradient = model.weightsSet.blankSparseCopy
        val gradientAccumulator = new LocalTensorsAccumulator(gradient)
        val value = new LocalDoubleAccumulator()
        e.accumulateExampleInto(model, gradientAccumulator, value)
        // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
        gradient.tensors.foreach(t => if (t.isInstanceOf[SparseIndexedTensor]) t.asInstanceOf[SparseIndexedTensor].apply(0))
        optimizer.synchronized {
          optimizer.step(model.weightsSet, gradient, value.value)
          examplesProcessed += 1
          accumulatedValue += value.value
          if (examplesProcessed % logEveryN == 0) {
            val accumulatedTime = System.currentTimeMillis() - t0
            logger.info(examplesProcessed + " examples at " + (1000.0*logEveryN/accumulatedTime) + " examples/sec. Average objective: " + (accumulatedValue / logEveryN))
            t0 = System.currentTimeMillis()
            accumulatedValue = 0
          }
        }
        null.asInstanceOf[Object]
      }
    }
  }).toSeq
  var runnables = null.asInstanceOf[java.util.Collection[Callable[Object]]]
  var iteration = 0
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    if (runnables eq null) runnables = examplesToRunnables[M](examples, model)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
  }
  def isConverged = iteration >= maxIterations
}

class LockingStochasticTrainer[M<:WeightsDef](model: M,
                                         optimizer: GradientOptimizer,
                                         nThreads: Int = Runtime.getRuntime.availableProcessors(),
                                         maxIterations: Int = 3)(implicit val locker: ExampleLocker)
        extends HogwildTrainer[M](model, optimizer, nThreads, maxIterations) {
  override def examplesToRunnables[M<: WeightsDef](es: Iterable[Example[M]], model: M): Seq[Callable[Object]] = es.map(e => {
    new Callable[Object] {
      def call() = {
        val gradient = model.weightsSet.blankSparseCopy
        val gradientAccumulator = new LocalTensorsAccumulator(gradient)
        val valueAccumulator = new LocalDoubleAccumulator()
        val ee = locker.getLockingExample(e, model)
        ee.lockExample()
        e.accumulateExampleInto(model, gradientAccumulator, valueAccumulator)
        // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
        gradient.tensors.foreach(t => if (t.isInstanceOf[SparseIndexedTensor]) t.asInstanceOf[SparseIndexedTensor].apply(0))
        optimizer.step(model.weightsSet, gradient, valueAccumulator.value)
        ee.unlockExample()
        null.asInstanceOf[Object]
      }
    }
  }).toSeq
}


class OnlineTrainer[M<:WeightsDef](val model:M, val optimizer:GradientOptimizer = new AdaGrad, val maxIterations: Int = 3, var logEveryN: Int = -1) extends Trainer[M] with util.FastLogging {
  var gradientAccumulator = new LocalTensorsAccumulator(model.weightsSet.blankSparseCopy)
  var iteration = 0
  val valueAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    var valuesSeenSoFar = 0.0
    var timePerIteration = 0L
    examples.zipWithIndex.foreach({ case (example, i) => {
      if ((i % logEveryN == 0) && (i != 0)) {
        logger.info(i + " examples in "+ (1000.0*logEveryN/timePerIteration)+" examples/sec. Average objective: " + (valuesSeenSoFar/logEveryN))
        valuesSeenSoFar = 0.0
        timePerIteration = 0
      }
      val t0 = System.currentTimeMillis()
      gradientAccumulator.tensor.zero()
      valueAccumulator.value = 0
      example.accumulateExampleInto(model, gradientAccumulator, valueAccumulator)
      valuesSeenSoFar += valueAccumulator.value
      optimizer.step(model.weightsSet, gradientAccumulator.tensor, valueAccumulator.value)
      timePerIteration += System.currentTimeMillis() - t0
    }})
  }
  def isConverged = iteration >= maxIterations
}

/** Train using one trainer, until it has converged, and then use the second trainer instead.
    Typically use is to first train with an online sochastic gradient ascent such as OnlineTrainer and AdaGrad,
    and then a batch trainer, like BatchTrainer and LBFGS. */
class TwoStageTrainer[M<:WeightsDef](val model: M, firstTrainer: Trainer[M], secondTrainer: Trainer[M]) {
  def processExamples(examples: Iterable[Example[M]]) {
    if (!firstTrainer.isConverged)
      firstTrainer.processExamples(examples)
    else
      secondTrainer.processExamples(examples)
  }
  def isConverged = firstTrainer.isConverged && secondTrainer.isConverged
}
