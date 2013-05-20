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
trait Trainer[E <: Example] {
  /** The Model that is being trained. */
//  def weightsSet: WeightsSet
  // TODO Trainer should probably have an overrideable method "newGradient" which we could override to get e.g. dense gradients for Online, sparse for Batch, etc -luke & alex
  /** Use all these Examples once to make progress towards training */
  def processExamples(examples: Iterable[E]): Unit
  /** Would more training help? */
  def isConverged: Boolean
  /** Repeatedly process the examples until training has converged. */
  def trainFromExamples(examples: Iterable[E]): Unit = while (!isConverged) processExamples(examples)
}

/** Learns the parameters of a Model by summing the gradients and values of all Examples, 
    and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS). */
class BatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[Example] with FastLogging {
  val gradientAccumulator = new LocalWeightsMapAccumulator(weightsSet.blankDenseCopy)
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  // TODO This is sad:  The optimizer determines which of gradient/value/margin it needs, but we don't know here
  // so we create them all, possibly causing the Example to do more work.
  def processExamples(examples: Iterable[Example]): Unit = {
    if (isConverged) return
    gradientAccumulator.tensorSet.zero()
    valueAccumulator.value = 0.0
    val startTime = System.currentTimeMillis
    examples.foreach(example => example.accumulateExampleInto(gradientAccumulator, valueAccumulator))
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(gradientAccumulator.tensorSet.oneNorm, valueAccumulator.value, timeString))
    optimizer.step(weightsSet, gradientAccumulator.tensorSet, valueAccumulator.value)
  }
  def isConverged = optimizer.isConverged
}

class ParallelBatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[Example] with FastLogging {
  def processExamples(examples: Iterable[Example]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalWeightsMapAccumulator] { def initialValue = new LocalWeightsMapAccumulator(weightsSet.blankDenseCopy) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { def initialValue = new LocalDoubleAccumulator }
    val startTime = System.currentTimeMillis
    examples.par.foreach(example => example.accumulateExampleInto(gradientAccumulator.get, valueAccumulator.get))
    val grad = gradientAccumulator.instances.reduce((l, r) => { l.combine(r); l }).tensorSet
    val value = valueAccumulator.instances.reduce((l, r) => { l.combine(r); l }).value
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(grad.oneNorm, value, timeString))
    optimizer.step(weightsSet, grad, value)
  }
  def isConverged = optimizer.isConverged
}

class SynchronizedBatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, val nThreads: Int = Runtime.getRuntime.availableProcessors())
  extends Trainer[Example] with FastLogging {
  import collection.JavaConversions._
  def examplesToRunnables(es: Iterable[Example], grad: WeightsMapAccumulator, va: DoubleAccumulator): Seq[Callable[Object]] =
    es.map(e => new Callable[Object] { def call() = { e.accumulateExampleInto(grad, va); null.asInstanceOf[Object] } }).toSeq

  val gradientAccumulator = new SynchronizedWeightsMapAccumulator(weightsSet.blankDenseCopy)
  val valueAccumulator = new SynchronizedDoubleAccumulator
  var runnables = null.asInstanceOf[java.util.Collection[Callable[Object]]]
  def processExamples(examples: Iterable[Example]): Unit = {
    if (runnables eq null) {
      runnables = examplesToRunnables(examples, gradientAccumulator, valueAccumulator)
    }
    gradientAccumulator.l.tensorSet.zero()
    valueAccumulator.l.value = 0
    val startTime = System.currentTimeMillis
    if (isConverged) return
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
    val ellapsedTime = System.currentTimeMillis - startTime
    val timeString = if (ellapsedTime > 120000) "%d minutes".format(ellapsedTime/60000) else if (ellapsedTime > 5000) "%d seconds".format(ellapsedTime/1000) else "%d milliseconds".format(ellapsedTime)
    logger.info("GradientNorm: %-10g  value %-10g %s".format(gradientAccumulator.tensorSet.oneNorm, valueAccumulator.l.value, timeString))
    optimizer.step(weightsSet, gradientAccumulator.tensorSet, valueAccumulator.l.value)
  }
  def isConverged = optimizer.isConverged
}

class HogwildTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3, var logEveryN : Int = -1)
  extends Trainer[Example] with FastLogging {
  import collection.JavaConversions._
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = System.currentTimeMillis()
  def examplesToRunnables(es: Iterable[Example]): Seq[Callable[Object]] = es.map(e => {
    new Callable[Object] { 
      def call() = {
        val gradient = weightsSet.blankSparseCopy
        val gradientAccumulator = new LocalWeightsMapAccumulator(gradient)
        val value = new LocalDoubleAccumulator()
        e.accumulateExampleInto(gradientAccumulator, value)
        // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
        gradient.tensors.foreach(t => if (t.isInstanceOf[SparseIndexedTensor]) t.asInstanceOf[SparseIndexedTensor].apply(0))
        optimizer.synchronized {
          optimizer.step(weightsSet, gradient, value.value)
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
  def processExamples(examples: Iterable[Example]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    if (runnables eq null) runnables = examplesToRunnables(examples)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
  }
  def isConverged = iteration >= maxIterations
}

class OnlineTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new AdaGrad, val maxIterations: Int = 3, var logEveryN: Int = -1) extends Trainer[Example] with util.FastLogging {
  var gradientAccumulator = new LocalWeightsMapAccumulator(weightsSet.blankSparseCopy)
  var iteration = 0
  val valueAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example]): Unit = {
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
      gradientAccumulator.tensorSet.zero()
      valueAccumulator.value = 0
      example.accumulateExampleInto(gradientAccumulator, valueAccumulator)
      valuesSeenSoFar += valueAccumulator.value
      optimizer.step(weightsSet, gradientAccumulator.tensorSet, valueAccumulator.value)
      timePerIteration += System.currentTimeMillis() - t0
    }})
  }
  def isConverged = iteration >= maxIterations
}

/** Train using one trainer, until it has converged, and then use the second trainer instead.
    Typically use is to first train with an online stochastic gradient ascent such as OnlineTrainer and AdaGrad,
    and then a batch trainer, like BatchTrainer and LBFGS. */
class TwoStageTrainer[E <: Example](firstTrainer: Trainer[E], secondTrainer: Trainer[E]) {
  def processExamples(examples: Iterable[E]) {
    if (!firstTrainer.isConverged)
      firstTrainer.processExamples(examples)
    else
      secondTrainer.processExamples(examples)
  }
  def isConverged = firstTrainer.isConverged && secondTrainer.isConverged
}

// Technically we could use other optimizers than GradientSteps but I want something
// which is guaranteed to use += and to not replace the tensors
class ParallelOnlineTrainer(weightsSet: WeightsSet, val optimizer: GradientStep, val maxIterations: Int = 3, var logEveryN: Int = -1, val nThreads: Int = Runtime.getRuntime.availableProcessors())
 extends Trainer[Example] with FastLogging {
  var iteration = 0
  var initialized = false

  import collection.JavaConversions._
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = System.currentTimeMillis()
  def examplesToRunnables(es: Iterable[Example]): Seq[Callable[Object]] = es.map(e => {
    new Callable[Object] {
      def call() = {
        val gradient = weightsSet.blankSparseCopy
        val gradientAccumulator = new LocalWeightsMapAccumulator(gradient)
        val value = new LocalDoubleAccumulator()
        e.accumulateExampleInto(gradientAccumulator, value)
        // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
        gradient.tensors.foreach(t => if (t.isInstanceOf[SparseIndexedTensor]) t.asInstanceOf[SparseIndexedTensor].apply(0))
        optimizer.step(weightsSet, gradient, value.value)
        this synchronized {
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

  def processExamples(examples: Iterable[Example]) {
    if (!initialized) replaceTensorsWithLocks()
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    if (runnables eq null) runnables = examplesToRunnables(examples)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
  }

  def isConverged = iteration >= maxIterations

  def replaceTensorsWithLocks() {
    for (key <- weightsSet.keys) {
      key.value match {
        case t: Tensor1 => weightsSet(key) = new LockingTensor1(t)
        case t: Tensor2 => weightsSet(key) = new LockingTensor2(t)
        case t: Tensor3 => weightsSet(key) = new LockingTensor3(t)
        case t: Tensor4 => weightsSet(key) = new LockingTensor4(t)
      }
    }
    initialized = true
  }

  private trait LockingTensor extends Tensor {
    val base: Tensor
    val lock = new util.RWLock
    def activeDomain = base.activeDomain
    def isDense = base.isDense
    def zero() { lock.withWriteLock(base.zero())}
    def +=(i: Int, incr: Double) { lock.withWriteLock( base.+=(i,incr))}
    override def +=(i: DoubleSeq, v: Double) = lock.withWriteLock(base.+=(i,v))
    def dot(ds: DoubleSeq) = lock.withReadLock(base.dot(ds))
    def update(i: Int, v: Double) { lock.withWriteLock(base.update(i,v)) }
    def apply(i: Int) = lock.withReadLock(base.apply(i))
  }

  private class LockingTensor1(val base: Tensor1) extends Tensor1 with LockingTensor {
    def dim1 = base.dim1
  }
  private class LockingTensor2(val base: Tensor2) extends Tensor2 with LockingTensor {
    def dim1 = base.dim1
    def dim2 = base.dim2
    def activeDomain1 = lock.withReadLock(base.activeDomain1)
    def activeDomain2 = lock.withReadLock(base.activeDomain2)
    override def *(other: Tensor1) = lock.withReadLock(base * other)
  }
  private class LockingTensor3(val base: Tensor3) extends Tensor3 with LockingTensor {
    def dim1 = base.dim1
    def dim2 = base.dim2
    def dim3 = base.dim3
    def activeDomain1 = lock.withReadLock(base.activeDomain1)
    def activeDomain2 = lock.withReadLock(base.activeDomain2)
    def activeDomain3 = lock.withReadLock(base.activeDomain3)
  }
  private class LockingTensor4(val base: Tensor4) extends Tensor4 with LockingTensor {
    def dim1 = base.dim1
    def dim2 = base.dim2
    def dim3 = base.dim3
    def dim4 = base.dim4
    def activeDomain1 = lock.withReadLock(base.activeDomain1)
    def activeDomain2 = lock.withReadLock(base.activeDomain2)
    def activeDomain3 = lock.withReadLock(base.activeDomain3)
    def activeDomain4 = lock.withReadLock(base.activeDomain4)
  }
}