/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._
import cc.factorie.model.WeightsSet
import cc.factorie.util.{FastLogging, _}

/**
 * Learns the parameters of a Model by processing the gradients and values from a collection of Examples.
 * @author Alexandre Passos
 */
trait Trainer {
  /**
   * Process the examples once.
   * @param examples Examples to be processed
   */
  def processExamples(examples: Iterable[Example]): Unit
  /** Would more training help? */
  def isConverged: Boolean
  /** Repeatedly process the examples until training has converged. */
  def trainFromExamples(examples: Iterable[Example]): Unit = while (!isConverged) processExamples(examples)
}

/**
 * Learns the parameters of a Model by summing the gradients and values of all Examples,
 *  and passing them to a GradientOptimizer (such as ConjugateGradient or LBFGS).
 * @param weightsSet The parameters to be optimized
 * @param optimizer The optimizer
 * @author Alexandre Passos
 */
class BatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, val maxIterations: Int = -1) extends Trainer with FastLogging {
  var iteration = 0
  val gradientAccumulator = new LocalWeightsMapAccumulator(weightsSet.blankDenseMap)
  val valueAccumulator = new LocalDoubleAccumulator(0.0)
  // TODO This is sad:  The optimizer determines which of gradient/value/margin it needs, but we don't know here
  // so we create them all, possibly causing the Example to do more work.
  def processExamples(examples: Iterable[Example]): Unit = {
    iteration += 1
    if (isConverged) return
    gradientAccumulator.tensorSet.zero()
    valueAccumulator.value = 0.0
    val startTime = System.currentTimeMillis
    examples.foreach(example => example.accumulateValueAndGradient(valueAccumulator, gradientAccumulator))
    val ellapsedTime = System.currentTimeMillis - startTime
    logger.info(TrainerHelpers.getBatchTrainerStatus(gradientAccumulator.tensorSet.oneNorm, valueAccumulator.value, ellapsedTime))
    optimizer.step(weightsSet, gradientAccumulator.tensorSet, valueAccumulator.value)
  }
  def isConverged = (maxIterations != -1 && iteration >= maxIterations) || optimizer.isConverged
}

/**
 * Learns the parameters of a model by computing the gradient and calling the
 * optimizer one example at a time.
 * @param weightsSet The parameters to be optimized
 * @param optimizer The optimizer
 * @param maxIterations The maximum number of iterations until reporting convergence
 * @param logEveryN After this many examples a log will be printed. If set to -1 10 logs will be printed.
 * @author Alexandre Passos
 */
class OnlineTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new AdaGrad, val maxIterations: Int = 3, var logEveryN: Int = -1) extends Trainer with util.FastLogging {
  var iteration = 0
  val valueAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    var valuesSeenSoFar = 0.0
    var timePerIteration = 0L
    var i = 0
    val iter = examples.iterator
    while (iter.hasNext) {
      val example = iter.next()
      val gradientAccumulator = new SmartGradientAccumulator
      if ((logEveryN != 0) && (i % logEveryN == 0) && (i != 0)) {
        logger.info(TrainerHelpers.getOnlineTrainerStatus(i, logEveryN, timePerIteration, valuesSeenSoFar))
        valuesSeenSoFar = 0.0
        timePerIteration = 0
      }
      val t0 = System.currentTimeMillis()
      gradientAccumulator.clear()
      valueAccumulator.value = 0
      example.accumulateValueAndGradient(valueAccumulator, gradientAccumulator)
      valuesSeenSoFar += valueAccumulator.value
      optimizer.step(weightsSet, gradientAccumulator.getMap, valueAccumulator.value)
      timePerIteration += System.currentTimeMillis() - t0
      i+=1
    }
  }
  def isConverged = iteration >= maxIterations
}

/** Train using one trainer, until it has converged, and then use the second trainer instead.
    Typical use is to first train with an online stochastic gradient ascent such as OnlineTrainer and AdaGrad,
    and then a batch trainer, like BatchTrainer and LBFGS.
    @author Alexandre Passos */
class TwoStageTrainer(firstTrainer: Trainer, secondTrainer: Trainer) {
  def processExamples(examples: Iterable[Example]) {
    if (!firstTrainer.isConverged)
      firstTrainer.processExamples(examples)
    else
      secondTrainer.processExamples(examples)
  }
  def isConverged = firstTrainer.isConverged && secondTrainer.isConverged
}

/** This parallel batch trainer keeps a single gradient in memory and locks accesses to it.
    It is useful when computing the gradient in each example is more expensive than
    adding this gradient to the accumulator.
    If it performs slowly then mini-batches should help, or the ThreadLocalBatchTrainer.
    @author Alexandre Passos */
class ParallelBatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = -1)
  extends Trainer with FastLogging {
  var iteration = 0
  val gradientAccumulator = new SynchronizedWeightsMapAccumulator(weightsSet.blankDenseMap)
  val valueAccumulator = new SynchronizedDoubleAccumulator
  def processExamples(examples: Iterable[Example]): Unit = {
    iteration += 1
    if (isConverged) return
    gradientAccumulator.l.tensorSet.zero()
    valueAccumulator.l.value = 0
    val startTime = System.currentTimeMillis
    util.Threading.parForeach(examples.toSeq, nThreads)(_.accumulateValueAndGradient(valueAccumulator, gradientAccumulator))
    val ellapsedTime = System.currentTimeMillis - startTime
    logger.info(TrainerHelpers.getBatchTrainerStatus(gradientAccumulator.l.tensorSet.oneNorm, valueAccumulator.l.value, ellapsedTime))
    optimizer.step(weightsSet, gradientAccumulator.tensorSet, valueAccumulator.l.value)
  }
  def isConverged = (maxIterations != -1 && iteration >= maxIterations) || optimizer.isConverged
}

/** This parallel batch trainer keeps a per-thread gradient to which examples add weights.
    It is useful when there is a very large number of examples, processing each example is
    fast, and the weights are not too big, as it has to keep one copy of the weights per thread.
    @author Alexandre Passos */
class ThreadLocalBatchTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, numThreads: Int = Runtime.getRuntime.availableProcessors()) extends Trainer with FastLogging {
  def processExamples(examples: Iterable[Example]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal(new LocalWeightsMapAccumulator(weightsSet.blankDenseMap))
    val valueAccumulator = new ThreadLocal(new LocalDoubleAccumulator)
    val startTime = System.currentTimeMillis
    util.Threading.parForeach(examples, numThreads)(example => example.accumulateValueAndGradient(valueAccumulator.get, gradientAccumulator.get))
    val grad = gradientAccumulator.instances.reduce((l, r) => { l.combine(r); l }).tensorSet
    val value = valueAccumulator.instances.reduce((l, r) => { l.combine(r); l }).value
    val ellapsedTime = System.currentTimeMillis - startTime
    logger.info(TrainerHelpers.getBatchTrainerStatus(grad.oneNorm, value, ellapsedTime))
    optimizer.step(weightsSet, grad, value)
  }
  def isConverged = optimizer.isConverged
}

/** This uses read-write locks on the tensors to ensure consistency while doing
    parallel online training.
    The guarantee is that while the examples read each tensor they will see a consistent
    state, but this might not be the state the gradients will get applied to.
    The optimizer, however, has no consistency guarantees across tensors.
    @author Alexandre Passos */
class ParallelOnlineTrainer(weightsSet: WeightsSet, val optimizer: GradientOptimizer, val maxIterations: Int = 3, var logEveryN: Int = -1, val nThreads: Int = Runtime.getRuntime.availableProcessors())
 extends Trainer with FastLogging {
  var iteration = 0
  var initialized = false
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = 0L

  private def processExample(e: Example) {
    val gradientAccumulator = new SmartGradientAccumulator
    val value = new LocalDoubleAccumulator()
    e.accumulateValueAndGradient(value, gradientAccumulator)
    // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
    val gradient = gradientAccumulator.getMap
    gradient.tensors.foreach({ case t: SparseIndexedTensor => t.apply(0); case _ => })
    optimizer.step(weightsSet, gradient, value.value)
    this synchronized {
      examplesProcessed += 1
      accumulatedValue += value.value
      if (logEveryN != 0 && examplesProcessed % logEveryN == 0) {
        val accumulatedTime = System.currentTimeMillis() - t0
        logger.info(TrainerHelpers.getOnlineTrainerStatus(examplesProcessed, logEveryN, accumulatedTime, accumulatedValue))
        t0 = System.currentTimeMillis()
        accumulatedValue = 0
      }
    }
  }

  def processExamples(examples: Iterable[Example]) {
    if (!initialized) replaceTensorsWithLocks()
    t0 = System.currentTimeMillis()
    examplesProcessed = 0
    accumulatedValue = 0.0
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    util.Threading.parForeach(examples.toSeq, nThreads)(processExample(_))
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
  def removeLocks() {
    for (key <- weightsSet.keys) {
      key.value match {
        case t: LockingTensor => weightsSet(key) = t.base
      }
    }
  }

  private trait LockingTensor extends Tensor with SparseDoubleSeq {
    val base: Tensor

    def activeDomainSize = lock.withReadLock { base.activeDomainSize }
    override def foreachActiveElement(f: (Int, Double) => Unit) { lock.withReadLock(base.foreachActiveElement(f)) }
    val lock = new util.RWLock
    def activeDomain = base.activeDomain
    def isDense = base.isDense
    def zero() { lock.withWriteLock(base.zero())}
    def +=(i: Int, incr: Double) { lock.withWriteLock( base.+=(i,incr))}
    override def +=(i: DoubleSeq, v: Double) = lock.withWriteLock(base.+=(i,v))
    def dot(ds: DoubleSeq) = lock.withReadLock(base.dot(ds))
    def update(i: Int, v: Double) { lock.withWriteLock(base.update(i,v)) }
    def apply(i: Int) = lock.withReadLock(base.apply(i))
    override def *=(d:Double): Unit = lock.withWriteLock { base *= d}
    override def *=(ds:DoubleSeq): Unit = lock.withWriteLock { base *= ds }
    override def /=(ds:DoubleSeq): Unit = lock.withWriteLock { base /= ds }
  }

  private class LockingTensor1(val base: Tensor1) extends Tensor1 with LockingTensor {
    def dim1 = base.dim1
    override def copy = lock.withReadLock { base.copy }
  }
  private class LockingTensor2(val base: Tensor2) extends Tensor2 with LockingTensor {
    def dim1 = base.dim1
    def dim2 = base.dim2
    def activeDomain1 = lock.withReadLock(base.activeDomain1)
    def activeDomain2 = lock.withReadLock(base.activeDomain2)
    override def *(other: Tensor1) = lock.withReadLock(base * other)
    override def leftMultiply(other: Tensor1) = lock.withReadLock(base leftMultiply other)
    override def copy = lock.withReadLock { base.copy }
  }
  private class LockingTensor3(val base: Tensor3) extends Tensor3 with LockingTensor {
    def dim1 = base.dim1
    def dim2 = base.dim2
    def dim3 = base.dim3
    def activeDomain1 = lock.withReadLock(base.activeDomain1)
    def activeDomain2 = lock.withReadLock(base.activeDomain2)
    def activeDomain3 = lock.withReadLock(base.activeDomain3)
    override def copy = lock.withReadLock { base.copy }
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
    override def copy = lock.withReadLock { base.copy }
  }
}

/** This online trainer synchronizes only on the optimizer, so reads on the weights
    can be done while they are being written to.
    It provides orthogonal guarantees than the ParallelOnlineTrainer, as the examples can have
    inconsistent reads from the same tensor but the optimizer will always
    have a consistent view of all tensors.
    @author Alexandre Passos */
class SynchronizedOptimizerOnlineTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3, var logEveryN : Int = -1)
  extends Trainer with FastLogging {
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = System.currentTimeMillis()
  private def processExample(e: Example): Unit = {
    val gradientAccumulator = new SmartGradientAccumulator
    val value = new LocalDoubleAccumulator()
    e.accumulateValueAndGradient(value, gradientAccumulator)
    // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
    val gradient = gradientAccumulator.getMap
    gradient.tensors.foreach({ case t: SparseIndexedTensor => t.apply(0); case _ => })
    optimizer synchronized {
      optimizer.step(weightsSet, gradient, value.value)
      examplesProcessed += 1
      accumulatedValue += value.value
      if (examplesProcessed % logEveryN == 0) {
        val accumulatedTime = System.currentTimeMillis() - t0
        logger.info(TrainerHelpers.getOnlineTrainerStatus(examplesProcessed, logEveryN, accumulatedTime, accumulatedValue))
        t0 = System.currentTimeMillis()
        accumulatedValue = 0
      }
    }
  }
  var iteration = 0
  def processExamples(examples: Iterable[Example]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    t0 = System.currentTimeMillis()
    examplesProcessed = 0
    accumulatedValue = 0.0
    util.Threading.parForeach(examples.toSeq, nThreads)(processExample(_))
  }
  def isConverged = iteration >= maxIterations
}

/**
 * A parallel online trainer which has no locks or synchronization.
 * Only use this if you know what you're doing.
 * @param weightsSet The parameters to optimize
 * @param optimizer The optimizer
 * @param nThreads How many threads to use
 * @param maxIterations The maximum number of iterations
 * @param logEveryN How often to log.
 * @param locksForLogging Whether to lock around logging. Disabling this might make logging not work at all.
 * @author Alexandre Passos
 */
class HogwildTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3, var logEveryN : Int = -1, val locksForLogging: Boolean = true)
  extends Trainer with FastLogging {
  var examplesProcessed = 0
  var accumulatedValue = 0.0
  var t0 = System.currentTimeMillis()
  val lock = new util.RWLock
  private def processExample(e: Example): Unit = {
    val gradientAccumulator = new SmartGradientAccumulator
    val value = new LocalDoubleAccumulator()
    e.accumulateValueAndGradient(value, gradientAccumulator)
    optimizer.step(weightsSet, gradientAccumulator.getMap, value.value)
    if (locksForLogging) lock.writeLock()
    try {
      examplesProcessed += 1
      accumulatedValue += value.value
      if (examplesProcessed % logEveryN == 0) {
        val accumulatedTime = System.currentTimeMillis() - t0
        logger.info(TrainerHelpers.getOnlineTrainerStatus(examplesProcessed, logEveryN, accumulatedTime, accumulatedValue))
        t0 = System.currentTimeMillis()
        accumulatedValue = 0
      }
    } finally {
      if (locksForLogging) lock.writeUnlock()
    }
  }
  var iteration = 0
  def processExamples(examples: Iterable[Example]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    t0 = System.currentTimeMillis()
    examplesProcessed = 0
    accumulatedValue = 0.0
    util.Threading.parForeach(examples.toSeq, nThreads)(processExample(_))
  }
  def isConverged = iteration >= maxIterations
}


object TrainerHelpers {
  def getTimeString(ms: Long): String =
    if (ms > 120000) f"${ms/60000}%d minutes" else if (ms> 5000) f"${ms/1000}%d seconds" else s"$ms milliseconds"
  def getBatchTrainerStatus(gradNorm: => Double, value: => Double, ms: => Long) =
    f"GradientNorm: $gradNorm%-10g  value $value%-10g ${getTimeString(ms)}%s"
  def getOnlineTrainerStatus(examplesProcessed: Int, logEveryN: Int, accumulatedTime: Long, accumulatedValue: Double) =
    f"$examplesProcessed%20s examples at ${1000.0*logEveryN/accumulatedTime}%5.2f examples/sec. Average objective: ${accumulatedValue / logEveryN}%5.5f"
}

/** A collection of convenience methods for creating Trainers and running them with recommended default values. 
    @author Alexandre Passos */
object Trainer {
  /**
   * Convenient function for training. Creates a trainer, trains until convergence, and evaluates after every iteration.
   * @param parameters The parameters to be optimized
   * @param examples The examples to train on
   * @param maxIterations The maximum number of iterations for training
   * @param evaluate The function for evaluation
   * @param optimizer The optimizer
   * @param useParallelTrainer Whether to use parallel training
   * @param useOnlineTrainer Whether to use online training
   * @param logEveryN How often to log, if using online training
   */
  def train(parameters: WeightsSet, examples: Seq[Example], maxIterations: Int, evaluate: () => Unit, optimizer: GradientOptimizer, useParallelTrainer: Boolean, useOnlineTrainer: Boolean, logEveryN: Int = -1, nThreads: Int = Runtime.getRuntime.availableProcessors(), miniBatch: Int)(implicit random: scala.util.Random) {
    parameters.keys.foreach(_.value) // make sure we initialize the values in a single thread
    optimizer.initializeWeights(parameters)
    val actualEx: Seq[Example] = if (miniBatch == -1) examples else MiniBatchExample(miniBatch, examples).toSeq
    val trainer = if (useOnlineTrainer && useParallelTrainer) new ParallelOnlineTrainer(parameters, optimizer=optimizer, maxIterations=maxIterations, logEveryN=logEveryN, nThreads=nThreads)
      else if (useOnlineTrainer && !useParallelTrainer) new OnlineTrainer(parameters, optimizer=optimizer, maxIterations=maxIterations, logEveryN=logEveryN)
      else if (!useOnlineTrainer && useParallelTrainer) new ParallelBatchTrainer(parameters, optimizer=optimizer, maxIterations=maxIterations, nThreads=nThreads)
      else new BatchTrainer(parameters, optimizer=optimizer, maxIterations=maxIterations)
    trainer match { case t: ParallelOnlineTrainer => t.replaceTensorsWithLocks(); case _ => }
    try {
      while (!trainer.isConverged) {
        trainer.processExamples(actualEx.shuffle)
        optimizer match { case o: ParameterAveraging => o.setWeightsToAverage(parameters); case _ => }
        evaluate()
        optimizer match { case o: ParameterAveraging => o.unSetWeightsToAverage(parameters); case _ => }
      }
    } finally {
      trainer match { case t: ParallelOnlineTrainer => t.removeLocks(); case _ => }
      optimizer.finalizeWeights(parameters)
    }
  }

  /**
   * A convenient way to call Trainer.train() for online trainers.
   * @param parameters The parameters to be optimized
   * @param examples The examples
   * @param evaluate The evaluation function
   * @param useParallelTrainer Whether to train in parallel
   * @param maxIterations The maximum number of iterations
   * @param optimizer The optimizer
   * @param logEveryN How often to log
   */
  def onlineTrain(parameters: WeightsSet, examples: Seq[Example], evaluate: () => Unit = () => (), useParallelTrainer: Boolean=false, maxIterations: Int = 3, optimizer: GradientOptimizer = new AdaGrad with ParameterAveraging, logEveryN: Int = -1 ,nThreads: Int = Runtime.getRuntime.availableProcessors(), miniBatch: Int = -1)(implicit random: scala.util.Random) {
    train(parameters, examples, maxIterations, evaluate, optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=true, logEveryN=logEveryN, nThreads=nThreads, miniBatch)
  }

  /**
   * A convenient way to call Trainer.train() for batch training.
   * @param parameters The parameters to be optimized
   * @param examples The examples
   * @param evaluate The evaluation function
   * @param useParallelTrainer Whether to use a parallel trainer
   * @param maxIterations The maximum number of iterations
   * @param optimizer The optimizer
   */
  def batchTrain(parameters: WeightsSet, examples: Seq[Example], evaluate: () => Unit = () => (), useParallelTrainer: Boolean=true, maxIterations: Int = 200, optimizer: GradientOptimizer = new LBFGS with L2Regularization, nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random) {
    train(parameters, examples, maxIterations, evaluate, optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=false, nThreads=nThreads, miniBatch= -1)
  }
}
