package cc.factorie.optimize

import cc.factorie.app.classify.LogLinearModel
import cc.factorie.la._
import cc.factorie.util.{Accumulator, LocalDoubleAccumulator, FastLogging, ThreadLocal}
import cc.factorie._
import java.util.concurrent.Callable

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

class ParallelBatchTrainer[M<:Model](val model: M, val optimizer: GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] with FastLogging {
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (isConverged) return
    val gradientAccumulator = new ThreadLocal[LocalWeightsTensorAccumulator] { def initialValue = new LocalWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor]) }
    val valueAccumulator = new ThreadLocal[LocalDoubleAccumulator] { def initialValue = new LocalDoubleAccumulator }
    examples.par.foreach(example => example.accumulateExampleInto(model, gradientAccumulator.get, valueAccumulator.get, null))
    val grad = gradientAccumulator.instances.reduce((l, r) => { l.combine(r); l }).tensor
    val value = valueAccumulator.instances.reduce((l, r) => { l.combine(r); l }).value
    optimizer.step(model.weightsTensor, grad, value, Double.NaN)
  }
  def isConverged = optimizer.isConverged
}

class SynchronizedBatchTrainer[M<:Model](val model: M, val optimizer: GradientOptimizer = new LBFGS with L2Regularization, val nThreads: Int = Runtime.getRuntime.availableProcessors()) extends Trainer[M] with FastLogging {
  import collection.JavaConversions._
  def examplesToRunnables[M<: Model](es: Iterable[Example[M]], model: M, grad: WeightsTensorAccumulator, va: LocalDoubleAccumulator): Seq[Callable[Object]] =
    es.map(e => new Callable[Object] { def call() = { e.accumulateExampleInto(model, grad, va, null); null.asInstanceOf[Object] } }).toSeq

  val gradientAccumulator = new SynchronizedWeightsTensorAccumulator(model.newBlankWeightsTensor.asInstanceOf[WeightsTensor])
  val valueAccumulator = new LocalDoubleAccumulator
  var runnables = null.asInstanceOf[java.util.Collection[Callable[Object]]]
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    if (runnables eq null) {
      runnables = examplesToRunnables[M](examples, model, gradientAccumulator, valueAccumulator)
    }
    gradientAccumulator.l.tensor.zero()
    valueAccumulator.value = 0
    if (isConverged) return
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
    optimizer.step(model.weightsTensor, gradientAccumulator.tensor, valueAccumulator.value, Double.NaN)
  }
  def isConverged = optimizer.isConverged
}

class HogwildTrainer[M<:Model](val model: M, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3) extends Trainer[M] {
  import collection.JavaConversions._
  def examplesToRunnables[M<: Model](es: Iterable[Example[M]], model: M): Seq[Callable[Object]] = es.map(e => {
    new Callable[Object] { 
      def call() = { 
        val gradient = model.newBlankSparseWeightsTensor
        val gradientAccumulator = new LocalWeightsTensorAccumulator(gradient)
        e.accumulateExampleInto(model, gradientAccumulator, null, null)
        // The following line will effectively call makeReadable on all the sparse tensors before acquiring the lock
        gradient.tensors.foreach(t => if (t.isInstanceOf[SparseIndexedTensor]) t.asInstanceOf[SparseIndexedTensor].apply(0))
        optimizer.synchronized { optimizer.step(model.weightsTensor, gradient, 0, 0) }
        null.asInstanceOf[Object]
      }
    }
  }).toSeq
  var runnables = null.asInstanceOf[java.util.Collection[Callable[Object]]]
  var iteration = 0
  def processExamples(examples: Iterable[Example[M]]): Unit = {
    iteration += 1
    if (runnables eq null) runnables = examplesToRunnables[M](examples, model)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    pool.invokeAll(runnables)
    pool.shutdown()
  }
  def isConverged = iteration >= maxIterations
}


trait AccumulatorMaximizer extends WeightsTensorAccumulator {
  acc: AccumulatorMaximizer =>
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

class GradientAccumulatorMaximizer(val model: Model, learningRate: Double = 0.1) extends AccumulatorMaximizer {
  val weights = model.weightsTensor.asInstanceOf[WeightsTensor]
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) {
    // FIXME: come back and make this more efficient - we can get away with multiplying the
    // left tensor since its the per-label gradient - luke
    t1 *= learningRate
    weights(family) += new Outer1Tensor2(t1, t2)
  }

  def accumulate(family: DotFamily, t: Tensor, factor: Double) { weights(family) += (t, learningRate * factor) }

  def accumulate(family: DotFamily, index: Int, value: Double) { weights(family)(index) += learningRate * value }
}

class L2GradientAccumulatorMaximizer(val model: Model, learningRate: Double = 0.1, val l2: Double = 0.1, val projectAfter: Int = 500) extends AccumulatorMaximizer {
  val weights = model.weightsTensor.asInstanceOf[WeightsTensor]
  var step = 0
  def lrate : Double = learningRate
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) {
    // FIXME: come back and make this more efficient - we can get away with multiplying the
    // left tensor since its the per-label gradient - luke
    t1 *= learningRate
    weights(family) += new Outer1Tensor2(t1, t2)
    tryProject()
    step += 1
  }
  def accumulate(family: DotFamily, t: Tensor, factor: Double) {
    weights(family) += (t, lrate * factor)
    tryProject()
    step += 1
  }
  private def tryProject() {
    if (step % projectAfter == 0) {
      weights *= math.min(1, (1 / math.sqrt(l2)) / weights.twoNorm)
    }
  }
  def accumulate(family: DotFamily, index: Int, value: Double) { sys.error("intentionally unimplemented") }
}

// This implements the AdaGrad algorithm (with Composite Mirror Descent update) from
// "Adaptive Subgradient Methods for Online Learning and Stochastic Optimization" by Duchi et al.
class AdagradAccumulatorMaximizer(val model: Model, learningRate: Double = 0.1, delta: Double = 0.1) extends AccumulatorMaximizer {
  val weights = model.weightsTensor.asInstanceOf[WeightsTensor]
  val sumGradientsSquared = model.newBlankDenseWeightsTensor
  def accumulateOuter(family: DotFamily, t1: Tensor1, t2: Tensor1) {
    accumulate(family, new Outer1Tensor2(t1, t2), 1.0)
  }
  def accumulate(family: DotFamily, t: Tensor, factor: Double) {
    // FIXME "factor" argument is ignored - luke
    val w = weights(family)
    val g = sumGradientsSquared(family)
    (w, t) match {
      case (w: DenseTensor, t: SparseIndexedTensor) =>
//        println("case 1")
        val indices = t._indices
        val values = t._values
        var i = 0
        while (i < t.activeDomainSize) {
          g += (indices(i), values(i) * values(i))
          w += (indices(i), values(i) * learningRate / (delta + math.sqrt(g(indices(i)))))
          i += 1
        }
      case (w: DenseTensor, t: Outer1Tensor2) if t.tensor2.isInstanceOf[SparseBinaryTensorLike1] =>
//        println("case 2")
        val l1 = t.tensor1.length
        val l2 = t.tensor2.length
        val t2Indices = t.tensor2.asInstanceOf[SparseBinaryTensorLike1].activeDomain
        val wArr = w.asArray
        val l2Active = t2Indices.length
        var i = 0
        while (i < l1) {
          val t1Val = t.tensor1(i)
          var j = 0
          if (t1Val != 0.0)
            while (j < l2Active) {
              val idx = i * l2 + t2Indices.array(j)
              g += (idx, t1Val * t1Val)
              wArr(idx) += t1Val * learningRate / (delta + math.sqrt(g(idx)))
              j += 1
            }
          i += 1
        }
      case (w: DenseTensor, t: Outer1Tensor2) if t.tensor2.isInstanceOf[SparseIndexedTensor] =>
//        println("case 3")
        val l1 = t.tensor1.length
        val l2 = t.tensor2.length
        val t2 = t.tensor2.asInstanceOf[SparseIndexedTensor]
        val t2Indices = t2._indices
        val t2Vals = t2._values
        val wArr = w.asArray
        val l2Active = t2.activeDomainSize
        var i = 0
        while (i < l1) {
          val t1Val = t.tensor1(i)
          var j = 0
          if (t1Val != 0.0)
            while (j < l2Active) {
              val idx = i * l2 + t2Indices(j)
              val t2Val = t2Vals(j)
              g += (idx, t1Val * t1Val * t2Val * t2Val)
              wArr(idx) += t1Val * t2Val * learningRate / (delta + math.sqrt(g(idx)))
              j += 1
            }
          i += 1
        }
      case (w:DenseTensor, t: DiscreteValue) =>
        val idx = t.intValue
        g(idx) += factor*factor
        w.asArray(idx) += factor * learningRate / (delta + math.sqrt(g(idx)))
      case (w:DenseTensor, t: SingletonBinaryTensor) =>
        val idx = t.singleIndex
        g(idx) += factor*factor
        w.asArray(idx) += factor * learningRate / (delta + math.sqrt(g(idx)))
      case (w:DenseTensor,  t:DenseTensor) =>
        val wArr = w.asArray
        val gArr = g.asArray
        val tArr = t.asArray
        var i = 0
        val ws = w.length
        while (i < ws) {
          gArr(i) += tArr(i)*factor*factor*tArr(i)
          wArr(i) += factor*tArr(i)/(delta + math.sqrt(gArr(i)))
          i += 1
        }
      case (w:DenseTensor, t:SingletonBinaryLayeredTensor2) => {
        t.inner match {
          case inner:SparseBinaryTensorLike1 =>
            val indices = inner.activeDomain
            val wArr = w.asArray
            val gArr = g.asArray
            var i = 0
            while (i < indices.length) {
              val idx = t.singleIndex(t.singleIndex1, indices(i))
              gArr(idx) += factor * factor
              wArr(idx) += factor * learningRate / (delta + math.sqrt(gArr(idx)))
              i += 1
            }
          case _ => sys.error("Unimplemented type: " + t.inner.getClass.getName)
        }
      }
      case _ =>
        sys.error("The types are not implemented: " + w.getClass.getName + " and " + t.getClass.getName)
    }
  }

  def accumulate(family: DotFamily, index: Int, value: Double) {
    sumGradientsSquared(family)(index) += value * value
    weights(family)(index) += value * learningRate / math.sqrt(sumGradientsSquared(family)(index))
  }
}

class InlineSGDTrainer[M<:Model](val model: M, val lrate : Double = 0.01, var optimizer: AccumulatorMaximizer = null, val maxIterations: Int = 3) extends Trainer[M] {
  if (optimizer == null) optimizer = new GradientAccumulatorMaximizer(model, lrate)
  var iteration = 0
  def processExamples(examples: Iterable[Example[M]]) {
    iteration += 1
    examples.foreach(e => e.accumulateExampleInto(model, optimizer, null, null))
  }

  def isConverged = iteration >= maxIterations
}

class SGDTrainer[M<:Model](val model:M, val optimizer:GradientOptimizer = new AdaGrad, val maxIterations: Int = 3, val logEveryN: Int = 10000) extends Trainer[M] with util.FastLogging {
  var gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankSparseWeightsTensor.asInstanceOf[WeightsTensor])
  var iteration = 0
  val marginAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example[M]]): Unit = {
    iteration += 1
    examples.zipWithIndex.foreach({ case (example, i) => {
      if (i % logEveryN == 0) logger.info(i + " examples")
      gradientAccumulator.tensor.zero()
      marginAccumulator.value = 0
      example.accumulateExampleInto(model, gradientAccumulator, null, marginAccumulator)
      optimizer.step(model.weightsTensor, gradientAccumulator.tensor, Double.NaN, marginAccumulator.value)
    }})
  }
  def isConverged = iteration >= maxIterations
}

class SGDThenBatchTrainer[M<:Model](val model: M, sgdTrainer: Trainer[M], batchTrainer: Trainer[M]) {
  def processExamples(examples: Iterable[Example[M]]) {
    if (!sgdTrainer.isConverged)
      sgdTrainer.processExamples(examples)
    else
      batchTrainer.processExamples(examples)
  }
  def isConverged = sgdTrainer.isConverged && batchTrainer.isConverged
}

class InlineSGDThenBatchTrainer[M<:Model](
  val model: M, val lrate: Double = 0.01, val numSgdPasses: Int = 5, var sgdOptimizer: AccumulatorMaximizer = null,
  val batchOptimizer: GradientOptimizer = new LBFGS with L2Regularization) extends Trainer[M] {
  var step = 1
  if (sgdOptimizer == null) sgdOptimizer = new GradientAccumulatorMaximizer(model, lrate)
  private val batchTrainer = new BatchTrainer[M](model, batchOptimizer)
  def processExamples(examples: Iterable[Example[M]]) {
    if (step <= numSgdPasses)
      examples.foreach(e => e.accumulateExampleInto(model, sgdOptimizer, null, null))
    else
      batchTrainer.processExamples(examples)
    step += 1
  }
  def isConverged = batchTrainer.isConverged
}

