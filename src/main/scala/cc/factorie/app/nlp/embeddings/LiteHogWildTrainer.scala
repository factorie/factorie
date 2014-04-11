package cc.factorie.app.nlp.embeddings
import cc.factorie.optimize.{ GradientOptimizer, Trainer, Example }
import cc.factorie.la.SmartGradientAccumulator
import cc.factorie.util.{ LocalDoubleAccumulator, Threading }
import cc.factorie.model.WeightsSet

class LiteHogwildTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer, val nThreads: Int = Runtime.getRuntime.availableProcessors(), val maxIterations: Int = 3)
  extends Trainer {

  var iteration = 0
  def processExample(e: Example): Unit = {
    val gradientAccumulator = new SmartGradientAccumulator
    val value = new LocalDoubleAccumulator()
    e.accumulateValueAndGradient(value, gradientAccumulator)
    optimizer.step(weightsSet, gradientAccumulator.getMap, value.value)
  }
  def processExamples(examples: Iterable[Example]): Unit = {
    Threading.parForeach(examples.toSeq, nThreads)(processExample(_))
  }
  def isConverged = iteration >= maxIterations
}
