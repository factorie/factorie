/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
