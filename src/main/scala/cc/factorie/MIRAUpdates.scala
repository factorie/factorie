/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */


package cc.factorie

import cc.factorie.la._
import collection.mutable.HashMap

// TODO Generalized this so that it doesn't require SampleRank

/** Changes parameters in the direction of the gradient, selecting jump size based on
 *   passive-aggressive MIRA method of Crammer and Singer.
 *
 *  @author Michael Wick
 */
trait MIRAUpdates extends GradientAscentUpdates with SampleRank {
  this: ProposalSampler[_] =>

  def learningRate: Double
  def learningRate_=(x:Double): Unit
  def model: Model
  def learningMargin: Double
  def useObjectiveDiffAsMargin: Boolean = true
  val aggressiveness: Double = 1.0

  // TODO: not sure about learning margin violations
  // TODO  This needs a careful code review.  -akm
  abstract override def updateWeights: Unit = {

    val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2

    val gradient = newGradientAccumulator

    addGradient(gradient, 1.0)

    if (useObjectiveDiffAsMargin)
      learningMargin = changeProposal.objectiveScore.abs
    else 1

    learningRate = math.min(kktMultiplier(changeProposal, gradient), aggressiveness)

    if (learningRate != 0)
      super.updateWeights
  }

  protected val epsilon: Double = 1e-9 // 0.000000001

  def kktMultiplier(changeProposal: Proposal, gradient: HashMap[DotFamily, Vector]): Double = {
    val margin = -(changeProposal.modelScore.abs)
    val l2sqrd: Double = computeSumL2Squared(gradient)
    val error: Double = learningMargin - margin
    var lambda: Double = 0

    if (l2sqrd < 0 - epsilon || 0 + epsilon < l2sqrd)
      lambda = error / l2sqrd

    if (lambda < 0) lambda = 0 // no error (the passive part of passive-aggressive)

    lambda
  }

  def computeSumL2Squared(vectorsMap: HashMap[DotFamily, Vector]): Double = {
    var result = 0.0
    for((t, v) <- vectorsMap)
      result += v dot v
    result
  }

}
