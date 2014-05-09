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
package cc.factorie.app.mf

import cc.factorie._
import cc.factorie.la.WeightsMapAccumulator
import util.DoubleAccumulator
import cc.factorie.variable.DiscreteDomain
import cc.factorie.model.Parameters

/**
 * User: apassos
 * Date: 4/5/13
 * Time: 3:04 PM
 */

/**
 * Learns a matrix factorization model to ensure that given
 * triples (query,positive,negative), the score of (query,positive)
 * is bigger than the score of (query,negative)
 *
 * Algorithm from http://www.australianscience.com.au/research/google/37180.pdf
 * "WSABIE: Scaling Up To Large Vocabulary Image Annotation" by Weston, Bengio, and Usunier
 */
object WSabie {
  class WSabieModel(val domain: DiscreteDomain, val numEmbeddings: Int, val rng: java.util.Random) extends Parameters {
    val weights = Weights(setToRandom(new la.DenseTensor2(numEmbeddings, domain.size), rng))
    def setToRandom(t: la.DenseTensor2, rng: java.util.Random): la.DenseTensor2 = {
      var i = 0
      while (i < t.length) {
        t(i) = rng.nextGaussian()
        i += 1
      }
      t
    }
    def score(query: la.Tensor1, vector: la.Tensor1) = (weights.value * query) dot (weights.value * vector)
    def rank(query: la.Tensor1, vectors: Seq[la.Tensor1]): Seq[la.Tensor1] = vectors.sortBy(v => -score(query, v))
  }

  class WSabieExample(model: WSabieModel, val query: la.Tensor1, val positive: la.Tensor1, val negative: la.Tensor1) extends optimize.Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
      val weights = model.weights.value
      val queryEmbeddings = weights * query
      val posEmbeddings = weights * positive
      val negEmbeddings = weights * negative

      val queryPos = queryEmbeddings dot posEmbeddings
      val queryNeg = queryEmbeddings dot negEmbeddings
      if (queryPos < queryNeg + 1) {
        if (value ne null) value.accumulate(queryPos - queryNeg - 1)
        if (gradient ne null) {
          gradient.accumulate(model.weights, posEmbeddings outer query)
          gradient.accumulate(model.weights, negEmbeddings outer query, -1)
          gradient.accumulate(model.weights, queryEmbeddings outer positive)
          gradient.accumulate(model.weights, queryEmbeddings outer negative, -1)
        }
      }
    }
  }
}
