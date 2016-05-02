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
package cc.factorie.util

import cc.factorie.la.{SparseTensor1, Tensor1}
import org.scalatest._

class TestDoubleSeq extends FlatSpec with Matchers {

  val nSamples = 1e7.toInt

  implicit val random = scala.util.Random

  "DenseDoubleSeq.sampleIndex" should "always sample a correct index" in {

    val masses = Array[Double](0, 10, 0, 1, 0)
    val totalMass = masses.sum
    val props = masses.map(_ / totalMass)

    val seq = new ArrayDoubleSeq(masses)

    val samples = (1 to nSamples).foldLeft(Array.fill(masses.size)(0.0)) { case (acc, i) =>
      acc(seq.sampleIndex(totalMass)) += 1.0
      acc
    } map (_ / nSamples)

    (Tensor1(props:_*) - Tensor1(samples:_*)).twoNorm should be <= 1e-2

  }

  "SparseDoubleSeq.sampleIndex" should "always sample a correct index" in {

    val masses = Array[Double](0, 10, 0, 1, 0)
    val totalMass = masses.sum
    val props = masses.map(_ / totalMass)

    val seq = new SparseTensor1(masses.size)
    masses.zipWithIndex foreach { case (v, i) =>
      seq += (i, v)
    }

    val samples = (1 to nSamples).foldLeft(Array.fill(masses.size)(0.0)) { case (acc, i) =>
      acc(seq.sampleIndex(totalMass)) += 1.0
      acc
    } map (_ / nSamples)

    (Tensor1(props:_*) - Tensor1(samples:_*)).twoNorm should be <= 1e-2

  }

}
