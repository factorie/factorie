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

trait Accumulator[A] {
  //def zero(): Unit
  def accumulate(t: A) : Unit
  // TODO Rename this method accumulate!
  def combine(ta: Accumulator[A]): Unit
}

trait DoubleAccumulator extends Accumulator[Double]

class LocalDoubleAccumulator(var value:Double = 0.0) extends DoubleAccumulator {
  def accumulate(t: Double) : Unit = value += t
  def combine(a: Accumulator[Double]): Unit = a match {
    case a: LocalDoubleAccumulator => value += a.value
  }
}

trait DoubleSeqAccumulator extends Accumulator[DoubleSeq] {
  def accumulate(index: Int, value: Double): Unit
}

class LocalDoubleSeqAccumulator(val tensor: MutableDoubleSeq) extends DoubleSeqAccumulator {
  def accumulate(t: DoubleSeq) = tensor += t
  def accumulate(index: Int, value: Double): Unit = tensor(index) += value
  def combine(a: Accumulator[DoubleSeq]): Unit = a match {
    case a: LocalDoubleSeqAccumulator => tensor += a.tensor
  }
}

