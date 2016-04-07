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
package cc.factorie.util.namejuggler

import scala.annotation.tailrec

/**
 * Shamelessly yoinked from edu.umass.cs.iesl.scalacommons
 */
object SeqUtils {
  /**
   * A generalization of foldLeft which allows removing more elements from the input than just the head (indeed, which
   * allows non-sequential inputs).  The idea is that the op function filters the input, adds some representation of
   * the removed items to the accumulator, and also returns the remainder.  This process recurses to steady state.
   *
   * @param accum An object representing results accumulated so far
   * @param x An object to filter
   * @param op A function that (in some sense) removes some contents from x and (in some sense) adds something to accum.
   * @tparam B The type of the accumulator
   * @tparam A The type of the input
   * @return
   */
  @tailrec
  def filterFoldLeft[B, A](accum: B, x: A, op: (B, A) => (B, A)): B = {
    val (b, a) = op(accum, x)
    if (b == accum) b // steady state
    else {
      filterFoldLeft[B, A](b, a, op)
    }
  }

  def mergeWarn[T, This <: Traversable[T]](a: This, b: This): This = {
    (a, b) match {
      case (Nil, Nil) => a
      case (p, Nil) => a
      case (Nil, q) => b
      case (p, q) => {
        if (p != q) {
          println("Merging unequal sequences, preferring: " + p + "  to " + q)
        }
        a
      }
    }
  }
}
