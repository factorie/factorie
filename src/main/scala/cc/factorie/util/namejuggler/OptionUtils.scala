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

/**
 * Shamelessly yoinked from edu.umass.cs.iesl.scalacommons
 */
object OptionUtils {
  def merge[T](a: Option[T], b: Option[T], merger: (T, T) => T): Option[T] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(merger(x, y))
      case (Some(x), None) => a
      case (None, Some(y)) => b
      case (None, None) => None
    }
  }

  def mergeWarn[T](a: Option[T], b: Option[T]): Option[T] = {
    def warn(x: T, y: T): T = {
      if (x != y) {
        println("Merging unequal values, preferring: " + x + "  to " + y)
      }
      x
    }
    merge(a, b, warn)
  }


  def mergeOrFail[T](a: Option[T], b: Option[T]): Option[T] = {
    def fail(x: T, y: T): T = {
      if (x != y) {
        throw new OptionMergeException(x, y)
      }
      x
    }

    merge(a, b, fail)
  }
}

class OptionMergeException[T](x: T, y: T) extends Exception("unequal values: " + x + "  ,  " + y)