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

object FastSorting {
  // NOTE: This sorts keeps the keys and values in correspondence, while sorting by the keys
  def quickSort(keys: Array[Int], values: Array[Int]): Unit = {
    // lo = pivot, hi = range
    def quickSortInner(keys: Array[Int], values: Array[Int], lo: Int, hi: Int): Unit = {
      if (lo < hi) {
        val part = partition(keys, values, lo, hi)
        quickSortInner(keys, values, lo, part)
        quickSortInner(keys, values, part + 1, hi)
      }
    }
    def partition(keys: Array[Int], values: Array[Int], lo: Int, hi: Int): Int = {
      val piv = (lo + hi) / 2
      val x = keys(piv)
      var i = lo - 1
      var j = hi + 1
      var res = -1
      while (res < 0) {
        i += 1
        while (i < hi && keys(i) < x) i += 1
        j -= 1
        while (j > lo && keys(j) > x) j -= 1
        if (i < j) swap(keys, values, i, j)
        else res = j
      }
      res
    }
    def swap(keys: Array[Int], values: Array[Int], i: Int, j: Int): Unit = {
      val keyTemp = keys(i)
      val valTemp = values(i)
      keys(i) = keys(j)
      values(i) = values(j)
      keys(j) = keyTemp
      values(j) = valTemp
    }
    quickSortInner(keys, values, 0, keys.length - 1)
  }
}
