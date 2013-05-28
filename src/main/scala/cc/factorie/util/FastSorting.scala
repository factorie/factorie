package cc.factorie.util

import cc.factorie._

object FastSorting {
  // WARNING this sorts both the keys and the values -luke
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
