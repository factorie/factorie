package cc.factorie.util

object FastSorting {
  // this single-array in-place quicksort code is adapted from some quicksort code from
  // http://codereview.stackexchange.com/questions/4022/java-implementation-of-quick-sort
  def quickSort(a: Array[Int]): Unit = {
    // p = pivot, r = range
    def quickSortInner(a: Array[Int], p: Int, r: Int): Unit = {
      if (p < r) {
        val q = partition(a, p, r)
        quickSortInner(a, p, q)
        quickSortInner(a, q + 1, r)
      }
    }
    def partition(a: Array[Int], p: Int, r: Int): Int = {
      val x = a(p)
      var i = p - 1
      var j = r + 1
      var res = -1
      while (res < 0) {
        i += 1
        while (i < r && a(i) < x) i += 1
        j -= 1
        while (j > p && a(j) > x) j -= 1
        if (i < j) swap(a, i, j)
        else res = j
      }
      res
    }
    def swap(a: Array[Int], i: Int, j: Int): Unit = {
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }
    quickSortInner(a, 0, a.length - 1)
  }

  // WARNING this sorts both the keys and the values -luke
  def quickSort(keys: Array[Int], values: Array[Int]): Unit = {
    // p = pivot, r = range
    def quickSortInner(keys: Array[Int], values: Array[Int], p: Int, r: Int): Unit = {
      if (p < r) {
        val q = partition(keys, values, p, r)
        quickSortInner(keys, values, p, q)
        quickSortInner(keys, values, q + 1, r)
      }
    }
    def partition(keys: Array[Int], values: Array[Int], p: Int, r: Int): Int = {
      val x = keys(p)
      var i = p - 1
      var j = r + 1
      var res = -1
      while (res < 0) {
        i += 1
        while (i < r && keys(i) < x) i += 1
        j -= 1
        while (j > p && keys(j) > x) j -= 1
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
