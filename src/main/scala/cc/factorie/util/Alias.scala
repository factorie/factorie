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

import scala.util.Random

// Vose's Alias Sampling implementation translated from Java version at
// http://alaska-kamtchatka.blogspot.com/2011/12/voses-alias-method.html
// This has linear time setup, uses linear space, and constant-time sampling from any categorical distribution
// pa is an array of weights for each category (doesn't have to be normalized)
class Alias(pa: Array[Double])(implicit random: Random) {
  val limit = pa.size
  private val prob = new Array[Double](limit)
  private val alias = new Array[Int](limit)
  locally {
    var sum = 0.0
    for (i <- 0 until limit)
      sum += pa(i)
    val scale = limit * 1.0 / sum
    val sa = new Array[Double](limit)
    for (i <- 0 until limit)
      sa(i) = pa(i) * scale
    init(sa)
  }
  private def init(sa: Array[Double]): Unit = {
    val small = new Array[Int](limit)
    val large = new Array[Int](limit)
    var ns = 0
    var nl = 0
    for (j <- 0 until limit)
      if (sa(j) > 1) {
        large(nl) = j
        nl += 1
      } else {
        small(ns) = j
        ns += 1
      }
    while (ns != 0 && nl != 0) {
      ns -= 1
      val j = small(ns)
      nl -= 1
      val k = large(nl)
      prob(j) = sa(j)
      alias(j) = k
      sa(k) = (sa(k) + sa(j)) - 1
      if (sa(k) > 1) {
        large(nl) = k
        nl += 1
      } else {
        small(ns) = k
        ns += 1
      }
    }
    while (ns != 0) {
      ns -= 1
      prob(small(ns)) = 1
    }
    while (nl != 0) {
      nl -= 1
      prob(large(nl)) = 1
    }
  }
  def sample(): Int = {
    val u = limit * random.nextDouble()
    val j = math.floor(u).asInstanceOf[Int]
    val p = u - j.asInstanceOf[Double]
    if (p <= prob(j)) j else alias(j)
  }
}