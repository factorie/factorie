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

import scala.util.Try

/** New functionality on String instances, available by implicit conversion in the cc.factorie.factorie package object. */
class StringExtras(val s: String) extends AnyVal {

  def toIntSafe: Option[Int] = Try(s.toInt).toOption
  def toDoubleSafe: Option[Double] = Try(s.toDouble).toOption

  def skipUntil(r:scala.util.matching.Regex): String = {
    r.findFirstMatchIn(s) match {
      case Some(m:scala.util.matching.Regex.Match) => s.substring(m.start)
      case None => s
    }
  }

  /** Return a new string that removes everything before a double newline.
      Useful for skipping newsgroup headers or email headers in plain text documents. */
  def skipHeader = skipUntil("\n\n".r)

  /** Implements Levenshtein Distance, with specific operation costs to go from this String to String s2. */
  def editDistance(s2: String, substCost: Int = 1, deleteCost: Int = 1, insertCost: Int = 1): Int = {
    if (s.length == 0) s2.length
    else if (s2.length == 0) s.length
    else {
      val d = Array.ofDim[Int](s.length + 1, s2.length + 1)
      for (i <- 0 to s.length)
        d(i)(0) = i * deleteCost
      for (i <- 0 to s2.length)
        d(0)(i) = i * insertCost
      for (i <- 1 to s.length; j <- 1 to s2.length) {
        val cost = if (s(i - 1) == s2(j - 1)) 0 else substCost
        d(i)(j) = math.min(d(i - 1)(j) + deleteCost, math.min(d(i)(j - 1) + insertCost, d(i - 1)(j - 1) + cost))
      }
      d(s.length)(s2.length)
    }
  }

}
