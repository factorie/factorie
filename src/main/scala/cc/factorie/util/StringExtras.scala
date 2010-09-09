/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util;

/** New functionality on Traversable instances, available by implicit conversion in the cc.factorie.factorie package object. */
trait StringExtras {
  val s: String

  def tokenize(r:scala.util.matching.Regex) = r.findAllIn(s)

  def skipUntil(r:scala.util.matching.Regex): String = {
    r.findFirstMatchIn(s) match {
      case Some(m:scala.util.matching.Regex.Match) => s.substring(m.start)
      case None => s
    }
  }

  def skipHeader = skipUntil("\n\n".r)

  /**Implements Levenshtein Distance, with specific operation costs to go from this to s2.  Original version was from scalanlp. */
  def editDistance(s2: String, substCost: Int = 1, deleteCost: Int = 1, insertCost: Int = 1): Int = {
    if (s.length == 0) s2.length
    else if (s2.length == 0) s.length
    else {
      val d = new Array[Array[Int]](s.length + 1, s2.length + 1)
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
