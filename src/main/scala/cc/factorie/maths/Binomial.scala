/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.maths

trait Binomial {
  //import LogProb._
  /**
   * Computes p(x;n,p) where x~B(n,p)
   */
  // Copied as the "classic" method from Catherine Loader.
  //  Fast and Accurate Computation of Binomial Probabilities.
  //   2001.  (This is not the fast and accurate version.)
  def logBinom(x:Int, n:Int, p:Double): Double = {
    logFactorial (n) - logFactorial (x) - logFactorial (n - x)
      + (x*math.log (p)) + ((n-x)*math.log (1-p))
   }

  /** Vastly inefficient O(x) method to compute cdf of B(n,p)  */
  def pbinom (x:Int, n:Int, p:Double): Double = {
    var sum: Double = Double.NegativeInfinity
    for (i <- 0 to x) sum = sumLogProb(sum, logBinom(i, n, p))
    math.exp(sum)
  }
}
