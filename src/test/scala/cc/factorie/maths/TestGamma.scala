/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.maths

import org.junit.Test

class TestGamma {
  @Test def runTest(): Unit = {
    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
    val xs = Seq[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    for (x <- xs) {
//      println(f"gamma($x%f) old: ${factorial(x.toInt - 1).toDouble}%f new: ${math.exp(logGamma(x))}%f")
      assert(math.abs(factorial(x.toInt - 1).toDouble) - math.exp(logGamma(x)) < .01)
    }
  }
}