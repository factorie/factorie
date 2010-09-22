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

object FactorialCache {
  val size = 13 // 12! = 479 001 600, 13! = 6 227 020 800, java.Integer.MAX_INT = (2^31) - 1 = 2 147 483 647
  private val cache = new Array[Int](size)
  cache(0) = 1; for (i <- 1 until size) cache(i) = i * cache(i-1)
  def factorial(n:Int): Int = cache(n)
}

trait Factorial { 
  //import Factorial._
  def factorial(n:Int): Double = if (n < FactorialCache.size) FactorialCache.factorial(n) else math.exp(logGamma(n+1))
  def logFactorial(n:Int): Double = logGamma(n+1)
}
