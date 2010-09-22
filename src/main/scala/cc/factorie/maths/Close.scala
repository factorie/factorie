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

trait Close {
  /** Numbers that are closer than this are considered equal */
  def almostEquals (d1:Double, d2:Double, epsilon:Double = 0.000001) : Boolean = math.abs (d1 - d2) < epsilon

  def almostEquals (d1:Array[Double], d2:Array[Double], eps:Double) : Boolean = {
    for (i <- 0 until d1.length) if (!almostEquals(d1(i), d2(i))) return false
    true
  }

  /* given two sequences calculate the L2 distance */
  def L2(a:Seq[Double], b:Seq[Double]): Double = {
    assert(a.size == b.size)
    var sum = 0.0
    for (i <- 0 until a.size) sum += (a(i)-b(i))*(a(i)-b(i))
    math.sqrt(sum)
  }

  // gsc
  /** Checks if <tt>min &lt;= value &lt;= max</tt>. */
  def isWithinRange(value:Double, min:Double, max:Double, epsilon: Double = 0.000001) =
    (value > min || almostEquals(value, min, epsilon)) &&
    (value < max || almostEquals(value, max, epsilon));

}
