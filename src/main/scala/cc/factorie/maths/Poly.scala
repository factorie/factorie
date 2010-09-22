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

trait Poly {
  def poly(coeff: Array[Double], x: Double): Double = {
    var result: Double = 0;
    var i: Int = coeff.length - 1;
    while (i >= 0) {
      result = result * x + coeff(i);
      i -= 1;
    }
    result;
  }

  // TODO Make this more efficient by avoiding .toArray
  def poly(coeff: Seq[Double], x:Double): Double = poly(coeff.toArray, x)
  //@deprecated("Broken, don't use.")
  //coeff.foldLeft(0.0)((result, v) => result * x + v)
}
