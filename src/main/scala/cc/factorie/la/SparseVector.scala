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

package cc.factorie.la
import cc.factorie._

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory.
    @author Andrew McCallum */
// For now, just alias to SparseHashVector.  Later perhaps this will be different
class SparseVector(size:Int) extends SparseHashVector(size) {
  def this(size:Int, occurrences:Seq[Int]) = { this(size); occurrences.foreach(increment(_, 1.0)) }
  //private var used = 0
  //private var capacity = 8
  //private val indices = new Array[Int](capacity)
  //private val values = new Array[Double](capacity)
  //private def ensureCapacity(c:Int): Unit = {}
}
