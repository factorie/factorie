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

package cc.factorie.app.tokenseq
import cc.factorie._

trait TokenInSeq[This<:TokenInSeq[This]] {
  def word: String
  def next: This
  def prev: This
  def hasNext: Boolean
  def hasPrev: Boolean
  def firstInSeq: This
  def tokensInSeq: Iterator[This] = new Iterator[This] {
    var t: This = firstInSeq
    def hasNext: Boolean = t != null
    def next: This = { val result = t; if (t.hasNext) t = t.next else t = null.asInstanceOf[This]; result }
  }
}

