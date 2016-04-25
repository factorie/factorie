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
package cc.factorie

package object model {
  /** An iterable collection for efficiently holding a single Factor.
    Used in various Template classes and in an implicit conversion from Factor to IterableSingleFactor
    so that unroll1,2,3,4 methods (which are supposed to return Iterable[Factor] can be written
    by end users to return a single Factor (which is then implicitly converted to this class).
    @author Andrew McCallum */
  implicit class IterableSingleFactor[F<:Factor](val factor:F) extends Iterable[F] {
    def iterator = Iterator.single(factor)
    override def size = 1
    override def head = factor
  }
}
