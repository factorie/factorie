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

/** A trait for objects that can provide type-matched substitutions of one object for another.
    @author Andrew McCallum */
@deprecated("Not used anywhere", "Before 10/06/15")
trait Substitutions {
  /** Given an object, return a substitute of the same type. 
      The returned value may be the original argument itself.
      It should never be null. */
  def sub[B](a:B): B
  /** Return true if the argument has a non-equal substitution. */
  def changes(a:Any): Boolean = a == sub(a)
}
