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

import cc.factorie.variable.MutableVar

import scala.language.implicitConversions

package object directed {
  /** Create a new DirectedFactor, make it the "parent" generating factor for this variable,
      and add this new factor to the given model. */
  implicit def generatedVarExtras[V<:Var](v:V) = new GeneratedVarWrapper(v)
  implicit def generatedMutableVarExtras[V<:MutableVar](v:V) = new GeneratedMutableVarWrapper(v)
}

