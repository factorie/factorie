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

package cc.factorie.generative
import cc.factorie._
import scala.collection.mutable.HashMap

/** Inferencer with some variational distributions.  
    Currently only supports variational approximations over single variables at a time.
    @author Andrew McCallum */
// TODO Consider variational approximations over sets of variables by defining something like qMap:Map[Factor,Variable] ??
// In any case, I like the idea of using Factor to represent the collection of variables in variational approximation
trait VariationalInferencer {
  /** A map from a variable to its varitional representation as distribution Q. */
  def qMap: scala.collection.Map[Variable,Variable]
  def qOrSelf(v:Variable): Variable = qMap.getOrElse(v, v)
  def q[V<:Variable with QDistribution](v:V) = qMap(v).asInstanceOf[V#QType]
  def qOrNull(v:Variable): Variable = qMap.getOrElse(v, null.asInstanceOf[Variable]) // TODO Redundant with above method?
}

