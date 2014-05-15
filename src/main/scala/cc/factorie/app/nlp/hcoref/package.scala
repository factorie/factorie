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
package cc.factorie.app.nlp

import cc.factorie.util.BasicEvaluatableClustering

/**
 * @author John Sullivan
 */
package object hcoref {
  implicit class NodeListUtils[Vars <: NodeVariables[Vars]](val nodes:Iterable[Node[Vars]]) {
    private val mentionToRoot = nodes.filter(_.isMention).map(m => m.id -> m.root.id)
    def predictedClustering = new BasicEvaluatableClustering(mentionToRoot)
  }

  implicit class NodeListGrountTruthUtils[Vars <: NodeVariables[Vars] with GroundTruth](val nodes:Iterable[Node[Vars]]) {
    //this logic is ugly, but should always be correct for mentions
    private val mentionToTruth = nodes.filter(_.isMention).map(m => m.id -> m.variables.truth.iterator.next()._1)
    def trueClustering = new BasicEvaluatableClustering(mentionToTruth)
  }
}
