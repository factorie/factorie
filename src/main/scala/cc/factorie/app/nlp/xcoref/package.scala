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

import cc.factorie.util.EvaluatableClustering

/**
 * @author John Sullivan
 */
package object xcoref {
  implicit class NodeListUtils[Vars <: NodeVariables[Vars]](val nodes:Iterable[Node[Vars]]) {
    private val mentionToRoot = nodes.filter(_.isMention).map(m => m.id -> m.root.id).toMap
    private val rootToMentions = nodes.filter(_.isRoot).map(r => r.id -> r.mentionsVar.value.map(_.id)).toMap
    private var mentionToTruth:Map[String, String] = null
    private var truthToMentions:Map[String, Iterable[String]] = null
    mentionToTruth = nodes.collect{ // the logic here is ugly, but for a mention it should always work
      case m:Node[_] if m.isMention && m.variables.isInstanceOf[GroundTruth] => m.id -> m.variables.asInstanceOf[GroundTruth].truth.iterator.next()._1
    }.toMap
    truthToMentions = nodes.collect{
      case m:Node[_] if m.isMention && m.variables.isInstanceOf[GroundTruth] => m.variables.asInstanceOf[GroundTruth].truth.iterator.next()._1 -> m.id
    }.groupBy(_._1).mapValues(_.map(_._2))

    def predictedClustering:EvaluatableClustering[String, String] = new EvaluatableClustering[String, String] {
      def clusterId(pointId: String) = mentionToRoot(pointId)
      def pointIds(clusterId: String) = rootToMentions(clusterId)
      val pointIds = mentionToRoot.keySet
      val clusterIds = rootToMentions.keySet
    }
    def trueClustering:Option[EvaluatableClustering[String, String]] = if(nodes.head.variables.isInstanceOf[GroundTruth]) {
      Some(new EvaluatableClustering[String, String] {
        def clusterId(pointId: String) = mentionToTruth(pointId)
        def pointIds(clusterId: String) = truthToMentions(clusterId)
        val pointIds = mentionToTruth.keySet
        val clusterIds = truthToMentions.keySet
      })
    } else {
      None
    }
  }
}
