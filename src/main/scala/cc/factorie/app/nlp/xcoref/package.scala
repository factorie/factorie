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
