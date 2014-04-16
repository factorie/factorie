package cc.factorie.util


/** A generic trait for clustering solution containers that can be evaluated. */
trait EvaluatableClustering[ClusterIdType,PointIdType] {
  def clusterIds: Iterable[ClusterIdType]
  def pointIds: Iterable[PointIdType]
  def pointIds(clusterId:ClusterIdType): Iterable[PointIdType]
  def intersectionSize(clusterId1:ClusterIdType, clusterId2:ClusterIdType): Int
  def clusterId(pointId:PointIdType): ClusterIdType
}


/** A method of evaluating the precision, recall and F1 of clustering solutions.  */
trait ClusterF1Evaluation {
  def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation
}

/** Precision and recall of N^2 point-by-point  */
object PairwiseClusterEvaluation extends ClusterF1Evaluation with FastLogging {
  def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
    var tp = 0.0
    var fp = 0.0
    var tn = 0.0
    var fn = 0.0
    val points = predicted.pointIds
    val total: Double = points.size
    var count = 0
    val result = new F1Evaluation
    // go through all mentions
    for (mid <- points) {
      // get the clusters
      val predId = predicted.clusterId(mid)
      val predCluster = predicted.pointIds(predId)
      val trueId = truth.clusterId(mid)
      val trueCluster = truth.pointIds(trueId)
      // calculate overlap
      val clusterOverlap: Double = predicted.intersectionSize(predId, trueId).doubleValue // This is very slow.  We should cache these intersectionSizes
      // update metrics
      tp += clusterOverlap - 1.0
      tn += total - predCluster.size - trueCluster.size + clusterOverlap
      fp += predCluster.size - clusterOverlap
      fn += trueCluster.size - clusterOverlap
      count += 1
      if (count % 100000 == 0) logger.info("count: " + count)
    }
    result.precisionNumerator = tp
    result.precisionDenominator = tp + fp
    result.recallNumerator = tp
    result.recallDenominator = tp + fn
    result
  }
}

object BCubedClusterEvaluation extends ClusterF1Evaluation with FastLogging {
  def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
      val result = new F1Evaluation
      result.precisionDenominator = predicted.pointIds.size
      result.recallDenominator = predicted.pointIds.size
      // go through each mention
      for (mid <- predicted.pointIds) {
        // get pred and true clusters
        val predId = predicted.clusterId(mid)
        val predCluster = predicted.pointIds(predId)
        val trueId = truth.clusterId(mid)
        val trueCluster = truth.pointIds(trueId)
        // calculate overlap between the two
        val clusterOverlap: Int = predicted.intersectionSize(predId, trueId)
        // add to metric
        // prec = overlap / pred.size
        result.precisionNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
        // rec = overlap / truth.size
        result.recallNumerator += clusterOverlap.doubleValue / trueCluster.size.doubleValue
      }
      result
    }
  }



// TODO Move BCubedNoSingletons, MUC and CEAF, etc. here from cc.factorie.util.coref. -akm
