package cc.factorie.util

import scala.collection.mutable.{LinkedHashMap,LinkedHashSet}

/** A generic trait for clustering solution containers that can be evaluated.
    intersectionSize must be implemented such that it will work when passed clusters from different EvaluatableClustering instances. */
trait EvaluatableClustering[ClusterIdType,PointIdType] {
  /** Return collection of clusters (or entities). */
  def clusterIds: Iterable[ClusterIdType]
  /** Return collection of points (or mentions). */
  def pointIds: Iterable[PointIdType]
  /** Return those points (or mentions) that are assigned to the given cluster (entity). */
  def pointIds(clusterId:ClusterIdType): Iterable[PointIdType]
  /** Return the cluster (entity) to which the given point (mention) is assigned. */
  def clusterId(pointId:PointIdType): ClusterIdType
  /** Return the number of points in common between the two given clusters (entities).
      If given two clusters from the same EvaluatableClustering, this should always return zero;
      but typically this would be called with two clusters from different EvaluatableClusterings.
      Typically this involves making sure clusterId.equals works on clusterIds from different EvaluatableClusterings. */
  def intersectionSize(clusterId1:ClusterIdType, clusterId2:ClusterIdType): Int
  /** Return a string representation of this clustering.
      It uses the toString representation of clusterId and pointId. */
  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    for (clusterId <- clusterIds) {
      sb.append(clusterId.toString); sb.append('\n')
      for (pointId <- pointIds(clusterId))
        sb.append(pointId.toString); sb.append('\n')
      sb.append('\n')
    }
    sb.toString
  }
}

/** A concrete implementation of EvaluatableClustering based simply on String clusterIds and pointIds. */
class BasicEvaluatableClustering(pointClusterPairs:Iterable[(String,String)]) extends EvaluatableClustering[String,String] {
  /** Initialize from a String in the format returned by EvaluatableClustering.toString */
  def this(serialized:String) = this(EvaluatableClustering.stringToPointClusterPairs(serialized))
  /** Initialize from a io.Source whose contents are in the format returned by EvaluatableClustering.toString */
  def this(serialized:io.Source) = this(EvaluatableClustering.stringToPointClusterPairs(serialized))
  private val clusterToPoints = new LinkedHashMap[String,LinkedHashSet[String]]
  private val pointToCluster = new LinkedHashMap[String,String]
  /** Add a new point-cluster pair, or change the cluster assignment of an existing point. */
  def update(pointId:String, clusterId:String): Unit = {
    val oldClusterId = pointToCluster(pointId)
    if (oldClusterId ne null) clusterToPoints(oldClusterId) -= pointId
    pointToCluster(pointId) = clusterId
    clusterToPoints(clusterId) += pointId
  }
  // EvaluatableClustering methods
  def clusterIds = clusterToPoints.keys
  def pointIds = clusterToPoints.values.flatten
  def pointIds(clusterId:String) = clusterToPoints(clusterId)
  def clusterId(pointId:String) = pointToCluster(pointId)
  def intersectionSize(clusterId1:String, clusterId2:String) = clusterToPoints(clusterId1).intersect(clusterToPoints(clusterId2)).size
}

/** Helper functions for BasicEvaluatableClustering. */
object EvaluatableClustering {
  /** Return default constructor argument for BasicEvaluatableClustering given a String in the format returned by EvaluatableClustering.toString. */
  def stringToPointClusterPairs(s:String): Iterable[(String,String)] = 
    stringToPointClusterPairs(io.Source.fromString(s))
  def stringToPointClusterPairs(source:io.Source): Iterable[(String,String)] = {
    val result = new scala.collection.mutable.ListBuffer[(String,String)]
    var clusterId: String = null
    for (line <- source.getLines()) {
      if (clusterId eq null) clusterId = line
      else if (line.length == 0) clusterId = null
      else result += Tuple2(clusterId, line)
    }
    result
  }
  def apply(file:java.io.File) = new BasicEvaluatableClustering(io.Source.fromFile(file))
}

/** A trait containing a method of evaluating the precision, recall and F1 of clustering solutions.  */
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
