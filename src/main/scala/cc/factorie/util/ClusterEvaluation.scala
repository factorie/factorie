package cc.factorie.util

import scala.collection.mutable._
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.la.DenseTensor2
import scala.Tuple2
import scala.Iterable

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
      val trueCluster = if(trueId != null)truth.pointIds(trueId) else Iterable.empty
      // calculate overlap
      val clusterOverlap: Double =  if(trueId != null)predicted.intersectionSize(predId, trueId).doubleValue else 0.0 // This is very slow.  We should cache these intersectionSizes
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
        val predCluster = if(predId != null) predicted.pointIds(predId) else Iterable.empty
        val trueId = truth.clusterId(mid)
        val trueCluster = if(trueId != null) truth.pointIds(trueId) else Iterable.empty
        // calculate overlap between the two
        val clusterOverlap: Int = if(trueId != null&&predId!=null)predicted.intersectionSize(predId, trueId) else 0
        // add to metric                                             123
        // prec = overlap / pred.size
        result.precisionNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
        // rec = overlap / truth.size
        result.recallNumerator += (if(trueCluster.size ==0) 0.0 else clusterOverlap.doubleValue / trueCluster.size.doubleValue)
      }
      result
    }
  }

object BCubedNoSingletonClusterEvaluation extends ClusterF1Evaluation with FastLogging {
  def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
    val result = new F1Evaluation
    //val kIndex = Indexa(keys)
    //val rIndex = Indexa(response)
    var acumP = 0.0//result.precisionNumerator
    var acumR = 0.0
    for(rChain <- predicted.clusterIds.toSeq.filterNot(e => predicted.pointIds(e).size == 1); m <- predicted.pointIds(rChain)) {
      val kChain = truth.clusterId(m)
      var ci = 0
      val ri = predicted.pointIds(rChain).size
      val ki = if (kChain != null) truth.pointIds(kChain).size else 0

      ci = if(kChain != null) predicted.intersectionSize(rChain, kChain)  else 0

      acumP += (if(ri != 0) ci / ri.toFloat else 0)
      acumR += (if(ki != 0) ci / ki.toFloat else 0)
    }

    // Mentions in key
    var keyMentions = 0
    for(kEntity <- truth.clusterIds) {
      keyMentions += truth.pointIds(kEntity).size
    }

    // Mentions in response
    var resMentions = 0
    for(rEntity <- predicted.clusterIds) {
      resMentions += predicted.pointIds(rEntity).size
    }
    result.recallNumerator = acumR //$denpre ? $numpre / $denpre : 0;
    result.recallDenominator = keyMentions
    result.precisionNumerator = acumP
    result.precisionDenominator = resMentions
    result
  }
}

object CEAFClusterEvaluation extends ClusterF1Evaluation with FastLogging {
  def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
    val ignoreSingletons = true
    val result = new F1Evaluation
    val predEntities = if (ignoreSingletons) predicted.clusterIds.filterNot(e => predicted.pointIds(e).size == 1).toSeq else predicted.clusterIds.toSeq
    val truthEntities = if (ignoreSingletons) truth.clusterIds.filterNot(e => truth.pointIds(e).size == 1).toSeq else truth.clusterIds.toSeq
    val weights = new DenseTensor2(predEntities.length, truthEntities.length)
    for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
      val ei = predicted.pointIds(predEntities(i)).toSeq
      val ej = truth.pointIds(truthEntities(j)).toSeq
      weights(i, j) = 2.0*ei.intersect(ej).size /(ei.size.toDouble + ej.size)
    }
    val matching = new AssignmentSolver(weights).solve()
    val num = matching.map(e => weights(e._1,e._2)).sum
    result.precisionNumerator = num
    result.recallNumerator = num
    result.precisionDenominator = predEntities.length
    result.recallDenominator = truthEntities.length
    result
  }
}

object MucClusterEvaluation extends ClusterF1Evaluation with FastLogging {
  def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
    val result = new F1Evaluation
    // Recall:
    // go through each true cluster
    for (trueId <- truth.clusterIds) {
      // find out how many unique predicted entities the mentions belong to
      val predEntities = truth.pointIds(trueId).map(m => predicted.clusterId(m))
      //for (mid <- truth.pointIds(trueId)) {
      //  predEntities += predicted.clusterId(mid))
      //}
      // set metrics
      result.recallNumerator += truth.pointIds(trueId).size - predEntities.size
      result.recallDenominator += truth.pointIds(trueId).size - 1
    }
    // Precision:
    // go through each predicted cluster
    for (predId <- predicted.clusterIds) {
      // find out how many unique true entities the mentions belong to
      val trueEntities= truth.pointIds(predId).map(m => truth.clusterId(m))

     // for (mid <- predicted.pointIds(predId)) {
      //  trueEntities += (truth.clusterId(mid))
      //}
      // set metrics
      result.precisionNumerator += predicted.pointIds(predId).size - trueEntities.size
      result.precisionDenominator += predicted.pointIds(predId).size - 1
    }
    result
  }
}
/*
foreach document {
           foreach cluster in returned answer {
                   foreach mention in cluster {
                           val truth = trueCluster(mention);
                           val ci = overlap(truth, cluster)
                           val ri = size(cluster);
                           val ki = size(truth);

                           Prec += ci / ri;
                           Rec += ci / ki;
                   }
           }
      }


{
  my ($keys, $response) = @_;
  my $kIndex = Indexa($keys);
  my $rIndex = Indexa($response);
  my $acumP = 0;
  my $acumR = 0;
  foreach my $rChain (@$response) {
    foreach my $m (@$rChain) {
      my $kChain = (defined($kIndex->{$m})) ? $keys->[$kIndex->{$m}] : [];
      my $ci = 0;
      my $ri = scalar(@$rChain);
      my $ki = scalar(@$kChain);

      # common mentions in rChain and kChain => Ci
      foreach my $mr (@$rChain) {
        foreach my $mk (@$kChain) {
          if ($mr == $mk) {
            $ci++;
            last;
          }
        }
      }

      $acumP += $ci / $ri if ($ri);
      $acumR += $ci / $ki if ($ki);
    }
  }

  # Mentions in key
  my $keymentions = 0;
  foreach my $kEntity (@$keys) {
    $keymentions += scalar(@$kEntity);
  }

  # Mentions in response
  my $resmentions = 0;
  foreach my $rEntity (@$response) {
    $resmentions += scalar(@$rEntity);
  }

  ShowRPF($acumR, $keymentions, $acumP, $resmentions) if ($VERBOSE);
  return($acumR, $keymentions, $acumP, $resmentions);
}
 */

// TODO Move BCubedNoSingletons, MUC and CEAF, etc. here from cc.factorie.util.coref. -akm
