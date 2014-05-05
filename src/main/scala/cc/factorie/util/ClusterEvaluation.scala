package cc.factorie.util

import scala.collection.mutable._
import scala.Iterable
import scala.Tuple2

/** A generic trait for clustering solution containers that can be evaluated.
    @author Andrew McCallum */
trait EvaluatableClustering[ClusterIdType,PointIdType] {
  /** Return collection of clusters (or entities). */
  def clusterIds: Iterable[ClusterIdType]
  /** Return collection of points (or mentions). */
  def pointIds: Iterable[PointIdType]
  /** Return those points (or mentions) that are assigned to the given cluster (entity). */
  def pointIds(clusterId:ClusterIdType): Iterable[PointIdType]
  /** Return the cluster (entity) to which the given point (mention) is assigned. */
  def clusterId(pointId:PointIdType): ClusterIdType
  // Return the total number of points in the clustering. -akm: No, I think we should just use pointIds.size.
  //def numPoints: Int = pointIds.size
  /** Return true if two EvaluableClusterings partition the same points in the same way. */
  override def equals(other:Any): Boolean = other match {
    case other:EvaluatableClustering[ClusterIdType,PointIdType] => 
      this.clusterIds.size == other.clusterIds.size && 
      clusterIds.forall(clusterId => {
        val thisPoints = pointIds(clusterId)
        val otherPoints = other.pointIds(other.clusterId(thisPoints.head))
        thisPoints.toSet == otherPoints.toSet
      })
    case _ => false
  }
  /** Return a string representation of this clustering, with clusterId and pointId strings separated by newlines.
      It uses the toString representation of clusterId and pointId, which should not contain newlines. */
  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    for (clusterId <- clusterIds) {
      sb.append(clusterId.toString); sb.append('\n')
      for (pointId <- pointIds(clusterId)) { sb.append(pointId.toString); sb.append('\n') }
      sb.append('\n')
    }
    sb.toString
  }
}

/** A concrete implementation of EvaluatableClustering based simply on String clusterIds and pointIds. 
    @author Andrew McCallum */
class BasicEvaluatableClustering extends EvaluatableClustering[String,String] {
  def this(pointClusterPairs:Iterable[(String,String)]) = { this(); pointClusterPairs.foreach(p => this.update(p._1, p._2)) }
  /** Initialize from a String in the format returned by EvaluatableClustering.toString */
  def this(serialized:String) = this(EvaluatableClustering.stringToPointClusterPairs(serialized))
  /** Initialize from a io.Source whose contents are in the format returned by EvaluatableClustering.toString */
  def this(serialized:io.Source) = this(EvaluatableClustering.stringToPointClusterPairs(serialized))
  private val clusterToPoints = new LinkedHashMap[String,LinkedHashSet[String]]
  private val pointToCluster = new LinkedHashMap[String,String]
  /** Add a new point-cluster pair, or change the cluster assignment of an existing point. */
  def update(pointId:String, clusterId:String): Unit = {
    val oldClusterId = pointToCluster.getOrElse(pointId, null)
    if (oldClusterId ne null) clusterToPoints(oldClusterId) -= pointId
    pointToCluster(pointId) = clusterId
    clusterToPoints.getOrElseUpdate(clusterId, new LinkedHashSet[String]) += pointId
  }
  /** Assign all points in clusterId2 into clusterId1. */
  def merge(clusterId1:String, clusterId2:String): Unit = for (p <- clusterToPoints(clusterId2)) update(p, clusterId1)
  /** Assign all points in same cluster as pointId2 into the cluster currently containing pointId1. */
  def mergeClosure(pointId1:String, pointId2:String): Unit = merge(pointToCluster(pointId1), pointToCluster(pointId2))
  /** Return collection of all (point,cluster) pairs. */
  def items: Iterable[(String,String)] = pointToCluster.toIterable
  /** Return a new BasicEvaluatableClustering with the same contents and clustering as this one. */
  def copy: BasicEvaluatableClustering = new BasicEvaluatableClustering(pointToCluster.toSeq)
  // EvaluatableClustering methods
  def clusterIds = clusterToPoints.keys
  def pointIds = clusterToPoints.values.flatten
  def pointIds(clusterId:String) = clusterToPoints(clusterId)
  def clusterId(pointId:String) = pointToCluster(pointId)
}

/** Helper functions for BasicEvaluatableClustering.
    @author Andrew McCallum */
object EvaluatableClustering {
  /** Return default constructor argument for BasicEvaluatableClustering given a String in the format returned by EvaluatableClustering.toString. */
  def stringToPointClusterPairs(s:String): Iterable[(String,String)] = 
    stringToPointClusterPairs(io.Source.fromString(s))
  /** Return default constructor argument for BasicEvaluatableClustering given a Source in the format returned by EvaluatableClustering.toString. */
  def stringToPointClusterPairs(source:io.Source): Iterable[(String,String)] = {
    val result = new scala.collection.mutable.ListBuffer[(String,String)]
    var clusterId: String = null
    for (line <- source.getLines()) {
      if (clusterId eq null) clusterId = line
      else if (line.length == 0) clusterId = null
      else result += Tuple2(line, clusterId)
    }
    result
  }
  /** Return a BasicEvaluatableClustering given a File in the format returned by EvaluatableClustering.toString. */
  def apply(file:java.io.File) = new BasicEvaluatableClustering(io.Source.fromFile(file))
  /** Return a BasicEvaluatableClustering given a String in the format returned by EvaluatableClustering.toString. */
  def apply(string:String) = new BasicEvaluatableClustering(string)

  /** Return a string containing pairwise, MUC and B3 evaluations of predicted vs truth clusterings. */
  def evaluationString[C,P](predicted:EvaluatableClustering[C,P], truth:EvaluatableClustering[C,P]): String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("P(mentions,entities) %d %d\n".format(predicted.pointIds.size, predicted.clusterIds.size))
    sb.append("T(mentions,entities) %d %d\n".format(truth.pointIds, truth.clusterIds.size))
    val pw = ClusterF1Evaluation.Pairwise(predicted, truth)
    //if (debug) TimeUtil.snapshot(pw.toString("PW"))
    sb.append(pw.toString("PW") + "\n")
    val muc = ClusterF1Evaluation.MUC(predicted, truth)
    //if (debug) TimeUtil.snapshot(muc.toString("MUC"))
    sb.append(muc.toString("MUC") + "\n")
    val b3 = ClusterF1Evaluation.BCubed(predicted, truth)
    //if (debug) TimeUtil.snapshot(b3.toString("B3"))
    sb.append(b3.toString("B3"))
    sb.toString
  }
}

object ClusterEvaluation {
  /** Command-line utility for printing pairwise, MUC and B3 evaluation of predicted vs truth clusterings stored in files. */
  def main(args:Array[String]): Unit = {
    if (args.length != 2) { System.err.println("Usage: predicted-file truth-file"); System.exit(-1) }
    val predicted = EvaluatableClustering(new java.io.File(args(1)))
    val truth = EvaluatableClustering(new java.io.File(args(2)))
    println(EvaluatableClustering.evaluationString(predicted, truth))
  }
    
}

/** A trait containing a method of evaluating the precision, recall and F1 of clustering solutions.
    @author Andrew McCallum */
trait ClusterF1Evaluation {
  def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation
}

//
///** Precision and recall of N^2 point-by-point  */
//object PairwiseClusterEvaluation extends ClusterF1Evaluation with FastLogging {
//  def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
//    var tp = 0.0
//    var fp = 0.0
//    var tn = 0.0
//    var fn = 0.0
//    val points = predicted.pointIds
//    val total: Double = points.size
//    var count = 0
//    val result = new F1Evaluation
//    // go through all mentions
//    for (mid <- points) {
//      // get the clusters
//      val predId = predicted.clusterId(mid)
//      val predCluster = predicted.pointIds(predId)
//      val trueId = truth.clusterId(mid)
//      val trueCluster = if(trueId != null)truth.pointIds(trueId) else Iterable.empty
//      // calculate overlap
//      val clusterOverlap: Double =  if(trueId != null)predicted.intersectionSize(predId, trueId).doubleValue else 0.0 // This is very slow.  We should cache these intersectionSizes
//      // update metrics
//      tp += clusterOverlap - 1.0
//      tn += total - predCluster.size - trueCluster.size + clusterOverlap
//      fp += predCluster.size - clusterOverlap
//      fn += trueCluster.size - clusterOverlap
//      count += 1
//      if (count % 100000 == 0) logger.info("count: " + count)
//=======
object ClusterF1Evaluation {

  /** Evaluation of a clustering by N2 point-by-point precision and recall. @author Sameer Singh, Andrew McCallum */
  object Pairwise extends ClusterF1Evaluation with FastLogging {
    def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
      var tp = 0.0
      var fp = 0.0
      var tn = 0.0
      var fn = 0.0
      val points = predicted.pointIds
      val total: Double = points.size
      var count = 0
      val result = new F1Evaluation
      // Maps (predId,trueId) cluster ids to the size of the intersections of their point members
      val intersection = new scala.collection.mutable.HashMap[(C,C),Int] {
        override def default(key:(C,C)) = predicted.pointIds(key._1).toSet.intersect(truth.pointIds(key._2).toSet).size
      }
      // go through all mentions
      for (mid <- points) {
        // get the clusters
        val predId = predicted.clusterId(mid)
        val predCluster = predicted.pointIds(predId)
        val trueId = truth.clusterId(mid)
        val trueCluster = if(trueId != null)truth.pointIds(trueId) else Iterable.empty
        // calculate overlap
        val clusterOverlap = if(trueId != null) intersection((predId, trueId)) else 0
        //val clusterOverlap: Double = predicted.pointIds(predId).toSet.intersect(truth.pointIds(trueId).toSet).size // This is very slow.  We should cache these intersectionSizes
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

  object BCubed extends ClusterF1Evaluation {
    def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
        val result = new F1Evaluation
        result.precisionDenominator = predicted.pointIds.size
        result.recallDenominator = predicted.pointIds.size
        // Maps (predId,trueId) cluster ids to the size of the intersections of their point members
        val intersection = new scala.collection.mutable.HashMap[(C,C),Int] {
          override def default(key:(C,C)) = predicted.pointIds(key._1).toSet.intersect(truth.pointIds(key._2).toSet).size
        }
        // go through each mention
        for (mid <- predicted.pointIds) {
          // get pred and true clusters
          val predId = predicted.clusterId(mid)
          val predCluster = predicted.pointIds(predId)
          val trueId = truth.clusterId(mid)
          val trueCluster = if(trueId != null)truth.pointIds(trueId) else Iterable.empty
          // calculate overlap between the two
          val clusterOverlap: Int = if(trueId != null) intersection((predId, trueId)) else 0
          // add to metric
          // prec = overlap / pred.size
          result.precisionNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
          // rec = overlap / truth.size
          result.recallNumerator += clusterOverlap.doubleValue / trueCluster.size.doubleValue
        }
        result
      }
    }
  
  /** B3 excluding singletons.
    This tries to match the logic in the conll evaluation script. It takes a list of mentions and clusters
    with no singletons and does the following (copied from perl and cleaned up):
        foreach document {
             foreach cluster in returned answer {
                     foreach mention in cluster {
                             val truth = trueCluster(mention);
                             val ci = overlap(truth, cluster)
                             val ri = size(cluster);
                             val ki =
  
                             Prec += ci / ri;
                             Rec += ci / ki;
                     }
             }
        }
  
        the denominator for precision is the number of mentions returned while for
        recall the number of true mentions
     }
     The source script is in http://conll.bbn.com/download/scorer.v4.tar.gz
    Version 4 Scorer
    */
  object OldBCubedNoSingletons extends ClusterF1Evaluation with FastLogging {
    def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
      val result = new F1Evaluation
      val predNonSingletons = predicted.pointIds.filter(m => predicted.pointIds(predicted.clusterId(m)).size > 1).toSet
      val goldNonSingletons = truth.pointIds.filter(m => truth.pointIds(truth.clusterId(m)).size > 1).toSet
      val denom = predNonSingletons.union(goldNonSingletons).size
      result.precisionDenominator = denom
      result.recallDenominator = denom
      // Maps (predId,trueId) cluster ids to the size of the intersections of their point members
      val intersection = new scala.collection.mutable.HashMap[(C,C),Int] {
        override def default(key:(C,C)) = predicted.pointIds(key._1).toSet.intersect(truth.pointIds(key._2).toSet).size
      }
      // go through each mention
      for (mid <- predicted.pointIds) {
        // get pred and true clusters
        val predId = predicted.clusterId(mid)
        val predCluster = predicted.pointIds(predId)
        val trueId = truth.clusterId(mid)
        val trueCluster = if(trueId != null) truth.pointIds(trueId) else Iterable.empty
        if (predCluster.size > 1 || trueCluster.size > 1) {
          // calculate overlap between the two
          val clusterOverlap: Int = intersection(predId, trueId)
          // add to metric
          // prec = overlap / pred.size
          result.precisionNumerator += clusterOverlap.doubleValue / predCluster.size
          // rec = overlap / truth.size
          result.recallNumerator += clusterOverlap.doubleValue / trueCluster.size
        }
      }
      result
    }
  }

  object MUC extends ClusterF1Evaluation with FastLogging {
    def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
      val result = new F1Evaluation
      // Recall:
      // go through each true cluster
      for (trueId <- truth.clusterIds) {
        val predEntities: Set[E] = new HashSet
        var singleClusters = 0
        for (mid <- truth.pointIds(trueId)) {
          if (predicted.clusterId(mid) != null) predEntities.add(predicted.clusterId(mid))
          else singleClusters += 1
        }
        result.recallNumerator += truth.pointIds(trueId).size - (predEntities.size + singleClusters)
        result.recallDenominator += truth.pointIds(trueId).size - 1
      }
      // Precision:
      // go through each predicted cluster
      for (predId <- predicted.clusterIds) {
        // find out how many unique true entities the mentions belong to
        val truthEntities: Set[E] = new HashSet
        var singleClusters = 0
        for (mid <- predicted.pointIds(predId)) {
          if (truth.clusterId(mid) != null) truthEntities.add(truth.clusterId(mid))
          else singleClusters += 1
        }
        result.precisionNumerator += predicted.pointIds(predId).size - (truthEntities.size + singleClusters)
        result.precisionDenominator += predicted.pointIds(predId).size - 1
      }

      result
    }
  }
  
  class CeafE(val ignoreSingletons: Boolean = true) extends ClusterF1Evaluation {
    def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
      val result = new F1Evaluation
      val predEntities = if (ignoreSingletons) predicted.clusterIds.toSeq.filter(predicted.pointIds(_).size > 1) else predicted.clusterIds.toSeq
      val truthEntities = if (ignoreSingletons) truth.clusterIds.toSeq.filter(truth.pointIds(_).size > 1) else truth.clusterIds.toSeq
      val weights = new cc.factorie.la.DenseTensor2(predEntities.length, truthEntities.length)
      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
        val ei = predicted.pointIds(predEntities(i)).toSet
        val ej = truth.pointIds(truthEntities(j)).toSet
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
  object CeafE extends CeafE(false)
  object CeafENoSingletons extends CeafE(true)
  
  class CeafM(val ignoreSingletons: Boolean = true) extends ClusterF1Evaluation {
    def apply[C,P](predicted: EvaluatableClustering[C,P], truth: EvaluatableClustering[C,P]): F1Evaluation = {
      val result = new F1Evaluation
      val predEntities = if (ignoreSingletons) predicted.clusterIds.toSeq.filter(predicted.pointIds(_).size > 1) else predicted.clusterIds.toSeq
      val truthEntities = if (ignoreSingletons) truth.clusterIds.toSeq.filter(truth.pointIds(_).size > 1) else truth.clusterIds.toSeq
      val weights = new cc.factorie.la.DenseTensor2(predEntities.length, truthEntities.length)
      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
        val ei = predicted.pointIds(predEntities(i)).toSet
        val ej = truth.pointIds(truthEntities(j)).toSet
        weights(i, j) = ei.intersect(ej).size

      }
      val matching = new AssignmentSolver(weights).solve()
      val num = matching.map(e => weights(e._1,e._2)).sum
      result.precisionNumerator = num
      result.recallNumerator = num
      result.precisionDenominator = predEntities.map(predicted.pointIds(_).size).sum
      result.recallDenominator = truthEntities.map(truth.pointIds(_).size).sum
      result
    }
  }
  object CeafM extends CeafM(false)
  object CeafMNoSingletons extends CeafM(true)

  /**
   *
   * Version 7 Scorer Bcubed calculation.  Matches logic from the official conll scorer
   */
  object BCubedNoSingletons extends ClusterF1Evaluation with FastLogging {
    def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
      val result = new F1Evaluation
      var acumP = 0.0//result.precisionNumerator
      var acumR = 0.0
      // Maps (predId,trueId) cluster ids to the size of the intersections of their point members
      val intersection = new scala.collection.mutable.HashMap[(E,E),Int] {
        override def default(key:(E,E)) = predicted.pointIds(key._1).toSet.intersect(truth.pointIds(key._2).toSet).size
      }
      for(rChain <- predicted.clusterIds; m <- predicted.pointIds(rChain)) {
        val kChain = truth.clusterId(m)
        var ci = 0
        val ri = predicted.pointIds(rChain).size
        val ki = if (kChain != null) truth.pointIds(kChain).size else 0

        ci = if(kChain != null) intersection(rChain, kChain)  else 0

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
}
//  object CEAFEClusterEvaluation extends ClusterF1Evaluation with FastLogging {
//    def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
//      val ignoreSingletons = true
//      val result = new F1Evaluation
//      val predEntities = if (ignoreSingletons) predicted.clusterIds.filterNot(e => predicted.pointIds(e).size == 1).toSeq else predicted.clusterIds.toSeq
//      val truthEntities = if (ignoreSingletons) truth.clusterIds.filterNot(e => truth.pointIds(e).size == 1).toSeq else truth.clusterIds.toSeq
//      val weights = new DenseTensor2(predEntities.length, truthEntities.length)
//      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
//        val ei = predicted.pointIds(predEntities(i)).toSeq
//        val ej = truth.pointIds(truthEntities(j)).toSeq
//        weights(i, j) = 2.0*ei.intersect(ej).size /(ei.size.toDouble + ej.size)
//      }
//      val matching = new AssignmentSolver(weights).solve()
//      val num = matching.map(e => weights(e._1,e._2)).sum
//      result.precisionNumerator = num
//      result.recallNumerator = num
//      result.precisionDenominator = predEntities.length
//      result.recallDenominator = truthEntities.length
//      result
//    }
//  }
//
//  object CEAFMClusterEvaluation extends ClusterF1Evaluation with FastLogging {
//    def apply[E,M](predicted: EvaluatableClustering[E,M], truth: EvaluatableClustering[E,M]): F1Evaluation = {
//      val ignoreSingletons = true
//      val result = new F1Evaluation
//      val predEntities = if (ignoreSingletons) predicted.clusterIds.filterNot(e => predicted.pointIds(e).size == 1).toSeq else predicted.clusterIds.toSeq
//      val truthEntities = if (ignoreSingletons) truth.clusterIds.filterNot(e => truth.pointIds(e).size == 1).toSeq else truth.clusterIds.toSeq
//      val weights = new DenseTensor2(predEntities.length, truthEntities.length)
//      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
//        val ei = predicted.pointIds(predEntities(i)).toSeq
//        val ej = truth.pointIds(truthEntities(j)).toSeq
//        weights(i, j) = ei.intersect(ej).size
//      }
//      val matching = new AssignmentSolver(weights).solve()
//      val num = matching.map(e => weights(e._1,e._2)).sum
//      result.precisionNumerator = num
//      result.recallNumerator = num
//      result.precisionDenominator = predEntities.map(e => predicted.pointIds(e).size).sum
//      result.recallDenominator = truthEntities.map(e => truth.pointIds(e).size).sum
//      result
//    }
//  }





// TODO Is there a way to do this without F1Evaluation.overrideF1? -akm

//  // Following the specification in http://stel.ub.edu/semeval2010-coref/sites/default/files/blanc-draft3.pdf
//  object Blanc extends MetricEvaluator {
//    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
//      val m = new Metric
//      var rc = 0.0 // coreferent and reported as such
//      var wc = 0.0 // non-coreferent and reported as coreferent
//      var wn = 0.0 // coreferent and reported as non-coreferent
//      var rn = 0.0 // non-coreferent and reported as such
//      val totalMentions = pred.getMentionIds.union(truth.getMentionIds).toSeq // TODO This is unnecessary. -sameer
//      for (i <- 0 until totalMentions.length; j <- 0 until i; mi = totalMentions(i); mj = totalMentions(j)) {
//        val predicted = pred.contains(mi) && pred.contains(mj) && pred.getEntity(mi) == pred.getEntity(mj)
//        val truthed = truth.contains(mi) && truth.contains(mj) && truth.getEntity(mi) == truth.getEntity(mj)
//        if (predicted && truthed) rc += 1
//        else if (predicted && !truthed) wc += 1
//        else if (!predicted && truthed) wn += 1
//        else rn += 1
//      }
//      val Pc = if (rc+wc > 0) rc/(rc+wc) else 0
//      val Rc = if (rc+wn > 0) rc/(rc+wn) else 0
//      val Pn = if (rn+wn > 0) rn/(rn+wn) else 0
//      val Rn = if (rn+wc > 0) rn/(rn+wc) else 0
//      m.precNumerator = 0.5*(Pc+Pn)
//      m.recallNumerator = 0.5*(Rc+Rn)
//      m.precDenominator = 1
//      m.recallDenominator = 1
//      val Fc = if (Pc+Rc > 0) 2.0*Pc*Rc/(Pc+Rc) else 0.0
//      val Fn = if (Pn+Rn > 0) 2.0*Pn*Rn/(Pn+Rn) else 0.0
//      m.f1Overridden = true
//      m.overrideF1 = 0.5*(Fc + Fn)
//      m.overrideF1Den += 1.0
//      m
//    }
//  }
