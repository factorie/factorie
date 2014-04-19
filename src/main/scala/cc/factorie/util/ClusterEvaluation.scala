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
      val predCluster = if(predId != null)predicted.pointIds(predId) else Iterable.empty
      val trueId = truth.clusterId(mid)
      val trueCluster = if(trueId != null)truth.pointIds(trueId) else Iterable.empty
      // calculate overlap
      val clusterOverlap: Double =  if(trueId != null&&predId!=null)predicted.intersectionSize(predId, trueId).doubleValue else 0.0 // This is very slow.  We should cache these intersectionSizes
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
    var acumP = 0//result.precisionNumerator
    var acumR = 0
    for(rChain <- predicted.clusterIds; m <- predicted.pointIds(rChain)) {
      val kChain = truth.clusterId(m)
      var ci = 0
      val ri = predicted.pointIds(rChain).size
      val ki = if (kChain != null) truth.pointIds(kChain).size else 0

      ci += predicted.intersectionSize(rChain, kChain)

      acumP += (if(ri != 0) ci / ri else 0)
      acumR += (if(ki != 0) ci / ki else 0)
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
    result.recallNumerator += acumR //$denpre ? $numpre / $denpre : 0;
    result.recallDenominator += keyMentions
    result.precisionNumerator += acumP
    result.precisionDenominator += resMentions
    //my $recall = $denrec ? $numrec / $denrec : 0;

//      if ($recall + $precisio) {
//        $f1 = 2 * $precisio * $recall / ($precisio + $recall);
//      }
//    }

//    print "Recall: ($numrec / $denrec) " . int($recall*10000)/100 . '%';
//    print "\tPrecision: ($numpre / $denpre) " . int($precisio*10000)/100 . '%';
//    print "\tF1: " . int($f1*10000)/100 . "\%\n";

    //ShowRPF($acumR, $keymentions, $acumP, $resmentions) if ($VERBOSE);
  //(acumR, keyMentions, acumP, resMentions)
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
