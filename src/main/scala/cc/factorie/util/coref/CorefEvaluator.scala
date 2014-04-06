package cc.factorie.util.coref

import collection.Set
import collection.mutable.{Set => MSet, HashSet}
import cc.factorie.la.DenseTensor2
import cc.factorie.util.AssignmentSolver

/**
 * @author sameer
 * @see EntityMap
 */
object CorefEvaluator {

  class Metric {
    var precNumerator = 0.0
    var precDenominator = 0.0
    var recallNumerator = 0.0
    var recallDenominator = 0.0
    var f1Overridden = false
    var overrideF1 = 0.0
    var overrideF1Den = 0.0

    def precision: Double = {
      if (precDenominator == 0.0) {
        1.0
      } else {
        precNumerator / precDenominator
      }
    }

    def recall: Double = {
      if (recallDenominator == 0.0) {
        1.0
      } else {
        recallNumerator / recallDenominator
      }
    }

    def f1: Double = {
      if (f1Overridden) overrideF1/overrideF1Den
      else {
        val r: Double = recall
        val p: Double = precision
        if(p + r == 0.0) 0.0
        else (2 * p * r) / (p + r)
      }
    }

    def toString(prefix: String): String = {
      "%s %6.3f %6.3f %6.3f".format(prefix, precision * 100.0, recall * 100.0, f1 * 100.0)
    }

    def microAppend(m: Metric) {
      precNumerator += m.precNumerator
      precDenominator += m.precDenominator
      recallNumerator += m.recallNumerator
      recallDenominator += m.recallDenominator
      if (m.f1Overridden) {
        f1Overridden = true
        overrideF1 += m.overrideF1
        overrideF1Den += m.overrideF1Den
      }
    }

    def macroAppend(m: Metric) {
      precNumerator += m.precision
      precDenominator += 1.0
      recallNumerator += m.recall
      recallDenominator += 1.0
      if (m.f1Overridden) {
        f1Overridden = true
        overrideF1 += m.overrideF1
        overrideF1Den += m.overrideF1Den
      }
    }
  }

  def overlap[M](ent1: Set[M], ent2: Set[M]): Int = {
    var common = 0
    if (ent1.size > ent2.size) return overlap(ent2, ent1)
    for (mid <- ent1) {
      if (ent2.contains(mid)) {
        common += 1
      }
    }
    common
  }

  abstract class MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric
  }

  object Pairwise extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      var tp = 0.0
      var fp = 0.0
      var tn = 0.0
      var fn = 0.0
      val total: Double = pred.getMentionIds.size
      var count = 0
      // go through all mentions
      for (mid <- pred.getMentionIds) {
        // get the clusters
        val predId = pred.getEntity(mid)
        val predCluster: Set[M] = pred.getMentions(predId)
        val trueId = truth.getEntity(mid)
        val trueCluster: Set[M] = truth.getMentions(trueId)
        // calculate overlap
        val clusterOverlap: Double = overlap(predCluster, trueCluster).doubleValue
        // update metrics
        tp += clusterOverlap - 1.0
        tn += total - predCluster.size - trueCluster.size + clusterOverlap
        fp += predCluster.size - clusterOverlap
        fn += trueCluster.size - clusterOverlap
        count += 1
        if (count % 100000 == 0) println("count: " + count)
      }
      val m: Metric = new Metric
      m.precNumerator = tp
      m.precDenominator = tp + fp
      m.recallNumerator = tp
      m.recallDenominator = tp + fn
      m
    }
  }

  object BCubed extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m = new Metric
      m.precDenominator = pred.getMentionIds.size
      m.recallDenominator = pred.getMentionIds.size
      // go through each mention
      for (mid <- pred.getMentionIds) {
        // get pred and true clusters
        val predId = pred.getEntity(mid)
        val predCluster: Set[M] = pred.getMentions(predId)
        val trueId = truth.getEntity(mid)
        val trueCluster: Set[M] = truth.getMentions(trueId)
        // calculate overlap between the two
        val clusterOverlap: Int = overlap(predCluster, trueCluster)
        // add to metric
        // prec = overlap / pred.size
        m.precNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
        // rec = overlap / truth.size
        m.recallNumerator += clusterOverlap.doubleValue / trueCluster.size.doubleValue
      }
      m
    }
  }

    /*
  This tries to match the logic in the conll evaluation script. It takes a list of mentions and clusters
  with no singletons and does the following (copied from perl and cleaned up):
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

      the denominator for precision is the number of mentions returned while for
      recall the number of true mentions
   }
   The source script is in http://conll.bbn.com/download/scorer.v4.tar.gz
  */
  object BCubedNoSingletons extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m = new Metric
      val predNonSingletons = pred.getMentionIds.filter(m => pred.getMentions(pred.getEntity(m)).size > 1)
      val goldNonSingletons = truth.getMentionIds.filter(m => truth.getMentions(truth.getEntity(m)).size > 1)
      val denom = predNonSingletons.union(goldNonSingletons).size
      m.precDenominator = denom
      m.recallDenominator = denom
      // go through each mention
      for (mid <- pred.getMentionIds) {
        // get pred and true clusters
        val predId = pred.getEntity(mid)
        val predCluster: collection.Set[M] = pred.getMentions(predId)
        val trueId = truth.getEntity(mid)
        val trueCluster: collection.Set[M] = truth.getMentions(trueId)
        if (predCluster.size > 1 || trueCluster.size > 1) {
          // calculate overlap between the two
          val clusterOverlap: Int = overlap(predCluster, trueCluster)
          // add to metric
          // prec = overlap / pred.size
          m.precNumerator += clusterOverlap.doubleValue / predCluster.size.doubleValue
          // rec = overlap / truth.size
          m.recallNumerator += clusterOverlap.doubleValue / trueCluster.size.doubleValue
        }
      }
      m
    }
  }


  object MUC extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m: Metric = new Metric
      // Recall:
      // go through each true cluster
      for (trueId: Long <- truth.getEntityIds) {
        // find out how many unique predicted entities the mentions belong to
        val predEntities: MSet[Long] = new HashSet
        for (mid <- truth.getMentions(trueId)) {
          predEntities.add(pred.getEntity(mid))
        }
        // set metrics
        m.recallNumerator +=
          truth.getMentions(trueId).size - predEntities.size
        m.recallDenominator += truth.getMentions(trueId).size - 1
      }
      // Precision:
      // go through each predicted cluster
      for (predId: Long <- pred.getEntityIds) {
        // find out how many unique true entities the mentions belong to
        val trueEntities: MSet[Long] = new HashSet
        for (mid <- pred.getMentions(predId)) {
          trueEntities.add(truth.getEntity(mid))
        }
        // set metrics
        m.precNumerator +=
          pred.getMentions(predId).size - trueEntities.size
        m.precDenominator += pred.getMentions(predId).size - 1
      }
      m
    }
  }

  class CeafE(val ignoreSingletons: Boolean = true) extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m: Metric = new Metric
      val predEntities = if (ignoreSingletons) pred.entities.keys.toSeq.filter(pred.entities(_).size > 1) else pred.entities.keys.toSeq
      val truthEntities = if (ignoreSingletons) truth.entities.keys.toSeq.filter(truth.entities(_).size > 1) else truth.entities.keys.toSeq
      val weights = new DenseTensor2(predEntities.length, truthEntities.length)
      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
        val ei = pred.entities(predEntities(i))
        val ej = truth.entities(truthEntities(j))
        weights(i, j) = 2.0*ei.intersect(ej).size /(ei.size.toDouble + ej.size)
      }
      val matching = new AssignmentSolver(weights).solve()
      val num = matching.map(e => weights(e._1,e._2)).sum
      m.precNumerator = num
      m.recallNumerator = num
      m.precDenominator = predEntities.length
      m.recallDenominator = truthEntities.length
      m
    }
  }

  class CeafM(val ignoreSingletons: Boolean = true) extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m: Metric = new Metric
      val predEntities = if (ignoreSingletons) pred.entities.keys.toSeq.filter(pred.entities(_).size > 1) else pred.entities.keys.toSeq
      val truthEntities = if (ignoreSingletons) truth.entities.keys.toSeq.filter(truth.entities(_).size > 1) else truth.entities.keys.toSeq
      val weights = new DenseTensor2(predEntities.length, truthEntities.length)
      for (i <- 0 until predEntities.length; j <- 0 until truthEntities.length) {
        val ei = pred.entities(predEntities(i))
        val ej = truth.entities(truthEntities(j))
        weights(i, j) = ei.intersect(ej).size
      }
      val matching = new AssignmentSolver(weights).solve()
      val num = matching.map(e => weights(e._1,e._2)).sum
      m.precNumerator = num
      m.recallNumerator = num
      m.precDenominator = predEntities.map(pred.entities(_).size).sum
      m.recallDenominator = truthEntities.map(truth.entities(_).size).sum
      m
    }
  }

  // Following the specification in http://stel.ub.edu/semeval2010-coref/sites/default/files/blanc-draft3.pdf
  object Blanc extends MetricEvaluator {
    def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M]): Metric = {
      val m = new Metric
      var rc = 0.0 // coreferent and reported as such
      var wc = 0.0 // non-coreferent and reported as coreferent
      var wn = 0.0 // coreferent and reported as non-coreferent
      var rn = 0.0 // non-coreferent and reported as such
      val totalMentions = pred.getMentionIds.union(truth.getMentionIds).toSeq // TODO This is unnecessary. -sameer
      for (i <- 0 until totalMentions.length; j <- 0 until i; mi = totalMentions(i); mj = totalMentions(j)) {
        val predicted = pred.contains(mi) && pred.contains(mj) && pred.getEntity(mi) == pred.getEntity(mj)
        val truthed = truth.contains(mi) && truth.contains(mj) && truth.getEntity(mi) == truth.getEntity(mj)
        if (predicted && truthed) rc += 1
        else if (predicted && !truthed) wc += 1
        else if (!predicted && truthed) wn += 1
        else rn += 1
      }
      val Pc = if (rc+wc > 0) rc/(rc+wc) else 0
      val Rc = if (rc+wn > 0) rc/(rc+wn) else 0
      val Pn = if (rn+wn > 0) rn/(rn+wn) else 0
      val Rn = if (rn+wc > 0) rn/(rn+wc) else 0
      m.precNumerator = 0.5*(Pc+Pn)
      m.recallNumerator = 0.5*(Rc+Rn)
      m.precDenominator = 1
      m.recallDenominator = 1
      val Fc = if (Pc+Rc > 0) 2.0*Pc*Rc/(Pc+Rc) else 0.0
      val Fn = if (Pn+Rn > 0) 2.0*Pn*Rn/(Pn+Rn) else 0.0
      m.f1Overridden = true
      m.overrideF1 = 0.5*(Fc + Fn)
      m.overrideF1Den += 1.0
      m
    }
  }

  def evaluate[M](pred: GenericEntityMap[M], truth: GenericEntityMap[M], debug: Boolean = false): String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("P(mentions,entities) %d %d\n".format(pred.numMentions, pred.numEntities))
    sb.append("T(mentions,entities) %d %d\n".format(truth.numMentions, truth.numEntities))
    val pw: Metric = Pairwise.evaluate(pred, truth)
    //if (debug) TimeUtil.snapshot(pw.toString("PW"))
    sb.append(pw.toString("PW") + "\n")
    val muc: Metric = MUC.evaluate(pred, truth)
    //if (debug) TimeUtil.snapshot(muc.toString("MUC"))
    sb.append(muc.toString("MUC") + "\n")
    val b3: Metric = BCubed.evaluate(pred, truth)
    //if (debug) TimeUtil.snapshot(b3.toString("B3"))
    sb.append(b3.toString("B3"))
    sb.toString
  }

  def main(argv: Array[String]) = {
    //seperate options from arguments
    val (opts, args) = argv.partition {
      _.startsWith("@")
    }

    //turning options array into map
    val optsMap = Map() ++ opts.map {
      x =>
        val pair = x.split("@{1,2}")(1).split("=")
        if (pair.length == 1) (pair(0), "true")
        else (pair(0), pair(1))
    }

    //use the option values
    val trueFile: String = optsMap.getOrElse("true", "")
    val predFile: String = optsMap.getOrElse("pred", "")

    //TimeUtil.init

    val predEntities: EntityMap = EntityMap.readFromFile(predFile)
    //TimeUtil.snapshot("Pred: " + predEntities.numMentions + " / " + predEntities.numEntities)
    val trueEntities: EntityMap = EntityMap.readFromFile(trueFile)
    //TimeUtil.snapshot("True: " + trueEntities.numMentions + " / " + trueEntities.numEntities)
    println(evaluate(predEntities, trueEntities))

    //TimeUtil.snapshot("Done")
  }
}
