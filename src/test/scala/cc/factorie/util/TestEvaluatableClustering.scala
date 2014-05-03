package cc.factorie.util

import org.junit.Test
import org.junit.Assert._
import cc.factorie.util._

class TestEvaluatableClustering {
  @Test def testBasicEvaluatableClustering(): Unit = {
    var g = new BasicEvaluatableClustering(for (i <- 1 to 10) yield Tuple2(i.toString, "a"))
    var c = new BasicEvaluatableClustering(for (i <- 1 to 10) yield Tuple2(i.toString, "b"))
    assert(ClusterF1Evaluation.Pairwise(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.BCubed(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.BCubedNoSingletons(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.MUC(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.CeafE(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.CeafM(c, g).f1 == 1.0)
    assert(g.clusterIds.size == c.clusterIds.size)
    assert(g == c)
    //println("Before Serialization\n"+c.toString)
    c = EvaluatableClustering(c.toString)
    //println("After Serialization\n"+c.toString)
    assert(g == c)
    
    g = new BasicEvaluatableClustering(for (i <- 1 to 10) yield Tuple2(i.toString, i.toString))
    c = new BasicEvaluatableClustering(for (i <- 1 to 10) yield Tuple2(i.toString, (i+1).toString))
    assert(ClusterF1Evaluation.Pairwise(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.BCubed(c, g).f1 == 1.0)
    assert(ClusterF1Evaluation.BCubedNoSingletons(c, g).f1 == 1.0)
    assert(g == c)
    //println("Before Serialization\n"+c.toString)
    c = EvaluatableClustering(c.toString)
    //println("After Serialization\n"+c.toString)
    assert(g == c)

    g = new BasicEvaluatableClustering(for (i <- 1 to 3) yield Tuple2(i.toString, (i/2).toString))
    c = new BasicEvaluatableClustering(for (i <- 1 to 3) yield Tuple2(i.toString, "a"))
    //println("F1="+PairwiseClusterEvaluation(c, g).f1)
    assert(ClusterF1Evaluation.Pairwise(c, g).f1 == 0.5)
    assert(g != c)
    //println("Before Serialization\n"+c.toString)
    val g2 = EvaluatableClustering(g.toString)
    //println("After Serialization\n"+c.toString)
    assert(g == g2)
    assert(g2 == g)
  }

}