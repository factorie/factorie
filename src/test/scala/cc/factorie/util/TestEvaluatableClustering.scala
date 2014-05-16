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