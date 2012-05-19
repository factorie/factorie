/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

/** Statistics for factors who scores are the log-probability of 
    label S1 given feature vector S2, according to a decision tree.
    @author Arti Ramesh */
trait DecisionTreeStatistics2[S1<:DiscreteValue,S2<:DiscreteTensorValue] extends TensorStatistics2[S1,S2] {
  // Number of different values taken on by s._1
  val numOutcomes: Int = statisticsDomains(0).asInstanceOf[DiscreteDomain].size
  case class DTNode(parent:DTNode, var yesChild:DTNode = null, var noChild:DTNode = null, var index:Int = -1, var p:Proportions = null) {
    def isLeaf = ((yesChild eq null) || (noChild eq null))
  }
  var root: DTNode = null
  def scoreScaling = 1.0
  // val s: StatisticsType;  s._1:DiscreteValue; s._2:Vector
  // Number of different values of s._1 == s._1.domain.size
  def score(s:StatisticsType): Double = score(s, root)
  protected def score(s:StatisticsType, node:DTNode): Double = 
    if (node.isLeaf) math.log(node.p(s._1.intValue))
    else score(s, if (s._2.apply(node.index) != 0.0) node.yesChild else node.noChild)
  def train(stats:Iterable[StatisticsType], maxDepth:Int = Int.MaxValue): Unit = {
    root = train(stats, maxDepth, null)
  }
  protected def train(stats:Iterable[StatisticsType], maxDepth:Int, parent:DTNode): DTNode = {
    val dtree = new DTNode(parent)
    if (stats.size < 5) {  // TODO Have a more sophisticated stopping criterion
      // We will make this dtree a leaf
      val p = new DenseProportions1(numOutcomes)
      stats.foreach(stat => p.+=(stat._1.intValue, 1.0))
      dtree.p = p
      return dtree
    }
    // This dtree will not be a leaf
    dtree.index = bestInfoGain(stats)
    val (yesStats, noStats) = stats.partition(_._2.apply(dtree.index) != 0.0)
    dtree.yesChild = train(yesStats, maxDepth-1, dtree)
    dtree.noChild = train(noStats, maxDepth-1, dtree)
    dtree
  }
  protected def  bestInfoGain(stats:Iterable[StatisticsType]): Int = throw new Error("Implement me.") // Be clever to make this efficient
  override def save(dirname:String, gzip: Boolean = false): Unit = {
    super.save(dirname, gzip)
    throw new Error("Not yet implemented")
  }
  override def load(dirname:String, gzip: Boolean = false): Unit = {
    super.load(dirname, gzip)
    throw new Error("Not yet implemented")
  }
}

/** A template for factors who scores are the log-probability of 
    label S1 given feature vector S2, according to a decision tree.
    @author Andrew McCallum */
abstract class DecisionTreeTemplateWithStatistics2[S1<:DiscreteVar,S2<:DiscreteTensorVar](implicit m1:Manifest[S1], m2:Manifest[S2])
extends Template2[S1,S2] with DecisionTreeStatistics2[S1#ValueType,S2#ValueType] {
  //def statistics(s1:S1, s2:S2) = Stat(s1, s2)
  def statistics(values:Values) = Stat(values._1, values._2)
  def train(labels: Iterable[S1]): Unit = throw new Error // train(labels.map(unroll1(_)).flatten.map(factor => factor.statistics /*.asInstanceOf[StatisticsType]*/))
}
