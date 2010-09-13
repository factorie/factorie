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
trait DecisionTreeStatistics2[S1<:DiscreteVar,S2<:DiscreteVars]
extends VectorStatistics2[S1,S2] {
  class DTNode(parent:DTNode, index:Int)
  def scoreScaling = 1.0
  def score(s:StatType): Double = 0.0 // TODO Return the log-probability of s1._1 * scoreScaling
  def train(stats:Iterable[StatType], maxDepth:Int = Int.MaxValue): Unit = {
    throw new Error("Not yet implemented")
  }
  override def save(dirname:String): Unit = {
    super.save(dirname)
    throw new Error("Not yet implemented")
  }
  override def load(dirname:String): Unit = {
    super.load(dirname)
    throw new Error("Not yet implemented")
  }
}

/** A template for factors who scores are the log-probability of 
    label S1 given feature vector S2, according to a decision tree.
    @author Andrew McCallum */
abstract class DecisionTreeTemplateWithStatistics2[S1<:DiscreteVar,S2<:DiscreteVars](label2features:(S1)=>S2)(implicit m1:Manifest[S1], m2:Manifest[S2])
extends Template2[S1,S2] with DecisionTreeStatistics2[S1,S2] {
  def unroll1(s1:S1) = new Factor(s1, label2features(s1))
  def unroll2(s2:S2) = throw new Error("Decision tree feature vectors of class "+s2.getClass+" are not expected to change.")
  def statistics(s1:S1, s2:S2): Iterable[Stat] = Stat(s1, s2)
  def train(labels: Iterable[S1]): Unit = train(labels.map(unroll1(_)).flatten.flatMap(_statistics(_)))
}
