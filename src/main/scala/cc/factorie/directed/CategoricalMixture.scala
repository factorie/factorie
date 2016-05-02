/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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

package cc.factorie.directed

import cc.factorie.infer.{DiscreteSummary1, Maximize, SimpleDiscreteMarginal1, Summary}
import cc.factorie.model.Model
import cc.factorie.util.SortedSparseCounts
import cc.factorie.variable._

class CategoricalMixture[A] extends DirectedFamily3[CategoricalVariable[A],Mixture[ProportionsVariable],DiscreteVariable] {
  case class Factor(override val _1:CategoricalVariable[A], override val _2:Mixture[ProportionsVariable], override val _3:DiscreteVariable) extends super.Factor(_1, _2, _3) with DiscreteGeneratingFactor with MixtureFactor {
    def gate = _3
    def pr(child:CategoricalValue[A], mixture:scala.collection.Seq[Proportions], z:DiscreteValue): Double = mixture(z.intValue).apply(child.intValue)
    def sampledValue(mixture:_2.Value, z:_3.Value)(implicit random: scala.util.Random): _1.Value = _1.domain.apply(mixture(z.intValue).sampleIndex).asInstanceOf[_1.Value]
    def prChoosing(child:CategoricalValue[A], mixture:scala.collection.Seq[Proportions], mixtureIndex:Int): Double = mixture(mixtureIndex).apply(child.intValue)
    def prChoosing(mixtureIndex:Int): Double = _2(mixtureIndex).value.apply(_1.intValue)
    def sampledValueChoosing(mixture:scala.collection.Seq[Proportions], mixtureIndex:Int)(implicit random: scala.util.Random): CategoricalValue[A] = _1.domain.apply(mixture(mixtureIndex).sampleIndex)
    def prValue(mixture:scala.collection.Seq[Proportions], mixtureIndex:Int, intValue:Int): Double = mixture.apply(mixtureIndex).apply(intValue)
    def prValue(intValue:Int) = prValue(_2.value.asInstanceOf[scala.collection.Seq[Proportions]], _3.intValue, intValue)
    override def updateCollapsedParents(weight:Double): Boolean = { _2(_3.intValue).value.masses.+=(_1.intValue, weight); true }
    // _2(_3.intValue) match case p:DenseCountsProportions => { p.increment(_1.intValue, weight)(null); true }
  }
  def newFactor(a: CategoricalVariable[A], b: Mixture[ProportionsVariable], c:DiscreteVariable) = new Factor(a, b, c)
}
object CategoricalMixture {
  def newFactor[A](a:CategoricalVariable[A], b:Mixture[ProportionsVariable], c:DiscreteVariable)(implicit random: scala.util.Random): CategoricalMixture[A]#Factor = {
    val dm = new CategoricalMixture[A]()
    dm.Factor(a, b, c)
  }
  def apply[A](p1: Mixture[ProportionsVariable],p2:DiscreteVariable)(implicit random: scala.util.Random) = (c:CategoricalVariable[A]) => newFactor[A](c, p1, p2)
}

class DiscreteMixtureCounts[A](val discreteDomain: CategoricalDomain[A], val mixtureDomain: DiscreteDomain) extends Seq[SortedSparseCounts] {

  // counts(wordIndex).countOfIndex(topicIndex)
  private val counts = Array.fill(discreteDomain.size)(new SortedSparseCounts(mixtureDomain.size))
  val mixtureCounts = new Array[Int](mixtureDomain.size)
  //override def clone: DiscreteMixtureCounts = null
  def apply(discreteIndex:Int) = counts(discreteIndex)
  def length = discreteDomain.size
  def iterator = counts.iterator
  def countsTotal: Int = {
    val result = counts.map(_.countsTotal).sum
    //var sum = counts.map(_.calculatedCountsTotal).sum
    //assert(result == sum, "result="+result+" sum="+sum+"\nresults="+counts.map(_.countsTotal).toSeq.take(20)+"\nsum="+counts.map(_.calculatedCountsTotal).toSeq.take(20))
    result
  }
  @inline final def increment(discrete:Int, mixture:Int, incr:Int) = {
    mixtureCounts(mixture) += incr
    assert(mixtureCounts(mixture) >= 0)
    counts(discrete).incrementCountAtIndex(mixture, incr)
  }
  def incrementFactor(f:CategoricalMixture[A]#Factor, incr:Int): Unit = increment(f._1.intValue, f._3.intValue, incr)
  def incrementFactor(f:PlatedCategoricalMixture.Factor, incr:Int): Unit = {
    val discretes = f._1
    val gates = f._3
    assert (discretes.length == gates.length)
    var i = discretes.length - 1
    while (i >= 0) {
      increment(discretes(i).intValue, gates(i).intValue, incr)
      i -= 1
    }
  }

  def incrementCountsAtPositions(discrete:Int, mixture1:Int, incr1:Int, pos1:Int,
                                               mixture2:Int, incr2:Int, pos2:Int) {
    mixtureCounts(mixture1) += incr1
    mixtureCounts(mixture2) += incr2

    assert(mixtureCounts(mixture1) >= 0)
    assert(mixtureCounts(mixture2) >= 0)

    counts(discrete).incrementCountsAtPositions(pos1, incr1, pos2, incr2)
  }
}


// TODO Currently only handles Gates of a CategoricalMixture; we should make it handle GaussianMixture also.
object MaximizeGate extends Maximize[Iterable[DiscreteVariable],Model] {
  // Underlying workhorse
  def maxIndex[A](gate:DiscreteVariable, df:Discrete.Factor, dmf:CategoricalMixture[A]#Factor): Int = {
    var max = Double.NegativeInfinity
    var maxi = 0
    val statistics: CategoricalMixture[A]#Factor#StatisticsType = dmf.currentStatistics //(dmf._1.value, dmf._2.value, dmf._3.value)
    var i = 0; val size = gate.domain.size
    while (i < size) {
      val pr = df._2.value(i) * dmf.prChoosing(i)
      if (pr > max) { max = pr; maxi = i }
      i += 1
    }
    maxi
  }
  /** Returns -1 on failure. */
  def maxIndex[A](gate:DiscreteVariable, model:Model): Int = {
    val factors = model.factors(gate).toSeq
    if (factors.size != 2) return -1
    (factors(0), factors(1)) match {
      case (df:Discrete.Factor, dmf:CategoricalMixture[A @unchecked]#Factor) => maxIndex(gate, df, dmf)
      case (dmf:CategoricalMixture[A @unchecked]#Factor, df:Discrete.Factor) => maxIndex(gate, df, dmf)
      case _ => -1
    }
  }
  // For typical direct callers
  def apply[A](gate:DiscreteVariable, df:Discrete.Factor, dmf:CategoricalMixture[A]#Factor): Unit = gate.set(maxIndex(gate, df, dmf))(null)
  def apply(gate:DiscreteVariable, model:Model): Unit = {
    val maxi = maxIndex(gate, model)
    if (maxi >= 0) gate.set(maxi)(null) else throw new Error("MaximizeGate unable to handle model factors.")
  }
  // For generic inference engines
  def infer[V<:DiscreteVariable](varying:V, model:Model): SimpleDiscreteMarginal1[V] = {
    new SimpleDiscreteMarginal1(varying, new SingletonProportions1(varying.domain.size, maxIndex(varying, model)))
  }
  def infer(variables:Iterable[DiscreteVariable], model:Model, marginalizing:Summary): DiscreteSummary1[DiscreteVariable] = {
    if (marginalizing ne null) throw new Error("Multivariate case yet implemented.")
    val result = new DiscreteSummary1[DiscreteVariable]
    for (v <- variables) result += infer(v, model)
    result
  }
  
}

