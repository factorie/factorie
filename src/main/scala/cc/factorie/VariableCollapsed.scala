/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.SeqAsVector
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

/* Several examples of varibles collapsed by marginalization. */

/** DiscreteValue integrated out by a Multinomial distribution.
    @author Andrew McCallum */
class MultinomialDiscrete2[A<:DiscreteVariable](val variable:A) {
  val distribution = new DenseCountsMultinomial[A](variable.domainSize)
  // TODO What about "parent"?
  def pr(i:Int): Double = distribution.pr(i)
  def pr: Double = distribution.pr(variable.intValue)
  // Manage sufficient statistics
  def increment(i:Int, incr:Double = 1.0)(implicit d:DiffList): Unit = distribution.increment(i, incr)
  def decrement(i:Int, incr:Double = 1.0)(implicit d:DiffList): Unit = distribution.increment(i, incr)
  /*def update(model:Model): Unit = {
    val distribution = new Array[Double](variable.domainSize)
    for (i <- 0 until variable.domainSize) {
      val d = new DiffList
      variable.setByIndex(i)(d)
      distribution(i) = d.scoreAndUndo(model)
    }
    Maths.normalize(distribution)
    distribution.set(distribution)
  }*/
  def mode: Int = distribution.maxPrIndex
  def setToMode(implicit d:DiffList): Unit = variable.setByIndex(this.mode)
}

class GaussianReal2[A<:RealVariable](val variable:A) {
  val parent: Gaussian1[A] = 
    variable match { case v:GeneratedRealVariable[A] => { v.generativeSource.value match { case p:Gaussian1[A] => p; case _ => null }}; case _ => null }
  val distribution = new Gaussian1[A]
  def pr(x:Double) = distribution.pr(x)
  private var evidenceSum = 0.0
  private var evidenceNormalizer = 0.0
  // Manage sufficient statistics
  def increment(e:Double): Unit = { evidenceSum += e; evidenceNormalizer += 1.0 }
  def decrement(e:Double): Unit = { evidenceSum -= e; evidenceNormalizer -= 1.0 }
  def update(model:Model): Unit = distribution.mean = evidenceSum / evidenceNormalizer
}


/*class DirichletMultinomial2[A<:DiscreteValue](val variable:Multinomial[A]) extends DiscreteDistribution[A] {
  type VariableType <: DirichletMultinomial[A]
  val parent: Dirichlet[A] = variable.generativeSource.value match { case p:Dirichlet[A] => p; case _ => null }
  val counts = new Array[Double](variable.length)
  var countsTotal = 0.0
  def size = variable.length
  def proportion: Seq[Double] = Array.tabulate(size)(pr(_))
  def maxPrIndex = Maths.maxIndex(proportion.toArray)
  def increment(index:Int, incr:Double): Unit = { counts(index) += incr; countsTotal += incr }
  def decrement(index:Int, incr:Double): Unit = { counts(index) -= incr; countsTotal -= incr }
  def pr(index:Int) : Double = {
    if (parent != null)
      (counts(index) + parent.alpha(index)) / (countsTotal + parent.alphaSum)
    else if (countsTotal == 0)
      1.0 / size
    else
      counts(index) / countsTotal
  }
  // Probability of a collection of counts; see http://en.wikipedia.org/wiki/Multivariate_Polya_distribution.
  // TODO Not tested!
  def pr(ocounts:Vector) : Double = {
    import Maths.{gamma => g}
    assert (ocounts.size == size)
    val n = norm(ocounts,1)
    val normalizer1 = g(n) / ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2))
    val normalizer2 = g(parent.alphaSum) / g(n + parent.alphaSum)
    val ratio = ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2 + parent.alpha(e._1)/g(parent.alpha(e._1))))
    normalizer1 * normalizer2 * ratio
  }
  def logpr(obsCounts:Vector): Double = Math.log(pr(obsCounts)) //indices.foldLeft(0.0)(_+logpr(_))
  override def preChange(o:A)(implicit d:DiffList) = increment(o.index, -1.0)
  override def postChange(o:A)(implicit d:DiffList) = increment(o.index, 1.0)
  override def estimate: Unit = {} // Nothing to do because estimated on the fly
}*/

/*object DirichletMultinomialDiscreteTemplate extends Template2[DirichletMultinomial2[_],DiscreteVariable] {
  def unroll1(dm:DirichletMultinomial2[_]) = 
}*/

