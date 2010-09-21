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



package cc.factorie.generative
import cc.factorie._

/** For variables that integrate themselves out, thus supporting collapsed Gibbs sampling.
    @author Andrew McCallum */
trait CollapsibleVariable extends GeneratedVariable {
  type CollapsedType <: CollapsedVariable
  def newCollapsed: CollapsedType
  def setFromCollapsed(c:CollapsedType)(implicit d:DiffList): Unit
}

// TODO Is there a need for CollapsibleVariable; perhaps just CollapsibleParameter is enough?

/** A Parameter that can be marginalized out (collapsed).
    @see DirichletMultinomial
    @author Andrew McCallum */
trait CollapsibleParameter extends Parameter with CollapsibleVariable {
  type CollapsedType <: CollapsedParameter
}


/* Several examples of marginal distributions representing collapsed variables. */

/** The distribution representing the marginal distribution of a variable. */
/*trait CollapsedDistribution[A<:Variable] {
  this: Distribution[A] =>
  def samplePreChange(o:A)(implicit d:DiffList): Unit
  def samplePostChange(o:A)(implicit d:DiffList): Unit
}
trait CollapsedGeneratedVariable {
  this: GeneratedVariable[_] =>
  def sourcePreChange(s:SourceType)(implicit d:DiffList): Unit
  def sourcePostChange(s:SourceType)(implicit d:DiffList): Unit
}*/

/** Something that represents the collapsing of an existing GeneratedVariable. */
trait CollapsedVariable extends GeneratedVariable 

trait CollapsedParameter extends CollapsedVariable with Parameter {
  /** Negative weight indicates removal of stats. */
  def updateChildStats(child:Variable, weight:Double): Unit 
  def clearChildStats: Unit
}


// /** DiscreteVar integrated out by a Multinomial distribution.
//     @author Andrew McCallum */
// class MultinomialDiscrete[A<:DiscreteVariable](val variable:A) {
//   val distribution = new DenseCountsMultinomial[A](variable.domainSize)
//   // TODO What about "parent"?
//   def pr(i:Int): Double = distribution.pr(i) // TODO This should account for the generativeSource?
//   def pr: Double = distribution.pr(variable.intValue)
//   // Manage sufficient statistics
//   def increment(i:Int, incr:Double = 1.0)(implicit d:DiffList): Unit = distribution.increment(i, incr)
//   def decrement(i:Int, incr:Double = 1.0)(implicit d:DiffList): Unit = distribution.increment(i, incr)
//   /*def update(model:Model): Unit = {
//     val distribution = new Array[Double](variable.domainSize)
//     for (i <- 0 until variable.domainSize) {
//       val d = new DiffList
//       variable.setByIndex(i)(d)
//       distribution(i) = d.scoreAndUndo(model)
//     }
//     Maths.normalize(distribution)
//     distribution.set(distribution)
//   }*/
//   def mode: Int = distribution.maxPrIndex
//   def setToMode(implicit d:DiffList): Unit = variable.setByIndex(this.mode)
// }

// class GaussianReal[A<:RealVariable](val variable:A) extends CollapsedVariable {
//   val parent: Gaussian1[A] = 
//     variable match { case v:GeneratedRealVariable[A] => { v.generativeSource.value match { case p:Gaussian1[A] => p; case _ => null }}; case _ => null }
//   val distribution = new Gaussian1[A]
//   def pr(x:Double) = distribution.pr(x)
//   private var evidenceSum = 0.0
//   private var evidenceNormalizer = 0.0
//   // Manage sufficient statistics
//   def increment(e:Double): Unit = { evidenceSum += e; evidenceNormalizer += 1.0 }
//   def decrement(e:Double): Unit = { evidenceSum -= e; evidenceNormalizer -= 1.0 }
//   def update(model:Model): Unit = distribution.mean = evidenceSum / evidenceNormalizer
// }

/** Marginalizes (collapses) a Multinomial, integrating out uncertainty with a Dirichlet.
    This is a DiscreteDistribution, but not a Multinomial. */
/*
class DirichletMultinomial[A<:DiscreteVar](val variable:Multinomial[A]) extends DiscreteDistribution[A] with CollapsedDistribution[A] {
  type VariableType <: DirichletMultinomial[A]
  val parent: Dirichlet[A] = variable.generativeSource.value match { case p:Dirichlet[A] => p; case _ => null }
  val counts = new Array[Double](variable.length)
  var countsTotal = 0.0
  variable.samples.foreach(this.increment(_.index, 1.0)) // Initialize our sufficient statistics with the state of our children
  def size = variable.length
  def sampleIndex: Int = {
    val s = cc.factorie.random.nextDouble; sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += pr(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  def proportion: Seq[Double] = Array.tabulate(size)(pr(_))
  def maxPrIndex = throw new Error // TODO Implement this; Maths.maxIndex(proportion.toArray)
  // TODO These should have difflist arguments!!
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
  def logpr(obsCounts:Vector): Double = math.log(pr(obsCounts)) //indices.foldLeft(0.0)(_+logpr(_))
  def addSample(o:A)(implicit d:DiffList) = increment(o.index, -1.0)
  def removeSample(o:A)(implicit d:DiffList) = increment(o.index, 1.0)
  //def sourcePreChange(s:SourceType)(implicit d:DiffList): Unit = {}
  //def sourcePostChange(s:SourceType)(implicit d:DiffList): Unit = {}
  override def estimate: Unit = {} // Nothing to do because estimated on the fly
}*/

/*object DirichletMultinomialDiscreteTemplate extends Template2[DirichletMultinomial2[_],DiscreteVariable] {
  def unroll1(dm:DirichletMultinomial2[_]) = 
}*/

