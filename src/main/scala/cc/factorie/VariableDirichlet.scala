/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

/** Base of the Dirichlet class hierarchy, needing only methods 'length' and 'mean'. 
    @author Andrew McCallum
*/
// TODO was GeneratedDiscreteValue
//trait AbstractDirichlet[O<:GeneratedDiscreteValue[O]] extends  ProportionDistribution[O] with RandomAccessSeq[Double] 
trait AbstractDirichlet[O<:DiscreteValue] extends  ProportionDistribution[O] with RandomAccessSeq[Double] {
  //type OutcomeType = GeneratedProportionValue[O] // Multinomial[O]
  final def length: Int = mean.length
  final def alpha(index:Int): Double = mean(index) * alphaSum
  def alphas: Seq[Double] = this
  var alphaSum: Double = 1.0 // TODO Is this a reasonable value?
  def alphaVector: Vector = throw new Error // mean.prVector * alphaSum
  def mean: Multinomial[O]
  //def mean(index:Int) : Double
  def apply(index:Int) = alpha(index)
  def outcomeDomain: O#DomainType
  // Was sampleOutcome: OutcomeType
  def sampleMultinomial: Multinomial[O] = throw new Error // TODO { val mul = new DenseCountsMultinomial[O](size); mul.sampleFrom(this)(null); /*throw new Error;*/ mul }
  def sampleOutcome: OutcomeType = sampleMultinomial
  def estimate: Unit = throw new Error("Method estimate is not implemented in this class.  You must add a trait for estimation.")
  def sampleOutcomes(n:Int) : Seq[OutcomeType] = for (i <- 0 until n) yield sampleOutcome
  //def sampleInto(m:OutcomeType): Unit = sampleInto(m:OutcomeType, SparseVector(size)(0.0)) // Waiting Scala 2.8 default args 
  //def sampleInto(m:OutcomeType, counts:{def apply(i:Int):Double; def size:Int}): Unit = m.set(sampleProportionsWithCounts(counts))
  def sampleProportionsWithCounts(counts:{def apply(i:Int):Double; def length:Int}): Seq[Double] = {
    //println("sampleInto")
    var norm = 0.0
    val c = new Array[Double](counts.length)
    // If m is a DirichletMultinomial, account for the fact that the m.pr(i) estimate will include m.source.alpha(i): 
    // m.pr(i) will be calculated from its counts, smoothed with the alphas here, in equal proportions (thanks to norm /= alphaSum below)
    // So we double the variance of random sample in order to make up for it.
    val varianceScaling = 1.0 // TODO Fix this!!! if (m.source != null && classOf[DirichletMultinomial[O]].isAssignableFrom(m.getClass)) 2.0 else 1.0
    for (i <- 0 until c.length) {
      //println("sampleInto alpha(i)="+alpha(i))
      c(i) = Maths.nextGamma((alpha(i)+counts(i))/varianceScaling, 1)(Global.random)
      if (c(i) <= 0.0) c(i) = 0.0001
      norm += c(i)
    }
    norm /= alphaSum // How many pseudo-counts to give the multinomial?  How about as many as this Dirichlet has.
    for (i <- 0 until c.length) {
      c(i) /= norm
      //println("sampleInto c(i)="+c(i))
    }
    c
  }
  //def pr[O2<:OutcomeType](m:O2) : Double = throw new Error("Not yet implemented")
  def pr(m:OutcomeType) : Double = Math.exp(logpr(m))
  override def logpr(m:OutcomeType) : Double = {
    var ret = Maths.logGamma(alphaSum)
    for (i <- 1 until size) ret -= Maths.logGamma(alpha(i))
    for (i <- 1 until size) ret += alpha(i) * Math.log(m(i)) // m(i) == m.pr(i)
    assert(ret == ret) // check for NaN
    ret
  }
}

/** Immutable Dirichlet with equal alpha for all dimensions. 
    @author Andrew McCallum */
@DomainInSubclasses
class SymmetricDirichlet[O<:GeneratedDiscreteValue[O]](initialAlpha:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] {
  type VariableType <: Dirichlet[O];
  type OutcomeDomainType = O
  val outcomeDomain = Domain[O](m)
  val mean = new UniformMultinomial[O]()(m)
  alphaSum = length
  override def keepGeneratedSamples = false
}

class UniformDirichlet[O<:GeneratedDiscreteValue[O]](implicit m:Manifest[O]) extends SymmetricDirichlet[O](1.0)

/** Default Dirichlet, with densely-represented mean, and estimation by moment-matching.
    @author Andrew McCallum */
@DomainInSubclasses
class Dirichlet[O<:GeneratedDiscreteValue[O]](val mean:Multinomial[O], sum:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] with DirichletMomentMatchingEstimator[O] {
  //println("Dirichlet")
  def this(initialAlpha:Double)(implicit m:Manifest[O]) = this(new DenseCountsMultinomial[O](Domain[O](m).size), initialAlpha*Domain[O](m).size)
  def this(initialAlphas:Seq[Double])(implicit m:Manifest[O]) = this(new DenseCountsMultinomial[O](initialAlphas), initialAlphas.foldLeft(0.0)(_+_))
  type VariableType <: Dirichlet[O];
  val outcomeDomain = Domain[O](m)
  alphaSum = sum
}

/** A Dirichlet whose mean is integrated out with distribution 'meanSource', 
    and whose 'alphaSum' is not integrated out, but re-estimated with calls to 'estimate'. */
/*
@deprecated // Not yet working or tested
class DirichletDirichlet[O<:DiscreteOutcome[O]](val meanSource:AbstractDirichlet[O], sum:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] with GenerativeVariable[AbstractDirichlet[O]]{
  def this(dir:AbstractDirichlet[O], initCounts:Seq[Double], as:Double)(implicit m:Manifest[O]) = { this(dir,as)(m); mean.set(initCounts) }
  def this(alphaSum:Double)(implicit m:Manifest[O]) = this(new Dirichlet(1.0), alphaSum)
  def this()(implicit m:Manifest[O]) = this(new Dirichlet(1.0), Domain[O](m).size)
  type VariableType <: DirichletDirichlet[O];
  type SourceType = AbstractDirichlet[O];
  class DomainInSubclasses
  def asOutcome = this
  val outcomeDomain = Domain[O](m)
  val mean = new DirichletMultinomial(meanSource)
  alphaSum = sum
  mean.setSource(meanSource)(null)
  def sampleFrom(source:ProportionGenerating[O])(implicit d:DiffList) = mean.sampleFrom(source)
  override def _registerSample(m:OutcomeType)(implicit d:DiffList) = {
    super._registerSample(m)
    throw new Error
    //mean.generate(m.toArray)
  }
  override def _unregisterSample(m:OutcomeType)(implicit d:DiffList) = {
    super._unregisterSample(m)
    throw new Error
    //mean.ungenerate(m.toArray) // TODO Yipes.  Dangerous, because m.pr could have changed in since mean.generate above
  }
  case class DirichletGenerateDiff(m:OutcomeType)(implicit d:DiffList) extends AutoDiff {
    def variable = DirichletDirichlet.this
    def redo = throw new Error
    def undo = throw new Error
  }
  case class DirichletUngenerateDiff(override val m:OutcomeType)(implicit d:DiffList) extends DirichletGenerateDiff(m) {
    override def redo = super.undo
    override def undo = super.redo
  }
  override def estimate: Unit = throw new Error("Not implemented")
  def sample(implicit d:DiffList):Unit = throw new Error("Not yet implemented")
}
*/

object Dirichlet {
  def apply[O<:GeneratedDiscreteValue[O]](initialAlpha:Double)(implicit m:Manifest[O]) = new SymmetricDirichlet[O](initialAlpha)
  def apply[O<:GeneratedDiscreteValue[O]](implicit m:Manifest[O]) = new SymmetricDirichlet[O](1.0)
}
  
/** Estimate the parameters of a Dirichlet by moment-matching.
    @author Andrew McCallum */
// TODO was GeneratedDiscreteValue
//trait DirichletMomentMatchingEstimator[O<:GeneratedDiscreteValue[O]] extends AbstractDirichlet[O] 
trait DirichletMomentMatchingEstimator[O<:GeneratedDiscreteValue[O]] extends AbstractDirichlet[O] {
  this : Dirichlet[O] =>
  private def setUniform: Unit = 
    mean.set(new RandomAccessSeq[Double] { def apply(i:Int) = uniformPseudoEvidence/length; def length = size})
  def minSamplesForVarianceEstimate = 10
  /** Add a uniform pseudo-multinomial to estimation, with this weight relative to real multinomials */
  def uniformPseudoEvidence = 0.1 // Set to non-zero to avoid logGamma(0.0), leading to NaN
  override def estimate : Unit = {
    if (generatedSamples.size == 0) { setUniform; alphaSum = 1.0; return }
    val smoothing = uniformPseudoEvidence / length
    val m = new Array[Double](length)
    for (i <- 0 until length) m(i) = smoothing // TODO Use Arrays.fill?
    for (s <- generatedSamples; i <- 0 until length) m(i) += s.localPr(i) // TODO just make sure that this Dirichlet's alphas are not part of m.pr!
    assert(Maths.almostEquals(m.sum, generatedSamples.size + smoothing*length))
    mean.set(m)
    //println("unnormalized mean "+_mean.take(20).toList)
    //println("normalized mean "+(new Range(0,_mean.size,1).map(mean(_)).toList))
    if (generatedSamples.size <= minSamplesForVarianceEstimate) { // Not enough samples for a reliable variance estimate
      alphaSum = length
      return
    }
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new Array[Double](length)
    for (s <- generatedSamples; i <- 0 until length) {
      val p = s.localPr(i) // Trying to ensure this Dirichlet's alphas are not part of the probability
      variance(i) += p * p
    }
    for (i <- 0 until length) {
      val a = mean(i)
      variance(i) = (variance(i) / (generatedSamples.size - 1.0)) - a*a
      assert(variance(i) == variance(i))
    }
    //println("mean "+mean.take(10).toList)
    //println("alphaSum="+alphaSum+" variance "+variance.take(10).toList)
    alphaSum = 0.0
    for (i <- 0 until length) {
      //println("logging "+((mean(i) * (1.0 - mean(i)) / variance(i)) - 1.0))
      if (mean(i) != 0.0) alphaSum += Math.log((mean(i) * (1.0 - mean(i)) / variance(i)) - 1.0)
      assert (alphaSum == alphaSum)
    }
    alphaSum = Math.exp(alphaSum / (length - 1))
  }
}

