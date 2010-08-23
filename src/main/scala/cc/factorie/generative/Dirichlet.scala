/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._

// Proportions ~ Dirichlet(Proportions, Precision)

trait Dirichlet extends Proportions with GeneratedVariable with CollapsibleParameter {
  /** Return the mean of the Dirichlet distribution from which this Proportions was generated.
      Note that the returned object may be a temporary one generated for the return value, 
      and may not have this Proportions as a child. */
  def mean: Proportions
  def precision: RealVarParameter
  def parents = List(mean, precision)
  def pr = math.exp(logpr)
  override def logpr: Double = logpr(mean, precision)
  def logpr(mean:Proportions, precision:RealVar): Double = {
    require(mean.length == this.length)
    var result = Maths.logGamma(precision.doubleValue)
    forIndex(length)((i:Int) => result -= Maths.logGamma(alpha(i)))
    forIndex(length)((i:Int) => result += alpha(i) * math.log(pr(i)))
    assert(result == result) // check for NaN
    result
  }
  override def logprFrom(parents:Seq[Parameter]) = parents match {
    case Seq(mean:Proportions, precision:RealVar) => logpr(mean, precision)
  }
  def prFrom(parents:Seq[Parameter]) = math.exp(logprFrom(parents))
  def alpha(index:Int): Double = mean(index) * precision.doubleValue
  type CollapsedType <: DirichletMultinomial
}

trait MutableDirichlet extends MutableProportions with Dirichlet {
  def sampleFrom(mean:Proportions, precision:RealVar, children:Iterable[DiscreteVar] = Nil)(implicit d:DiffList): Unit = {
    set(Dirichlet.sampleFrom(mean, precision, children))
  }
  def sample(implicit d:DiffList): Unit = sampleFrom(mean, precision)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList) = parents match {
    case Seq(mean:Proportions, precision:RealVarParameter) => sampleFrom(mean, precision)
  }
}

object Dirichlet {
  def sampleFrom(mean:Proportions, precision:RealVar, children:Iterable[DiscreteVar] = Nil): Array[Double] = {
    def alpha(index:Int): Double = mean(index) * precision.doubleValue
    var norm = 0.0
    val p = new Array[Double](mean.length)
    val c = new Array[Double](mean.length)
    for (child <- children) c(child.intValue) += 1.0
    forIndex(mean.length)(i => {
      p(i) = Maths.nextGamma(alpha(i) + c(i), 1)(cc.factorie.random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    })
    forIndex(mean.length)(i => p(i) /= norm)
    p
  }
}

class DenseDirichlet(initialMean:Proportions, initialPrecision:RealVarParameter, p:Seq[Double] = Nil) extends DenseProportions(if (p.length == 0) initialMean.asSeq else p) with MutableDirichlet {
  def this(size:Int, alpha:Double) = this(new UniformProportions(size), new RealConstantParameter(alpha * size), Nil)
  protected val meanRef: ParameterRef[Proportions,Dirichlet] = new ParameterRef(initialMean, this)
  protected val precisionRef = new ParameterRef(initialPrecision, this)
  def mean = meanRef.value
  def mean_=(newMean:Proportions)(implicit d:DiffList): Unit = meanRef.set(newMean)
  def precision = precisionRef.value
  def precision_=(newPrecision:RealVarParameter)(implicit d:DiffList): Unit = precisionRef.set(newPrecision)
  override def parentRefs = List(meanRef, precisionRef)
  def ~(mean:Proportions, precision:RealVarParameter): this.type = { mean_=(mean)(null); precision_=(precision)(null); this }
  type CollapsedType = DenseDirichletMultinomial
  def newCollapsed = new DenseDirichletMultinomial(mean, precision)
}

// TODO Perhaps all Dirichlet* classes should be re-implemented in terms of "alpha" instead of "precision" in order to avoid some of this awkwardness.

class GrowableDenseDirichlet(val alpha:Double, p:Seq[Double] = Nil) extends GrowableDenseCountsProportions with MutableDirichlet {
  //def this(alpha:Double) = this(new GrowableUniformProportions(this), new RealVariableParameter(alpha))
  def mean = new GrowableUniformProportions(this)
  def precision = new RealFunction {
    def doubleValue = alpha * GrowableDenseDirichlet.this.length
    def pr = 1.0
    def parents = List(GrowableDenseDirichlet.this) // TODO But note that GrowableDenseDirichlet doesn't have this as a child.
  }
  type CollapsedType = GrowableDenseDirichletMultinomial
  def newCollapsed = new GrowableDenseDirichletMultinomial(alpha)
}

object DirichletMomentMatching {
  def estimate(mean:DenseProportions, precision:RealVariableParameter, model:Model): Unit = {
    assert(mean.children.size == precision.children.size) // TODO We are assuming that the contents are the same.
    //val factors = model.factors(List(mean, precision)); assert(factors.size == mean.children.size); assert(factors.size == precision.children.size)
    // Calculcate and set the mean
    val m = new Array[Double](mean.length)
    for (child <- mean.children) child match { case p:Proportions => forIndex(m.size)(i => m(i) += p(i)) }
    forIndex(m.size)(m(_) /= mean.children.size)
    mean.set(m)(null)
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new Array[Double](mean.length)
    for (child <- mean.children) child match { case p:Proportions => forIndex(mean.length)(i => variance(i) = p(i) * p(i)) }
    forIndex(mean.length)(i => variance(i) = (variance(i) / mean.children.size - 1.0) - (m(i) * m(i)))
    var alphaSum = 0.0
    forIndex(mean.length)(i => if (m(i) != 0.0) alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    precision := Math.exp(alphaSum / (mean.length - 1))
  }
}
