/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

// Proportions ~ Dirichlet(Proportions, Precision)

trait Dirichlet extends Proportions with GeneratedVariable with CollapsibleParameter {
  /** Return the mean of the Dirichlet distribution from which this Proportions was generated.
      Note that the returned object may be a temporary one generated for the return value, 
      and may not have this Proportions as a child. */
  def mean: Proportions
  def precision: RealValueParameter
  def pr = math.exp(logpr)
  override def logpr: Double = logpr(mean, precision)
  def logpr(mean:Proportions, precision:RealValue): Double = {
    var result = Maths.logGamma(precision.doubleValue)
    forIndex(length)((i:Int) => result -= Maths.logGamma(alpha(i)))
    forIndex(length)((i:Int) => result += alpha(i) * math.log(pr(i)))
    assert(result == result) // check for NaN
    result
  }
  def alpha(index:Int): Double = mean(index) * precision.doubleValue
  type CollapsedType = DenseDirichletMultinomial
  def newCollapsed = new DenseDirichletMultinomial(mean, precision)
}

class DenseDirichlet(initialMean:Proportions, initialPrecision:RealValueParameter, p:Seq[Double] = Nil) extends DenseProportions(p) with Dirichlet {
  def this(size:Int, alpha:Double) = this(new UniformProportions(size), new RealConstantParameter(alpha * size), Nil)
  protected val meanRef: ParameterRef[Proportions,Dirichlet] = new ParameterRef(initialMean, this)
  protected val precisionRef = new ParameterRef(initialPrecision, this)
  def mean = meanRef.value
  def mean_=(newMean:Proportions)(implicit d:DiffList): Unit = meanRef.set(newMean)
  def precision = precisionRef.value
  def precision_=(newPrecision:RealValueParameter)(implicit d:DiffList): Unit = precisionRef.set(newPrecision)
  def parents = List(mean, precision)
  override def parentRefs = List(meanRef, precisionRef)
  def ~(mean:Proportions, precision:RealValueParameter): this.type = { mean_=(mean)(null); precision_=(precision)(null); this }
  def sampleFrom(mean:Proportions, precision:RealValue, children:Iterable[DiscreteValue] = Nil)(implicit d:DiffList): Unit = {
    var norm = 0.0
    val p = new Array[Double](length)
    val c = new Array[Double](length)
    for (child <- children) c(child.intValue) += 1.0
    forIndex(this.length)(i => {
      p(i) = Maths.nextGamma(alpha(i) + c(i), 1)(Global.random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    })
    forIndex(this.length)(i => p(i) /= norm)
    set(p)
  }
  def sample(implicit d:DiffList): Unit = sampleFrom(mean, precision)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList) = parents match {
    case Seq(mean:Proportions, precision:RealValueParameter) => sampleFrom(mean, precision)
  }
}

trait DirichletMultinomial extends Proportions with CollapsedParameter with GeneratedValue {
  def mean: Proportions
  def precision: RealValueParameter
  def parents = List(mean, precision)
  mean.addChild(this)(null)
  precision.addChild(this)(null)
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit
  def zero: Unit
  def counts(index:Int): Double
  def countsTotal: Double
  def pr = 1.0 // TODO implement.  Since this is collapsed, what should it be?  1.0?
  def detatch: Unit = { mean.removeChild(this)(null); precision.removeChild(this)(null) } // TODO Is this necessary?
  override def apply(index:Int) : Double = {
    val alphaSum = precision.doubleValue
    (counts(index) + mean(index) * alphaSum) / (countsTotal + alphaSum)
  }
  /*override def addChild(c:GeneratedValue)(implicit d:DiffList): Unit = {
    // xxx c match { case v:DiscreteValue => increment(v.intValue, 1.0); case _ => throw new Error } // xxx This seems to be the slowness culprit
    super.addChild(c)(d)
  }
  override def removeChild(c:GeneratedValue)(implicit d:DiffList): Unit = {
    //println("DirichletMultinomial.removeChild "+c)
    // xxx c match { case v:DiscreteValue => increment(v.intValue, -1.0); case _ => throw new Error } 
    super.removeChild(c)(d)
  }*/
  def clearChildStats: Unit = this.zero
  def updateChildStats(child:Variable, weight:Double): Unit = child match {
    case d:DiscreteValue => increment(d.intValue, weight)(null)
    case p:Proportions if (p.length == length) => forIndex(length)(i => increment(i, p(i) * weight)(null))
  }
  // Perhaps DirichletMultinomial should not be a GeneratedVariable?  But it does have parents and children.
  def sample(implicit d:DiffList): Unit = new Error
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = new Error
}

class DenseDirichletMultinomial(val mean:Proportions, val precision:RealValueParameter) extends DenseCountsProportions(mean.length) with DirichletMultinomial {
  def this(size:Int, alpha:Double) = this(new UniformProportions(size), new RealVariableParameter(alpha*size))
  //def this(dirichlet:Dirichlet) = this(dirichlet.mean, dirichlet.precision)
}

class GrowableDenseDirichletMultinomial(val alpha:Double) extends GrowableDenseCountsProportions with DirichletMultinomial {
  lazy val mean = new GrowableUniformProportions(this)
  lazy val precision = new RealFunction {
    def doubleValue = alpha * GrowableDenseDirichletMultinomial.this.length
    def pr = 1.0
    def parents = List(GrowableDenseDirichletMultinomial.this) // TODO But note that GrowableDenseDirichletMultinomial doesn't have this as a child.
  }
}

object DirichletMomentMatching {
  def estimate(mean:DenseProportions, precision:RealVariableParameter, model:Model): Unit = {
    assert(mean.children.size == precision.children.size) // TODO We are assuming that the contents are the same.
    //val factors = model.factors(List(mean, precision)); assert(factors.size == mean.children.size); assert(factors.size == precision.children.size)
    // Calulcate and set the mean
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
