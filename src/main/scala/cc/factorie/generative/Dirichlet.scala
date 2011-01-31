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

// Proportions ~ Dirichlet(Proportions, Precision)

class DirichletTemplate extends GenerativeTemplateWithStatistics3[Dirichlet,Proportions,RealVarParameter] {
  def unroll1(d:Dirichlet) = Factor(d, d.mean, d.precision)
  def unroll2(p:Proportions) = for (d <- p.childrenOfClass[Dirichlet]) yield Factor(d, p, d.precision)
  def unroll3(prec:RealVarParameter) = for (d <- prec.childrenOfClass[Dirichlet]) yield Factor(d, d.mean, prec)
  def logpr(s:Stat) = math.log(pr(s))
  def pr(s:Stat): Double = pr(s._1, s._2, s._3)
  def pr(value:ProportionsValue, mean:ProportionsValue, precision:Double): Double = {
    def alpha(index:Int): Double = mean(index) * precision.doubleValue
    require(mean.length == value.length)
    var result = maths.logGamma(precision)
    forIndex(value.length)((i:Int) => result -= maths.logGamma(alpha(i)))
    forIndex(value.length)((i:Int) => result += (alpha(i) - 1.0) * math.log(value(i)))
    assert(result == result, "mean="+mean.toList+" precision="+precision+" p="+value.toList) // NaN?
    result
  }
  def sampledValue(s:Stat): ProportionsValue = sampledValue(s._2, s._3, Nil)
  def sampledValue(mean:Seq[Double], prec:Double, children:Iterable[DiscreteVar] = Nil): ProportionsValue = 
    new ProportionsValue {
      private val array = Dirichlet.sampleFrom(mean, prec, children)
      def apply(i:Int) = array(i)
      def length = array.length
    }
}
object DirichletTemplate extends DirichletTemplate

trait Dirichlet extends Proportions with GeneratedVar with CollapsibleParameter {
  /** Return the mean of the Dirichlet distribution from which this Proportions was generated.
      Note that the returned object may be a temporary one generated for the return value, 
      and may not have this Proportions as a child. */
  def mean: Proportions
  def precision: RealVarParameter
  val generativeTemplate = DirichletTemplate
  def generativeFactor = new DirichletTemplate.Factor(this, mean, precision)
  // TODO Why was this next line causing strange compile errors?
  //override def parents: Seq[Parameter] = List(mean, precision)
  def alpha(index:Int): Double = mean(index) * precision.doubleValue
  def alphas: Seq[Double] = mean.map(_ * precision.doubleValue)
  //def sampleProportions: Proportions = new DenseProportions(mean.length) 
  type CollapsedType <: DirichletMultinomial
}

trait MutableDirichlet extends MutableProportions with Dirichlet with MutableGeneratedVar {
  override def defaultEstimator = MutableDirichletEstimator
}

object Dirichlet {
  def sampleFrom(mean:Seq[Double], precision:Double, children:Iterable[DiscreteVar] = Nil): Array[Double] = {
    def alpha(index:Int): Double = mean(index) * precision
    var norm = 0.0
    val p = new Array[Double](mean.length)
    val c = new Array[Double](mean.length)
    for (child <- children) c(child.intValue) += 1.0
    forIndex(mean.length)(i => {
      p(i) = maths.nextGamma(alpha(i) + c(i), 1)(cc.factorie.random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    })
    forIndex(mean.length)(i => p(i) /= norm)
    p
  }
}

/** Proportions, Dirichlet-distributed, with dense separate values for all dimensions. */
class DenseDirichlet(initialMean:Proportions, initialPrecision:RealVarParameter, p:Seq[Double] = Nil) extends DenseProportions(if (p.length == 0) initialMean else p) with MutableDirichlet  {
  def this(size:Int, alpha:Double) = this(new UniformProportions(size), new RealVariableParameter(alpha * size), Nil)
  //def this[T<:DiscreteVars](alpha:Double)(implicit m:Manifest[T]) = this(Domain.get[T](m.erasure).size, alpha)
  // TODO Dispense with these meanRef and precisionRef
  protected val meanRef: ParameterRef[Proportions,Dirichlet] = new ParameterRef(initialMean, this)
  protected val precisionRef = new ParameterRef(initialPrecision, this)
  def mean = meanRef.value
  def mean_=(newMean:Proportions)(implicit d:DiffList): Unit = meanRef.set(newMean)
  def precision = precisionRef.value
  def precision_=(newPrecision:RealVarParameter)(implicit d:DiffList): Unit = precisionRef.set(newPrecision)
  // TODO Why was this next line causing strange compile errors?
  //override def parentRefs = List(meanRef, precisionRef)
  def ~(mean:Proportions, precision:RealVarParameter): this.type = { mean_=(mean)(null); precision_=(precision)(null); this }
  type CollapsedType = DenseDirichletMultinomial
  def newCollapsed = new DenseDirichletMultinomial(mean, precision)
  def setFromCollapsed(c:CollapsedType)(implicit d:DiffList): Unit = set(c)(d)
}


object MutableDirichletEstimator extends Estimator[MutableProportions] {
  // TODO Get rid of this "map".  Where is this method called?
  def estimate(d:MutableProportions, map:scala.collection.Map[Variable,Variable]): Unit = d match {
    case d:MutableDirichlet => {
      val e = new DenseCountsProportions(d.length)
      // Make sure parents (mean and precision) are not influenced by the map
      assert(map(d.mean) == null); assert(map(d.precision) == null)
      // Sum in influence of parents
      e.increment(d.mean.map(_ * d.precision.doubleValue))(null)
      // Sum in influence of children
      for ((child, weight) <- d.weightedGeneratedChildren(map)) child match {
        case x:DiscreteVar => e.increment(x.intValue, weight)(null)
        case p:Proportions => forIndex(p.length)(i => e.increment(i, weight * p(i))(null))
      }
      // Set the DenseDirichlet to the newly estimated value
      d.set(e)(null)
    }
  }
}

// TODO Perhaps all Dirichlet* classes should be re-implemented in terms of "alpha:Masses" instead of "precision" in order to avoid some of this awkwardness.

class GrowableDenseDirichlet(val alpha:Double /*, p:Seq[Double] = Nil*/ ) extends GrowableDenseCountsProportions with MutableDirichlet {
  //def this(alpha:Double) = this(new GrowableUniformProportions(this), new RealVariableParameter(alpha))
  def mean = new GrowableUniformProportions(this)
  def precision = new RealFunction {
    def doubleValue = alpha * GrowableDenseDirichlet.this.length
    def pr = 1.0
    def parents = List(GrowableDenseDirichlet.this) // TODO But note that GrowableDenseDirichlet doesn't have this as a child.
  }
  type CollapsedType = GrowableDenseDirichletMultinomial
  def newCollapsed = new GrowableDenseDirichletMultinomial(alpha)
  def setFromCollapsed(c:CollapsedType)(implicit d:DiffList): Unit = set(c)(d)
}


object DirichletMomentMatching {
  def estimate(mean:DenseProportions, precision:RealVariableParameter, model:Model = cc.factorie.generative.defaultGenerativeModel): Unit = {
    val meanChildren = mean.generatedChildren
    assert(meanChildren.size > 1)
    assert(meanChildren.size == precision.generatedChildren.size) // TODO We are assuming that the contents are the same.
    //val factors = model.factors(List(mean, precision)); assert(factors.size == mean.children.size); assert(factors.size == precision.children.size)
    // Calculcate and set the mean
    val m = new ProportionsArrayValue(new Array[Double](mean.length))
    for (child <- meanChildren) child match { case p:Proportions => forIndex(m.size)(i => m(i) += p(i)) }
    forIndex(m.size)(m(_) /= meanChildren.size)
    mean.set(m)(null)
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new Array[Double](mean.length)
    for (child <- meanChildren) child match { case p:Proportions => forIndex(mean.length)(i => { val diff = p(i) - mean(i); variance(i) += diff * diff })}
    //for (child <- meanChildren) child match { case p:Proportions => forIndex(mean.length)(i => variance(i) += p(i) * p(i)) }
    //println("variance1="+variance.toList)
    forIndex(mean.length)(i => variance(i) /= (meanChildren.size - 1.0))
    //forIndex(mean.length)(i => variance(i) = (variance(i) / meanChildren.size - 1.0) - (m(i) * m(i)))
    //println("variance2="+variance.toList)
    var alphaSum = 0.0
    forIndex(mean.length)(i => if (m(i) != 0.0) alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    precision := math.exp(alphaSum / (mean.length - 1))
    assert(precision.doubleValue == precision.doubleValue, "alphaSum="+alphaSum+" variance="+variance.toList+" mean="+m.toList) // Check for NaN
  }
}
