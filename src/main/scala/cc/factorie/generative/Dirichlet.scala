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

object Dirichlet extends GenerativeFamily2[Proportions,Masses] {
  self =>
  def pr(value:ProportionsValue, alpha:Seq[Double]): Double = {
    require(value.length == alpha.length)
    var result = maths.logGamma(alpha.sum)
    forIndex(value.length)((i:Int) => result -= maths.logGamma(alpha(i)))
    forIndex(value.length)((i:Int) => result += (alpha(i) - 1.0) * math.log(value(i)))
    assert(result == result, "alpha="+alpha.toList+" p="+value.toList) // NaN?
    result
  }
  def sampledValue(masses:Seq[Double], children:Iterable[DiscreteVar] = Nil): ProportionsValue = 
    new ProportionsValue {
      private val array = sampledArray(masses, children)
      def apply(i:Int) = array(i)
      def length = array.length
    }
  // TODO Make a more general argument type than Iterable[DiscreteVar], like Iterable[Int] (but I'm concerned about boxing)
  def sampledArray(alpha:Seq[Double], children:Iterable[DiscreteVar] = Nil): Array[Double] = {
    var norm = 0.0
    val p = new Array[Double](alpha.length)
    val c = new Array[Double](alpha.length)
    for (child <- children) c(child.intValue) += 1.0
    forIndex(alpha.length)(i => {
      p(i) = maths.nextGamma(alpha(i) + c(i), 1)(cc.factorie.random)
      if (p(i) <= 0.0) p(i) = 0.0001
      norm += p(i)
    })
    forIndex(alpha.length)(i => p(i) /= norm)
    p
  }
  case class Factor(_1:Proportions, _2:Masses) extends super.Factor {
    def pr(s:StatisticsType) = self.pr(s._1, s._2)
    def sampledValue(s:StatisticsType): ProportionsValue = self.sampledValue(s._2, Nil)
    override def updateCollapsedChild(): Boolean = _1 match {
      case p:DenseCountsProportions => { p.increment(_2.value)(null); true }
      case _ => false
    }
  }
  def newFactor(a:Proportions, b:Masses) = Factor(a, b)
}

object DirichletMomentMatching {
  def estimate(masses:MutableMasses, model:Model = cc.factorie.generative.GenerativeModel): Unit = {
    val numChildren = masses.childFactors.size
    //val massesChildren = masses.children //mean.generatedChildren // TODO Without "generatedChildren" this no longer works for Dirichlet mixtures.
    assert(numChildren > 1)
    //val factors = model.factors(List(mean, precision)); assert(factors.size == mean.children.size); assert(factors.size == precision.children.size)
    // Calculcate and set the mean
    val m = new ProportionsArrayValue(new Array[Double](masses.length))
    for (factor <- masses.childFactors) factor match { 
      case f:Dirichlet.Factor => {
        require(masses.length == f._1.length) // Make sure that each child Proportions has same length as parent masses
        forIndex(m.size)(i => m(i) += f._1(i))
      }
    }
    forIndex(m.size)(m(_) /= numChildren)
    //mean.set(m)(null)
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    val variance = new Array[Double](masses.length)
    for (factor <- masses.childFactors) factor match { 
      case f:Dirichlet.Factor => forIndex(masses.length)(i => { val diff = f._1(i) - m(i); variance(i) += diff * diff })
    }
    //for (child <- meanChildren) child match { case p:Proportions => forIndex(mean.length)(i => variance(i) += p(i) * p(i)) }
    //println("variance1="+variance.toList)
    forIndex(masses.length)(i => variance(i) /= (numChildren - 1.0))
    //forIndex(mean.length)(i => variance(i) = (variance(i) / meanChildren.size - 1.0) - (m(i) * m(i)))
    //println("variance2="+variance.toList)
    var alphaSum = 0.0
    forIndex(masses.length)(i => if (m(i) != 0.0) alphaSum += math.log((m(i) * (1.0 - m(i)) / variance(i)) - 1.0))
    val precision = math.exp(alphaSum / (masses.length - 1))
    assert(precision == precision, "alphaSum="+alphaSum+" variance="+variance.toList+" mean="+m.toList) // Check for NaN
    forIndex(m.size)(i => m(i) = m(i) * precision)
    masses.set(m)(null)
  }
}

/*
trait Dirichlet extends Proportions with GeneratedVar with CollapsibleParameter with VarWithCollapsedType[DirichletMultinomial] {
  // Return the mean of the Dirichlet distribution from which this Proportions was generated.
  //   Note that the returned object may be a temporary one generated for the return value, 
  //    and may not have this Proportions as a child. 
  def mean: Proportions
  def precision: RealVarParameter
  // TODO Why was this next line causing strange compile errors?
  //override def parents: Seq[Parameter] = List(mean, precision)
  def alpha(index:Int): Double = mean(index) * precision.doubleValue
  def alphas: Seq[Double] = mean.map(_ * precision.doubleValue)
  //def sampleProportions: Proportions = new DenseProportions(mean.length)
  def generatedChildValues: Iterable[DiscreteValue] = {
    val result = new scala.collection.mutable.ArrayBuffer[DiscreteValue]
    for (child <- children) child match {
      case mcs:MixtureComponents[_] => {
        val indexInMixture = mcs.indexOf(this)
        require(indexInMixture >= 0)
        for (grandchild <- mcs) grandchild match {
          case dmv:DiscreteMixtureVar => if (dmv.choice.intValue == indexInMixture) result += dmv.value
          case pdmv:PlatedDiscreteMixtureVar => forIndex(pdmv.length)(seqIndex => {
            if (pdmv.choice.intValue(seqIndex) == indexInMixture) result += pdmv.value(seqIndex)
          })
        }
      }
      case gdv:GeneratedDiscreteVar => result += gdv.value
    }
    result
  }
}

trait MutableDirichlet extends MutableProportions with Dirichlet with MutableGeneratedVar {
  override def defaultEstimator = MutableDirichletEstimator
}


class DenseDirichlet(initialMean:Proportions, initialPrecision:RealVarParameter, p:Seq[Double] = Nil) extends DenseProportions(if (p.length == 0) initialMean else p) with MutableDirichlet with VarWithCollapsedType[DenseDirichletMultinomial]  {
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
  def newCollapsed = {
    val dm = new DenseDirichletMultinomial(mean, precision)
    for (child <- children) child match {
      case mcs:MixtureComponents[_] => {
          val indexInMixture = mcs.indexOf(this)
          require(indexInMixture >= 0)
          for (grandchild <- mcs) grandchild match {
            case dmv:DiscreteMixtureVar => if (dmv.choice.intValue == indexInMixture) dm.increment(dmv.intValue, 1.0)(null)
            case pdmv:PlatedDiscreteMixtureVar => forIndex(pdmv.length)(seqIndex => {
              if (pdmv.choice.intValue(seqIndex) == indexInMixture) dm.increment(pdmv.intValue(seqIndex), 1.0)(null)
            })
          }
      }
      case pgdv:PlatedGeneratedDiscreteVar => pgdv.foreach(discreteValue => dm.increment(discreteValue.intValue, 1.0)(null))
      case gdv:GeneratedDiscreteVar => dm.increment(gdv.intValue, 1.0)(null)
    }
    //println("DenseDirichletMultinomial countsTotal="+dm.countsTotal)
    dm
  }
  def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
    case ddm:DenseDirichletMultinomial => set(ddm)(d)
  }
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
      throw new Error("Implementation pending a re-visitation of Parameter.weightedGeneratedChildren implementation.")
      // Sum in influence of children
      //for ((child, weight) <- d.weightedGeneratedChildren(map)) child match {
      //  case x:DiscreteVar => e.increment(x.intValue, weight)(null)
      //  case p:Proportions => forIndex(p.length)(i => e.increment(i, weight * p(i))(null))
      //}
      // Set the DenseDirichlet to the newly estimated value
      d.set(e)(null)
    }
  }
}

// TODO Perhaps all Dirichlet* classes should be re-implemented in terms of "alpha:Masses" instead of "precision" in order to avoid some of this awkwardness.

class GrowableDenseDirichlet(val alpha:Double, val dimensionDomain: DiscreteDomain) extends GrowableDenseCountsProportions(8) with MutableDirichlet with VarWithCollapsedType[GrowableDenseDirichletMultinomial] {
  //def this(alpha:Double) = this(new GrowableUniformProportions(this), new RealVariableParameter(alpha))
  def mean = new GrowableUniformProportions(this)
  def precision = new RealFunction {
    def doubleValue = alpha * GrowableDenseDirichlet.this.length
    def pr = 1.0
    def parents = List(GrowableDenseDirichlet.this) // TODO But note that GrowableDenseDirichlet doesn't have this as a child.
  }
  def newCollapsed = {
    val dm = new GrowableDenseDirichletMultinomial(alpha, dimensionDomain)
    for (child <- children) child match {
      case mcs:MixtureComponents[_] => {
        val indexInMixture = mcs.indexOf(this)
        require(indexInMixture >= 0)
        for (grandchild <- mcs.children) grandchild match {
          case dmv:DiscreteMixtureVar => if (dmv.choice.intValue == indexInMixture) dm.increment(dmv.intValue, 1.0)(null)
          case pdmv:PlatedDiscreteMixtureVar => forIndex(pdmv.length)(seqIndex => {
            if (pdmv.choice.intValue(seqIndex) == indexInMixture) dm.increment(pdmv.intValue(seqIndex), 1.0)(null)
          })
        }
      }
      case gdv:GeneratedDiscreteVar => dm.increment(gdv.intValue, 1.0)(null)
    }
    //println("GrowableDenseDirichlet countsTotal="+dm.countsTotal)
    dm
  }
  def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
    case gddm:GrowableDenseDirichletMultinomial => set(gddm)(d)
  }
}

*/

