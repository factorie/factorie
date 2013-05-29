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
import cc.factorie._
import cc.factorie.la._
import cc.factorie.util.{DoubleSeq,IntSeq}
import scala.util.Random

// Proportions Values

/** A Tensor containing non-negative numbers summing to 1.0.  It is the parameter of a Discrete or Multinomial distribution. 
    Proportions contain Masses, which may not sum to 1.0;
    some Proportions subclasses can have their value changed by incrementing these inner masses.
    All Proportions also inherit directly from Masses, but, naturally, these Masses always sum to 1.0, and generally are not directly mutable.
    @author Andrew McCallum */
trait Proportions extends Masses with ReadOnlyTensor {
  def masses: Masses
  @inline final override def pr(i:Int): Double = apply(i)
  override def stringPrefix = "Proportions"
  override def toString = this.asSeq.take(maxToStringLength).mkString(stringPrefix+"(", ",", if (length > 10) "...)" else ")")
}

object Proportions {
  /** Return a zero-mass Proportions with the same dimensionality and sparsity as the Tensor argument. */
  def blankCopy(t:Tensor1): Proportions1 = t match {
    case t:DenseTensor1 => new DenseProportions1(t.dim1)
    case t:SparseTensor1 => throw new Error("Not yet implemeneted")
  }
  def blankCopy(t:Tensor2): Proportions2 = t match {
    case t:DenseTensor2 => new DenseProportions2(t.dim1, t.dim2)
    //case t:SparseTensor2 => throw new Error("Not yet implemeneted")
  }
  def blankCopy(t:Tensor3): Proportions3 = t match {
    case t:DenseTensor3 => new DenseProportions3(t.dim1, t.dim2, t.dim3)
    //case t:SparseTensor2 => throw new Error("Not yet implemeneted")
  }
}

trait Proportions1 extends Masses1 with Proportions { def masses: Masses1 }
trait Proportions2 extends Masses2 with Proportions { def masses: Masses2 }
trait Proportions3 extends Masses3 with Proportions { def masses: Masses3 }
trait Proportions4 extends Masses4 with Proportions { def masses: Masses4 }

// Proportions Values of dimensionality 1

class SingletonProportions1(dim1:Int, singleIndex:Int) extends SingletonMasses1(dim1, singleIndex, 1.0) with Proportions1 {
  def masses = this
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = singleIndex
}
class UniformProportions1(dim1:Int) extends UniformMasses1(dim1, 1.0/dim1) with Proportions1 {
  def masses = this
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}
class GrowableUniformProportions1(sizeProxy:Iterable[Any], uniformValue:Double = 1.0) extends Proportions1 {
  val masses = new GrowableUniformMasses1(sizeProxy, uniformValue)
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  def massTotal = 1.0
  def dim1 = masses.length
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, masses.length)
  def isDense = true
  @inline final override def apply(index:Int) = {
    val result = 1.0 / masses.length
    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
    result
  }
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = r.nextInt(dim1)
}

trait DenseProportions extends Proportions {
  def apply(index:Int): Double = {
    val mt = masses.massTotal
    if (mt == 0.0) 1.0 / length else masses.apply(index) / mt
  }
  // Should this method be here? it's a really good way to get bugs when you're looking for masses.massTotal -luke
  // I understand, but Proportions is itself a Masses, and its massTotal is indeed 1.0, so I don't see a way around it.
  def massTotal = 1.0
  def isDense = true
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  override def zero(): Unit = masses.zero()
}

class DenseProportions1(override val dim1:Int) extends Proportions1 with DenseProportions {
  def this(ds:DoubleSeq) = { this(ds.length); this.masses += ds }
  def this(a:Array[Double]) = { this(a.length); this.masses += a }
  def this(dim1:Int, uniformValue:Double) = { this(dim1); this.masses += uniformValue }
  val masses = new DenseMasses1(dim1)
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, dim1)
}
class DenseProportions2(override val dim1:Int, override val dim2:Int) extends Proportions2 with DenseProportions {
  val masses = new DenseMasses2(dim1, dim2)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
}
class DenseProportions3(override val dim1:Int, override val dim2:Int, override val dim3:Int) extends Proportions3 with DenseProportions {
  val masses = new DenseMasses3(dim1, dim2, dim3)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
  def activeDomain3 = new cc.factorie.util.RangeIntSeq(0, dim3)
}
class DenseProportions4(override val dim1:Int, override val dim2:Int, override val dim3:Int, override val dim4:Int) extends Proportions4 with DenseProportions {
  val masses = new DenseMasses4(dim1, dim2, dim3, dim4)
  def activeDomain =  new cc.factorie.util.RangeIntSeq(0, length)
  def activeDomain1 = new cc.factorie.util.RangeIntSeq(0, dim1)
  def activeDomain2 = new cc.factorie.util.RangeIntSeq(0, dim2)
  def activeDomain3 = new cc.factorie.util.RangeIntSeq(0, dim3)
  def activeDomain4 = new cc.factorie.util.RangeIntSeq(0, dim4)
}

class GrowableDenseProportions1(val sizeProxy:Iterable[Any]) extends Proportions1 with DenseProportions {
  override def dim1 = sizeProxy.size
  val masses = new GrowableDenseMasses1(sizeProxy)
  def activeDomain = new cc.factorie.util.RangeIntSeq(0, dim1)
}

/** An immutable Proportions from a pre-normalized Tensor. */
abstract class NormalizedTensorProportions(val tensor:Tensor, checkNormalization:Boolean = true) extends Proportions {
  protected def _tensor: Tensor
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  if (checkNormalization) require(maths.almostEquals(tensor.sum, 1.0, 0.0001))
  @inline final def apply(i:Int) = _tensor.apply(i)
  def massTotal = 1.0
  def isDense = _tensor.isDense
}
class NormalizedTensorProportions1(override val tensor:Tensor1, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions1 {
  protected val _tensor = tensor
  def dim1 = _tensor.dim1
  def activeDomain = _tensor.activeDomain
  def masses = this
}
class NormalizedTensorProportions2(override val tensor:Tensor2, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions2 {
  protected val _tensor = tensor
  def dim1 = _tensor.dim1
  def dim2 = _tensor.dim2
  def activeDomain = _tensor.activeDomain
  def activeDomain1 = _tensor.activeDomain1
  def activeDomain2 = _tensor.activeDomain2
  def masses = this
}
class NormalizedTensorProportions3(override val tensor:Tensor3, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions3 {
  protected val _tensor = tensor
  def dim1 = _tensor.dim1
  def dim2 = _tensor.dim2
  def dim3 = _tensor.dim3
  def activeDomain = _tensor.activeDomain
  def activeDomain1 = _tensor.activeDomain1
  def activeDomain2 = _tensor.activeDomain2
  def activeDomain3 = _tensor.activeDomain3
  def masses = this
}
class NormalizedTensorProportions4(override val tensor:Tensor4, checkNormalization:Boolean = true) extends NormalizedTensorProportions(tensor, checkNormalization) with Proportions4 {
  protected val _tensor = tensor
  def dim1 = _tensor.dim1
  def dim2 = _tensor.dim2
  def dim3 = _tensor.dim3
  def dim4 = _tensor.dim4
  def activeDomain = _tensor.activeDomain
  def activeDomain1 = _tensor.activeDomain1
  def activeDomain2 = _tensor.activeDomain2
  def activeDomain3 = _tensor.activeDomain3
  def activeDomain4 = _tensor.activeDomain4
  def masses = this
}

class SortedSparseCountsProportions1(val dim1:Int) extends Proportions1 {
  val masses = new SortedSparseCountsMasses1(dim1)
  def dot(t: DoubleSeq): Double = throw new Error("No efficient dot for " + this.getClass.getName)
  def massTotal = 1.0
  def activeDomain = masses.activeDomain  // throw new Error("Not implemented")
  def isDense = false
  var prior: Masses = null  // TODO We need somehow to say that this isDeterministic function of this.prior.
  
  def apply(index:Int): Double = {
    if (prior eq null) {
      if (masses.countsTotal == 0) 1.0 / length
      else masses.countOfIndex(index).toDouble / masses.countsTotal
    } else {
      if (masses.countsTotal == 0) prior(index) / prior.massTotal
      else masses.countOfIndex(index).toDouble / masses.countsTotal
    }
  }
  override def zero(): Unit = masses.zero()
  // Note that "def zero()" defined in SortedSparseCountsMasses1 does not zero this.prior
  override def top(n:Int): cc.factorie.util.TopN[String] = {
    val len = math.min(n, masses.numPositions)
    val result = new cc.factorie.util.TopN[String](len)
    var i = 0
    while (i < len) {
      result += (masses.indexAtPosition(i), masses.countAtPosition(i).toDouble / masses.countsTotal)
      i += 1
    }
    result
  }
  override def sampleIndex(massTotal:Double)(implicit r:Random): Int = masses.sampleIndex(massTotal)(r)
}



// Proportions Variable

trait ProportionsDomain extends MassesDomain with Domain[Proportions]
object ProportionsDomain extends ProportionsDomain

trait ProportionsVar extends MassesVar with ValueBound[Proportions] {
  override def value: Proportions
  def domain: ProportionsDomain = ProportionsDomain
  // TODO What else should go here?
}
trait MutableProportionsVar[A<:Proportions] extends MutableMassesVar[A] with ProportionsVar

class ProportionsVariable extends MutableProportionsVar[Proportions] {
  def this(initialValue:Proportions) = { this(); set(initialValue)(null) }
  //val massesVariable = new MassesVariable(tensor.masses) // TODO Is there a risk that tensor.masses may not have its final value yet here?  Yes!  It could be changed at any time via _set!!!
  
  // Methods that track modifications on a DiffList
  def updateMasses(index:Int, newValue:Double)(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented")
    tensor.masses.update(index, newValue)
  }
  def incrementMasses(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    if (d ne null) d += IncrementProportionsMassesIndexDiff(index, incr)
    tensor.masses.+=(index, incr)
  }
  def incrementMasses(incr:Tensor)(implicit d:DiffList): Unit = {
    require(incr.length == tensor.length)
    if (d ne null) d += IncrementProportionsMassesDiff(incr)
    tensor.masses += incr
  }
  def zeroMasses(implicit d:DiffList): Unit = {
    if (d ne null) d += ZeroProportionsMassesDiff(tensor.toArray)
    tensor.masses.zero()
  }
  
  case class IncrementProportionsMassesIndexDiff(index:Int, incr:Double) extends Diff {
    def variable = ProportionsVariable.this
    def undo = tensor.masses.+=(index, -incr)
    def redo = tensor.masses.+=(index, incr)
  }
  case class IncrementProportionsMassesDiff(t: Tensor) extends Diff {
    def variable = ProportionsVariable.this
    def undo = tensor.masses -= t // Note this relies on Tensor t not having changed.
    def redo = tensor.masses += t
  }
  case class ZeroProportionsMassesDiff(prev: Array[Double]) extends Diff {
    def variable = ProportionsVariable.this
    def undo = tensor.masses += prev
    def redo = tensor.masses.zero()
  }

}
// In the future, we might also need a ProportionsVar1, ProportionsVar2, etc. -akm

object ProportionsVariable {
  def uniform(dim:Int) = new ProportionsVariable(new UniformProportions1(dim))
  def dense(dim:Int) = new ProportionsVariable(new DenseProportions1(dim))
  def growableDense(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableDenseProportions1(sizeProxy))
  def growableUniform(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableUniformProportions1(sizeProxy, 1.0))
  def sortedSparseCounts(dim:Int) = new ProportionsVariable(new SortedSparseCountsProportions1(dim))
  //def growableSparseCounts(sizeProxy:Iterable[Any]) = new ProportionsVariable(new SortedSparseCountsProportions1(sizeProxy)) // Not yet implemented
}

trait ProportionsMarginal extends Marginal {
  //def variables = Seq(_1)
  def _1: ProportionsVar
  def mean: Proportions
  def variance: Double
  //def setToMaximize(implicit d:DiffList): Unit = _1.asInstanceOf[ProportionsVariable].set(mean)
}

class ProportionsAssignment(p:MutableProportionsVar[Proportions], v:Proportions) extends Assignment1[MutableProportionsVar[Proportions]](p, v) with ProportionsMarginal {
  //final def _1 = p // TODO Consider renaming Assignment1.var1 back to _1
  def mean = throw new Error // TODO!!! Should be this instead: value1
  def variance = Double.PositiveInfinity // TODO Is this the right value?
}


object MaximizeProportions extends Maximize {
  import cc.factorie.generative.{GenerativeModel,Dirichlet,Discrete,Mixture,CategoricalMixture,PlatedDiscreteMixture}
  /*override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal]): Option[Summary[ProportionsAssignment]] = {
    if (variables.size != 1) return None
    variables.head match {
      case mp: ProportionsVariable => {
        model match {
          case model:GenerativeModel => Some(new SingleSummary(new ProportionsAssignment(mp, maxProportions(mp, model, null))))
          case _ => return None
        }
      }
      case _ => return None
    }
  }*/
  override def infer(variables:Iterable[Var], model:Model, summary:Summary = null): Option[Summary] = {
    // override def infer(variables:Iterable[Variable], model:Model): Option[Summary[ProportionsAssignment]] = 
    if (variables.size != 1) return None
    (variables.head, model, summary) match {
      case (mp:ProportionsVariable, model:GenerativeModel, null) => {
        Some(new SingletonSummary(new ProportionsAssignment(mp, maxProportions(mp, model, null))))
      }
      case (mp:ProportionsVariable, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]) => {
        Some(new SingletonSummary(new ProportionsAssignment(mp, maxProportions(mp, model, summary))))
      }
      case _ => None
    }
  }
  def apply(p:ProportionsVariable, model:GenerativeModel): Unit = apply(p, model, null)
  def apply(p:ProportionsVariable, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]): Unit = p.set(maxProportions(p, model, summary))(null)
  def maxProportions[A](p:ProportionsVariable, model:GenerativeModel, summary:DiscreteSummary1[DiscreteVar]): Proportions = {
    // Zero an accumulator
    var e: DenseProportions1 = new DenseProportions1(p.tensor.length) // TODO Consider instead passing this in as an argument, so that SparseProportions could be used
    // Initialize with prior; find the factor that is the parent of "p", and use its Dirichlet masses for initialization
    model.parentFactor(p) match {
	  case f:Dirichlet.Factor => e.masses := f._2.tensor
	  case null => {}
      case _ => throw new Error // TODO Do something more reasonable here?
    }
    // Incorporate children
    @inline def incrementForDiscreteVar(dv:DiscreteVar, incr:Double): Unit = {
      val marginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(dv) 
      if (marginal eq null) e.masses.+=(dv.intValue, incr) else e.masses.+=(marginal.proportions, incr)
    }
    for (factor <- model.extendedChildFactors(p)) factor match {
      //case parent:GenerativeFactor if (parent.child == p) => {} // Parent factor of the Proportions we are estimating already incorporated above
      // The array holding the mixture components; the individual components (CategoricalMixture.Factors) will also be among the extendedChildFactors
      case m:Mixture.Factor => {}
      // A simple DiscreteVar child of the Proportions
      case d:Discrete.Factor => incrementForDiscreteVar(d._1, 1.0)
      // A DiscreteVar child of a Mixture[Proportions]
      case dm:CategoricalMixture[A]#Factor => {
        val gate = dm._3
        val gateMarginal:DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(gate) 
        //val qFactors = if (qModel eq null) Nil else qModel.factors(Seq(gate))
        val mixtureIndex = dm._2.indexOf(p) // Yipes!  Linear search.
        if (gateMarginal eq null) {
          if (dm._3.intValue == mixtureIndex) incrementForDiscreteVar(dm._1, 1.0)
        } else {
          incrementForDiscreteVar(dm._1, gateMarginal.proportions(mixtureIndex))
        }
      }
      case pdm:PlatedDiscreteMixture.Factor => {
        // We don't yet handle variational summary of PlatedDiscreteMixture.Factor
        val mixtureIndex = pdm._2.indexOf(e)
        var i = 0; val len = pdm._1.length
        while (i < len) { if (pdm._3.intValue(i) == mixtureIndex) e.masses.+=(pdm._3.intValue(i), 1.0); i += 1 }
      }
    }
    e
  }
}
