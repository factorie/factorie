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

package cc.factorie.model

import cc.factorie.variable.{DiscreteVar, TensorVar, Var}
import cc.factorie.{la, model}

import scala.reflect.ClassTag

abstract class Template2[N1<:Var,N2<:Var](implicit nm1:ClassTag[N1], nm2:ClassTag[N2]) extends Family2[N1,N2] with Template
{
  val neighborClass1 = nm1.runtimeClass
  val neighborClass2 = nm2.runtimeClass
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2)

  def addLimitedDiscreteCurrentValuesIn12(variables:Iterable[Var]): Unit = {
    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1) && classOf[DiscreteVar].isAssignableFrom(neighborClass2)) 
      for (variable <- variables; factor <- factors(variable)) factor.addLimitedDiscreteCurrentValues12 //limitedDiscreteValues.+=((factor._1.asInstanceOf[DiscreteVar].intValue, factor._2.asInstanceOf[DiscreteVar].intValue))
  }

//  override def limitDiscreteValuesIteratorAsIn(variables:Iterable[DiscreteVar]): Unit = {
//    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1) && classOf[DiscreteVar].isAssignableFrom(neighborClass2)) 
//      for (variable <- variables; factor <- factors(variable)) limitedDiscreteValues.+=((factor._1.asInstanceOf[DiscreteVar].intValue, factor._2.asInstanceOf[DiscreteVar].intValue))
//  }

  final override def addFactors(v:Var, result:scala.collection.mutable.Set[model.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass)) result ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass)) result ++= unroll2(v.asInstanceOf[N2])
    unroll(v) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  //* Override this method if you want to re-capture old unrollCascade functionality. */ 
  def unroll(v:Var): Iterable[Factor] = Nil
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]

  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = {
    // Loop over vars separately from factors to avoid creating all factors for what could be very large data
    (classOf[DiscreteVar].isAssignableFrom(neighborClass1), classOf[DiscreteVar].isAssignableFrom(neighborClass2)) match {
      case (true, true) => {
        for (v <- vars; factor <- factors(v).asInstanceOf[Iterable[Factor2[DiscreteVar,DiscreteVar]]]) {
          if (limitedDiscreteValues12 eq null) limitedDiscreteValues12 = new la.SparseBinaryTensor2(factor._1.domain.dimensionSize, factor._2.domain.dimensionSize)
          limitedDiscreteValues12.+=(factor._1.intValue, factor._2.intValue)
          if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new la.SparseBinaryTensor1(factor._1.domain.dimensionSize)
          limitedDiscreteValues1.+=(factor._1.intValue)
        }
      }
      case (true, false) => {
        for (v <- vars; factor <- factors(v).asInstanceOf[Iterable[Factor2[DiscreteVar,DiscreteVar]]]) {
          if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new la.SparseBinaryTensor1(factor._1.domain.dimensionSize)
          limitedDiscreteValues1.+=(factor._1.intValue)
        }
      }
      case (false, true) => {
        throw new Error("Not yet implemented.")
      }
      case (false, false) => {}
    }
  }
}


abstract class TupleTemplate2[N1<:Var:ClassTag,N2<:Var:ClassTag] extends Template2[N1,N2] with TupleFamily2[N1,N2]
abstract class TupleTemplateWithStatistics2[N1<:Var:ClassTag,N2<:Var:ClassTag] extends Template2[N1,N2] with TupleFamilyWithStatistics2[N1,N2]
abstract class TensorTemplate2[N1<:Var:ClassTag,N2<:Var:ClassTag] extends Template2[N1,N2] with TensorFamily2[N1,N2]
abstract class TensorTemplateWithStatistics2[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag] extends Template2[N1,N2] with TensorFamilyWithStatistics2[N1,N2]
abstract class DotTemplate2[N1<:Var:ClassTag,N2<:Var:ClassTag] extends Template2[N1,N2] with DotFamily2[N1,N2]
abstract class DotTemplateWithStatistics2[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag] extends Template2[N1,N2] with DotFamilyWithStatistics2[N1,N2]

/*
trait DiscreteFactorSettings2 extends Template {
  this: VectorTemplate {
    type TemplateType <: DotTemplate
    type Neighbor1Type <: DiscreteVar
    type Neighbor2Type <: DiscreteVar
    type FactorType <: { def _1:DiscreteVariable ; def _2:DiscreteVariable }
  } =>
  lazy val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize1 = ndd1.size
  lazy val ndd2: DiscreteDomain = throw new Error // TODO nd2.asInstanceOf[DiscreteDomain[DiscreteVar]]
  lazy val ndsize2 = ndd2.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = true
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = {
    if (settingsSparsified) {
      forIndex(sparseSettings1.length)(i => {
        factor._1.set(i)(null)
        forIndex(sparseSettings1(i).length)(j => {
          factor._2.set(j)(null)
          f
        })
      })
    } else {
      var i = 0
      while (i < ndsize1) {
        factor._1.set(i)(null)
        var j = 0
        while (j < ndsize2) {
          factor._2.set(j)(null)
          f
          j += 1
        }
      }
    }
  }
  // Call function f for each valid (possibly sparsified) variable value setting 
  // of the neighboring variables specified in 'vs'. 
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = {
    if (vs.size == 1) {
      val v = vs.head
      if (factor._1 eq v) {
        // vary v1, keep v2 constant
        val v = factor._1 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings2(factor._2.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize1)(i => { v.set(i)(null); f })
      } else if (factor._2 eq v) {
        // vary v2, keep v1 constant
        val v = factor._2 // Get it with the correct type
        if (settingsSparsified) {
          val sparseSettings = sparseSettings1(factor._1.intValue)
          forIndex(sparseSettings.length)(i => { v.set(sparseSettings(i))(null); f })
        } else forIndex(ndsize2)(i => { v.set(i)(null); f })
      }
    } else if (vs.size == 2) {
      throw new Error("Not yet implemented.")
    } else throw new Error("Asked to vary settings of too many variables.")
  }

  private var settingsSparsified = false
  // Redundant storage of valid v1,v2 value pairs
  private var sparseSettings1: Array[Array[Int]] = null // first index=v1, second index=v2
  private var sparseSettings2: Array[Array[Int]] = null // first index=v2, second index=v1
  // Initialize sparseSettings1 and sparseSettings2 to cover all values in factors touching the variables in 'vs'.
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    println("Template sparsifySettingsFor ndsize1="+ndsize1+" ndsize2="+ndsize2)
    assert (ndsize1 > 0, "sparsifySettingsFor before Domain size properly set.")
    assert (ndsize2 > 0, "sparsifySettingsFor before Domain size properly set.")
    val sparse1 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    val sparse2 = new HashMap[Int,scala.collection.mutable.Set[Int]]
    vs.foreach(v => {
      this.factors(v).foreach(f => {
        sparse1.getOrElseUpdate(f._1.intValue, new HashSet[Int]) += f._2.intValue
        sparse2.getOrElseUpdate(f._2.intValue, new HashSet[Int]) += f._1.intValue
      })
    })
    sparseSettings1 = new Array[Array[Int]](ndsize1)
    sparseSettings2 = new Array[Array[Int]](ndsize2)
    forIndex(sparseSettings1.length)(i => sparseSettings1(i) = sparse1.getOrElse(i, new HashSet[Int]).toArray)
    forIndex(sparseSettings2.length)(i => sparseSettings2(i) = sparse2.getOrElse(i, new HashSet[Int]).toArray)
    settingsSparsified = true 
  }
}
*/


