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
import cc.factorie.{la, model, _}

import scala.collection.mutable.Set
import scala.reflect.ClassTag

/** A Template that creates Factors with one neighbor. */
abstract class Template1[N1<:Var](implicit nm1: ClassTag[N1]) extends /*ModelWithFactorType with*/ Template with Family1[N1]
{
  val neighborClass1 = nm1.runtimeClass
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1)

  
//  def addLimitedDiscreteValuesIn(variables:Iterable[DiscreteVar]): Unit = {
//    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1)) 
//      for (variable <- variables; factor <- factors(variable)) limitedDiscreteValues.+=(factor._1.asInstanceOf[DiscreteVar].intValue)
//  }
  
  // Factors
  //def factors(v:Variable): Iterable[FactorType] = { val result = new collection.mutable.LinkedHashSet[cc.factorie.Factor]; addFactors(v, result); result.asInstanceOf[Iterable[FactorType]] }
  final override def addFactors(v:Var, result:Set[model.Factor]): Unit = {
    val vClass = v.getClass
    if (nm1.runtimeClass.isAssignableFrom(vClass)) unroll1(v.asInstanceOf[N1]) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
    unroll(v) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  //* Override this method if you want to re-capture old unrollCascade functionality. */ 
  def unroll(v:Var): Iterable[Factor] = Nil
  def unroll1(v:N1): Iterable[FactorType] = new Factor(v)
  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = {
    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1)) {
      for (v <- vars; factor <- factors(v).asInstanceOf[Iterable[Factor1[DiscreteVar]]]) {
        if (limitedDiscreteValues1 eq null) limitedDiscreteValues1 = new la.SparseBinaryTensor1(factor._1.domain.dimensionSize)
        limitedDiscreteValues1.+=(factor._1.intValue)
      }
    }
  }
}

abstract class TupleTemplate1[N1<:Var:ClassTag] extends Template1[N1] with TupleFamily1[N1]
abstract class TupleTemplateWithStatistics1[N1<:Var:ClassTag] extends Template1[N1] with TupleFamilyWithStatistics1[N1]
abstract class TensorTemplate1[N1<:Var:ClassTag] extends Template1[N1] with TensorFamily1[N1]
abstract class TensorTemplateWithStatistics1[N1<:TensorVar:ClassTag] extends Template1[N1] with TensorFamilyWithStatistics1[N1]
abstract class DotTemplate1[N1<:Var:ClassTag] extends Template1[N1] with DotFamily1[N1]
abstract class DotTemplateWithStatistics1[N1<:TensorVar:ClassTag] extends Template1[N1] with DotFamilyWithStatistics1[N1]


/*
trait DiscreteFactorSettings1 extends Template {
  this: VectorTemplate { type Neighbor1Type <: DiscreteVar; type FactorType <: { def _1:DiscreteVariable } } =>
  val ndd1: DiscreteDomain = throw new Error // TODO nd1.asInstanceOf[DiscreteDomain[DiscreteVar]]
  val nds1 = ndd1.size
  // Managing settings iteration
  override def hasSettingsIterator: Boolean = this.isInstanceOf[Template { type FactorType <: { def _1:DiscreteVariable } }]
  override def forSettings(factor:FactorType)(f: =>Unit): Unit = factor._1 match {
    case v1: DiscreteVariable => {
      if (settingsSparsified && (sparseSettingsValues ne null))
        forIndex(sparseSettingsValues.length)(i => { v1.set(sparseSettingsValues(i))(null); f })
      else
        forIndex(nds1)(i => { v1.set(i)(null); f })
    }
    case _ => throw new RuntimeException("Settings of this factor are not iterable")
  }
  override def forSettingsOf(factor:FactorType, vs:Seq[Variable])(f: =>Unit): Unit = {
    require(vs.size == 1); require(factor._1 == vs.head)
    forSettings(factor)(f)
  }
  def forSettingsExcept(factor:FactorType, v:Variable)(f: =>Unit): Unit = require(factor._1 == v)
  private var settingsSparsified = false
  private var sparseSettingsValues: Array[Int] = null
  override def sparsifySettingsFor(vs:Iterable[Variable]): Unit = {
    val sparseInts = new HashSet[Int]
    // Only works for DiscreteVar
    vs.foreach(_ match { case v:DiscreteVar => sparseInts += v.intValue })
    sparseSettingsValues = sparseInts.toArray
    settingsSparsified = true
  }
}
*/

