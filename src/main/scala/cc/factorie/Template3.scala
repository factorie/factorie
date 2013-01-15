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

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.util.{Random,Sorting}
import scala.util.Random
import scala.math
import scala.util.Sorting
import cc.factorie.la._
import cc.factorie.util.Substitutions
import java.io._

abstract class Template3[N1<:Var,N2<:Var,N3<:Var](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends ModelWithFactorType with Family3[N1,N2,N3] with ModelAsTemplate {
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2, neighborClass3)
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass3)) { assert(ta.length == 1); ta.head.erasure } else null }
//  override def limitDiscreteValuesIteratorAsIn(variables:Iterable[DiscreteVar]): Unit = {
//    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1) &&
//        classOf[DiscreteVar].isAssignableFrom(neighborClass2) &&
//        classOf[DiscreteVar].isAssignableFrom(neighborClass3))
//      for (variable <- variables; factor <- factors(variable))
//        limitedDiscreteValues.+=((
//          factor._1.asInstanceOf[DiscreteVar].intValue,
//          factor._2.asInstanceOf[DiscreteVar].intValue,
//          factor._3.asInstanceOf[DiscreteVar].intValue))
//  }
  override def addFactors(v:Var, result:scala.collection.mutable.Set[cc.factorie.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass) && ((neighborDomain1 eq null) || (neighborDomain1 eq v.domain))) result ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && ((neighborDomain2 eq null) || (neighborDomain2 eq v.domain))) result ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass) && ((neighborDomain3 eq null) || (neighborDomain3 eq v.domain))) result ++= unroll3(v.asInstanceOf[N3])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) result ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) result ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) result ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    if (tryCascade) { val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) cascadeVariables.foreach(addFactors(_, result)) }
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")

}

abstract class TupleTemplate3[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest] extends Template3[N1,N2,N3] with TupleFamily3[N1,N2,N3]
abstract class TupleTemplateWithStatistics3[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest] extends Template3[N1,N2,N3] with TupleFamilyWithStatistics3[N1,N2,N3]
abstract class TensorTemplate3[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest] extends Template3[N1,N2,N3] with TensorFamily3[N1,N2,N3]
abstract class TensorTemplateWithStatistics3[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest] extends Template3[N1,N2,N3] with TensorFamilyWithStatistics3[N1,N2,N3]
abstract class DotTemplate3[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest] extends Template3[N1,N2,N3] with DotFamily3[N1,N2,N3]
abstract class DotTemplateWithStatistics3[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest] extends Template3[N1,N2,N3] with DotFamilyWithStatistics3[N1,N2,N3]


//abstract class TemplateWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
//  def statistics(value1:N1#Value, value2:N2#Value, value3:N3#Value): StatisticsType = Statistics(value1, value2, value3)
//}
//
//abstract class TemplateWithTensorStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with TensorStatistics3[N1#Value,N2#Value,N3#Value]  {
//  def statistics(value1:N1#Value, value2:N2#Value, value3:N3#Value): StatisticsType = Statistics(value1, value2, value3)
//}
//
//abstract class TemplateWithDotStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with FamilyWithDotStatistics3[N1,N2,N3]  {
//  type FamilyType <: TemplateWithDotStatistics3[N1,N2,N3]
//  //def statistics(value1:N1#Value, value2:N2#Value, value3:N3#Value): StatisticsType = Stat(value1, value2, value3)
//}
