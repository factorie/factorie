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

abstract class Template4[N1<:Var,N2<:Var,N3<:Var,N4<:Var](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends ModelWithFactorType with Family4[N1,N2,N3,N4] with ModelAsTemplate {
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  val neighborClass4 = nm4.erasure
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2, neighborClass3, neighborClass4)
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass3)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc4a = { val ta = nm4.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass4)) { assert(ta.length == 1); ta.head.erasure } else null }

  override def addFactors(v:Var, ret:scala.collection.mutable.Set[cc.factorie.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass) && ((neighborDomain1 eq null) || (neighborDomain1 eq v.domain))) ret ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && ((neighborDomain2 eq null) || (neighborDomain2 eq v.domain))) ret ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass) && ((neighborDomain3 eq null) || (neighborDomain3 eq v.domain))) ret ++= unroll3(v.asInstanceOf[N3])
    if (neighborClass4.isAssignableFrom(v.getClass) && ((neighborDomain4 eq null) || (neighborDomain4 eq v.domain))) ret ++= unroll4(v.asInstanceOf[N4])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    if ((nc4a ne null) && nc4a.isAssignableFrom(v.getClass)) ret ++= unroll4s(v.asInstanceOf[N4#ContainedVariableType])
    if (tryCascade) { val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) cascadeVariables.foreach(addFactors(_, ret)) }
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll4(v:N4): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")
  def unroll4s(v:N4#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll4s.")

}

abstract class TupleTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TupleFamily4[N1,N2,N3,N4]
abstract class TupleTemplateWithStatistics4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TupleFamilyWithStatistics4[N1,N2,N3,N4]
abstract class TensorTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with TensorFamily4[N1,N2,N3,N4]
abstract class TensorTemplateWithStatistics4[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest,N4<:TensorVar:Manifest] extends Template4[N1,N2,N3,N4] with TensorFamilyWithStatistics4[N1,N2,N3,N4]
abstract class DotTemplate4[N1<:Var:Manifest,N2<:Var:Manifest,N3<:Var:Manifest,N4<:Var:Manifest] extends Template4[N1,N2,N3,N4] with DotFamily4[N1,N2,N3,N4]
abstract class DotTemplateWithStatistics4[N1<:TensorVar:Manifest,N2<:TensorVar:Manifest,N3<:TensorVar:Manifest,N4<:TensorVar:Manifest] extends Template4[N1,N2,N3,N4] with DotFamilyWithStatistics4[N1,N2,N3,N4]


//abstract class TemplateWithStatistics4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with Statistics4[N1#Value,N2#Value,N3#Value,N4#Value] {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
//}
//
//abstract class TemplateWithTensorStatistics4[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar,N4<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with TensorStatistics4[N1#Value,N2#Value,N3#Value,N4#Value]  {
//  def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Statistics(v1, v2, v3, v4)
//}
//
//abstract class TemplateWithDotStatistics4[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar,N4<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template4[N1,N2,N3,N4] with FamilyWithDotStatistics4[N1,N2,N3,N4] {
//  type FamilyType <: TemplateWithDotStatistics4[N1,N2,N3,N4]
//  //def statistics(v1:N1#Value, v2:N2#Value, v3:N3#Value, v4:N4#Value) = Stat(v1, v2, v3, v4)
//}
//
