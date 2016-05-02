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

import cc.factorie.model
import cc.factorie.variable.{DiscreteVar, TensorVar, Var}

import scala.reflect.ClassTag

abstract class Template4[N1<:Var,N2<:Var,N3<:Var,N4<:Var](implicit nm1:ClassTag[N1], nm2:ClassTag[N2], nm3:ClassTag[N3], nm4:ClassTag[N4]) extends Family4[N1,N2,N3,N4] with Template {
  val neighborClass1 = nm1.runtimeClass
  val neighborClass2 = nm2.runtimeClass
  val neighborClass3 = nm3.runtimeClass
  val neighborClass4 = nm4.runtimeClass
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2, neighborClass3, neighborClass4)

  final override def addFactors(v:Var, result:scala.collection.mutable.Set[model.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass)) result ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass)) result ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass)) result ++= unroll3(v.asInstanceOf[N3])
    if (neighborClass4.isAssignableFrom(v.getClass)) result ++= unroll4(v.asInstanceOf[N4])
    unroll(v) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  //* Override this method if you want to re-capture old unrollCascade functionality. */ 
  def unroll(v:Var): Iterable[Factor] = Nil
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll4(v:N4): Iterable[FactorType]
  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = throw new Error("Not yet implemented.") 
}

abstract class TupleTemplate4[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag,N4<:Var:ClassTag] extends Template4[N1,N2,N3,N4] with TupleFamily4[N1,N2,N3,N4]
abstract class TupleTemplateWithStatistics4[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag,N4<:Var:ClassTag] extends Template4[N1,N2,N3,N4] with TupleFamilyWithStatistics4[N1,N2,N3,N4]
abstract class TensorTemplate4[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag,N4<:Var:ClassTag] extends Template4[N1,N2,N3,N4] with TensorFamily4[N1,N2,N3,N4]
abstract class TensorTemplateWithStatistics4[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag,N3<:TensorVar:ClassTag,N4<:TensorVar:ClassTag] extends Template4[N1,N2,N3,N4] with TensorFamilyWithStatistics4[N1,N2,N3,N4]
abstract class DotTemplate4[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag,N4<:Var:ClassTag] extends Template4[N1,N2,N3,N4] with DotFamily4[N1,N2,N3,N4]
abstract class DotTemplateWithStatistics4[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag,N3<:TensorVar:ClassTag,N4<:TensorVar:ClassTag] extends Template4[N1,N2,N3,N4] with DotFamilyWithStatistics4[N1,N2,N3,N4]
