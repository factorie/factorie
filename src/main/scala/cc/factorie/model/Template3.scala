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
import cc.factorie.variable.{DiscreteVar, TensorVar, Var, VectorVar}

import scala.reflect.ClassTag

abstract class Template3[N1<:Var,N2<:Var,N3<:Var](implicit nm1:ClassTag[N1], nm2:ClassTag[N2], nm3:ClassTag[N3]) extends Family3[N1,N2,N3] with Template {
  val neighborClass1 = nm1.runtimeClass
  val neighborClass2 = nm2.runtimeClass
  val neighborClass3 = nm3.runtimeClass
  def neighborClasses: Seq[Class[_]] = Seq(neighborClass1, neighborClass2, neighborClass3)
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
  final override def addFactors(v:Var, result:scala.collection.mutable.Set[model.Factor]): Unit = {
    if (neighborClass1.isAssignableFrom(v.getClass)) result ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass)) result ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass)) result ++= unroll3(v.asInstanceOf[N3])
    unroll(v) match { case fs:IterableSingleFactor[Factor] => result += fs.factor; case Nil => {}; case fs => result ++= fs }
  }
  //* Override this method if you want to re-capture old unrollCascade functionality. */ 
  def unroll(v:Var): Iterable[Factor] = Nil
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]

  def limitDiscreteValuesAsIn(vars:Iterable[DiscreteVar]): Unit = 
    for (v <- vars; factor <- factors(v)) factor.asInstanceOf[model.Factor] match {
      case factor:Factor3[VectorVar @unchecked,VectorVar @unchecked,VectorVar @unchecked] =>
        (classOf[DiscreteVar].isAssignableFrom(neighborClass1), classOf[DiscreteVar].isAssignableFrom(neighborClass2), classOf[DiscreteVar].isAssignableFrom(neighborClass3)) match {
//        (factor._1.isInstanceOf[DiscreteVar], factor._2.isInstanceOf[DiscreteVar], factor._3.isInstanceOf[DiscreteVar]) match {  // TODO No need to check types every time. -akm
        case (true, true, true) =>
          getLimitedDiscreteValues123(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue, factor._2.asInstanceOf[DiscreteVar].intValue, factor._3.asInstanceOf[DiscreteVar].intValue)
          getLimitedDiscreteValues12(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue, factor._2.asInstanceOf[DiscreteVar].intValue)
          getLimitedDiscreteValues1(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue)
        case (true, true, false) =>
          getLimitedDiscreteValues12(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue, factor._2.asInstanceOf[DiscreteVar].intValue)
          getLimitedDiscreteValues1(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue)
        case (true, false, false) =>
          getLimitedDiscreteValues1(factor).+=(factor._1.asInstanceOf[DiscreteVar].intValue)
        case (false, false, false) => {}
        case _ => throw new Error("Combination of DiscreteVar not yet implemented.")
      } 
    }
}

abstract class TupleTemplate3[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag] extends Template3[N1,N2,N3] with TupleFamily3[N1,N2,N3]
abstract class TupleTemplateWithStatistics3[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag] extends Template3[N1,N2,N3] with TupleFamilyWithStatistics3[N1,N2,N3]
abstract class TensorTemplate3[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag] extends Template3[N1,N2,N3] with TensorFamily3[N1,N2,N3]
abstract class TensorTemplateWithStatistics3[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag,N3<:TensorVar:ClassTag] extends Template3[N1,N2,N3] with TensorFamilyWithStatistics3[N1,N2,N3]
abstract class DotTemplate3[N1<:Var:ClassTag,N2<:Var:ClassTag,N3<:Var:ClassTag] extends Template3[N1,N2,N3] with DotFamily3[N1,N2,N3]
abstract class DotTemplateWithStatistics3[N1<:TensorVar:ClassTag,N2<:TensorVar:ClassTag,N3<:TensorVar:ClassTag] extends Template3[N1,N2,N3] with DotFamilyWithStatistics3[N1,N2,N3]
