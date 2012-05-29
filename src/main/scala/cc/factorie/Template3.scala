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

abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Family3[N1,N2,N3] with Template {
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val neighborClass3 = nm3.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc3a = { val ta = nm3.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass3)) { assert(ta.length == 1); ta.head.erasure } else null }

  override def limitDiscreteValuesIteratorAsIn(variables:Iterable[DiscreteVar]): Unit = {
    if (classOf[DiscreteVar].isAssignableFrom(neighborClass1) &&
        classOf[DiscreteVar].isAssignableFrom(neighborClass2) &&
        classOf[DiscreteVar].isAssignableFrom(neighborClass3))
      for (variable <- variables; factor <- factors(variable))
        limitedDiscreteValues.+=((
          factor._1.asInstanceOf[DiscreteVar].intValue,
          factor._2.asInstanceOf[DiscreteVar].intValue,
          factor._3.asInstanceOf[DiscreteVar].intValue))
  }
  
  def factorsWithDuplicates(v: Variable): Iterable[FactorType] = {
    val ret = new ArrayBuffer[FactorType]
    if (neighborClass1.isAssignableFrom(v.getClass) && ((neighborDomain1 eq null) || (neighborDomain1 eq v.domain))) ret ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && ((neighborDomain2 eq null) || (neighborDomain2 eq v.domain))) ret ++= unroll2(v.asInstanceOf[N2])
    if (neighborClass3.isAssignableFrom(v.getClass) && ((neighborDomain3 eq null) || (neighborDomain3 eq v.domain))) ret ++= unroll3(v.asInstanceOf[N3])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    if ((nc3a ne null) && nc3a.isAssignableFrom(v.getClass)) ret ++= unroll3s(v.asInstanceOf[N3#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factorsWithDuplicates(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll3(v:N3): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")
  def unroll3s(v:N3#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll3s.")

}


abstract class TemplateWithStatistics3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with Statistics3[N1#Value,N2#Value,N3#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values._3)
}

abstract class TemplateWithTensorStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with TensorStatistics3[N1#Value,N2#Value,N3#Value]  {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values._3)
}

abstract class TemplateWithDotStatistics3[N1<:DiscreteTensorVar,N2<:DiscreteTensorVar,N3<:DiscreteTensorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3] with DotFamily with DotStatistics3[N1#Value,N2#Value,N3#Value]  {
  type FamilyType <: TemplateWithDotStatistics3[N1,N2,N3]
  @deprecated("Use instead weights(i,j,k)") def weight(index0:Int, index1:Int, index2:Int): Double = weights(
    index0 * statisticsDomains._2.dimensionDomain.size  * statisticsDomains._3.dimensionDomain.size +
          index1 * statisticsDomains._3.dimensionDomain.size +
          index2)
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values._3)
}
