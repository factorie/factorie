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

abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Family2[N1,N2] with Template
{
  val neighborClass1 = nm1.erasure
  val neighborClass2 = nm2.erasure
  val nc1a = { val ta = nm1.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }
  val nc2a = { val ta = nm2.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass2)) { assert(ta.length == 1); ta.head.erasure } else null }

  override def factors(v: Variable): Iterable[FactorType] = {
    var ret = new ListBuffer[FactorType]
    if (neighborClass1.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain1 eq v.domain) || (_neighborDomain1 eq null))) ret ++= unroll1(v.asInstanceOf[N1])
    if (neighborClass2.isAssignableFrom(v.getClass) && (!matchNeighborDomains || (_neighborDomain2 eq v.domain) || (_neighborDomain2 eq null))) ret ++= unroll2(v.asInstanceOf[N2])
    if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unroll1s(v.asInstanceOf[N1#ContainedVariableType])
    if ((nc2a ne null) && nc2a.isAssignableFrom(v.getClass)) ret ++= unroll2s(v.asInstanceOf[N2#ContainedVariableType])
    val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) ret ++= cascadeVariables.flatMap(factors(_))
    ret
  }
  def unroll1(v:N1): Iterable[FactorType]
  def unroll2(v:N2): Iterable[FactorType]
  def unroll1s(v:N1#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll1s.")
  def unroll2s(v:N2#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unroll2s.")

}


abstract class TemplateWithStatistics2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with Statistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

abstract class TemplateWithVectorStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with VectorStatistics2[N1#Value,N2#Value] {
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

abstract class TemplateWithDotStatistics2[N1<:DiscreteVectorVar,N2<:DiscreteVectorVar](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2] with DotStatistics2[N1#Value,N2#Value] {
  type FamilyType <: TemplateWithDotStatistics2[N1,N2]
  def statistics(values:Values): StatisticsType = Stat(values._1, values._2, values.inner.map(_.statistics))
}

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


