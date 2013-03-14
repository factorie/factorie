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

import cc.factorie.la._

trait Family {
  type FamilyType <: Family // like a self-type
  type FactorType <: Factor // TODO Remove this?
  type StatisticsType <: Any
  type NeighborType1
  @inline final def thisFamily: this.type = this
  def defaultFactorName = this.getClass.getName
  var factorName: String = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setFactorName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  final def %(n:String): this.type = setFactorName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  trait Factor extends cc.factorie.Factor {
    type StatisticsType = Family.this.StatisticsType
    def family: FamilyType = Family.this.asInstanceOf[FamilyType]
    def _1: NeighborType1 // TODO Consider getting rid of this.
    override def factorName = family.factorName
    override def equalityPrerequisite: AnyRef = Family.this
    override def valuesScore(tensor:Tensor): Double = Family.this.valuesScore(tensor)
    def statisticsScore(tensor:Tensor): Double = Family.this.statisticsScore(tensor)
  }
  def valuesScore(tensor:Tensor): Double = throw new Error("Not yet implemented.")
  def statisticsScore(tensor:Tensor): Double = throw new Error("Not yet implemented.")
}

trait FamilyWithNeighborDomains extends Family {
  def neighborDomains: Seq[Domain[_]]
}

trait FamilyWithNeighborClasses extends Family {
  def neighborClasses: Seq[Class[_]]
}

// TODO Is this used?  Should it be? Perhaps yes.  TensorFamily extends Statistics[Tensor] -akm
trait Statistics[A] extends Family {
  type FamilyType <: Statistics[A]
  type StatisticsType = A
}

/** A Family whose Factors have statistics that are Tensors. */
trait TensorFamily extends Family {
  type FamilyType <: TensorFamily
  type StatisticsType = Tensor
  //trait Statistics extends super.Statistics { def tensor: Tensor }
}

/** A Family whose Factors have scores calculated as a dot-product between sufficient statistics Tensors and the Family's weights Tensor. */
trait DotFamily extends TensorFamily {
  type FamilyType <: DotFamily
  def weights: Tensor
  @inline final override def statisticsScore(t:Tensor): Double = t dot weights
}

import cc.factorie.util._
class DotFamilyCubbie(val family:DotFamily) extends Cubbie {
  val weights = AnySlot[Tensor]("weights")
  weights := family.weights
}


