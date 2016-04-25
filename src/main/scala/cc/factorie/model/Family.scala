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

import cc.factorie.la._
import cc.factorie.variable.Domain

/** A Family contains a Factor definition for Factors that share the same scoring function and statistics functions.
    @author Andrew McCallum */
trait Family {
  type FamilyType <: Family // like a self-type
  //type FactorType <: Factor // TODO Remove this?
  type StatisticsType <: Any
  type NeighborType1
  @inline final def thisFamily: this.type = this
  def defaultFactorName = this.getClass.getName
  var factorName: String = defaultFactorName
  /** Assign this Template a name which will be used later when its factors are printed. */
  def setFactorName(n:String): this.type = { factorName = n; this }
  /** Assign this Template a name which will be used later when its factors are printed. */
  final def %(n:String): this.type = setFactorName(n) // because % is the comment character in shell languages such as /bin/sh and Makefiles.
  trait Factor extends cc.factorie.model.Factor {
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

/** A Family for which the Domains of the neighboring variables are known. 
    @author Andrew McCallum */
trait FamilyWithNeighborDomains extends Family {
  def neighborDomains: Seq[Domain]
}

/** A Family for which the classes of the neighboring variables are known.
    @author Andrew McCallum */
trait FamilyWithNeighborClasses extends Family {
  def neighborClasses: Seq[Class[_]]
}

// TODO Is this used?  Should it be? Perhaps yes.  TensorFamily extends Statistics[Tensor] -akm
trait Statistics[A] extends Family {
  type FamilyType <: Statistics[A]
  type StatisticsType = A
}

/** A Family whose Factors have statistics that are WeightsMap. 
    @author Andrew McCallum */
trait TensorFamily extends Family {
  type FamilyType <: TensorFamily
  type StatisticsType = Tensor
  //trait Statistics extends super.Statistics { def tensor: Tensor }
}

/** A Family whose Factors have scores calculated as a dot-product between sufficient statistics WeightsMap and the Family's weightsSet Tensor. 
    @author Andrew McCallum */
trait DotFamily extends TensorFamily {
  type FamilyType <: DotFamily
  /** This can only be set given a Parameters. Code will look like
    * val weights = model.Weights(new DenseTensor1(10))
    * or something of the sort */
  def weights: Weights
  @inline final override def statisticsScore(t:Tensor): Double = weights.value dot t
}



