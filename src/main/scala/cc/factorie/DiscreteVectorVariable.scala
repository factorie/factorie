///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie
//import cc.factorie.la._
//import scala.util.Random
//import scala.collection.mutable.ArrayBuffer
//
//trait DiscreteVectorVar extends VectorVar with VarAndValueType[DiscreteVectorVar,DiscreteVectorValue] {
//  def domain: DiscreteVectorDomain
//  def contains(dimension:Int): Boolean = vector.apply(dimension) != 0.0
//  def activeDomain = vector.activeDomain
//}
//
///** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
//abstract class DiscreteVectorVariable extends VectorVariable with DiscreteVectorVar {
//  thisVariable =>
//  _set(new SparseVector(domain.dimensionSize) with DiscreteVectorValue { def domain = thisVariable.domain })
//}
//
///** A sparse binary vector with length determined by a DiscreteDomain */
//trait SparseBinaryDiscreteVectorVar extends DiscreteVectorVar with VarAndValueType[SparseBinaryDiscreteVectorVar,SparseBinaryVector with DiscreteVectorValue] {
//  //type ValueType <: cc.factorie.la.SparseBinaryVector with DiscreteVectorValue
//  def length = domain.dimensionSize //allocSize // TODO Remove this?
//  //def apply(i:Int) = vector.apply(i)
//  def zero(): Unit = value.zero()
//  def +=(i:Int): Unit = { if (frozen) throw new Error("Cannot append to frozen SparseBinaryDiscreteVectorVar."); value.include(i) }
//  //def ++=(is:Iterable[Int]): Unit = is.foreach(i => vector.+=(i)) // Conflicts with ++=(Iterable[T])
//  var frozen = false
//  def freeze() = frozen = true
//  override def isConstant = frozen
//}
//
//abstract class SparseBinaryDiscreteVectorVariable extends VectorVariable with SparseBinaryDiscreteVectorVar {
//  thisVariable =>
//  _set(new cc.factorie.la.SparseBinaryVector(-1) with DiscreteVectorValue {
//    def domain = thisVariable.domain
//    override def length = domain.dimensionSize
//  })
//}
//
//trait SparseIndexedDiscreteVectorVar extends DiscreteVectorVar with VarAndValueType[SparseIndexedDiscreteVectorVar,SparseIndexedVector with DiscreteVectorValue] {
//  def length = domain.dimensionSize
//  override def increment(index:Int, incr:Double): Unit = {
//    if (frozen) throw new Error("Cannot append to frozen SparseIndexedDiscreteVectorVar.")
//    value.increment(index, incr)
//  }
//  var frozen = false
//  def freeze() = frozen = true
//  override def isConstant = frozen
//}
//
//abstract class SparseIndexedDiscreteVectorVariable extends VectorVariable with SparseIndexedDiscreteVectorVar {
//  thisVariable =>
//  _set(new cc.factorie.la.SparseIndexedVector(-1) with DiscreteVectorValue {
//    def domain = thisVariable.domain
//    override def length = domain.dimensionSize
//  })
//}
//
