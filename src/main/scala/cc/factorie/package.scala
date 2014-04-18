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

package cc
import scala.util.Random
import cc.factorie.util._
import scala.language.implicitConversions
import scala.reflect.ClassTag
import cc.factorie.model.{IterableSingleFactor, Factor}
import cc.factorie.variable.TensorVar
import scala.reflect.runtime.universe._

package object factorie extends CubbieConversions {
  var random = new Random(0)
  def setRandomSeed(seed: Long): Unit = {
    random = new Random(seed)
  }
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f

  implicit class AnyExtras[T](val a: T) extends AnyVal {
    def cast[U](implicit u : TypeTag[U], t:TypeTag[T]): Option[U] = if (typeOf[U] <:< typeOf[T]) Some(a.asInstanceOf[U]) else None
    def toNotNull: Option[T] = Option(a)
  }

  implicit def traversableExtras[A](t: Traversable[A]) = new cc.factorie.util.TraversableExtras[A](t)
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras(x)
  implicit def singleFactorIterable[F<:Factor](f:F): Iterable[F] = new IterableSingleFactor(f)

  def assertStringEquals(expr:Any, str:String) = org.junit.Assert.assertTrue("The string representation '" + expr.toString + "' does not match the expected value: '" + str +"'", expr.toString == str)
  def assertMinimalAccuracy(got:Double, goal:Double): Unit = assert(got >= goal, s"Accuracy ${got} is less than expected ${goal}.")

  type DenseTensor1 = cc.factorie.la.DenseTensor1
  type DenseTensor2 = cc.factorie.la.DenseTensor2
  type DenseTensor3 = cc.factorie.la.DenseTensor3
  type DenseTensor4 = cc.factorie.la.DenseTensor4

  type Tensor1 = cc.factorie.la.Tensor1
  type Tensor2 = cc.factorie.la.Tensor2
  type Tensor3 = cc.factorie.la.Tensor3
  type Tensor4 = cc.factorie.la.Tensor4

  type Var = variable.Var

  type Assignment = variable.Assignment
  type HashMapAssignment = variable.HashMapAssignment

  type BooleanVariable = variable.BooleanVariable

  type Diff = variable.Diff
  type DiffList = variable.DiffList

  type DiscreteDomain = variable.DiscreteDomain

  type DiscreteVariable = variable.DiscreteVariable

  type HashFeatureVectorVariable = variable.HashFeatureVectorVariable

  type RealVariable = variable.RealVariable

  type Cubbie = util.Cubbie

  type GlobalLogging = util.GlobalLogging

  type Example = optimize.Example

  type Factor = model.Factor

  type Model = model.Model
  type CombinedModel = model.CombinedModel
  type TemplateModel = model.TemplateModel
  type Parameters = model.Parameters

  type GibbsSampler = infer.GibbsSampler
}
