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
import cc.factorie.util.CubbieConversions
import scala.language.implicitConversions
import scala.reflect.ClassTag

package object factorie extends CubbieConversions {

  // Common cases for DotFamily weightsSet
//  type Tensor = cc.factorie.la.Tensor
//  type Tensor1 = cc.factorie.la.Tensor1
//  type Tensor2 = cc.factorie.la.Tensor2
//  type Tensor3 = cc.factorie.la.Tensor3
//  type Tensor4 = cc.factorie.la.Tensor4
//  type DenseTensor1 = cc.factorie.la.DenseTensor1
//  type DenseTensor2 = cc.factorie.la.DenseTensor2
//  type DenseTensor3 = cc.factorie.la.DenseTensor3
//  type DenseTensor4 = cc.factorie.la.DenseTensor4
//  type SparseTensor1 = cc.factorie.la.SparseTensor1
//  type DenseLayeredTensor2 = cc.factorie.la.DenseLayeredTensor2
//  type Dense2LayeredTensor3 = cc.factorie.la.Dense2LayeredTensor3
  
  // TODO Consider removing these.  Yes. -akm
  //type DiscreteTensorValue = cc.factorie.la.Tensor
  //type CategoricalTensorValue = cc.factorie.la.Tensor

  // TODO Should we have this?  Is there a more Java-standard way to provide this?
  // TODO If we keep it, find a way to automatically maintain this string value
  //def factorieVersionString = "0.9.0.SNAPSHOT"

  trait ThisType[+This<:AnyRef] {
    this: This =>
    type ThisType = This
  }

  //type BooleanValue = CategoricalValue[Boolean]

  //var randomSeed = 0
  //implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  implicit var random = new Random(0)
  def setRandomSeed(seed: Long): Unit = {
    random = new Random(seed)
  }
  
  // No, Model might expect a Seq[Label].  It isn't the same to just pass a single Seq(label)?? 
  //  implicit def modelVariables2Variable(model:Model[Iterable[Variable]]): Model2[Variable] = new Model2[Variable] {
  //    def factors(variable:Variable): Iterable[Factor] = model.factors(Seq(variable))
  //  }
//  implicit def modelElement2Iterable[C](model:ModelWithContext[C]): ModelWithContext[Iterable[C]] = {
//    if (model eq null) return null
//    new Element2IterableModel[C](model)
//  }
//  new Model[Iterable[V]] {
//    def factors(variables:Iterable[V]): Iterable[Factor] = {
//      val result = new collection.mutable.LinkedHashSet[Factor] // Because there might be duplicates, even of Variables
//      variables.foreach(v => model.addFactors(v, result))
//      result
//    }
//  }
//  implicit def modelVariable2DiffList(model:Model[Variable]): Model[DiffList] = {
//    if (model eq null) return null
//    new Variable2DiffListModel(model)
//  }
//    new Model[DiffList] {
//    def factors(dl:DiffList): Iterable[Factor] = {
//      val result = new collection.mutable.LinkedHashSet[Factor] // Because there might be duplicates, even of Variables in the DiffList
//      dl.foreach(d => if (d.variable ne null) model.addFactors(d.variable, result))
//      result
//    }
//  }
//  implicit def modelVariables2DiffList(model:Model[Iterable[Variable]]): Model[DiffList] = new Model[DiffList] {
//    def factors(dl:DiffList): Iterable[Factor] = model.factors(dl.variables)
//  }
//  implicit def modelDiffList2Variables(model:Model[DiffList]): Model[Iterable[Variable]] = new Model[Iterable[Variable]] {
//    def factors(variables:Iterable[Variable]): Iterable[Factor] = model.factors(new DiffList ++= variables.map(NoopDiff(_)))
//  }
//  
//  implicit def iterableExampleDiffList2Variable(examples:Iterable[Example[Model[DiffList]]]): Iterable[Example[Model[Variable]]] = 
//    examples.map(e =>
//      new Example[Model[Variable]] {
//        def accumulateExampleInto(model:Model[Variable], gradient:WeightsTensorAccumulator, value:DoubleAccumulator, margin:DoubleAccumulator): Unit =
//          e.accumulateExampleInto(model, gradient, value, margin)
//      })

  // TODO Consider removing this now that we have separate, more specific samplers.
  // TODO Consider also removing SamplerSuite?
  // Yes to both.
//  @deprecated("Will be removed.")
//  val defaultSampler = new SamplerSuite

  type Logging = cc.factorie.util.Logging
  type FastLogging = cc.factorie.util.FastLogging
  type GlobalLogging = cc.factorie.util.GlobalLogging
  type Attr = cc.factorie.util.Attr
  type Cubbie = cc.factorie.util.Cubbie

  //def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

  // TODO Remove these.  They aren't actually very efficient
  // Faster alternatives to for (i <- 0 until n)
  def forIndex(n:Int)(f:Int=>Any): Unit = { 
    var i = 0
    while (i < n) { f(i); i += 1 }
  }
  def forReverseIndex(n:Int)(f:Int=>Any): Unit = {
    var i = n - 1
    while (i >= 0) { f(i); i -= 1 }
  }
  def forallIndex(n:Int)(f:Int=>Boolean): Boolean = { 
    var i = 0
    while (i < n) { if (!f(i)) return false; i += 1 }
    true
  }
  def mapIndex[@specialized A:ClassTag](n:Int)(f:Int=>A): Array[A] = {
    val result = new Array[A](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }

  implicit class AnyExtras[T](val a: T) extends AnyVal {
    def cast[U](implicit m: ClassTag[U]): Option[U] = if (m >:> ClassTag(a.getClass)) Some(a.asInstanceOf[U]) else None
    def toNotNull: Option[T] = if (a != null) Some(a) else None
  }

  implicit def traversableExtras[A](t: Traversable[A]) = new cc.factorie.util.TraversableExtras[A](t)
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras(x)
  implicit def regexToSegmenter(r:scala.util.matching.Regex) = new cc.factorie.app.strings.RegexSegmenter(r)
  implicit def singleFactorIterable[F<:Factor](f:F): Iterable[F] = new IterableSingleFactor(f)
  // TODO Remove this
  //implicit def file2Source(f:java.io.File): scala.io.Source = scala.io.Source.fromFile(f)

  //implicit def boolean2BooleanValue(b:Boolean): BooleanValue = if (b) BooleanDomain.trueValue else BooleanDomain.falseValue
  // TODO Consider making implicit conversions for IntegerVariable and RealVariable also
 
  def assertStringEquals(expr:Any, str:String) = require(expr.toString == str, "The string representation '" + expr.toString + "' does not match the expected value: '" + str +"'")
  
}
