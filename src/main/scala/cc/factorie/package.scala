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

  // TODO: does this need to be in the package object?
  trait ThisType[+This<:AnyRef] {
    this: This =>
    type ThisType = This
  }


  //var randomSeed = 0
  //implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  var random = new Random(0)
  def setRandomSeed(seed: Long): Unit = {
    random = new Random(seed)
  }

  // TODO: do these need to be in the package object? They don't look like an essential part
  // of the external API and can easily be imported from util
  type Logging = cc.factorie.util.Logging
  type FastLogging = cc.factorie.util.FastLogging
  type GlobalLogging = cc.factorie.util.GlobalLogging
  type Attr = cc.factorie.util.Attr
  type Cubbie = cc.factorie.util.Cubbie

  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

  implicit class AnyExtras[T](val a: T) extends AnyVal {
    def cast[U](implicit m: ClassTag[U]): Option[U] = if (m >:> ClassTag(a.getClass)) Some(a.asInstanceOf[U]) else None
    def toNotNull: Option[T] = if (a != null) Some(a) else None
  }

  implicit def traversableExtras[A](t: Traversable[A]) = new cc.factorie.util.TraversableExtras[A](t)
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras(x)
  implicit def singleFactorIterable[F<:Factor](f:F): Iterable[F] = new IterableSingleFactor(f)

  def assertStringEquals(expr:Any, str:String) = require(expr.toString == str, "The string representation '" + expr.toString + "' does not match the expected value: '" + str +"'")
}
