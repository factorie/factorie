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

package object factorie {

  // TODO Should we have this?  Is there a more Java-standard way to provide this?
  // TODO If we keep it, find a way to automatically maintain this string value
  //def factorieVersionString = "0.9.0.SNAPSHOT"

  var randomSeed = 0
  implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  
  val defaultModel = new Model
  val defaultObjective = new Model(new ZeroOneLossTemplate[CoordinatedLabelVariable[AnyRef]]())

  // TODO Consider removing this now that we have separate, more specific samplers.
  // TODO Consider also removing SamplerSuite?
  val defaultSampler = new SamplerSuite

  type Logging = cc.factorie.util.Logging
  type FastLogging = cc.factorie.util.FastLogging
  type GlobalLogging = cc.factorie.util.GlobalLogging

  //def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

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
  def mapIndex[@specialized A:ClassManifest](n:Int)(f:Int=>A): Array[A] = {
    val result = new Array[A](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }

  implicit def traversableExtras[A](x:Traversable[A]) = new cc.factorie.util.TraversableExtras[A] { val t = x }
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras { val s = x }
  implicit def regexToSegmenter(r:scala.util.matching.Regex) = new cc.factorie.app.strings.RegexSegmenter(r)
  implicit def singleFactorIterable[F<:Factor](f:F): Iterable[F] = new Iterable[F] { def iterator = Iterator.single(f) }
  implicit def file2Source(f:java.io.File): scala.io.Source = scala.io.Source.fromFile(f)

  implicit def boolean2BooleanValue(b:Boolean): BooleanValue = if (b) BooleanDomain.trueValue else BooleanDomain.falseValue
  // TODO Consider making implicit conversions for IntegerVariable and RealVariable also

}
