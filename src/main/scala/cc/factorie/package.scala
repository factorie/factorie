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
  // TODO Consider renaming this "defaultRandom", 
  // anticipating the time when all these definitions make go in "package object factorie"?
  
  val defaultModel = new Model
  val defaultObjective = new Model(new InitializedTemplate(new Label01LossTemplate[CoordinatedLabelVariable[AnyRef]]()))

  // TODO Consider removing this now that we have separate, more specific samplers.
  // TODO Consider also removing SamplerSuite?
  val defaultSampler = new SamplerSuite
  //defaultSampler += new GenericSampler(new GeneratedVariableSampler)
  //defaultSampler += new GenericSampler(new GibbsSampler[Variable with IterableSettings](defaultModel))

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
  //implicit def singleStatIterable[S<:Stat](s:S): Iterable[S] = new Iterable[S] { def iterator = Iterator.single(s) }
  //implicit def seq2Vars[V<:Variable](seq:Seq[V]): Vars[V] = new SeqVars(seq) // Causing Scala 2.8.0 compiler to crash
  implicit def file2Source(f:java.io.File): scala.io.Source = scala.io.Source.fromFile(f)

  class InitializedTemplate(val template:Template) {
    assert(template.isInitialized == true)
  }
  implicit def template2initialized(t:Template): InitializedTemplate = new InitializedTemplate(t)
  implicit def template2initialized1[S1<:VectorVar](t:VectorStatistics1[S1])(implicit m:Manifest[S1]): InitializedTemplate = new InitializedTemplate(t.init)
  implicit def template2initialized2[S1<:VectorVar,S2<:VectorVar](t:VectorStatistics2[S1,S2])(implicit m1:Manifest[S1], m2:Manifest[S2]): InitializedTemplate = new InitializedTemplate(t.init)
  implicit def template2initialized3[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar](t:VectorStatistics3[S1,S2,S3])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]): InitializedTemplate = new InitializedTemplate(t.init)
  implicit def template2initialized4[S1<:VectorVar,S2<:VectorVar,S3<:VectorVar,S4<:VectorVar](t:VectorStatistics4[S1,S2,S3,S4])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]): InitializedTemplate = new InitializedTemplate(t.init)

  // TODO Consider this carefully.
  // Should this be "new BooleanObservation(b)" or "BooleanObservation(b)"
  // The later might inappropriate deduplicate Factors if used as arguments to Factor construction?
  // But the former is faster for all other cases.
  // I'm not convinced the later would actually happen naturally in practice.
  implicit def boolean2BooleanObservation(b:Boolean): BooleanObservation = BooleanObservation(b)
  // TODO Consider making implicit conversions for IntegerObservation and RealObservation also

}
