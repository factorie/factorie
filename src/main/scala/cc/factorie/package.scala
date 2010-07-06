/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc

package object factorie {

  def factorieVersionString = "0.9.0.SNAPSHOT"

  def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

  def forIndex(n:Int)(f:Int=>Any): Unit = { 
    var i = 0
    while (i < n) { f(i); i += 1 }
  }
  def mapIndex[@specialized A:ClassManifest](n:Int)(f:Int=>A): Array[A] = {
    val result = new Array[A](n)
    var i = 0
    while (i < n) { result(i) = f(i); i += 1 }
    result
  }

  implicit def traversableExtras[A](x:Traversable[A]) = new cc.factorie.util.TraversableExtras[A] { val t = x }
  implicit def stringExtras(x:String) = new cc.factorie.util.StringExtras { val s = x }
  implicit def singleFactorIterable[F<:Factor](f:F): Iterable[F] = new Iterable[F] { def iterator = Iterator.single(f) }
  implicit def singleStatIterable[S<:Stat](s:S): Iterable[S] = new Iterable[S] { def iterator = Iterator.single(s) }

  /** A container for "var-args"-like arbitrary number of Variables neighboring a Factor.
    @see Template1  */
  // TODO  Get rid of this and just use Seq?  But is Seq(v1,v2,v3) implemented as List?  Is this efficient?
  //type Vars[V<:Variable] = scala.collection.immutable.Seq[V] // Or should this be immutable.Set?  Should order matter when de-duplicating Factors?

  /*
   * TODO I will add this trick when we have Scala 2.8. -akm
  implicit def template2initialized(t:Template): InitializedTemplate = new InitializedTemplate(t)
  implicit def template2initialized1[S1<:DiscreteVars](t:VectorStatistics1[S1])(implicit m:Manifest[S1]): InitializedTemplate = new InitializedVectorTemplate1[S1](t)
  implicit def template2initialized2[S1<:DiscreteVars,S2<:DiscreteVars](t:VectorStatistics2[S1,S2])(implicit m1:Manifest[S1], m2:Manifest[S2]): InitializedTemplate = new InitializedVectorTemplate2[S1,S2](t)
  implicit def template2initialized3[S1<:DiscreteVars,S2<:DiscreteVars,S3<:DiscreteVars](t:VectorStatistics3[S1,S2,S3])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]): InitializedTemplate = new InitializedVectorTemplate3[S1,S2,S3](t)
  implicit def template2initialized4[S1<:DiscreteVars,S2<:DiscreteVars,S3<:DiscreteVars,S4<:DiscreteVars](t:VectorStatistics4[S1,S2,S3,S4])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]): InitializedTemplate = new InitializedVectorTemplate4[S1,S2,S3,S4](t)
  */


}
