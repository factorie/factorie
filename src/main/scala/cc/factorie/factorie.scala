/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest

/* package */ object factorie {

  def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n force) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n force) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")
  
  /*
   * TODO I will add this trick when we have Scala 2.8. -akm
  implicit def template2initialized(t:Template): InitializedTemplate = new InitializedTemplate(t)
  implicit def template2initialized1[S1<:DiscreteValues](t:VectorStatistics1[S1])(implicit m:Manifest[S1]): InitializedTemplate = new InitializedVectorTemplate1[S1](t)
  implicit def template2initialized2[S1<:DiscreteValues,S2<:DiscreteValues](t:VectorStatistics2[S1,S2])(implicit m1:Manifest[S1], m2:Manifest[S2]): InitializedTemplate = new InitializedVectorTemplate2[S1,S2](t)
  implicit def template2initialized3[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues](t:VectorStatistics3[S1,S2,S3])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]): InitializedTemplate = new InitializedVectorTemplate3[S1,S2,S3](t)
  implicit def template2initialized4[S1<:DiscreteValues,S2<:DiscreteValues,S3<:DiscreteValues,S4<:DiscreteValues](t:VectorStatistics4[S1,S2,S3,S4])(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3], m4:Manifest[S4]): InitializedTemplate = new InitializedVectorTemplate4[S1,S2,S3,S4](t)
  */
  
}
