/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/* package */ object factorie {

  def repeat[T](n:Int)(f: =>T) : Iterable[T] = for (i <- 0 until n force) yield f
  def repeat(n:Int)(f: =>Unit) : Unit = for (i <- 0 until n force) f
  def time(f: =>Unit) : Long = {
    val start = System.currentTimeMillis
    f
    start - System.currentTimeMillis
  }
  def printTime(f: =>Unit) : Unit = println(time(f)/1000.0+" seconds")

}
