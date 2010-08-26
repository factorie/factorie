/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.la
import cc.factorie._

/** An object able to translate a sequence of Vector objects to a single, flat Array[Double],
    and back again.  Note that the order of the sequence of Vectors matters tremendously. 
    This class is used, for example, to map the weight vectors from a sequence of Templates
    into an Array[Double], as required for Optimizable.  Note also that the Array[Double]
    is not the same as the outer product of the vectors; it is a concatenation.
    @see Optimizable
    @see OptimizableTemplates
    @author Andrew McCallum */
class ArrayFromVectors(theVectors:Seq[Vector]) {
  //def this(theTemplates:Iterable[DotTemplate]) = this(theTemplates.map(_.weights))
  val vectors = theVectors.toArray // Save a copy, in case theVectors would be changed
  //templates.foreach(_.freeze)

  val vectorsArraySize = vectors.foldLeft(0)(_+_.activeDomain.size)
  def getVectorsInArray(a:Array[Double] = null): Array[Double] = {
    val result = if (a != null) a else new Array[Double](vectorsArraySize)
    var i = 0
    vectors.foreach(v => for (j <- v.activeDomain) { result(i) = v(j); i += 1 })
    result
  }
  def setVectorsFromArray(a:Array[Double]): Unit = {
    var i = 0
    vectors.foreach(v => for (j <- v.activeDomain) { v(j) = a(i); i += 1 })
  }
  def incrArrayWithVectors(a:Array[Double]): Unit = {
    var i = 0
    vectors.foreach(v => for (j <- v.activeDomain) { a(i) += v(j); i += 1 })
  }
  def incrArrayWithVectors(a:Array[Double], factor:Double): Unit = {
    var i = 0
    vectors.foreach(v => for (j <- v.activeDomain) { a(i) += v(j) * factor; i += 1 })
  }
  private lazy val vectorsSizes = vectors.map(_.activeDomain.size).toArray
  private lazy val vectorsActiveDomain = vectors.map(_.activeDomain.toArray)
  def vectorValueAtArrayIndex(index:Int): Double = {
    var i = index
    var j = 0
    while (i > vectorsSizes(j)) { i += vectorsSizes(j); j += 1 }
    vectors(j)(vectorsActiveDomain(j)(i))
  }
  def vectorValueAtArrayIndex_=(index:Int, d:Double): Unit = {
    var i = index
    var j = 0
    while (i > vectorsSizes(j)) { i += vectorsSizes(j); j += 1 }
    vectors(j)(vectorsActiveDomain(j)(i)) = d
  }
}

