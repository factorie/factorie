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
  def getVectorsInArray(a:Array[Double]) = {
    var i = 0
    //vectors.foreach(v => for (j <- v.activeDomain) { a(i) = v(j); i += 1 })
    vectors.foreach(v => v.forActiveDomain(j => { a(i) = v(j); i += 1 }))
    a
  }
  def setVectorsFromArray(a:Array[Double]): Unit = {
    var i = 0
    //vectors.foreach(v => for (j <- v.activeDomain) { v(j) = a(i); i += 1 })
    vectors.foreach(v => v.forActiveDomain(j => { v(j) = a(i); i += 1 }))
  }
  def incrArrayWithVectors(a:Array[Double]): Unit = {
    var i = 0
    //vectors.foreach(v => for (j <- v.activeDomain) { a(i) += v(j); i += 1 })
    vectors.foreach(v => v.forActiveDomain(j => { a(i) += v(j); i += 1 }))
  }
  def incrArrayWithVectors(a:Array[Double], factor:Double): Unit = {
    var i = 0
    //vectors.foreach(v => for (j <- v.activeDomain) { a(i) += v(j) * factor; i += 1 })
    vectors.foreach(v => v.forActiveDomain(j => { a(i) += v(j) * factor; i += 1 }))
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

