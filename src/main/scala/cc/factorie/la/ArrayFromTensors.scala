/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
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

@deprecated("Not used anywhere", "Before 10/06/15")
class ArrayFromTensors(theTensors:Seq[Tensor]) {
  //def this(theTemplates:Iterable[DotTemplate]) = this(theTemplates.map(_.weightsSet))
  val tensors = theTensors.toIndexedSeq // Save a copy, in case theVectors would be changed
  //templates.foreach(_.freeze)

  val tensorsArraySize = tensors.foldLeft(0)(_+_.activeDomain.size)
  def getTensorsInArray(a:Array[Double]) = {
    var i = 0
    tensors.foreach(v => v.foreachActiveElement((j,d) => { a(i) = v(j); i += 1 }))
    a
  }
  def setTensorsFromArray(a:Array[Double]): Unit = {
    var i = 0
    tensors.foreach(v => v.foreachActiveElement((j,d) => { v(j) = a(i); i += 1 }))
  }
  def incrArrayWithTensors(a:Array[Double]): Unit = {
    var i = 0
    tensors.foreach(v => v.foreachActiveElement((j,d) => { a(i) += v(j); i += 1 }))
  }
  def incrArrayWithTensors(a:Array[Double], factor:Double): Unit = {
    var i = 0
    tensors.foreach(v => v.foreachActiveElement((j,d) => { a(i) += v(j) * factor; i += 1 }))
  }
  private lazy val tensorsSizes = tensors.map(_.activeDomain.size).toArray
  private lazy val tensorsActiveDomain = tensors.map(_.activeDomain.toArray)
  def tensorValueAtArrayIndex(index:Int): Double = {
    var i = index
    var j = 0
    while (i > tensorsSizes(j)) { i += tensorsSizes(j); j += 1 }
    tensors(j)(tensorsActiveDomain(j)(i))
  }
  def tensorValueAtArrayIndex_=(index:Int, d:Double): Unit = {
    var i = index
    var j = 0
    while (i > tensorsSizes(j)) { i += tensorsSizes(j); j += 1 }
    tensors(j)(tensorsActiveDomain(j)(i)) = d
  }
}
