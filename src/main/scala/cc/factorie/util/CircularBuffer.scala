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

package cc.factorie.util

import scala.reflect.ClassTag

/**
 */
class CircularBuffer[A](val size : Int)(implicit m : ClassTag[A]) {
  private val buffer = new Array[A](size)
  private var position = 0

  flush()

  def flush() : Unit = {
    var i = 0
    while (i < size) {
      buffer(i) = null.asInstanceOf[A]
      i += 1
    }
  }

  def +=(item : A) : A = {
    val output = buffer(position)
    buffer(position) = item
    position += 1
    position = position % size
    return output
  }

  def apply(i : Int) : A = {
    return buffer(i)
  }

  def getPosition() : Int = {
    return position
  }

  def getLast() : A = {
    if (position == 0) {
      return apply(size-1)
    } else {
      return apply((position-1)%4)
    }
  }

  override def toString() : String = {
    val builder = new StringBuffer()

    var i = 0
    while (i < size) {
      builder.append("(")
      builder.append(i)
      builder.append(",")
      if (buffer(i) != null) {
        builder.append(buffer(i).toString())
      } else {
        builder.append("Null")
      }
      builder.append(")")
      i += 1
    }

    builder.toString()
  }
}
