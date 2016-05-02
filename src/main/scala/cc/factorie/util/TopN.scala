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

class TopEntry[A<:AnyRef](val index:Int, val score:Double, val category:A) {
  override def toString = (if (category eq null) "(null)" else category.toString)+" "+score
}

/** For gathering a sorted list of at most N indexed entries, sorted by a floating-point score. */
class TopN[A<:AnyRef](maxLength:Int) extends Seq[TopEntry[A]] {
  /*def this(maxLength:Int, contents:Seq[Double], categories:Seq[A] = null) = {
    this(maxLength)
    //println("TopN %d %d".format(contents.length, categories.length))
    var i = 0
    while (i < contents.length) {
      this += (i, contents(i), if (categories ne null) categories(i) else null.asInstanceOf[A])
      i += 1
    }
  }*/
  def this(maxLength:Int, contents:DoubleSeq, categories:Seq[A]) = {
    this(maxLength)
    var i = 0
    while (i < contents.length) {
      this += (i, contents(i), if (categories ne null) categories(i) else null.asInstanceOf[A])
      i += 1
    }
  }
  def this(maxLength:Int, contents:DoubleSeq) = this(maxLength, contents, null)
  def this(maxLength:Int, contents:Array[Double], categories:Seq[A]) = {
    this(maxLength)
    var i = 0
    while (i < contents.length) {
      this += (i, contents(i), if (categories ne null) categories(i) else null.asInstanceOf[A])
      i += 1
    }
  }
  def this(maxLength:Int, contents:Array[Double]) = this(maxLength, contents, null)
  private val _seq = new Array[TopEntry[A]](maxLength)
  private var _length: Int = 0
  def length = _length
  def apply(i:Int) = _seq(i)
  def iterator: Iterator[TopEntry[A]] = new Iterator[TopEntry[A]] {
    var i = 0
    def hasNext = i < _length
    def next() = { i += 1; _seq(i-1) }
  }
  def +=(index:Int, pr:Double, category:A = null.asInstanceOf[A]): Unit = {
    if (_length < maxLength || (pr > _seq(_length-1).score && pr > 0.0)) {
     if (_length < maxLength) { _seq(_length) = new TopEntry[A](index, pr, category); _length += 1 }
     else if (pr > _seq(_length-1).score) _seq(_length-1) = new TopEntry[A](index, pr, category)
     var i = _length - 1
     while (i > 0 && _seq(i).score > _seq(i-1).score) {
       val tmp = _seq(i)
       _seq(i) = _seq(i-1)
       _seq(i-1) = tmp
       i -= 1
     }
    }
  }
  
  override def toString(): String = mkString(", ")
  
}