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
import cc.factorie.util._

// TODO Finish this implementation
@deprecated("Not used anywhere", "Before 10/06/15")
class ConcatenatedTensor(theTensors:Seq[Tensor]) extends ReadOnlyTensor with Tensor1 with DenseDoubleSeq {
  def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)
  def activeDomainSize = activeDomain.size
  def tensors: Array[Tensor] = theTensors.toArray
  def foreachTensor(f:Tensor=>Unit): Unit = { var i = 0; while (i < tensors.length) { f(tensors(i)); i += 1 }}
  def isDense = throw new Error("Not yet implemented")
  lazy val lengths: Array[Int] = { val a = new Array[Int](tensors.length); var i = 0; while (i < a.length) { a(i) = tensors(i).length; i += 1 }; a }
  lazy val lengthsSums: Array[Int] = { val a = new Array[Int](lengths.length); a(0) = lengths(0); var i = 1; while (i < a.length) { a(i) = a(i-1) + lengths(i); i += 1 }; a }
  lazy val offsets: Array[Int] = { val a = new Array[Int](lengths.length); a(0) = 0; var i = 1; while (i < a.length) { a(i) = a(i-1) + lengths(i-1); i += 1 }; a }
  lazy val dim1 = lengths.sum
  def activeDomain: IntSeq = throw new Error("Not yet implemented")
  // Careful!  This will be very slow.  You should really try to use the more specific methods, such as += 
  def apply(index:Int): Double = {
    throw new Error("This is very slow.  I'm throwing an error here to find situations where this would be called, and then we should try to find a faster alternative.")
    var i = 0
    var sum = 0
    while (i < lengths.length) {
      if (index < sum) return tensors(i-1).apply(index-lengthsSums(i-1))
      sum += lengths(i)
      i += 1
    }
    throw new Error("Index out of bounds: "+index)
  }
  override def toString: String = {
    tensors.map(_.toString).mkString("\n")
  }

  override def different(t:DoubleSeq, threshold:Double): Boolean = t match {
    case t: ConcatenatedTensor => {
      assert(t.tensors.length == theTensors.length)
      (0 until theTensors.length).exists(i=> tensors(i).different(t.tensors(i), threshold))
    }
    case t:DoubleSeq => {
       throw new IllegalStateException("shouldn't be comparing to flat DoubleSeq")
    }
  }

  override def containsNaN: Boolean =  {
      (0 until theTensors.length).exists(i=> tensors(i).containsNaN)
  }

  override def :=(t:DoubleSeq): Unit = t match {
    case t: ConcatenatedTensor => {
      assert(t.tensors.length == theTensors.length)
      (0 until theTensors.length).map(i=> tensors(i):=t.tensors(i))
    }
    case t:DoubleSeq => {
       throw new IllegalStateException("shouldn't be comparing to flat DoubleSeq")
    }
  }

  override def copy: ConcatenatedTensor = new ConcatenatedTensor(tensors.map(_.copy))
  override def :=(a:Array[Double]): Unit = { var i = 0; while (i < tensors.length) { tensors(i).:=(a, offsets(i)); i += 1 } } 
  override def toArray: Array[Double] = { val a = new Array[Double](length); var i = 0; while (i < tensors.length) { System.arraycopy(tensors(i).asArray, 0, a, offsets(i), tensors(i).length); i +=1 }; a }
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:ConcatenatedTensor => for (pair <- tensors.zip(t.tensors)) pair._1.+=(pair._2, f)
  }
  override def +=(d:Double): Unit = foreachTensor(_ += d)
  override def *=(d:Double): Unit = foreachTensor(_ *= d)
  protected def sumOverTensors(f:Tensor=>Double): Double = { val len = tensors.length; var s = 0.0; var i = 0; while (i < len) { s += f(tensors(i)); i += 1 }; s }
  override def oneNorm: Double = sumOverTensors(_.twoNorm)
  override def twoNormSquared: Double = sumOverTensors(_.twoNormSquared)
  override def dot(t:DoubleSeq): Double = t match {
    case t:ConcatenatedTensor => { var s = 0.0; for (pair <- tensors.zip(t.tensors)) s += pair._1 dot pair._2; s }
  }
}

