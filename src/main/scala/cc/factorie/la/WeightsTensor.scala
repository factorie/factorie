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
import cc.factorie.util._

//* A Tensor to represent the weights in a collection of DotFamilies as the keys in a HashMap from DotFamily to Tensor. */
class WeightsTensor(val newTensor:DotFamily=>Tensor = (df:DotFamily) => Tensor.newSparse(df.weights)) extends Tensor1 {
  // This functionality moved to TemplateModel
  //def this(model:TemplateModel, newTensor:DotFamily=>Tensor) = { this(newTensor); val wt = this; model.familiesOfClass[DotFamily].foreach(f => wt(f) = newTensor(f)) } 
  private val _map = new scala.collection.mutable.LinkedHashMap[DotFamily,Tensor] {
    override def default(f:DotFamily) = { val t = newTensor(f); this(f) = t; t }
  }
  override def zero() = _map.valuesIterator.foreach(_.zero())
  def families = _map.keys
  def dim1: Int = _map.values.map(_.length).sum // TODO This should really never be called, I think.  Should we print a warning? -akm
  override def dimensionsMatch(t:Tensor): Boolean = t match {
    case t:WeightsTensor => true // We can never know whether an element is simply missing due to sparsity.  Was: _map.keys.toSeq equals t._map.keys.toSeq
    case _ => false
  }
  // Careful!  This will be very slow.  You should really try to use the more specific methods, such as += 
  def apply(index:Int): Double = {
    throw new Error("This is very slow.  I'm throwing an error here to find situations where this would be called, and then we should try to find a faster alternative.")
    var i = 0
    var sum = 0
    for (t <- _map.values) {
      val len = t.length
      if (index < sum + len) return t.apply(index-sum)
      sum += len
    }
    throw new Error("Index out of bounds: "+index)
  }
  def activeDomain: IntSeq = throw new Error("Method activeDomain not defined for WeightTensors.")
  def isDense = false
  override def stringPrefix = "WeightsTensor"
  def apply(f:DotFamily): Tensor = _map.apply(f)
  def update(f:DotFamily, t:Tensor) = _map(f) = t
  override def *=(d:Double): Unit = _map.values.foreach(_.*=(d))
  protected def sumOverTensors(f:Tensor=>Double): Double = { var s = 0.0; _map.values.foreach(s += f(_)); s }
  override def oneNorm: Double = if (_map.size == 0) 0.0 else sumOverTensors(_.twoNorm)
  override def twoNormSquared: Double = sumOverTensors(_.twoNormSquared)
  //override def toArray: Array[Double] = throw new Error("Cannot be implemented.")
  override def toArray: Array[Double] = { val a = new Array[Double](dim1); var offset = 0; _map.values.foreach(t => { System.arraycopy(t.asArray, 0, a, offset, t.length); offset += t.length }); a }
  override def copy: WeightsTensor = { val t = new WeightsTensor(newTensor); _map.keys.foreach(k => t(k) = apply(k).copy); t }
  override def blankCopy: WeightsTensor = {
    // TODO: Add a "sizeProxy" to weights tensor so we can do a lazy copy and keep proper dimensions?
    //    val w = new WeightsTensor({
    //      case f: DotFamily if _map.contains(f) => _map(f).blankCopy
    //      case f => newTensor(f)
    //    })
    val w = new WeightsTensor(newTensor)
    _map.foreach({ case (f, fw) => w(f) = fw.blankCopy })
    w
  }
  override def different(t:DoubleSeq, threshold:Double): Boolean = t match {
    case t:WeightsTensor => _map.keys.exists(k => _map(k).different(t._map(k), threshold))
  }
  override def containsNaN: Boolean = _map.values.exists(_.containsNaN)
  override def :=(t:DoubleSeq): Unit = t match {
    case t:WeightsTensor => _map.keys.foreach(k => if (t(k) eq null) apply(k).zero() else apply(k) := t(k))
  }
  override def +=(i: Int, v: Double): Unit = _map.keys.foreach(k => apply(k) += (i, v))
  override def +=(t:DoubleSeq, f:Double): Unit = t match {
    case t:WeightsTensor => t._map.keys.foreach(k => apply(k).+=(t.apply(k), f))
  }
  override def +=(ds:DoubleSeq, factor:DoubleSeq): Unit = ds match {
    case t:WeightsTensor => {
      factor match {
        case t2:WeightsTensor => {
          t._map.keys.foreach(k => apply(k).+=(t.apply(k), t2.apply(k)))
        }
      }
    }
  }
  override def +=(ds:DoubleSeq, factor:DoubleSeq,scalar:Double): Unit = ds match {
    case t:WeightsTensor => {
      factor match {
        case t2:WeightsTensor => {
          t._map.keys.foreach(k => apply(k).+=(t.apply(k), t2.apply(k),scalar))
        }
      }
    }
  }
  override def dot(t:DoubleSeq): Double = t match {
    case t:WeightsTensor => { var s = 0.0; for (k <- t._map.keys) { val t2 = apply(k); if (t2 ne null) s += t2 dot t.apply(k) }; s }
  }
  override def toString: String = _map.keys.map((f:DotFamily) => {
    val tensor = this(f)
    f.defaultFactorName+" "+tensor.getClass+" dims="+tensor.dimensions.mkString(",")+" contents:"+tensor.toString
  }).mkString("\n")
}
