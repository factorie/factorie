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

package cc.factorie
import java.io.{File,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}

/** A Domain that has a positive integer size.  
    Set its size by Domain[MyDiscrete].size = 9; or Domain[MyDiscrete].size = Domain[MyOther].size. 
    @author Andrew McCallum */
class DiscreteDomain[V<:DiscreteVars](implicit m:Manifest[V]) extends VectorDomain[V] with Iterable[cc.factorie.DiscreteValue[V]] {
  thisDomain =>
  private var _frozen = false
  private var _size: Int = -1
  private var _sizeFunction: ()=>Int = null
  def size_=(size:Int): Unit = size_=(() => size)
  def size_=(sizeFunction: ()=>Int): Unit = {
    if (_size == -1) _sizeFunction = sizeFunction
    else throw new Error("DiscreteDomain["+m.erasure.getName+"].size already accessed; cannot re-set size.")
  }
  /** This method will call the sizeFunction to get the correct size. */
  private def setSize(): Unit = 
    if (_sizeFunction != null) { val s = _sizeFunction.apply; require(s > 0); require(s >= _size); _size = s; _frozen = true }
    else throw new Error(getClass.getName+": DiscreteDomain size must be set; e.g. Domain[MyDiscrete].size = 10")
  def setSize(s:Int): Unit = if (!_frozen) _size = s else throw new Error(getClass.getName+": DiscreteDomain size is already frozen and cannot be set.")
  override def size: Int = { if (_size == -1) setSize(); _size }
  def length: Int = size // TODO Reverse the order of this size/length dependency
  override def freeze: Unit = { setSize(); _frozen = true }
  def allocSize = size
  override def maxVectorSize = allocSize
  // For Iterable[DiscreteValue]
  def iterator = _elements.iterator

  // 'protected' so that only the 'getValue' method should construct these objects
  class DiscreteValue(val index:Int) extends cc.factorie.DiscreteValue[V] {
    def singleIndex = index // needed for SingletonBainaryVec
    def length = thisDomain.size // needed for SingletonBainaryVec
    def intValue = index // just a convenient alias
    def domain = thisDomain
    override def toString = index.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.index == other.index; case _ => false }
  }
  type ValueType <: cc.factorie.DiscreteValue[V] // or DiscreteValue specific to this class
  //type ValueType = DomainType#ValueType
  /** Maps from integer index to the DiscreteValue objects */
  protected var _elements = new scala.collection.mutable.ArrayBuffer[ValueType]
  def getValue(index:Int): ValueType = {
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) for (i <- _elements.size to index) _elements += new DiscreteValue(i).asInstanceOf[ValueType] // Here new a DiscreteValue gets created
    _elements(index)
  }

  // Serialization
  override def save(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    val s = new PrintWriter(new FileWriter(f))
    s.println(size)
    s.close
  }
  override def load(dirname:String): Unit = {
    val f = new File(dirname+"/"+filename)
    val s = new BufferedReader(new FileReader(f))
    val line = s.readLine
    val readSize = Integer.parseInt(line)
    this.size_=(()=>readSize)
  }
}

trait DiscreteValue[V<:DiscreteVars] extends cc.factorie.la.SingletonBinaryVec {
  def index: Int
  def domain: DiscreteDomain[V]
}
