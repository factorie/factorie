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

trait DiscreteValues extends cc.factorie.la.Vector with DomainType[DiscreteDomain] {
  def domain: DomainType
}

/** A value in a DiscreteDomain. */
trait DiscreteValue extends DiscreteValues with cc.factorie.la.SingletonBinaryVec {
  def index: Int
}

/** A Domain for DiscreteVars, whose values are DiscreteValues (or perhaps a collection of DiscreteValues).
    The domain has a positive integer size.  The method 'size' is abstract. */
abstract class DiscreteDomain extends VectorDomain with ValueType[DiscreteValue] {
  thisDomain =>
  private var _frozen = false
  def size: Int // Ensure that this method will be overridden
  def length: Int = size // TODO Reverse the order of this size/length dependency?
  override def freeze: Unit = { _frozen = true }
  def allocSize = size
  override def maxVectorSize = allocSize
  var maxRequestedInt: Int = 0

  // Access sort of like a collection
  def values: scala.collection.Seq[Value] = _elements
  def iterator = _elements.iterator
  def apply(index:Int): Value  = getValue(index)
  def unapply(value:Value): Option[Int] = Some(value.index)
  //def unapply(entry:CategoricalValue): Option[Int] = if (_indices.contains(entry)) Some(_indices(entry).index) else None

  // 'protected' so that only the 'getValue' method should construct these objects
  class DiscreteValue(val index:Int) extends cc.factorie.DiscreteValue {
    final def singleIndex = index // needed for SingletonBainaryVec
    final def length = thisDomain.size // needed for SingletonBinaryVec
    final def intValue = index // just a convenient alias
    final def domain = thisDomain
    override def toString = index.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.index == other.index; case _ => false }
  }
  /** Maps from integer index to the DiscreteValue objects */
  private val __elements = new scala.collection.mutable.ArrayBuffer[Value]
  def _elements = __elements // Define this way so that _elements can be overridden

  // TODO Consider renaming this method to something without the 'get'.  Perhaps valueAtIndex()
  def getValue(index:Int): cc.factorie.DiscreteValue = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) for (i <- _elements.size to index) _elements += new DiscreteValue(i).asInstanceOf[Value] // Here new a DiscreteValue gets created
    _elements(index)
  }

  def asSingleton: DiscreteDomain = new DiscreteDomain {
    override def _elements = thisDomain._elements
    override def size = thisDomain.size
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
    require(size == readSize)
  }
}
