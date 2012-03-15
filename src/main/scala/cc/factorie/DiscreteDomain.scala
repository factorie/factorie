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

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

// For variables that hold a single discrete value

/** A value in a DiscreteDomain. */
trait DiscreteValue extends DiscreteVectorValue with cc.factorie.la.SingletonBinaryVec {
  def domain: DiscreteDomain
  def intValue: Int
}

trait DiscreteDomain extends DiscreteVectorDomain with IterableDomain[DiscreteValue] with ValueType[DiscreteValue] {
  thisDomain =>
  // Make method 'size' abstract again.
  // Note that we are reversing the order of the traditional size/length dependency
  // Note that this only works if DiscreteDomain is a class, not a trait.
  def size: Int
  def dimensionDomain = this
  /** If true, do not allow this domain to change. */
  protected var _frozen = false
  override def freeze(): Unit = _frozen = true
  /** Can new category values be added to this Domain? */
  def frozen = _frozen

  def allocSize = size // TODO Remove this?
  var maxRequestedInt: Int = 0

  /** Maps from integer index to the DiscreteValue objects */
  private val __elements = new scala.collection.mutable.ArrayBuffer[ValueType]
  def _elements = __elements // Define this way so that _elements can be overridden

  def values: IndexedSeq[ValueType] = __elements
  // Access sort of like a collection
  //def values: scala.collection.Seq[ValueType] = _elements
  def length = size
  def apply(index:Int): ValueType  = getValue(index)
  def unapply(value:ValueType): Option[Int] = if (value.domain == this) Some(value.intValue) else None
  def iterator = _elements.iterator

  // TODO Make this 'protected' so that only the 'getValue' method should construct these objects?
  class DiscreteValue(val intValue:Int) extends cc.factorie.DiscreteValue {
    //type DomainType = cc.factorie.DiscreteDomain
    final def singleIndex = intValue // needed for SingletonBainaryVec
    final def length = thisDomain.size // needed for SingletonBinaryVec
    def domain = thisDomain
    override def toString = intValue.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.intValue == other.intValue; case _ => false }
  }

  // TODO Consider renaming this method to something without the 'get'.  Perhaps valueAtIndex()
  def getValue(index:Int): ValueType = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) {
      _elements synchronized { for (i <- _elements.size to index) _elements += new DiscreteValue(i) }
    } //.asInstanceOf[Value] // Here new a DiscreteValue gets created
    _elements(index)
  }

  // Serialization
  override def save(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val writer = new PrintWriter(new BufferedOutputStream({
      if (gzip)
        new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(f)))
      else
        new FileOutputStream(f)
    }))

    writer.println(size)
    writer.close
  }

  override def load(dirname:String, gzip: Boolean = false): Unit = {
    val f = new File(dirname + "/" + filename + { if (gzip) ".gz" else "" })
    val reader = new BufferedReader(new InputStreamReader({
      if (gzip)
        new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))
      else
        new FileInputStream(f)
    }))
    val line = reader.readLine
    val readSize = Integer.parseInt(line)
    require(size == readSize)
  }
}


