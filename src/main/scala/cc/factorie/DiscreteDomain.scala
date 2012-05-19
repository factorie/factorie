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
import cc.factorie.la._
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

// For variables that hold a single discrete value

/** A value in a DiscreteDomain. */
trait DiscreteValue extends SingletonBinaryTensorLike1 {
  def domain: DiscreteDomain
  @inline final def intValue: Int = singleIndex // TODO Consider swapping singleIndex <-> intValue
  @inline final def dim1 = domain.size
}


// Because DiscreteDomain is an IndexedSeq it can be passed as a sizeProxy
class DiscreteDomain(sizeProxy:Iterable[Any]) extends IndexedSeq[DiscreteValue] with DiscreteTensorDomain with ValueType[DiscreteValue] {
  thisDomain =>
  def this(size:Int) = { this(null.asInstanceOf[Iterable[Any]]); _size = size }
  def dimensionDomain: DiscreteDomain = this
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

  // If _size >= 0 it is used to determine DiscreteDomain.size, otherwise _sizeProxy.size is used. 
  private var _size = -1
  private val _sizeProxy = sizeProxy
  def length = if (_size >= 0) _size else _sizeProxy.size
  def apply(index:Int): ValueType = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) {
      _elements synchronized { for (i <- _elements.size to index) _elements += new DiscreteValue(i) }
    } //.asInstanceOf[Value] // Here new a DiscreteValue gets created
    _elements(index)
  }
  def unapply(value:ValueType): Option[Int] = value match {
    case dv:DiscreteValue => Some(dv.intValue)
    case _ => None
  }//if (value.domain == this) Some(value.intValue) else None
  override def iterator: Iterator[ValueType] = _elements.iterator
  def getAll(c: Iterator[Int]) = c map apply
  def getAll(c: List[Int]) = c map apply
  def getAll(c: Array[Int]) = c map apply
  def getAll(c: Set[Int]) = c map apply

  protected class DiscreteValue(val singleIndex:Int) extends cc.factorie.DiscreteValue {
    def domain = thisDomain
    override def toString = intValue.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.intValue == other.intValue; case _ => false }
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
    loadFromReader(reader)
  }

  def loadFromReader(reader: BufferedReader): Unit = {
    val line = reader.readLine
    val readSize = Integer.parseInt(line)
    require(size == readSize)
    reader.close()
  }
  
}
