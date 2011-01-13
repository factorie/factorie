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

// For variables that hold one or more discrete value weights in a vector

// Was DiscreteValues
trait DiscretesValue extends cc.factorie.la.Vector with DomainType[DiscreteVectorDomain] {
  def domain: DomainType
}


/** The sub-domain in a DiscreteVectorDomain that gives the dimensionality of the vectors of the DiscreteVectorDomain. */
trait DimensionDomainType[+DDT<:DiscreteDomain] {
  type DimensionDomain = DDT
}

/** A Domain for variables whose value is a DiscretesValue, which is a Vector that also has a pointer back to its domain.
    This domain has a non-negative integer size.  The method 'size' is abstract. */
trait DiscreteVectorDomain extends VectorDomain with ValueType[DiscretesValue] with DimensionDomainType[DiscreteDomain] {
  /** The maximum size to which this domain will be allowed to grow.  
      The 'size' method may return values smaller than this, however.
      This method is used to pre-allocate a Template's parameter arrays and is useful for growing domains. */
  def size: Int
  def maxVectorLength: Int = size  // TODO Get rid of this?
  def dimensionDomain: DimensionDomain // = new DiscreteDomain { def size = thisDomain.size }
  def vectorDimensionName(i:Int): String = i.toString
  def freeze(): Unit = {}
}


// For variables that hold a single discrete value

/** A value in a DiscreteDomain. */
trait DiscreteValue extends DiscretesValue with cc.factorie.la.SingletonBinaryVec with DomainType[DiscreteDomain] {
  def index: Int
  final def intValue = index // TODO Consider removing this alias?
}

trait DiscreteDomain extends DiscreteVectorDomain with IterableDomain[DiscreteValue] with ValueType[DiscreteValue] {
  thisDomain =>
  private var _frozen = false
  def size: Int
  def dimensionDomain = this
  //def length: Int = size // TODO Reverse the order of this size/length dependency?
  override def freeze(): Unit = { _frozen = true }
  def allocSize = size // TODO Remove this?
  var maxRequestedInt: Int = 0

  // Access sort of like a collection
  def values: scala.collection.Seq[ValueType] = _elements
  def iterator = _elements.iterator
  def apply(index:Int): ValueType  = getValue(index)
  def unapply(value:ValueType): Option[Int] = if (value.domain == this) Some(value.index) else None

  // TODO Make this 'protected' so that only the 'getValue' method should construct these objects?
  class DiscreteValue(val index:Int) extends cc.factorie.DiscreteValue {
    //type DomainType = cc.factorie.DiscreteDomain
    final def singleIndex = index // needed for SingletonBainaryVec
    final def length = thisDomain.size // needed for SingletonBinaryVec
    final def domain = thisDomain.asInstanceOf[DomainType] // TODO Why is this cast necessary
    override def toString = index.toString
    override def equals(other:Any): Boolean = 
      other match { case other:DiscreteValue => this.index == other.index; case _ => false }
  }
  /** Maps from integer index to the DiscreteValue objects */
  private val __elements = new scala.collection.mutable.ArrayBuffer[ValueType]
  def _elements = __elements // Define this way so that _elements can be overridden

  // TODO Consider renaming this method to something without the 'get'.  Perhaps valueAtIndex()
  def getValue(index:Int): ValueType = {
    if (index > maxRequestedInt) maxRequestedInt = index
    if (index >= size) throw new IllegalArgumentException("DiscreteDomain.getValue: index "+index+" larger than size "+size)
    if (index >= _elements.size) for (i <- _elements.size to index) _elements += new DiscreteValue(i) //.asInstanceOf[Value] // Here new a DiscreteValue gets created
    _elements(index)
  }

  /** Return a DiscreteDomain that shares the same underlying size and DiscreteValue objects.
      For example, if you want a BinaryFeatureVector and a CategoricalVariable to share the same underlying
      CategoricalValues (and String-Integer mapping) then you would write code something like:
      <pre>
      // Representing multiple rolls of a 6-sided die.
      object RollsDomain extends DiscreteVectorDomain { def size = 6 }
      class Rolls(i:Int) extends VectorVariable(i) { def domain = RollsDomain }
      </pre>
      */
  /*def discreteDomain: DiscreteDomain = new DiscreteDomain {
    override def _elements = thisDomain._elements
    override def size = thisDomain.size
  }*/

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


