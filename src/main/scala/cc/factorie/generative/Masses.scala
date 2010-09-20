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



package cc.factorie.generative
import cc.factorie._

/** Masses is a Seq[Double] with all elements non-negative.
    Proportions ~ Dirichlet(Masses)
    Masses ~ Multinomial(Proportions) */
// TODO Note, this is currently unused, since Dirichlet is instead parameterized by mean & precision
trait Masses extends Variable with IndexedSeqEqualsEq[Double] {
  override def toString = this.mkString(printName+"(", ",", ")")
  def activeDomain: Iterable[Int]
  //def vector: Vector // TODO Consider IncrementableMasses that actually store the counts in a Vector, ala BinaryVectorVariable
}

/** Masses that are also a Parameter.  Could in future be used as the parameters of a Dirichlet distribution, 
    but Dirichlet is currently parameterized instead by mean and precision. */
trait MassesParameter extends Masses with Parameter

/** Masses that are mutable only through increment method. 
    Useful for Proportions, Dirichlet, DirichletMultinomial, etc. */
trait IncrementableMasses extends Masses {
  protected val _masses: scala.collection.mutable.IndexedSeq[Double]
  protected var _massesTotal: Double = 0.0
  def length: Int = _masses.length
  def apply(i:Int) = _masses(i)
  def massesTotal = _massesTotal
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
    _masses(index) += incr; _massesTotal += incr
    if (d ne null) d += IncrementableMassesDiff(index, incr)
    assert(_masses(index) >= 0, "mass("+index+")="+_masses(index)+" after incr="+incr)
    assert(_massesTotal >= 0, "massTotal="+_massesTotal+" after incr="+incr)
  }
  def increment(masses:Seq[Double])(implicit d:DiffList): Unit = {
    for (i <- 0 until masses.length) { 
      _masses(i) += masses(i); _massesTotal += masses(i)
      assert(_masses(i) >= 0, "mass("+i+")="+_masses(i)+" after incr="+masses(i))
      assert(_massesTotal >= 0, "massTotal="+_massesTotal+" after incr="+masses(i))
    }
  }
  def zero(implicit d:DiffList = null): Unit = 
    for (i <- 0 until length) if (_masses(i) > 0.0) increment(i, -_masses(i))
  def set(cs:Seq[Double], normalize:Boolean = true): Unit = {
    // TODO normalize is currently ignored.
    zero(null); increment(cs)(null)
  }
  case class IncrementableMassesDiff(index:Int, incr:Double) extends Diff {
    def variable = IncrementableMasses.this
    def undo = { _masses(index) -= incr; _massesTotal -= incr; assert(_masses(index) >= 0.0); assert(_massesTotal >= 0.0) }
    def redo = { _masses(index) += incr; _massesTotal += incr }
  }
  case class IncrementableMassesSeqDiff(cs: Seq[Double]) extends Diff {
    def variable = IncrementableMasses.this
    def undo = { for (i <- 0 until cs.length) { _masses(i) -= cs(i); _massesTotal -= cs(i) } }
    def redo = { for (i <- 0 until cs.length) { _masses(i) += cs(i); _massesTotal += cs(i) } }
  }
}

trait ArrayIncrementableMasses extends IncrementableMasses {
  protected val _masses = new scala.collection.mutable.WrappedArray.ofDouble(Array[Double](this.length))
  def activeDomain: Iterable[Int] = Range(0, _masses.length)
}

trait HashIncrementableMasses extends IncrementableMasses {
  protected val _masses = new scala.collection.mutable.IndexedSeq[Double] {
    private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(i:Int) = 0.0 }
    def length: Int = h.size
    def apply(key:Int): Double = h(key)
    def update(key:Int, value:Double): Unit = h.put(key, value)
    def zero = h.clear
    def keys = h.keys
  }
  def activeDomain: Iterable[Int] = _masses.keys
  override def zero(implicit d:DiffList = null): Unit = {
    if (d ne null) d += IncrementableMassesSeqDiff(_masses.map(d => -d))
    _masses.zero
  }
}

// TODO
// Make Masses inherit from Vector.  NO!  Variables can never inherit from Vector because of "equals" incompatibility.
// IncrementableMasses takes the place of IncrementableCounts, and is used in CountsProportions
// SettableMasses is new name for MutableMasses
// Also make a "Growable" version

/*
// Counts is a Seq[Double] with all elements non-negative.
//  Counts ~ Multinomial(Proportions) 
//  @author Andrew McCallum 
trait Counts extends Vector with IndexedSeqEqualsEq[Double] with Variable {
  override def toString = this.mkString(printName+"(", ",", ")")
}

class SparseCounts(theDomainSize:Int) extends SparseVector(theDomainSize) with Counts {
  def this(ds:Int, occurrences:Seq[Int]) = { this(ds); occurrences.foreach(increment(_, 1.0)) }
  def this(v:Vector) = { this(v.size); v.activeElements.foreach({case (index,value) => this(index) = value})
}
*/

trait SetableMasses extends Masses {
  def set(p:Seq[Double])(implicit d:DiffList): Unit 
}

class DenseMasses(m:Seq[Double]) extends SetableMasses {
  def this(dim:Int) = this(Seq.fill(dim)(1.0))
  private var _m = new Array[Double](length)
  if (m != Nil) this := m else setUniform(null)
  @inline final def length = _m.size
  @inline final def apply(index:Int) = _m(index)
  def activeDomain = Range(0, _m.size)
  def set(m:Seq[Double])(implicit d:DiffList): Unit = {
    assert(m.size == _m.size, "size mismatch: new="+m.size+", orig="+_m.size)
    val newM = m.toArray
    if (d ne null) d += MassesDiff(_m, newM)
    _m = newM
  }
  def :=(p:Seq[Double]) = set(p)(null)
  def setUniform(implicit d:DiffList): Unit = set(Seq.fill(_m.length)(1.0))
  case class MassesDiff(oldM:Array[Double], newM:Array[Double]) extends Diff {
    def variable = DenseMasses.this
    def undo = _m = oldM
    def redo = _m = newM
  }
}
