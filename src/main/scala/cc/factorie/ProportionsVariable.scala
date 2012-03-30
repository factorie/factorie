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
import cc.factorie._
import cc.factorie.la._
import cc.factorie.util.{DoubleSeq,IntSeq}
import scala.util.Random

// Proportions Values

// TODO Rename Proportions when we make the full substitution!
//  Then also create a separate Proportions1 that inherits from Masses1 and Tensor1
// TODO Perhaps instead Proportions should contain a Masses, but not inherit from Masses?  (Suggested by Alexandre)  -akm
trait Proportions extends Masses {
  abstract override def apply(i:Int): Double = super.apply(i) / massTotal
  override def sampleIndex(implicit r:Random): Int = {
    var b = 0.0; val s = r.nextDouble; var i = 0
    while (b <= s && i < length) { assert (apply(i) >= 0.0); b += apply(i); i += 1 }
    assert(i > 0)
    i - 1
  } 
  def sampleIndex: Int = sampleIndex(cc.factorie.random)
  @inline final def pr(index:Int) = apply(index)
  @inline final def logpr(index:Int) = math.log(apply(index))
  override def stringPrefix = "Proportions"
  override def toString = this.toSeq.take(10).mkString(stringPrefix+"(", ",", ")")

  class DiscretePr(val index:Int, val pr:Double)
  class DiscretePrSeq(val maxLength:Int) extends Seq[DiscretePr] {
    def this(maxLength:Int, contents:Seq[Double]) = { this(maxLength); var i = 0; while (i < contents.length) { this += (i, contents(i)); i += 1 } }
    private val _seq = new Array[DiscretePr](maxLength)
    private var _length: Int = 0
    def length = _length
    def apply(i:Int) = _seq(i)
    def iterator: Iterator[DiscretePr] = new Iterator[DiscretePr] {
      var i = 0
      def hasNext = i < _length
      def next = { i += 1; _seq(i-1) }
    }
    def +=(index:Int, pr:Double): Unit = {
      if (_length < maxLength || (pr > _seq(_length-1).pr && pr > 0.0)) {
       if (_length < maxLength) { _seq(_length) = new DiscretePr(index, pr); _length += 1 }
       else if (pr > _seq(_length-1).pr) _seq(_length-1) = new DiscretePr(index, pr)
       var i = _length - 1
       while (i > 0 && _seq(i).pr > _seq(i-1).pr) {
         val tmp = _seq(i)
         _seq(i) = _seq(i-1)
         _seq(i-1) = tmp
         i -= 1
       }
      }
    }
  }
  def top(n:Int): Seq[DiscretePr] = new DiscretePrSeq(n, this.toSeq)
}


// Proportions Values of dimensionality 1

class SingletonProportions1(dim1:Int, singleIndex:Int) extends SingletonMasses1(dim1, singleIndex, 1.0) with Proportions {
  @inline final override def apply(index:Int) = if (index == singleIndex) 1.0 else 0.0
}
class UniformProportions1(dim1:Int) extends UniformMasses1(dim1, 1.0) with Proportions {
  @inline override final def apply(i:Int): Double = 1.0 / dim1
}
class GrowableUniformProportions1(sizeProxy:Iterable[Any], uniformValue:Double = 1.0) extends GrowableUniformMasses1(sizeProxy, uniformValue) with Proportions {
  @inline final override def apply(index:Int) = {
    val result = 1.0 / length
    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
    result
  }
}

class DenseProportions1(override val dim1:Int) extends DenseMasses1(dim1) with Proportions
class GrowableDenseProportions1(sizeProxy:Iterable[Any]) extends GrowableDenseMasses1(sizeProxy) with Proportions

class SortedSparseCountsProportions1(dim1:Int) extends SortedSparseCountsMasses1(dim1) with Proportions {
  // TODO We need somehow to say that this isDeterministic function of this.prior.
  var prior: Masses = null
  
  // TODO Fix this by implementing a SortedSparseCountsMasses1
  override def apply(index:Int): Double = {
    if (prior eq null) {
      if (countsTotal == 0) 1.0 / length
      else countOfIndex(index).toDouble / countsTotal
    } else {
      if (countsTotal == 0) prior(index) / prior.massTotal
      else countOfIndex(index).toDouble / countsTotal
    }
  }
  // Note that "def zero()" defined in SortedSparseCountsMasses1 does not zero this.prior
  override def top(n:Int): Seq[DiscretePr] =
    for (i <- 0 until math.min(n, numPositions)) yield 
      new DiscretePr(indexAtPosition(i), countAtPosition(i).toDouble / countsTotal)

}



// Proportions Variable

trait ProportionsVar[P<:Proportions] extends MassesVar[P] with VarAndValueType[ProportionsVar[P],P]
class ProportionsVariable[P<:Proportions] extends MassesVariable[P] with ProportionsVar[P] {
  def this(initialValue:P) = { this(); _set(initialValue) }
}

object ProportionsVariable {
  def uniform(dim:Int) = new ProportionsVariable(new UniformProportions1(dim))
  def dense(dim:Int) = new ProportionsVariable(new DenseProportions1(dim))
  def growableDense(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableDenseProportions1(sizeProxy))
  def growableUniform(sizeProxy:Iterable[Any]) = new ProportionsVariable(new GrowableUniformProportions1(sizeProxy))
  def sparseCounts(dim:Int) = new ProportionsVariable(new SortedSparseCountsProportions1(dim))
}
