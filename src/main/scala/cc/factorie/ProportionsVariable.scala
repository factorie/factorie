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
trait IncrementableProportions extends IncrementableMasses with Proportions


// Proportions Values of dimensionality 1

class SingletonProportions1(dim1:Int, singleIndex:Int) extends SingletonMasses1(dim1, singleIndex, 1.0) with Proportions {
  @inline final override def apply(index:Int) = if (index == singleIndex) 1.0 else 0.0
}
class UniformProportions1(dim1:Int) extends UniformMasses1(dim1, 1.0) with Proportions {
  @inline override final def apply(i:Int): Double = 1.0 / dim1
}
class GrowableUniformProportions1(sizeProxy:Iterable[Any], uniformValue:Double) extends GrowableUniformMasses1(sizeProxy, uniformValue) with Proportions {
  @inline final override def apply(index:Int) = {
    val result = 1.0 / length
    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
    result
  }
}

class DenseProportions1(override val dim1:Int) extends DenseMasses1(dim1) with IncrementableProportions
class GrowableDenseProportions1(sizeProxy:Iterable[Any]) extends GrowableDenseMasses1(sizeProxy) with IncrementableProportions

class SortedSparseCountsProportions1(dim1:Int) extends SortedSparseCountsMasses1(dim1) with IncrementableProportions {
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
  //override def zero(): Unit = counts.zero() // Note that this doesn't zero the prior
  override def top(n:Int): Seq[DiscretePr] =
    for (i <- 0 until math.min(n, numPositions)) yield 
      new DiscretePr(indexAtPosition(i), countAtPosition(i).toDouble / countsTotal)

}



// Proportions Variables

trait ProportionsVar extends MassesVar with VarAndValueType[ProportionsVar,Proportions]
class ProportionsVariable extends MassesVariable with ProportionsVar {
  def this(initialValue:Proportions) = { this(); _set(initialValue) }
}

trait IncrementableProportionsVar extends IncrementableMassesVar with VarAndValueType[IncrementableProportionsVar,IncrementableProportions]
class IncrementableProportionsVariable extends IncrementableMassesVariable with IncrementableProportionsVar {
  def this(initialValue:IncrementableProportions) = { this(); _set(initialValue) }
  def setUniform(implicit d:DiffList): Unit = {
    if (d ne null) throw new Error("Not yet implemented.")
    tensor.zero(); tensor += 1.0
  }
}

// We should provide some way to construct these things that are more concise... but how? -akm
object ProportionsVariable {
  def dense1(dim1:Int) = new IncrementableProportionsVariable(new DenseProportions1(dim1)) 
}
