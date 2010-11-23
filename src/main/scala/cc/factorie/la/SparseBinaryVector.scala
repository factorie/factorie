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
// For the java version of compute matrix:
// import cc.factorie.OuterProduct.{outerProductArray => outerProductArray}
import OuterProductMath.outerProductArray

/** A Vector that may contain mostly zeros, with a few 1.0's, represented compactly in memory.
    @author Andrew McCallum */
class SparseBinaryVector(val theLength:Int, indices:Array[Int] = null, copyArray:Boolean = true, preSorted:Boolean = false) extends Vector {
  /* init as flat outer product of vectors */
  def this(v1:SparseBinaryVector, v2:SingletonBinaryVector) = 
    this(v1.size * v2.size, 
         outerProductArray(v1.ind, v1.activeDomainSize, v2.singleIndex, v2.size),
         copyArray = false, 
         preSorted = true)

  /* init as flat outer product of vectors */
  def this(v1:SparseBinaryVector, v2:SparseBinaryVector) = 
    this(v1.size * v2.size, 
         outerProductArray(v1.ind, v1.activeDomainSize, v2.ind, v2.activeDomainSize, v2.size), 
         copyArray = false, 
         preSorted = true)

  /* init as flat outer product of vectors */
  def this(v1:SparseBinaryVector, v2:SparseBinaryVector, v3:SparseBinaryVector) = 
    this(v1.size * v2.size * v3.size, 
         outerProductArray(v1.ind, v1.activeDomainSize, v2.ind, v2.activeDomainSize, v2.size, v3.ind, v3.activeDomainSize, v3.size), 
         copyArray = false, 
         preSorted = true)

  /* init as flat outer product of vectors */
  def this(v1:SparseBinaryVector, v2:SparseBinaryVector, v3:SingletonBinaryVector) = 
    this(v1.size * v2.size * v3.size, 
         outerProductArray(v1.ind, v1.activeDomainSize, v2.ind, v2.activeDomainSize, v2.size, v3.singleIndex, v3.size), 
         copyArray = false, 
         preSorted = true)

  /* init as flat outer product of vectors */
  def this(v1:SparseBinaryVector, v2:SingletonBinaryVector, v3:SingletonBinaryVector) = 
    this(v1.size * v2.size * v3.size, 
         outerProductArray(v1.ind, v1.activeDomainSize, v2.singleIndex, v2.size, v3.singleIndex,  v3.size), 
         copyArray = false, 
         preSorted = true)

  def length = theLength
  def defaultInitialCapacity = 4
  protected[la] var ind: Array[Int] = null
  protected[la] var _size: Int = 0
  if (indices eq null) ind = new Array[Int](defaultInitialCapacity)
  else if (copyArray) { ind = new Array[Int](indices.length); System.arraycopy(indices, 0, ind, 0, ind.length); _size = ind.length }
  else { ind = indices; _size = ind.length }
  if (!preSorted && indices != null) scala.util.Sorting.quickSort(ind)

  def activeDomainSize = _size
  def activeDomain: Iterable[Int] = new IndexedSeq[Int] { def apply(i:Int) = ind(i); def length = _size }
  override def forActiveDomain(f: (Int)=>Unit): Unit = forIndex(_size)((i:Int) => f(ind(i)))
  /** Ensure that the array "ind" is big enough to allow ind(n). */
  private def ensureCapacity(n:Int): Unit = {
    if (ind.length - 1 < n) {
      var newsize = ind.length * 2
      while (newsize < n) { newsize *= 2; /*println("newsize "+newsize)*/ }
      //println("newsize = "+newsize+" ind.length="+ind.length+" n="+n+" _size="+_size)
      val new_indices = new Array[Int](newsize)
      Array.copy(ind, 0, new_indices, 0, ind.size)
      ind = new_indices
    }
  }

  /** Search the array 'ind' for the index at which value x could be inserted in sorted order.
      @param start the lowest index to consider
      @parm end one plus the highest index that already contains data
      Return the index into 'ind' such that ind(index) == x, 
      or ind(index-1) < x < ind(index)
      or index == end.
      */
  private def _positionLte(x:Int, start:Int, end:Int): Int = {
    //println("SparseBinaryVector._positionLte "+x+" "+start+" "+end+" diff="+diff)
    val diff = end - start
    if (diff == 0) return start
    if (diff == 1) return if (ind(start) >= x) start else end
    val middle = start + (diff / 2)
    val midval = ind(middle)
    if (midval == x) return middle
    else if (x < midval) _positionLte(x, start, middle)
    else _positionLte(x, middle+1, end)
  }

  /** Return true iff the integer 'index' is contains in ind.  */
  private def _contains(x:Int, start:Int, end:Int): Boolean = {
    // /println("SparseBinaryVector._contains "x+" "+start+" "+end+" diff="+diff)
    val diff = end - start
    if (diff == 0) return false
    if (diff == 1) return ind(start) == x
    val middle = start + (diff / 2)
    val midval = ind(middle)
    if (midval == x) return true
    else if (x < midval) _contains(x, start, middle)
    else _contains(x, middle+1, end)
  }
  //private var lastPosition = 0 // TODO Implement this speed-up, also used in scalala
  def apply(index:Int): Double = if (_contains(index, 0, _size)) 1.0 else 0.0
  def zero(): Unit = { _size = 0 }
  override def iterator = new Iterator[Double] {
    var i = -1
    var position = 0
    def hasNext = i < SparseBinaryVector.this.length
    def next = {
      i += 1
      if (ind(position) < i) position += 1
      if (ind(position) == i) 1.0 else 0.0
    }
  }
  def activeElements = new Iterator[(Int,Double)] {
    var i = -1
    def hasNext = i < _size - 1
    def next = { i += 1; (ind(i), 1.0) }
  }
  /** Add a value, (while keeping the content Array[Int] sorted). */
  def +=(theValue:Int): Unit = {
    val i = _positionLte(theValue, 0, _size)
    //println
    //println("  In Vector+=")
    //println("    > insert value: " + theValue)
    //println("    > indices:      " + ind.toList)
    //println("    > |indices|:    " + _size)
    if (i >= ind.length || ind(i) != theValue) {
      //println("    ===> inserting at index: " + i)
      ensureCapacity(_size)
      System.arraycopy(ind, i, ind, i+1, _size-i) // Shift later part of the array one position to make space
      ind(i) = theValue
      _size += 1
    } else if (i == _size) _size += 1 // Efficiently handle _size==0, theValue==0
  }
  // Removed because conflicts with cc.factorie.CategoricalBinaryVector.++=(Iterable[T])
  //def ++=(theValues:Iterable[Int]): Unit = theValues.foreach(this.+=(_))
  def dot(v:Vector): Double = v match {
    case v:DenseVector => {
      var i = 0; var result = 0.0
      while (i < _size) { result += v(ind(i)); i += 1 }; result
    }
    case v:SparseHashVector => {
      var i = 0; var result = 0.0
      while (i < _size) { result += v(ind(i)); i += 1 }; result
    }
    case v:SingletonBinaryVector => v dot this
    case v:SingletonVector => v dot this
    case v:VectorTimesScalar => v dot this
    case v:SparseBinaryVector => throw new Error("Not yet implemented.")
    case _ => throw new Error("SparseBinaryVector.dot does not handle "+v.getClass.getName)
  }

  override def toString = getClass.getName+"("+"len="+length+" 1s=("+ind.view(0, _size).mkString("[", ", ", "]")+")"

  override def flatOuter(v1:Vector, v2:Vector):Vector = (v1,v2) match {
    case (v1:SparseBinaryVector    ,v2:SparseBinaryVector)    => new SparseBinaryVector(this, v1, v2)
    case (v1:SparseBinaryVector    ,v2:SingletonBinaryVector) => new SparseBinaryVector(this, v1, v2)
    case (v1:SingletonBinaryVector ,v2:SparseBinaryVector)    => new SparseBinaryVector(this, v2, v1)
    case (v1:SingletonBinaryVector ,v2:SingletonBinaryVector) => new SparseBinaryVector(this, v1, v2)
  }

  override def flatOuter(v:Vector):Vector = v match {
    case that:SparseBinaryVector => new SparseBinaryVector(this, that)
    case that:SingletonBinaryVector => new SparseBinaryVector(this, that)
  }
}
