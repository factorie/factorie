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

/** A vector, for holding a sequence of Doubles and performing various linear algebra operations.
    See also @see cc.factorie.generative.Counts
    @author Andrew McCallum */
trait Vector extends scala.collection.mutable.IndexedSeq[Double] {
  def length: Int
  def activeDomainSize: Int
  def activeDomain: Iterable[Int]
  def forActiveDomain(f: (Int)=>Unit): Unit = activeDomain.foreach(f(_))
  def dot(v:Vector): Double
  def activeElements: Iterator[(Int,Double)]
  def oneNorm: Double = activeElements.foldLeft(0.0)(_ + _._2)
  def update(index:Int, value:Double): Unit = throw new Error("Method update not defined on class "+getClass.getName)
  def +=(v:Vector): Unit = throw new Error("Method +=(Vector) not defined on class "+getClass.getName)
  def +=(s:Double): Unit = throw new Error("Method +=(Double) not defined on class "+getClass.getName)
  def *(scalar:Double) = new VectorTimesScalar(this, scalar)
  // def toString = this.take(15).mkString(printName+"(", ",", if (length > 15) "...)" else ")")
  // override def toString = this.take(math.min(5, length)).mkString(getClass.getName+"(", ",", if (length > 5) "...)" else ")")
  def flatOuter(that:Vector): Vector = throw new Error("Method flatOuter(Vector) not defined on class "+getClass.getName)
  def flatOuter(v1:Vector, v2:Vector):Vector = throw new Error("Method flatOuter(Vector, Vector) not defined on class "+getClass.getName)
}

/** A lazy product of a Vector and a scalar.
    @author Andrew McCallum */
class VectorTimesScalar(val vector:Vector, val scalar:Double) extends Vector {
  def length = vector.length
  def activeDomainSize: Int = vector.activeDomainSize
  def activeDomain: Iterable[Int] = vector.activeDomain
  def dot(v:Vector): Double = vector.dot(v) * scalar
  def activeElements: Iterator[(Int,Double)] = new Iterator[(Int,Double)] {
    val iter = vector.activeElements
    def hasNext = iter.hasNext
    def next = { val n = iter.next; (n._1, n._2 * scalar) }
  }
  def apply(index:Int) = vector.apply(index) * scalar
}


/** A Vector that has all zeros, except one position containing a 1.0.
    @author Andrew McCallum */
class SingletonBinaryVector(val theLength:Int, val singleIndex:Int) extends Vector {
  def length = theLength
  def activeDomainSize = 1
  def activeDomain: Iterable[Int] = Seq(singleIndex)
  override def forActiveDomain(f: (Int)=>Unit): Unit = f(singleIndex)
  def apply(index:Int): Double = if (index == singleIndex) 1.0 else 0.0
  def dot(v:Vector) = v(singleIndex)
  def activeElements = Iterator.single((singleIndex, 1.0))

  override def flatOuter(v:Vector):Vector = v match {
    case that:SparseBinaryVector => 
      new SparseBinaryVector(that, this)
    case that:SingletonBinaryVector => 
      new SingletonBinaryVector(this.size * that.size, this.singleIndex * that.size + that.singleIndex)
  }

  override def flatOuter(v1:Vector, v2:Vector):Vector = (v1,v2) match {
    case (v1:SparseBinaryVector    ,v2:SparseBinaryVector)    => new SparseBinaryVector(v1, v2, this)
    case (v1:SparseBinaryVector    ,v2:SingletonBinaryVector) => new SparseBinaryVector(v1, v2, this)
    case (v1:SingletonBinaryVector ,v2:SparseBinaryVector)    => new SparseBinaryVector(v2, this, v1)
    case (v1:SingletonBinaryVector ,v2:SingletonBinaryVector) => 
      new SingletonBinaryVector(this.size * v1.size * v2.size,
                                (this.singleIndex * v1.size + v1.singleIndex) * v2.size + v2.singleIndex)
  }
}

/** A Vector that has all zeros, except one position containing some arbitrary Double 'value'.
    @author Andrew McCallum */
class SingletonVector(val theLength:Int, val singleIndex:Int, val value:Double) extends Vector {
  var default = 0.0
  def length = theLength
  def activeDomainSize = 1
  def activeDomain: Iterable[Int] = Seq(singleIndex)
  override def forActiveDomain(f: (Int)=>Unit): Unit = f(singleIndex)
  def apply(index:Int): Double = if (index == singleIndex) value else default
  def dot(v:Vector) = v(singleIndex) * value
  def activeElements = Iterator.single((singleIndex, value))
}

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

object OuterProductMath {
  /** All pairwise computations for SingletonBinaryVector and SparseBinaryVectors */
  /* note: singleton * singleton is done inline in flatOuter() */

  /* sparse * sparse*/
  def outerProductArray(a1: Array[Int], s1:Int, a2: Array[Int], s2:Int, a2width:Int) = {
    val arr = new Array[Int](s1 * s2)
    var i = 0; var n = 0
    while (i<s1) {
      var j = 0
      val m = a1(i) * a2width
      while (j<s2) {
        arr(n) = m + a2(j)
        j +=1
        n += 1
      }
      i += 1
    }
    arr
  }

  /* sparse * singleton */
  def outerProductArray(a1: Array[Int], s1:Int, i2: Int, a2width:Int) = {
    val arr = new Array[Int](s1)
    var i = 0
    while (i<s1) {
      arr(i) = a1(i) * a2width + i2
      i +=1
    }
    arr
  }

  /** All three-way computations for SingletonBinaryVector and SparseBinaryVector combinations */
  /* note: singleton * singleton * singleton is done inline in flatOuter() */


  /* sparse * sparse * singleton */
  def outerProductArray(a1: Array[Int], s1: Int,              // sparse
                        a2: Array[Int], s2: Int, a2width:Int, // sparse
                        i3: Int, i3width:Int): Array[Int]     // singleton
  = {
    val arr = new Array[Int](s1 * s2)
    var i1 = 0; var n = 0
    while (i1<s1) {
      var i2 = 0
      val m1 = a1(i1) * a2width
      while (i2<s2) {
        val m2 = (m1 + a2(i2)) * i3width
        arr(n) = m2 + i3
        n += 1
        i2 += 1
      }
      i1 += 1
    }
    arr
  }  

  /* sparse * singleton * singleton */
  def outerProductArray(a1: Array[Int], s1: Int,          // sparse
                        i2: Int, i2width:Int,             // singleton
                        i3: Int, i3width:Int): Array[Int] // singleton
  = {
    val arr = new Array[Int](s1)
    var i1 = 0
    while (i1<s1) {
      val m1 = a1(i1) * i2width
      val m2 = (m1 + i2) * i3width + i3
      arr(i1) = m2
      i1 += 1
    }
    arr
  }  

  /* sparse * sparse * sparse */
  def outerProductArray(a1: Array[Int], s1:Int,                          // sparse
                        a2: Array[Int], s2:Int, a2width:Int,             // sparse
                        a3: Array[Int], s3:Int, a3width:Int): Array[Int] // sparse
  = {
    val arr = new Array[Int](s1 * s2 * s3)
    var i1 = 0; var n = 0
    while (i1<s1) {
      var i2 = 0
      val m1 = a1(i1) * a2width
      while (i2<s2) {
        var i3 = 0
        val m2 = (m1 + a2(i2)) * a3width
        while (i3<s3) {
          arr(n) = m2 + a3(i3)
          i3 += 1
          n += 1
        }
        i2 += 1
      }
      i1 += 1
    }
    arr
  }
}

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory.
    @author Andrew McCallum */
class SparseVector(size:Int) extends SparseHashVector(size) {
  def this(size:Int, occurrences:Seq[Int]) = { this(size); occurrences.foreach(increment(_, 1.0)) }
  //private var used = 0
  //private var capacity = 8
  //private val indices = new Array[Int](capacity)
  //private val values = new Array[Double](capacity)
  //private def ensureCapacity(c:Int): Unit = {}
}

/** A Vector that may contain mostly zeros, with a few arbitrary non-zeros, represented compactly in memory,
    implemented as a HashMap from Int indices to Double values.
    @author Andrew McCallum */
class SparseHashVector(val length:Int) extends Vector {
  var default = 0.0
  private val h = new scala.collection.mutable.HashMap[Int,Double] { override def default(index:Int) = SparseHashVector.this.default }
  def apply(index:Int) = h(index)
  def activeElements = h.iterator
  def activeDomainSize = h.size
  def activeDomain: Iterable[Int] = h.keys
  override def forActiveDomain(f: (Int)=>Unit): Unit = h.keys.foreach(f(_))
  override def update(index:Int, value:Double) = h(index) = value
  def increment(index:Int, incr:Double): Unit = h(index) = h(index) + incr
  def dot(v:Vector): Double = v match {
    case dv:DenseVector => {
      var result = 0.0
      h.iterator.foreach({case(index,value) => result += dv(index) * value})
      result
    }
    case v:SparseBinaryVector => v dot this
    case v:VectorTimesScalar => v dot this
    case v:SingletonBinaryVector => v dot this
    case v:SingletonVector => v dot this
    case sv:SparseHashVector => {
      var result = 0.0
      if (v.size > this.size) h.iterator.foreach({case(index,value) => result += sv(index) * value})
      else sv.h.iterator.foreach({case(index,value) => result += h(index) * value})
      result
    }
    case _ => throw new Error("SparseHashVector.dot does not handle "+v.getClass.getName)
  }
  override def +=(v:Vector): Unit = v.activeElements.foreach({case(index,value) => h.update(index, h(index) + value)})
  override def +=(s:Double): Unit = {
    default += s
    h.keys.foreach(index => h.update(index, h(index) + s))
  }
}

/** A Vector that may contain arbitrary Double values, represented internally as an Array[Double].
    @author Andrew McCallum */
class DenseVector(val length:Int) extends Vector {
  private val a = new Array[Double](length)
  def activeDomainSize = a.size
  def activeDomain = new Range(0, length, 1)
  override def forActiveDomain(f: (Int)=>Unit): Unit = forIndex(length)(f(_))
  def apply(index:Int): Double = a(index)
  override def update(index:Int, value:Double): Unit = a(index) = value
  def set(value:Double): Unit = java.util.Arrays.fill(a, value)
  def activeElements = new Iterator[(Int,Double)] {
    var i = -1
    def hasNext = i < DenseVector.this.length - 1
    def next = { i += 1; (i, a(i)) }
  }
  def dot(v:Vector): Double = v match {
    case v:DenseVector => {
      var result = 0.0
      var i = 0
      while (i < a.size) { result += a(i)*v(i); i += 1 }
      result
    }
    case _ => v dot this
  }
  override def +=(v:Vector): Unit = for ((index,value) <- v.activeElements) a(index) += value
  override def +=(s:Double): Unit = {
    var i = 0
    while (i < a.size) { a(i) += s; i += 1 }
  }
}

/** Provides a convenient constructor for DenseVector objects.
    @author Andrew McCallum */
object DenseVector {
  def apply(size:Int)(default:Double) = { 
    val result = new DenseVector(size)
    result.set(default)
    result
  }
}

/*
class ConcatenatedVector(val vectors:Seq[Vector]) extends Vector {
  val activeDomainSizes = vectors.map(_.activeDomainSize)
  val activeDomainSize = vectors.reduceLeft(_+_.activeDomainSize)
  def activeDomain = Range(0, activeDomainSize)
  def dot(v:Vector): Double = throw new Error("Not yet implemented.")
  def activeElements: Iterator[(Int,Double)] = throw new Error("Not yet implemented.")
  def update(index:Int, value:Double): Unit = {
    val maxLowerIndex = domainsSizes.find(_)
  }
}
*/
