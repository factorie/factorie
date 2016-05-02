/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */


package cc.factorie.util

// List of (index,count), sorted in descending order by count, stored sparsely
// Useful in Proportions where sampling is efficient because outcomes are considered in order of highest-count first.
// TODO Consider making a Growable version of this.
class SortedSparseCounts(dim:Int, capacity:Int = 2, val keepTrimmed:Boolean = false) {
  /** Initialize counts from an unsorted list of indices */
  def this(dim:Int, initial:Array[Int], keepDense: Boolean) = {
    this(dim, initial.length)
    dbuf = new Array[Int](dim)
    var nonZeroCount = 0
    _countsTotal = initial.length
    var i = initial.length - 1
    while (i >= 0) {
      val j = initial(i)
      if (dbuf(j) == 0) nonZeroCount += 1
      dbuf(j) += 1
      i -= 1
    }
    i = 0
    while (i < dim) {
      if (dbuf(i) > 0) {
        buf(siz) = coti(dbuf(i), i)
        siz += 1
      }
      i += 1
    }
    trim()
    if (!keepDense) dbuf = null
    // TODO Sort more efficiently
    for (i <- 1 until siz) bubbleDownFrom(i)
    //assert(countsTotal == calculatedCountsTotal) // TODO Remove this
    //assert(check, counts.toString)
  }
  def this(dim:Int, initial:Array[Int]) = this(dim, initial, false)
  require(dim > 1)

  val _length: Int = dim
  def numPositions: Int = siz
  private var _countsTotal: Int = 0 // total of all counts in buf
  def countsTotal = _countsTotal
  def calculatedCountsTotal = (0 until siz).foldLeft(0)((sum,i) => sum + co(buf(i))) // just for error checking
  // Make sure we have enough bits to represent the dimension of the multinomial
  private val topicMask = if (java.lang.Integer.bitCount(dim) == 1) dim-1 else java.lang.Integer.highestOneBit(dim) * 2 - 1
  private val topicBits = java.lang.Integer.bitCount(topicMask)
  private var siz = 0 // number of used entries in buf
  private var dbuf: Array[Int] = null // dense buf, allocated only if keepDense=true
  private var buf = new Array[Int](capacity) // stores both count and topic packed into a single Int, indexed by pos
  //def buffer = buf // TODO Remove this method
  protected def ensureCapacity(cap:Int): Unit = {
    if (buf.length < cap) {
      val newbuf = new Array[Int](cap+1) // allocate 1 extra space
      System.arraycopy(buf, 0, newbuf, 0, buf.length)
      buf = newbuf
    }
  }
  def trim(): Unit = {
    if (siz < buf.length-1) { // Don't bother if we'll only save 1 space
      val newbuf = new Array[Int](siz)
      System.arraycopy(buf, 0, newbuf, 0, siz)
      buf = newbuf
    }
  }
  require (dim < Short.MaxValue)
  //private val _posIndex: Array[Short] = if (keepIndex) Array.fill[Short](dim)(-1) else null
  @inline private def ti(coti:Int) = coti & topicMask // topic from packed count&index 
  @inline private def co(coti:Int) = coti >> topicBits // count from packed count&index
  @inline private def coti(count:Int, index:Int): Int = { assert(index < _length); (count << topicBits) | index }
  def activeIndices = new TruncatedArrayIntSeq(buf.take(siz).map(ti), siz) // Note that the IntSeq contents could be changed out from under it!
  protected def bubbleDownFrom(pos:Int): Unit = {
    val newb = buf(pos)
    var i = pos - 1
    while (i >= 0 && buf(i) < newb) {
      val tmp = buf(i); buf(i) = newb; buf(i+1) = tmp // swap
      i -= 1
    }
  }
  protected def bubbleUpFrom(pos:Int): Unit = {
    //assert(check, counts.toString)
    //val prevCounts = new scala.collection.mutable.ArrayBuffer[String]; prevCounts += counts.toString
    val newb = buf(pos)
    var i = pos + 1
    while (i < siz && buf(i) > newb) {
      val tmp = buf(i); buf(i) = newb; buf(i-1) = tmp // swap
      //prevCounts += counts.toString
      i += 1
    }
    //assert(check, "pos="+pos+" newb=("+ti(newb)+","+co(newb)+")\n"+prevCounts.mkString("\n")+"\n"+counts.toString)
  }
  def deletePosition(pos:Int): Unit = {
    require(co(buf(pos)) == 0) // otherwise we need to adjust _countsTotal and dbuf(index) = 0
    if (pos < siz - 1) System.arraycopy(buf, pos+1, buf, pos, siz-(pos+1))
    siz -= 1
    if (keepTrimmed && siz < buf.length - 3) { // Only try shrinking if we have 3 extra spaces
      val newbuf = new Array[Int](siz+1) // Allocate 1 extra space
      System.arraycopy(buf, 0, newbuf, 0, siz)
      buf = newbuf
    }
  }
  def countAtPosition(pos:Int) = co(buf(pos))
  def indexAtPosition(pos:Int) = ti(buf(pos))
  def incrementCountAtPosition(pos:Int, incr:Int): Unit = {
    //val prevCounts = counts.toString
    //val prevTi = ti(buf(pos))
    //assert(check, prevCounts)
    if (dbuf ne null) dbuf(ti(buf(pos))) += incr
    //val newb = buf(pos) + (incr << topicBits)
    val newCount = co(buf(pos)) + incr
    val newb = coti(newCount, ti(buf(pos)))
    //assert(ti(newb) == prevTi)
    buf(pos) = newb
    _countsTotal += incr
    assert(newCount >= 0)
    if (newCount == 0) deletePosition(pos)
    else if (incr > 0) bubbleDownFrom(pos)
    else if (incr < 0) bubbleUpFrom(pos)
    //assert(countsTotal == calculatedCountsTotal) // TODO Remove this
    //assert(check, "\npos="+pos+" incr="+incr+" newCount="+newCount+"\n"+prevCounts+"\n"+counts.toString) // TODO Remove this
  }

  def incrementCountsAtPositions(pos1:Int, incr1:Int, pos2:Int, incr2:Int) {

    assert(pos1 != pos2, "Positions should be different!")

    val ti1 = ti(buf(pos1))
    val ti2 = ti(buf(pos2))

    if (dbuf ne null) {
      dbuf(ti1) += incr1
      dbuf(ti2) += incr2
    }

    val newCount1 = co(buf(pos1)) + incr1
    val newCount2 = co(buf(pos2)) + incr2

    val newb1 = coti(newCount1, ti1)
    val newb2 = coti(newCount2, ti2)

    //buf(pos1) = newb1
    //buf(pos2) = newb2

    _countsTotal += (incr1 + incr2)
    assert(newCount1 >= 0 && newCount2 >= 0)

    //TODO: deletePosition() requires the element getting deleted to be equal to 0.
    // That's why we are currently setting the values even though they will be deleted. Fix this.
    if (newCount1 == 0 & newCount2 ==0){
      buf(pos1) = newb1
      deletePosition(pos1)

      val newPos2 = if(pos2 > pos1) pos2 - 1 else pos2
      buf(newPos2) = newb2
      deletePosition(newPos2)

    } else if(newCount1 == 0 && newCount2 != 0){
      buf(pos2) = newb2
      val changeInPos1 = if (incr2 > 0) bubbleDownFrom(pos2, pos1) else if (incr2 < 0) bubbleUpFrom(pos2, pos1) else 0

      val newPos1 = pos1 + changeInPos1
      buf(newPos1) = newb1
      deletePosition(newPos1)

    } else if(newCount1 != 0 && newCount2 == 0){
      buf(pos1) = newb1
      val changeInPos2 = if (incr1 > 0) bubbleDownFrom(pos1, pos2) else if (incr1 < 0) bubbleUpFrom(pos1, pos2) else 0

      val newPos2 = pos2 + changeInPos2
      buf(newPos2) = newb2
      deletePosition(newPos2)

    } else {
      buf(pos1)  = newb1
      val changeInPos2 = if (incr1 > 0) bubbleDownFrom(pos1, pos2) else if (incr1 < 0) bubbleUpFrom(pos1, pos2) else 0

      val newPos2 = pos2 + changeInPos2
      buf(newPos2) = newb2
      if (incr2 > 0) bubbleDownFrom(newPos2) else if (incr2 < 0) bubbleUpFrom(newPos2)
    }

  }

  //TODO: merge this function with the other bubbleDownFrom()
  protected def bubbleDownFrom(pos:Int, otherPos:Int): Int = {
    val newb = buf(pos)
    var i = pos - 1
    var changeInOtherPos = if (otherPos < pos && buf(otherPos) < newb) 1 else 0
    while (i >= 0 && buf(i) < newb) {
      val tmp = buf(i); buf(i) = newb; buf(i+1) = tmp // swap
      i -= 1
    }

    changeInOtherPos
  }

  //TODO: merge this function with the other bubbleUpFrom()
  protected def bubbleUpFrom(pos:Int, otherPos:Int): Int = {
    //assert(check, counts.toString)
    //val prevCounts = new scala.collection.mutable.ArrayBuffer[String]; prevCounts += counts.toString
    val newb = buf(pos)
    var i = pos + 1
    var changeInOtherPos = if(otherPos > pos && buf(otherPos) > newb) -1 else 0
    while (i < siz && buf(i) > newb) {
      val tmp = buf(i); buf(i) = newb; buf(i-1) = tmp // swap
      //prevCounts += counts.toString
      i += 1
    }

    //assert(check, "pos="+pos+" newb=("+ti(newb)+","+co(newb)+")\n"+prevCounts.mkString("\n")+"\n"+counts.toString)
    changeInOtherPos
  }

  // TODO Make this do binary search instead of linear search
  def positionOfIndex(index:Int): Int = {
    var i = 0
    while (i < siz) {
      if (ti(buf(i)) == index) return i
      i += 1
    }
    -1
  }
  def countOfIndex(index:Int): Int = {
    if (dbuf ne null) {
      dbuf(index)
    } else {
      val pos = positionOfIndex(index)
      if (pos == -1) 0 else co(buf(pos))
    }
  }
  def incrementCountAtIndex(index:Int, incr:Int): Unit = {
    //assert(check, counts.toString)
    //val prevCounts = counts.toString
    val pos = positionOfIndex(index)
    if (pos == -1) {
      if (incr <= 0) {
        System.err.println(this.counts.toString())
        throw new Error("index="+index+" count="+incr) // TODO just test "incr"
      }
      ensureCapacity(siz+1)
      buf(siz) = coti(incr, index)
      if (dbuf ne null) { /* assert(dbuf(index) == 0);*/ dbuf(index) = incr }
      _countsTotal += incr
      siz += 1
      bubbleDownFrom(siz-1)
      //println("SortedSparseCounts pos="+pos+" siz="+siz+" coti="+coti(incr, index))
      //assert(countsTotal == calculatedCountsTotal, "ct="+countsTotal+" cct="+calculatedCountsTotal) // Remove this
      /*if (!check) {  // TODO Remove this
        println(prevCounts)
        println(counts.toString)
        assert(false)
      }*/
    } else {
      incrementCountAtPosition(pos, incr)
      //assert(check, counts.toString) // TODO Remove this
    }
  }
  // Next method for CountsProportions trait
  def clear(): Unit = {
    siz = 0
    _countsTotal = 0
    if (dbuf ne null) java.util.Arrays.fill(dbuf, 0)
  }

  def counts: Iterable[(Int,Int)] = // (count,index)
    for (i <- 0 until siz) yield (ti(buf(i)), co(buf(i)))
  def forCounts(f:(Int,Int)=>Unit): Unit = 
    for (i <- 0 until siz) f(ti(buf(i)), co(buf(i)))
  // I don't want there to be any cc.factorie._ dependencies in here. -akm
  /*def printCounts(domain:CategoricalDomain[String]): Unit = {
    for (i <- 0 until siz) print(domain.getCategory(ti(buf(i)))+"="+co(buf(i))+" ")
    println
  }*/
  /** Return false if there is internal inconsistency */
  def check: Boolean = {
    return true
    for (i <- 0 until siz-1) {
      val b1 = buf(i); val b2 = buf(i+1)
      val ti1 = ti(b1); val ti2 = ti(b2) 
      val co1 = co(b1); val co2 = co(b2)
      if (ti1 == ti2) return false
      if (co1 < co2) return false
      if (co1 == 0 || co2 == 0) return false
    }
    if (siz == 1 && co(buf(0)) == 0) return false // To catch the siz==1 case 
    true
  }

  def copyBuffer(srcCounts: SortedSparseCounts) {
    siz = srcCounts.siz
    if (siz > buf.length) buf = new Array[Int](siz)
    System.arraycopy(srcCounts.buf, 0 , buf, 0, siz)
  }
}

