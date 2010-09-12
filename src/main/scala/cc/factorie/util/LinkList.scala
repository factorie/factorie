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



package cc.factorie.util

/** A Seq represented as a doubly-linked list of This.  
    Any one element represents the Seq of all the links of its sequence.  
    WARNING: It can be confusing that the "the sequence" and 
    "individual elements of the sequence" are represented by the same object.
    @author Andrew McCallum
    @see VarInMutableSeq
 */
trait LinkList[This >: Null <: LinkList[This]] extends AnyRef with Seq[This] {
  this : This =>
  var prev:This = null
  var next:This = null
 
  def hasNext = next ne null
  def hasPrev = prev ne null
  def hasNext(n:Int) : Boolean = { var s = this; for (i <- 0 until n) { if (s.next eq null) return false; s = s.next }; return true }
  def hasPrev(n:Int) : Boolean = { var s = this; for (i <- 0 until n) { if (s.prev eq null) return false; s = s.prev }; return true }
 
  def lengthToFirst : Int = if (prev eq null) 0 else 1 + prev.lengthToFirst
  def position = lengthToFirst
  def lengthToLast  : Int = if (next eq null) 0 else 1 + next.lengthToLast
  def length: Int = 1 + first.lengthToLast
  override def size: Int = length
  override def first: This = if (prev eq null) this else prev.head
  override def last: This = if (next eq null) this else next.last
  def apply(i:Int): This = first.nextBy(i)
  
  def nextBy(n:Int): This =
    if (n == 0) this
    else if (next eq null) throw new IndexOutOfBoundsException("unknown element")
    else next.nextBy(n - 1)
  
  def getNextBy(n:Int): Option[This] =
    if (n == 0) Some(this)
    else if (next eq null) None
    else next.getNextBy(n - 1)
  
  def prevBy(n:Int): This =
    if (n == 0) this
    else if (prev eq null) throw new IndexOutOfBoundsException("unknown element")
    else prev.prevBy(n - 1)
    
  def getPrevBy(n:Int): Option[This] =
    if (n == 0) Some(this)
    else if (prev eq null) None
    else prev.getPrevBy(n - 1)

  /** Return an Iterator over all links in the sequence of which this is a member. */
  override def iterator: Iterator[This] = new Iterator[This] {
    var elems = LinkList.this.head
    def hasNext = (elems ne null)
    def next = { val res = elems; elems = elems.next; res }
  }

  // TODO Consider changing name to be more consistent with Scala 2.8 "iterator" method name.
  /** Return an iterator over all links before this, in order, starting with this.prev */
  def prevElements: Iterator[This] = new Iterator[This] {
    var elems = LinkList.this.next
    def hasNext = (elems ne null)
    def next = { val res = elems; elems = elems.next; res }
  }
  
  /** Return an iterator over all links after this, in reverse order, starting with this.next */
  def nextElements: Iterator[This] = new Iterator[This] {
    var elems = LinkList.this.prev
    def hasNext = (elems ne null)
    def next = { val res = elems; elems = elems.prev; res }
  }
  
  /** Cut the links to the "prev" side of this.  Returns the original this.prev */
  def trimPrev: This = {
    val ret = this.prev
    if (prev ne null) prev.next = null
    prev = null
    ret
  }
  
  /** Cut the links to the "next" side of this.  Returns the original this.next */
  def trimNext: This = {
    val ret = this.next
    if (next ne null) next.prev = null
    next = null
    ret
  }

  /** Put all the links in the collection "that" at end of the sequence "this". */
  def append(that:This): Unit =
    if (that eq null) ()
    else if (that.prev != null) throw new IllegalArgumentException("Trying to append the middle of another LinkList") // TODO alternatively we could allow for weaves of lists
    else if (next eq null) {
      next = that
      that.prev = this
    } else
      next.append(that)

  /** Insert list "that" after this link */
  def postInsert(that:This): Unit = if (that ne null) {
    if (that.prev != null) throw new IllegalArgumentException("Trying to insert the middle of another LinkList")
    that.append(trimNext)
    next = that
    that.prev = this
  }
 
  /** Insert list "that" before this link */
  def preInsert(that:This): Unit = if (prev ne null) prev.postInsert(that) else prepend(that)
 
  /** Returning this.head permits usage such as: val token = null; token = new Token() prepend token; */
  def prepend(that:This): This = if (that eq null) this.head else {
    if (that.prev != null) throw new IllegalArgumentException("Trying to prepend the middle of another LinkList")
    if (prev eq null) {
      val last = that.last
      prev = last
      that.last.next = this
    } else prev.prepend(that)
    this.head
  }

  def remove() : This = {
    if (next ne null) next.prev = prev
    if (prev ne null) prev.next = next
    prev = null
    next = null
    this
  }
 
  def remove(n:Int) : This = if (n == 1) remove else {
    assert (n > 0)
    val last = this.next(n)
    val nnex = last.next // the next node after last
    if (prev ne null) prev.next = nnex
    if (nnex ne null) nnex.prev = prev
    prev = null
    last.next = null
    this
  }
 
  /** Replace the single link "this" with the collection of links beginning with "that" */
  def replaceWith(that:This): Unit = if (that ne this) {
    if (that.prev != null) throw new IllegalArgumentException("Trying to replaceWith the middle of another LinkList")
    that.last.next = next
    that.prev = prev
    if (next ne null) next.prev = that.last
    if (prev ne null) prev.next = that
    prev = null
    next = null
  }

  // TODO consider prepending all method names that operate on individual links with "link" ??
  /** Swap the single link "this" with the single link "that" */
  def swapWith(that:This): Unit = if (that ne this) {
    val origLength = this.head.length 
    val thisNext = next
    val thisPrev = prev
    if (this.next eq that) { // Neighbors: this, that
      if (prev != null) prev.next = that
      if (that.next != null) that.next.prev = this
      next = that.next
      prev = that
      that.prev = thisPrev
      that.next = this
    } else if (that.next eq this) { // Neighbors: that, this
      if (next != null) next.prev = that
      if (that.prev != null) that.prev.next = this
      next = that
      prev = that.prev
      that.prev = this
      that.next = thisNext
    } else {
      if (prev != null) prev.next = that
      if (next != null) next.prev = that
      if (that.prev != null) that.prev.next = this
      if (that.next != null) that.next.prev = this
      next = that.next
      prev = that.prev
      that.next = thisNext
      that.prev = thisPrev
    }
    assert(origLength == this.head.length && origLength == that.head.length)
  }
 
  def swapWithNext: Unit =
    if (next eq null) throw new IllegalStateException("No next with which to swap.")
    else swapWith(next)
 
  def swapWithNext(n:Int): Unit = swapWith(this.next(n))

  /** Swap the 'length' tokens preceeding this with hthe 'length' tokens following this, preserving the order of each of the spans swapped. */
  def swapNextPrev(length:Int): Unit = {
    var s = prev(length)
    var t = next
    for (i <- 0 until length) { s.swapWith(t); s = s.next; t = t.next } 
  }
 
}
