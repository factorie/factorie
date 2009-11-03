package cc.factorie.util

/** A Seq represented as a doubly-linked list of This.  
 * Any one element represents the Seq of itself and all following links.  If you want the full collection, use this.first... */
// TODO Consider changing this to that "length" and "elements" always include all elements of the collection?
trait LinkList[This >: Null <: LinkList[This]] extends AnyRef with Seq[This] {
	this : This =>
	var prev:This = null
	var next:This = null
 
	def hasNext = next ne null
	def hasPrev = prev ne null
  def hasNext(n:Int) : Boolean = { var s = this; for (i <- 0 until n) { if (s.next eq null) return false; s = s.next }; return true }
  def hasPrev(n:Int) : Boolean = { var s = this; for (i <- 0 until n) { if (s.prev eq null) return false; s = s.prev }; return true }
 
	def lengthToFirst : Int = if (prev eq null) 0 else 1 + prev.lengthToFirst
	def lengthToLast  : Int = if (next eq null) 0 else 1 + next.lengthToFirst
 	def length: Int = 1 + lengthToLast
  override def size: Int = 1 + lengthToLast
  override def first: This = if (prev eq null) this else prev.first
  override def last: This = if (next eq null) this else next.last
  def apply(i:Int): This = next(i)
	
	def next(n:Int): This =
		if (n == 0) this
		else if (next eq null) throw new IndexOutOfBoundsException("unknown element")
		else next.next(n - 1)
	
	def getNext(n:Int): Option[This] =
		if (n == 0) Some(this)
		else if (next eq null) None
		else next.getNext(n - 1)
  
  def prev(n:Int): This =
    if (n == 0) this
    else if (prev eq null) throw new IndexOutOfBoundsException("unknown element")
    else prev.prev(n - 1)
    
  def getPrev(n:Int): Option[This] =
    if (n == 0) Some(this)
    else if (prev eq null) None
    else prev.getPrev(n - 1)

  /** Return an Iterator over all links after and including this. */
  override def elements: Iterator[This] = new Iterator[This] {
  	var elems = LinkList.this
  	def hasNext = (elems ne null)
  	def next = { val res = elems; elems = elems.next; res }
  }
  
  /** Return an iterator over all links before this, in reverse order, starting with this.prev */
  def prevElements: Iterator[This] = new Iterator[This] {
  	var elems = LinkList.this.prev
  	def hasNext = (elems ne null)
  	def next = { val res = elems; elems = elems.prev; res }
  }
  
  /** Cut the links to the "prev" side of this */
  def trimPrev : This = {
    if (prev ne null) prev.next = null
    prev = null
    this
  }
  
  /** Cut the links to the "next" side of this. */
  def trimNext : This = {
    if (next ne null) next.prev = null
    next = null
    this
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
		that.append(next)
		next = that
		that.prev = this
	}
 
  /** Insert list "that" before this link */
	def preInsert(that:This): Unit = if (prev ne null) prev.postInsert(that) else prepend(that)
 
	/** Returning this.first permits usage such as token = new Token() prepend token */
	def prepend(that:This): This = if (that eq null) this.first else {
	  if (that.prev != null) throw new IllegalArgumentException("Trying to prepend the middle of another LinkList")
		if (prev eq null) {
		  val last = that.last
			prev = last
			that.last.next = this
		} else prev.prepend(that)
		this.first
  }

	def remove() : This = {
		if (next ne null)	next.prev = prev
		if (prev ne null)	prev.next = next
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
	/** Swap link "this" with link "that" */
	def swapWith(that:This): Unit = if (that ne this) {
	  val thisNext = next
	  val thisPrev = prev
	  if (prev != null) prev.next = that
	  if (next != null) next.prev = that
	  if (that.prev != null) that.prev.next = this
	  if (that.next != null) that.next.prev = this
	  next = that.next
	  prev = that.prev
	  that.next = thisNext
	  that.prev = thisPrev
	}
 
	def swapWithNext: Unit =
		if (next eq null) throw new IllegalStateException("No next with which to swap.")
		else swapWith(next)
 
	def swapWithNext(n:Int): Unit = swapWith(this.next(n))

	def swapNextPrev(length:Int): Unit = {
	  var s = prev(length)
	  var t = next
	  for (i <- 0 until length) { s.swapWith(t); s = s.next; t = t.next } 
	}
 
}
