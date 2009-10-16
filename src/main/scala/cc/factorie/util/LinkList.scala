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

    override def elements: Iterator[This] = new Iterator[This] {
  	var elems = LinkList.this
  	def hasNext = (elems ne null)
  	def next = { val res = elems; elems = elems.next; res }
  }
  
  def prevElements: Iterator[This] = new Iterator[This] {
  	var elems = LinkList.this.prev
  	def hasNext = (elems ne null)
  	def next = { val res = elems; elems = elems.prev; res }
  }

	def append(that:This): Unit =
		if (that eq null) ()
		else if (that.prev != null) throw new IllegalArgumentException("trying to append the middle of another DLinkedList") // TODO alternatively we could allow for weaves of lists
		else if (next eq null) {
			next = that
			that.prev = this
		} else
			next.append(that)

	def insert(that:This): Unit = if (that ne null) {
		that.append(next)
		next = that
		that.prev = this
	}
 
	def insertBefore(that:This): Unit = if (prev ne null) prev.insert(that) else prepend(that)
 
	/** Returning this.last permits usage such as token = new Token() prepend token */
	def prepend(that:This): This = if (that eq null) this.last else {
		if (prev eq null) {
		  val last = that.last
			prev = last
			that.last.next = this
		} else prev.prepend(that)
		this.last
  }

	def remove() {
		if (next ne null)	next.prev = prev
		if (prev ne null)	prev.next = next
		prev = null
		next = null
  }
 
	// TODO consider renaming to some form of "update"
	def replaceWith(that:This): Unit = if (that ne this) {
	  if (next ne null) next.prev = that
	  if (prev ne null) prev.next = that
	  prev = null
	  next = null
	}
 
	def swapWithNext: Unit =
		if (next eq null) throw new IllegalStateException("No next with with to swap.")
		else {
		  val n = next
		  replaceWith(n)
		  n.insert(this)
		}
 
	def swapWithNext(n:Int): Unit = if (n == 1) swapWithNext else if (n != 0) { 
	  val s = next(n)
	  val p = s.prev
	  replaceWith(s)
	  p.insert(this)
	}

	def pivot(pivot:Int, length:Int): Unit = {
	  ()
	}
 
}
