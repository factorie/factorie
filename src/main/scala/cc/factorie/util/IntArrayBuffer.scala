package cc.factorie.util
import scala.collection.IndexedSeq
import scala.collection.Seq

/** Embeds an efficient array of Ints, with convenient _append, _prepend, etc. */
trait ProtectedIntArrayBuffer { 
  protected def _initialCapacity = 4
  private var _arr = new Array[Int](_initialCapacity)
  private var _size = 0
  @inline final protected def _setCapacity(cap:Int): Unit = {
    if (_arr.length != cap) {
      require(cap >= _size && cap >= 0)
      val newArray = new Array[Int](cap)
      if (_size > 0) compat.Platform.arraycopy(_arr, 0, newArray, 0, _size)
      _arr = newArray
    }
  }
  protected def _capacityGrowthFactor: Double = 1.5
  @inline final protected def _ensureCapacity(cap:Int): Unit = 
    if (cap > _arr.length) _setCapacity(math.max(cap, (_arr.length * _capacityGrowthFactor).toInt))
  protected def _considerShrinkingCapacity: Unit = if (_size > 0 && _arr.length > _size * 2) _setCapacity(_size)
  protected def _trimCapacity: Unit = _setCapacity(_size) // What if _size == 0?
  protected def _reduceToSize(newSize:Int): Unit = { _size = newSize; _considerShrinkingCapacity }
  @inline final protected def _array: Array[Int] = // Carefully, dangerous to access directly 
    if (_size == _arr.length) _arr 
    else { val a = new Array[Int](_size); System.arraycopy(_arr, 0, a, 0, _size); a }
  @inline final protected def _length = _size
  @inline final protected def _apply(index:Int): Int = _arr(index)
  @inline final protected def _update(index:Int, value:Int): Unit = _arr(index) = value
  @inline final protected def _append(elem: Int): this.type = { _ensureCapacity(_size + 1); _arr(_size) = elem; _size += 1; this }
  protected def _copyToArray(a:Array[Int]): Unit = System.arraycopy(_arr, 0, a, 0, _size)
  protected def _mapToArray[A](a:Array[A], f:Int=>A): Unit = { var i = 0; while (i < _size) { a(i) = f(_arr(i)); i += 1 }; a }
  protected def _toSeq: IndexedSeq[Int] = new IndexedSeq[Int] {
    private val arr = new Array[Int](_size); System.arraycopy(_arr, 0, arr, 0, _size)
    final def length = arr.length
    final def apply(i:Int) = arr(i)
  }
  protected def _clear(): Unit = { _reduceToSize(0) }
  protected def _sizeHint(len: Int) = if (len >= _size && len >= 1) _setCapacity(len)
  protected def _set(elts: Array[Int]): Unit = { _ensureCapacity(elts.length); System.arraycopy(elts, 0, _arr, 0, elts.length); _size = elts.length }
  protected def _set(elts: Seq[Int]): Unit = { _ensureCapacity(elts.length); var i = elts.length; while (i >= 0) { _arr(i) = elts(i); i += 1 }; _size = elts.length }
  protected def _appendAll(elts: Array[Int]): Unit = {
    _ensureCapacity(_size + elts.length)
    System.arraycopy(elts, 0, _arr, _size, elts.length)
    _size += elts.length
    _setCapacity(_size) // assuming won't soon be adding more, save memory & make _array more efficient
  }
  protected def _appendAll(elts: TraversableOnce[Int]): Unit = {
    val n = elts.size
    _ensureCapacity(_size + n)
    elts.foreach(i => { _arr(_size) = i; _size += 1 })
    _setCapacity(_size) // assuming won't soon be adding more, save memory & make _array more efficient
  }
  protected def _prepend(elt: Int): Unit = {
    _ensureCapacity(_size + 1)
    System.arraycopy(_arr, 0, _arr, 1, _size)
    _arr(0) = elt
    _size += 1
  }
  protected def _prependAll(elts: TraversableOnce[Int]): Unit = _insertAll(0, elts.toTraversable)
  protected def _insert(index: Int, elt:Int): Unit = {
    _ensureCapacity(_size + 1)
    System.arraycopy(_arr, index, _arr, index + 1, _size - index)
    _arr(index) = elt
  }
  protected def _insertAll(index: Int, seq: scala.collection.Traversable[Int]): Unit = {
    if (index < 0 || index > _size) throw new IndexOutOfBoundsException(index.toString)
    val xs = seq.toList
    val len = xs.length
    _ensureCapacity(_size + len)
    System.arraycopy(_arr, index, _arr, index+len, _size-index)
    xs.copyToArray(_arr, index)
    _size += len
  }
  protected def _remove(index: Int, count: Int) {
    require(count >= 0, "removing non-positive number of elements")
    if (index < 0 || index > _size - count) throw new IndexOutOfBoundsException(index.toString)
    System.arraycopy(_arr, index + count, _arr, index, _size - (index + count))
    _reduceToSize(_size - count)
  }
  protected def _remove(n: Int): Unit = _remove(n, 1)
}

class IntArrayBuffer extends ProtectedIntArrayBuffer with IndexedSeq[Int] {
  def this(initialCapacity:Int) = { this(); _setCapacity(initialCapacity) }
  def +=(i:Int): Unit = _append(i)
  def ++=(is:Array[Int]): Unit = _appendAll(is)
  def ++=(is:Seq[Int]): Unit = _appendAll(is)
  def +=:(i:Int): Unit = _prepend(i)
  def insert(index:Int, elt:Int): Unit = _insert(index, elt)
  def apply(index:Int): Int = _apply(index)
  def update(index:Int, value:Int): Unit = _update(index, value)
  def length: Int = _length
}
