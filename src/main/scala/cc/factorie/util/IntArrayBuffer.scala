package cc.factorie.util
import scala.collection.IndexedSeq
import scala.collection.Seq

/** Embeds an efficient array of Ints, with convenient _append, _prepend, etc. */
trait ProtectedIntArrayBuffer { 
  protected def _initialCapacity = 4
  private var _arr = new Array[Int](_initialCapacity)
  private var _size = 0
  protected def _setCapacity(cap:Int): Unit = {
    require(cap >= _size)
    val newArray = new Array[Int](cap)
    if (_size > 0) compat.Platform.arraycopy(_arr, 0, newArray, 0, _size)
    _arr = newArray
  }
  protected def _capacityGrowthFactor: Double = 1.5
  protected def _ensureCapacity(cap:Int): Unit = if (cap > _arr.length) _setCapacity(math.max(cap, (_arr.length * _capacityGrowthFactor).toInt))
  protected def _considerShrinking: Unit = if (_size > 0 && _arr.length > _size * 2) _setCapacity(_size)
  protected def _reduceToSize(newSize:Int): Unit = { _size = newSize; _considerShrinking }
  @inline final protected def _array: Array[Int] = _arr // Carefully, dangerous to access directly
  @inline final protected def _length = _size
  @inline final protected def _apply(index:Int): Int = _arr(index)
  @inline final protected def _update(index:Int, value:Int): Unit = _arr(index) = value
  @inline final protected def _append(elem: Int): this.type = { _ensureCapacity(_size + 1); _arr(_size) = elem; _size += 1; this }
  protected def _toSeq: Seq[Int] = _arr.toSeq
  protected def _clear(): Unit = { _reduceToSize(0) }
  protected def _sizeHint(len: Int) = if (len >= _size && len >= 1) _setCapacity(len)
  protected def _set(xs: Array[Int]): Unit = { _ensureCapacity(xs.length); System.arraycopy(xs, 0, _arr, 0, xs.length); _size = xs.length }
  protected def _set(xs: Seq[Int]): Unit = { _ensureCapacity(xs.length); var i = xs.length; while (i >= 0) { _arr(i) = xs(i); i += 1 }; _size = xs.length }
  protected def _appendAll(xa: Array[Int]): Unit = { _ensureCapacity(_size + xa.length); System.arraycopy(xa, 0, _arr, _size, xa.length); _size += xa.length }
  protected def _appendAll(xs: TraversableOnce[Int]): Unit = {
    val n = xs.size
    _ensureCapacity(_size + n)
    xs.foreach(i => { _arr(_size) = i; _size += 1 })
  }
  protected def _prepend(elem: Int): Unit = {
    _ensureCapacity(_size + 1)
    System.arraycopy(_arr, 0, _arr, 1, _size)
    _arr(0) = elem
    _size += 1
  }
  protected def _prependAll(xs: TraversableOnce[Int]): Unit = _insertAll(0, xs.toTraversable)
  protected def _insertAll(n: Int, seq: scala.collection.Traversable[Int]): Unit = {
    if (n < 0 || n > _size) throw new IndexOutOfBoundsException(n.toString)
    val xs = seq.toList
    val len = xs.length
    _ensureCapacity(_size + len)
    System.arraycopy(_arr, n, _arr, n+len, _size-n)
    xs.copyToArray(_arr, n)
    _size += len
  }
  protected def _remove(n: Int, count: Int) {
    require(count >= 0, "removing non-positive number of elements")
    if (n < 0 || n > _size - count) throw new IndexOutOfBoundsException(n.toString)
    System.arraycopy(_arr, n + count, _arr, n, _size - (n + count))
    _reduceToSize(_size - count)
  }
  protected def _remove(n: Int): Unit = _remove(n, 1)
}

class IntArrayBuffer extends ProtectedIntArrayBuffer with IndexedSeq[Int] {
  def this(initialCapacity:Int) = { this(); _setCapacity(initialCapacity) }
  def +=(i:Int): Unit = _append(i)
  def ++=(is:Array[Int]): Unit = _appendAll(is)
  def +=:(i:Int): Unit = _prepend(i)
  def apply(index:Int): Int = _apply(index)
  def update(index:Int, value:Int): Unit = _update(index, value)
  def length: Int = _length
}
