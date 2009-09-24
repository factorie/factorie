package cc.factorie.util

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import scala.collection.mutable._;

/**
 * Class that mimics Java's string indexer, but for anything.
 *
 * Two extra views are provided: the index.synchronized view
 * enables threadsafe access and the index.immutable view keeps
 * prevents the (view) from being updated. 
 *
 * @author dlwh, dramage, with a few changes by mccallum
 */
//@serializable 
trait Index[T] extends (T => Int) with Collection[T] {
	/**Forward map from int to object */
	private val objects = new ArrayBuffer[T]

	/**Map from object back to int index */
	private lazy val indices = Map[T, Int]()

	/**If positive, throw error if Index reaches size larger than this.  Use for growable multi-dim Factor weights */
	var maxSize = -1
	// TODO consider putting the following method back in later -akm
	//override def maxSize_=(s:Int) : Unit = if (maxSize >= size) maxSize = s else throw new Error("Trying to set maxSize smaller than size.")

	/**If true, do not allow this Index to change. */
	private var frozen = false

	def freeze = {frozen = true; this}

	/**The size others might want to allocate to hold data relevant to this Index.  If maxSize is set can be bigger than size. */
	def allocSize = if (maxSize < 0) size else maxSize

	override def size = indices.size

	override def elements = objects.elements

	def contains(entry: T) = indices.contains(entry)

	override def apply(entry: T) = index(entry)

	def unapply(pos: Int): Option[T] = if (pos < size) Some(get(pos)) else None

	/**Return an object at the given position or throws an exception if it's not found. */
	def get(pos: Int): T = objects(pos)

	/**Return a densely-packed positive integer index for the given object.  By default,
	allocate a new index (at the end) if the object was not found, but if immutable may return -1 */
	def index(entry: T) : Int = {
		def nextMax = {
			val m = objects.size
			if (maxSize > 0 && m >= maxSize) throw new Error("Index size exceeded maxSize")
			objects += entry;
			m
		}
		if (frozen) indices.getOrElse(entry, -1)
		else indices.getOrElseUpdate(entry, nextMax);
	}
 
	/** Like index, but throw an exception if the entry is not already there. */
	def getIndex(entry:T) : Int = indices.getOrElse(entry, throw new Error("Entry not present; use index() to cause a lookup."))

	/**Override indexOf's slow, deprecated behavior. */
	override def indexOf[B >: T](elem: B): Int = index(elem.asInstanceOf[T]);

	/**Clears the index. */
	def clear() = {indices.clear(); objects.clear(); }

	// These accessors are kept separate to preserve collection subtype
	def indexAll(c: Iterator[T]) = c map apply;
	def indexAll(c: Iterable[T]) = c map apply;
	def indexAll(c: Collection[T]) = c map apply;
	def indexAll(c: List[T]) = c map apply;
	def indexAll(c: Array[T]) = c map apply;
	def indexAll(c: Set[T]) = c map apply;

	def indexKeys[V](c: scala.collection.Map[T, V]) = Map[T, V]() ++ c.map {case (a, b) => (this(a), b)}

	def indexValues[K](c: scala.collection.Map[K, T]) = Map[K, T]() ++ c.map {case (a, b) => (a, this(b))}

	def getAll(c: Iterator[Int]) = c map unapply;
	def getAll(c: Iterable[Int]) = c map unapply;
	def getAll(c: Collection[Int]) = c map unapply;
	def getAll(c: List[Int]) = c map unapply;
	def getAll(c: Array[Int]) = c map unapply;
	def getAll(c: Set[Int]) = c map unapply;

	// Index views.

	/**Returns an immutable view of the index. */
	def immutable: Index[T] = {
		val outer = this;
		new Index[T] {
			override def elements = outer.elements;
			override def size = outer.size;
			override def get(pos: Int) = outer.get(pos);
			override def index(t: T) = outer.indices.getOrElse(t, -1);
			override def clear = {};
		}
	}

	/**Returns a synchronized view of the index. */
	def synchronized: Index[T] = {
		val outer = this;
		new Index[T] {
			override def elements = outer.elements;
			override def size = outer.size;
			override def get(pos: Int) = synchronized {outer.get(pos); }

			override def index(t: T) = synchronized {outer.index(t); }

			override def clear = synchronized {outer.clear; }
		}
	}
}

/**
 * Utilities for manipulating and creating Index objects.
 */
object Index extends Index[Any] {
	/**Constructs an empty index. */
	def apply[T](): Index[T] = new Index[T] {};

	/**Constructs an Index from some elements. */
	def apply[T](elements: Iterator[T]): Index[T] = {
		val index = new Index[T] {};
		// read through all elements now -- don't lazily defer evaluation
		for (element <- elements) {
			index.index(element);
		}
		return index;
	}

	/**Constructs an Index from some elements. */
	def apply[T](iterable: Iterable[T]): Index[T] = {
		val index = new Index[T] {};
		// read through all elements now -- don't lazily defer evaluation
		for (element <- iterable) {
			index.index(element);
		}
		return index;
	}

	/**
	 * Loads a String index, one line per item with line
	 * numbers (starting at 0) as the indices.
	 */
	def load(source: {def getLines: Iterator[String]}): Index[String] = {
		apply(source.getLines.map(_.stripLineEnd));
	}
}
