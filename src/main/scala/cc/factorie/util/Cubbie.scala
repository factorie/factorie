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

import cc.factorie.la.Tensor

import scala.collection.mutable
import scala.reflect._

/**
 * A Cubbie provides typed access to an underlying Map data-structure. This map can come
 * from various sources, such as JSON objects or MongoDB objects.
 *
 * Cubbies are designed
 * to make access to (often persistent) data safe, easy and generic. This comes at
 * the price of efficiency, so clients should not access Cubbies in inner-loops
 * of computations. In practice this may require a further conversion step from Cubbies
 * into a more efficient representation.
 */
class Cubbie {
  thisCubbie =>

  def version: String = "1.0"
  def cubbieName: String = getClass.getName

  //  def this(map:scala.collection.mutable.HashMap[String,Any]) = { this(); this._map = map }
  // Managing raw underlying map that hold the data

  /**
   * The type of underlying Map. We use the generic scala mutable Map interface.
   * If clients want to use their own Map data structure (such as MongoDB maps) they
   * only need to provide a wrapper mutable.Map implementation around their data.
   */
  type MapType = mutable.Map[String, Any]

  /**
   * Set the underlying map for this cubbie.
   * @param map the map that will be used as underlying datastructure
   * @return this cubbie.
   */
  def setMap(map: MapType): this.type = {
    _map = map
    this
  }

  /**
   * Constructor that takes the underlying map.
   * @param map the underlying map to use.
   */
  def this(map: mutable.Map[String, Any]) = {
    this()
    this._map = map
  }

  /**
   * Underlying map member.
   * todo: fix map vs _map confusion one day.
   */
  private var __map: MapType = null


  /**
   * Returns underlying map.
   * todo: remove this
   * @return the current underlying map. If empty, a new map is created and set.
   */
  def _map = {
    if (__map == null) __map = new mutable.HashMap[String, Any]
    __map
  }

  /**
   * Alternative method to set underyling map.
   * todo: remove this.
   * @param map the underlying map.
   */
  def _map_=(map: mutable.Map[String, Any]) {
    __map = map
  }

  /**
   * Creates a default map.
   * @return a default map to be used as underyling map.
   */
  def _newDefaultMap: MapType = new scala.collection.mutable.HashMap[String, Any]
  
  /**
   * Prints out the underlying map
   * @return a string representation of the underyling map.
   */
  override def toString = _map.toString()

  /**
   * The map key to use for the ID field.
   * @return the key to use for the id field.
   */
  def idName = "_id"

  /**
   * Typed access to the cubbie class.
   * @return the Cubbie class.
   */
  def cubbieClass = getClass.asInstanceOf[Class[Cubbie]]

  /**
   * Create a new random ID.
   * @return random ID.
   */
  def newId = java.util.UUID.randomUUID.timestamp

  /**
   * Returns the ID of the cubbie. Creates a new ID if no ID has yet been set.
   * @return an identifier for this cubbie.
   */
  final def id: Any = {
    // "final" because we need to ensure that the _id gets inserted into the
    var result = _map(idName) // avoid getOrElseUpdate because it will allocate a closure object
    if (result != null) result
    else {
      result = newId
      _map.update(idName, result)
      result
    }
  }

  //todo: maps throw exceptions when key is not defined, need to adapt requirement
  /**
   * Set an id for this cubbie.
   * @param i an ID. Generally can be any type of object, but during serialization
   *          some types may not be storable by the underyling serialization framework.
   */
  def id_=(i: Any) {
    _map.update(idName, i)
  }

  // Classes for holding key:value pairs

  /**
   * A Cubbie has a collection of slots (or fields) that store the attributes of the
   * cubbie. These slots map one-to-one to the fields of the underlying map.
   * @tparam T the type of the attribute.
   */
  trait AbstractSlot[+T] {

    /**
     * The current value of the slot.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: T

    /**
     * Convenience method for more concise access to slot value.
     * @return value of the slot.
     */
    def apply() = value

    /**
     * The name of this slot. This corresponds to the map field the slot is using
     * to store its value.
     * @return
     */
    def name: String

    /**
     * Returns Some(value) if the cubbie has the given slot, or None otherwise.
     * @return Some(value) if the cubbie has the given slot, or None otherwise.
     */
    def opt: Option[T]

    /**
     * The cubbie this slot is contained in.
     * @return this cubbie.
     */
    def cubbie: thisCubbie.type = thisCubbie

  }

  /**
   * Every cubbie has an ID. This ID is itself a field of the underlying map, and hence
   * can also be accessed through a slot.
   */
  object Id extends AbstractSlot[Any] {
    def name = "_id"

    def value = id

    def opt = Some(id)

    def :=(newId: Any) {
      cubbie.id = newId
    }

    def set(newId: Any): thisCubbie.type = {
      this := newId
      thisCubbie
    }

    def apply(newId: Any): thisCubbie.type = {
      set(newId)
    }
  }

  //  val idSlot = new IdSlot

  /**
   * An AbstractInverseSlot is a slot that contains a collection of
   * other cubbies, namely the ones that have the given target value as
   * value of the given foreign slot. This type of slot is fundamentally
   * different to the standard slot, which stores its values in the underlying map.
   *
   * @tparam A the type of cubbies this slot contains.
   */
  sealed trait AbstractInverseSlot[+A <: Cubbie] {
    /**
     * The name of this slot.
     * @return a string name.
     */
    def name: String

    /**
     * The slot of the cubbies that needs to have a particular target value
     * for the cubbie to be in this slot.
     * @return the foreign slot.
     */
    def foreignSlot: Cubbie => A#AbstractSlot[Any]

    /**
     * Can there be several cubbies with the target value?
     * @return true iff there can be several cubbies with the given target value.
     */
    def unique: Boolean = false

    /**
     * What value does the foreign slot should have.
     * @return Some(target-value) or None if this cubbie has no values in this slot.
     */
    def target: Option[Any]
  }

  /**
   * The Inverse slot is a default implementation of the AbstractInverseSlot.
   * @param name the name for this slot.
   * @param slot the foreign slot.
   * @tparam A the type of cubbies this slot contains.
   */
  case class InverseSlot[A <: Cubbie:ClassTag](name: String,
                                      slot: A => A#AbstractRefSlot[Cubbie])
    extends AbstractInverseSlot[A] {

    /**
     * This method returns some set of cubbies associated with this slot based
     * on the provided cache object.
     * @param cache A mapping from cubbie slots to iterables of cubbies. The iterable
     *              for this  cubbie slot should correspond to a set of cubbies that
     *              have the given target value for the given slot.
     * @return all cubbies that are associated with this slot in the cache.
     */
    def value(implicit cache: Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]): Iterable[A] = {
      cache(this.asInstanceOf[InverseSlot[Cubbie]]).asInstanceOf[Iterable[A]]
    }

    //todo: this is probably very slow, as I need access the classtag, runtimeClass, create new object etc.
    def value2(implicit cache: collection.Map[(Class[Cubbie], String, Any), Iterable[Cubbie]]) = {
      val foreignCubbie = classTag[A].runtimeClass.newInstance().asInstanceOf[A]
      val foreignSlot = slot(foreignCubbie)
      cache((foreignCubbie.cubbieClass, foreignSlot.name, cubbie.id)).asInstanceOf[Iterable[A]]
    }

    def foreignSlot = (c: Cubbie) => slot(c.asInstanceOf[A])

    def target = Some(cubbie.id)

    def tag = classTag[A]

    def cubbie: thisCubbie.type = thisCubbie

  }

  /**
   * Default implementation of an AbstractSlot.
   * param name the name of the slot.
   * @tparam T the type of the attribute.
   */
  trait Slot[T] extends AbstractSlot[T] {
    val name: String
    def value: T

    /**
     * Set the value for this slot.
     * @param value value to set.
     */
    def :=(value: T)

    /**
     * Set a value for this slot but inform the provided hook before this happens.
     * @param value the value to set.
     * @param preHook the hook to call before setting the value.
     */
    def :=!(value: T)(implicit preHook: (Cubbie#AbstractSlot[Any], Any) => Unit) {
      preHook(this, value)
      this := value
    }

    /**
     * Set the value and return the containing Cubbie.
     * @param value the value to set.
     * @return the cubbie this slot belongs to.
     */
    def apply(value: T): thisCubbie.type = set(value)


    def opt = if (_map.isDefinedAt(name)) Some(value) else None

    /**
     * Set a raw value into the underlying map. Should generally only be used
     * in other library code.
     * @param value the value to be set.
     */
    def rawPut(value: Any) {
      _map.update(name, value)
    }

    /**
     * Does the cubbie have this slot.
     * @return true iff the underyling map has this slot.
     */
    def isDefined: Boolean = _map.isDefinedAt(name)

    /**
     * Set the value of this slot using an option. If parameter
     * is Some(value) the value is set, if None nothing is changed.
     * @param opt the option to use.
     */
    def :=(opt: Option[T]) {
      for (value <- opt) this := value
    }

    /**
     * Set value of slot and return this Cubbie.
     * @param value value to set.
     * @return this cubbie.
     */
    def set(value: T): thisCubbie.type = {
      this := value
      thisCubbie
    }

    /**
     * Set the value of this slot using an option. If parameter
     * is Some(value) the value is set, if None nothing is changed. Returns
     * this cubbie.
     * @param opt the option to use.
     * @return the encompassing cubbie.
     */
    def set(opt: Option[T]): thisCubbie.type = {
      for (value <- opt) this := value
      thisCubbie
    }

    override def toString = name + ":" + _map(name)
  }

  /**
   * A slot containing primitive values (ints, strings, booleans etc.).
   * param name the name of this slot.
   * @tparam T the type of the attribute.
   */
  trait PrimitiveSlot[T] extends Slot[T] {
    def value: T = _map(name).asInstanceOf[T]
    def :=(value: T): Unit = _map.update(name, value)
  }

  case class IntSlot(name: String) extends PrimitiveSlot[Int] {
    override def value = _map(name) match {
      case i: Int => i
      case d: Double => d.toInt
      case s: String => s.toInt
      case x: Any => x.asInstanceOf[Int]
    }
  }

  case class BooleanSlot(name: String) extends PrimitiveSlot[Boolean]

  case class DoubleSlot(name: String) extends PrimitiveSlot[Double] {
    override def value = _map(name) match {
      case d: Double => d
      case i: Int => i.toDouble
      case s: String => s.toDouble
      case x: Any => x.asInstanceOf[Double]
    }
  }

  case class StringSlot(name: String) extends PrimitiveSlot[String]

  case class DateSlot(name: String) extends PrimitiveSlot[java.util.Date]
  // TODO We need other primitive types supported in BSON

  // Note: This is not supported by cc.factorie.db.mongo.MongoCubbieCollection
  case class TensorSlot(name: String) extends PrimitiveSlot[Tensor]

  // These are specially handled in cc.factorie.db.mongo.MongoCubbieCollection
  case class IntSeqSlot(name:String) extends PrimitiveSlot[IntSeq] {
    override def value = _map(name) match {
      case s: Seq[Any] if s.isEmpty => new IntArrayBuffer()
      case x: Any => x.asInstanceOf[IntSeq]
    }
  }
  
  case class DoubleSeqSlot(name:String) extends PrimitiveSlot[DoubleSeq] {
    override def value = _map(name) match {
      case s: Seq[Any] if s.isEmpty => new DoubleArrayBuffer()
      case x: Any => x.asInstanceOf[DoubleSeq]
    }
  }


  /** This allows any type to be stored in a slot.  But note that BSON and JSON only support the above restricted set of types.
      FACTORIE uses this to store WeightsMap (because copying them to a DoubleListSlot would take too much memory for dependency parsing).
      So Cubbie serialization of DotFamily only works for PrintStream serialization, not BSON and JSON. */
  case class AnySlot[A<:Any](name:String) extends PrimitiveSlot[A]
  
  /**
   * A slot containing a list of primitives.
   * @tparam A the type of primitives the list contains.
   */
  trait PrimitiveListSlot[A] extends Slot[Seq[A]] {

    import collection.JavaConversions._

    /**
     * Returns the Seq stored in the underlying map. Does some internal conversion if
     * the underlying list is not a Seq but something convertible.
     * todo: remove the case statement here and rely on underlying map implementation
     *        to return the correct Seq representation.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: Seq[A] = _map(name) match {
      case s: Seq[A] => s
      case al: java.util.ArrayList[A @unchecked] => al.toSeq
      case m: java.util.Map[String @unchecked, A @unchecked] => Range(0, m.size).map(i => m.get(i.toString))
      case m: mutable.Map[String @unchecked, A @unchecked] => m.map(_._2).toSeq
      case null => null
    }

    def :=(value: Seq[A]) {
      _map.update(name, value)
    } // TODO Perhaps we should store a Map[String,Any] here instead, like BSON?  Avoids the need for conversion later
  }

  case class IntListSlot(name: String) extends PrimitiveListSlot[Int]

  case class BooleanListSlot(name: String) extends PrimitiveListSlot[Boolean]

  case class DoubleListSlot(name: String) extends PrimitiveListSlot[Double]

  case class StringListSlot(name: String) extends PrimitiveListSlot[String]

  case class TensorListSlot(name: String) extends PrimitiveListSlot[Tensor]


  /**
   * A slot that contains a list of cubbies.
   * @param name the name of the slot.
   * @param constructor the cubbie constructor that will be used to create cubbies for
   *                    underyling map objects.
   * @tparam A the type of the cubbies in the list.
   */
  case class CubbieListSlot[A <: Cubbie](name: String, constructor: () => A) extends Slot[Seq[A]] {

    /**
     * Returns the list of cubbies in this slot. The underlying map is expected to
     * contain a list of maps for the field corresponding to this slot. The slot
     * will then convert the maps to cubbies using the provided constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: Seq[A] = _map(name) match {
      case null => null
      case s: Seq[AnyRef @unchecked] => if (s.length == 0) Nil
      else s.map(m => {
        val c = constructor()
        c._map = m.asInstanceOf[MapType]
        c
      }) // The AnyRef is expected to be a Scala or Java Map
      //      case al:java.util.ArrayList[A] => if (al.size == 0) Nil else al.toSeq.map(m => { val c = constructor(); c._map = m; c }) // The AnyRef is expected to be a Scala or Java Map
    }

    /**
     * Set the sequence of cubbies. The slot will only store the corresponding list
     * of maps in its field.
     * @param value value to set.
     */
    def :=(value: Seq[A]) {
      _map.update(name, value.map(c => c._map))
    } // Actually put in the sequence of Maps, not sequence of Cubbies
  }


  /**
   * A RefSlot is a simple type of AbstractInverseSlot which "contains" all cubbies
   * of a given type with the given ID. Assuming uniqueness of IDs, it's a
   * unique inverse slot.
   *
   * The RefSlot field only stores the actual ID of the referenced cubbie. This means
   * that calling value or apply() on this slot returns (untyped) ids. Likewise,
   * the setter methods only take ids. Convenience methods exist to pass in
   * cubbies instead.
   *
   * @param name the name of the slot.
   * @param constructor the cubbie constructor for cubbies the slot contains.
   * @tparam A the type of cubbies this slot contains.
   */
  case class RefSlot[A <: Cubbie](name: String, constructor: () => A)
    extends Slot[Any] with AbstractRefSlot[A] with AbstractInverseSlot[A] {
    def value = _map(name)

    override def unique = true

    def foreignSlot = _.asInstanceOf[A].Id

    def target = opt

    def slot = (a: A) => a.Id

    def :=(ref: Any) {
      if (ref.isInstanceOf[Cubbie]) throw new Error("Use ::= to set RefSlot by a Cubbie")
      _map.update(name, ref)
    }

    def ::=(value: A) {
      _map.update(name, value.id)
    }
  }

  /**
   * A helper trait for libraries that need ref slots to be covariant.
   * @tparam A
   */
  trait AbstractRefSlot[+A <: Cubbie] extends AbstractSlot[Any] {

    /**
     * The value of a RefSlot is the id of the referenced cubbie. To get an actual cubbie, clients need
     * to call this method. It takes as implicit parameter a mapping from ids to cubbies. This is a deliberate
     * choice: the slot itself is not supposed to store any internal state/mapping to cubbies, it only stores
     * the id.
     * @param tr mapping from ids to cubbies.
     * @return the object associated with the given id in the given mapping.
     */
    def deref(implicit tr: scala.collection.Map[Any, Cubbie]) = tr(value).asInstanceOf[A]
    //    def ->(coll:MongoCubbieCollection[A]):GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(this,coll)
  }

  /**
   * A slot that contains a cubbie.
   * @param name the name of the slot.
   * @param constructor the constructor to create the contained cubbie wrapper with.
   * @tparam A the type of cubbie the slot contains.
   */
  case class CubbieSlot[A <: Cubbie](override val name: String, constructor: () => A) extends Slot[A] {
    /**
     * Return the cubbie contained in this slot. This assumes
     * that the underlying map has a corresponding field that contains a
     * map. This map is then wrapped with a cubbie created by the given constructor.
     * @return the value of slot. May throw an exception if the underlying map has
     *         no field corresponding to this slot.
     */
    def value: A = {
      val a = constructor()
      a._map = _map(name).asInstanceOf[MapType]
      a
    }

    /**
     * Stores the map underlying the passed in cubbie into the underyling map of this cubbie.
     * @param value value to set.
     */
    def :=(value: A) {
      _map.update(name, value._map)
    }
  }

}

