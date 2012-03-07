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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MutableMap}

// Property, ala NeXTStep PropertyLists, used for JSON-like serialization©
// Call it "Tyson" for "Typed JSON"
// or "Tymap" for "Typed Map"
// or just call it a Facade afterall; it is a "facade" of a Map, but it is the core data of a Document---"opposite of a facade"
// Its purpose is to interact with serialization, so perhaps "Smap" or "Sermap"
// Perhaps "Cubbie" and "IntSlot"!

// Design goal:  You can initialize with cubbie._map = DBObject and it just works without conversion
//  although other direction: cubbie._map => DBObject will require some conversion
class Cubbie { thisCubbie =>
  import scala.collection.mutable.Map
  def this(map:scala.collection.mutable.HashMap[String,Any]) = { this(); this._map = map }
//  def this(map:java.util.HashMap[String,Any]) = { this(); this._map = map }
  // Managing raw underlying map that hold the data
  def setMap(map: MapType): this.type = { _map = map; this }
  type MapType = MutableMap[String, Any]
  var _map: MapType = null
  def _newDefaultMap: AnyRef = new scala.collection.mutable.HashMap[String,Any]
  def _rawGet(name:String): Any = {_map(name)}
  def _rawGetOrElse(name:String, default: =>Any) = _map.getOrElse(name, default)
  def _rawGetOrElseUpdate(name:String, default: =>Any) = _map.getOrElseUpdate(name, default)
  def _rawPut(name:String, value:Any) {_map(name) = value }
  // Access to collection of all map contents
  def elements: Iterator[(String,Any)] = _map.iterator
  override def toString = toString(0, elements)
  def toString(indent:Int, elements: Iterator[(String,Any)]): String = _map.toString()
    // Managing the "id"; aligns with MongoDB "_id"
  def idName = "_id"
  def newId = java.util.UUID.randomUUID.timestamp
  final def id: Any = { // "final" because we need to ensure that the _id gets inserted into the
    var result = _rawGet(idName) // avoid getOrElseUpdate because it will allocate a closure object
    if (result != null) result
    else { result = newId; _rawPut(idName, result); result }
  }
  //todo: maps throw exceptions when key is not defined, need to adapt requirement
  def id_=(i:Any): Unit = { /*require(_rawGet(idName) == null); */_rawPut(idName, i) }
  // Classes for holding key:value pairs

  trait AbstractSlot[+T] {
    def value:T
    def name:String
  }



  abstract class Slot[T](val name:String) extends AbstractSlot[T] {
    def value: T
    def :=(value:T): Unit
    def raw: Any = _rawGet(name)
    def rawPut(value:Any) {_rawPut(name,value)}
    def isDefined: Boolean = null != _rawGet(name)
    def :=(opt:Option[T]): Unit = for (value <- opt) this := (value)
    def set(value:T): thisCubbie.type = { this := value; thisCubbie }
    def set(opt: Option[T]): thisCubbie.type = { for (value <- opt) this := value; thisCubbie }
    def cubbie:thisCubbie.type = thisCubbie
    override def toString = name+":"+_rawGet(name)
  }
  abstract class PrimitiveSlot[T](n:String) extends Slot[T](n) {
    def value: T = _rawGet(name).asInstanceOf[T]
    def :=(value:T): Unit = _rawPut(name, value)
  }
  //case class AnySlot(override val name:String) extends PrimitiveSlot[Any](name) // Too dangerous?
  case class IntSlot(override val name:String) extends PrimitiveSlot[Int](name)
  case class BooleanSlot(override val name:String) extends PrimitiveSlot[Boolean](name)
  case class DoubleSlot(override val name:String) extends PrimitiveSlot[Double](name)
  case class StringSlot(override val name:String) extends PrimitiveSlot[String](name)
  case class DateSlot(override val name:String) extends PrimitiveSlot[java.util.Date](name)
  // TODO We need other primitive types supported in BSON
  abstract class PrimitiveListSlot[A](override val name:String) extends Slot[Seq[A]](name) {
    import collection.JavaConversions._
    def value: Seq[A] = _rawGet(name) match {
      case s:Seq[A] => s
      case al:java.util.ArrayList[A] => al.toSeq
      case m:java.util.Map[String,A] => Range(0, m.size).map(i => m.get(i.toString))
      case m:Map[String,A] => m.map(_._2).toSeq
      case null => null
    }
    def :=(value:Seq[A]) = _rawPut(name, value) // TODO Perhaps we should store a Map[String,Any] here instead, like BSON?  Avoids the need for conversion later
  }
  case class IntListSlot(override val name:String) extends PrimitiveListSlot[Int](name)
  case class BooleanListSlot(override val name:String) extends PrimitiveListSlot[Boolean](name)
  case class DoubleListSlot(override val name:String) extends PrimitiveListSlot[Double](name)
  case class StringListSlot(override val name:String) extends PrimitiveListSlot[String](name)
  case class CubbieListSlot[A<:Cubbie](override val name:String, constructor: ()=>A) extends Slot[Seq[A]](name) {
    import collection.JavaConversions._
    def value: Seq[A] = _rawGet(name) match {
      case null => null
      case s:Seq[AnyRef] => if (s.length == 0) Nil else s.map(m => { val c = constructor(); c._map = m.asInstanceOf[MapType]; c }) // The AnyRef is expected to be a Scala or Java Map
//      case al:java.util.ArrayList[A] => if (al.size == 0) Nil else al.toSeq.map(m => { val c = constructor(); c._map = m; c }) // The AnyRef is expected to be a Scala or Java Map
    }
    def :=(value:Seq[A]) = _rawPut(name, value.map(c => c._map)) // Actually put in the sequence of Maps, not sequence of Cubbies
  }
  case class RefSlot[A<:Cubbie](override val name:String, constructor:()=>A) extends Slot[Any](name) {
    def value = _rawGet(name)
    def :=(ref:Any): Unit = { if (ref.isInstanceOf[Cubbie]) throw new Error("Use ::= to set RefSlot by a Cubbie"); _rawPut(name, ref) }
    def ::=(value:A): Unit = _rawPut(name, value.id)
    def deref(implicit tr:CubbieRefs) = tr(value).asInstanceOf[A]
  }
  case class CubbieSlot[A<:Cubbie](override val name:String, constructor: ()=>A) extends Slot[A](name) {
    def value: A = { val a = constructor(); a._map = _rawGet(name).asInstanceOf[MapType]; a }
    def :=(value:A) = _rawPut(name, value._map)
  }
}

// Also make a version of this that caches objects as they come out of MongoDB  
class CubbieRefs extends scala.collection.mutable.HashMap[Any,Cubbie]


