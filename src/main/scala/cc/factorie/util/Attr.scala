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

import scala.reflect.ClassTag

// TODO Why insist on AnyRef?  Why not just Any?  This would make app.nlp.DocumentProcessor a little cleaner. -akm

/** Provides member "attr" which is a map from a class to an attribute value (instance of that class).
    This is used to attach arbitrary "attributes" to objects that inherit from this trait.
    Conveniently these attributes do not need to be pre-compiled into the object as class members,
    and yet when fetched they are returned with the correct Scala type known.
    
    For example, attributes are used to attach a part-of-speech label to a cc.factorie.app.nlp.Token,
    to attach a ParseTree to a Sentence, and coreference information to a Document.
    
    Basic example usage: object foo extends Attr; foo.attr += "bar"; require(foo.attr[String] == "bar"); foo.attr.remove[String].
    
    @author Andrew McCallum */
trait Attr extends Serializable {
  /** A collection of attributes, keyed by the attribute class. */

  def getAttr = attr

  object attr extends Serializable {
    private var _attr: Array[AnyRef] = new Array[AnyRef](2)
    /** The number of attributes present. */
    def length: Int = { var i = 0; while ((i < _attr.length) && (_attr(i) ne null)) i += 1; i }
    /** The capacity of the array holding the attributes.  May be more than "length", the number of attributes present. */
    def capacity: Int = _attr.length
    private def setCapacity(cap:Int): Unit = { val ta = new Array[AnyRef](cap); System.arraycopy(_attr, 0, ta, 0, math.min(cap, math.min(cap, _attr.length))) }
    /** Make sure there is capacity of at least "cap"  */
    def ensureCapacity(cap:Int): Unit = if (cap > _attr.length) { val ta = new Array[AnyRef](cap); System.arraycopy(_attr, 0, ta, 0, _attr.length) }
    /** Increase capacity by "incr". */
    def increaseCapacity(incr:Int): Unit = { val ta = new Array[AnyRef](_attr.length+incr); System.arraycopy(_attr, 0, ta, 0, _attr.length); _attr = ta }
    /** Remove the attribute at index i. */
    def removeIndex(i:Int): Unit = {
      val len = length
      if (i == len - 1) _attr(i) = null
      else {
        System.arraycopy(_attr, i+1, _attr, i, len-i-1)
        _attr(len-1) = null
      }
    }
    /** Re-allocate the attribute array to remove any excess capacity */
    def trimCapacity(): Unit = { val l = length; if (l < _attr.length) setCapacity(l) }
    /** Add the given attribute, with key equal to its class. */
    def +=[C<:AnyRef](value:C): C = {
      var i = 0
      val key = value.getClass
      while (i < _attr.length && (_attr(i) ne null) && _attr(i).getClass != key)
        i += 1
      if (i == _attr.length)
        increaseCapacity(1)
      _attr(i) = value
      value
    }
    /** Returns the index of the last attribute whose class is assignable from the argument.
        Attributes occur in the order in which they were inserted.
        Note this means you can add a:MyClass, then add b:SubclassOfMyClass, then index[MyClass] will return the index of "b". */
    @inline final def index(key:Class[_]): Int = {
      var i = _attr.length - 1
      while (i >= 0) {
        if ((_attr(i) ne null) && key.isAssignableFrom(_attr(i).getClass))
          return i
        i -= 1
      }
      -1
    }
    /** Returns the index of the last attribute whose class is assignable from the argument.
        Attributes occur in the order in which they were inserted.
        Note this means you can add a:MyClass, then add b:SubclassOfMyClass, then index[MyClass] will return the index of "b". */
    @inline final def index[C<:AnyRef]()(implicit m: ClassTag[C]): Int = index(m.runtimeClass)
    /** Return the index of the last attribute whose class is exactly the argument.
        Attributes occur in the order in which they were inserted. */
    @inline final def indexExactly(key:Class[_]): Int = {
      var i = _attr.length - 1
      while (i >= 0) {
        if (key eq _attr(i).getClass) return i
        i -= 1
      }
      -1
    }
    /** Return true if there is an attribute of class equal to or subclass of the argument. */
    def contains[C<:AnyRef]()(implicit m: ClassTag[C]): Boolean = index(m.runtimeClass) >= 0
    /** Return true if there is an attribute of class equal to or subclass of the argument. */
    def contains(key:Class[_]): Boolean = index(key) >= 0
    /** Return true if there is an attribute of class exactly equal to the argument. */
    def containsExactly[C<:AnyRef]()(implicit m: ClassTag[C]): Boolean = indexExactly(m.runtimeClass) >= 0
    /** Return true if there is an attribute of class exactly equal to the argument. */
    def containsExactly(key: Class[_]): Boolean = indexExactly(key) >= 0

    /** Returns a sequence of all attributes with classes assignable to C (i.e. that are either C or a subclass of C). */
    def all[C<:AnyRef]()(implicit m: ClassTag[C]): Seq[C] = {
      val key = m.runtimeClass
      val result = new scala.collection.mutable.ArrayBuffer[C]
      var i = 0
      while (i < _attr.length) {
        if ((_attr(i) ne null) && key.isAssignableFrom(_attr(i).getClass)) result += _attr(i).asInstanceOf[C]
        i += 1
      }
      result
    }
    /** Remove all attributes with class matching or subclass of C.
        For example, to remove all attributes call remove[AnyRef].
        If call results in no removals, will not throw an Error. */
    def remove[C<:AnyRef](implicit m: ClassTag[C]): Unit = {
      val key = m.runtimeClass
      var i = 0
      while (i < _attr.length) {
        if ((_attr(i) ne null) && key.isAssignableFrom(_attr(i).getClass)) removeIndex(i)
        else i += 1
      }
    }
    /** Return a sequence of all attributes */
    def values: Seq[AnyRef] = {
      val result = new scala.collection.mutable.ArrayBuffer[AnyRef]
      var i = 0
      while (i < _attr.length) {
        if (_attr(i) ne null) result += _attr(i)
        i += 1
      }
      result
    }
    /** Fetch the first value associated with the given class.  If none present, return null. */
    def apply[C<:AnyRef]()(implicit m: ClassTag[C]): C = {
      var i = index(m.runtimeClass)
      if (i >= 0) _attr(i).asInstanceOf[C] else null.asInstanceOf[C]
    }
    /** Fetch the first value associated with the given class.  If none present, return null. */
    def apply[C<:AnyRef](key:Class[C]): C ={
      var i = index(key)
      if (i >= 0) _attr(i).asInstanceOf[C] else null.asInstanceOf[C]
    }

    /** Fetch the first attribute who class is exactly equal to the given class.  If none present, return null. */
    def exactly[C<:AnyRef]()(implicit m: ClassTag[C]): C = {
      var i = indexExactly(m.runtimeClass)
      if (i >= 0) _attr(i).asInstanceOf[C] else null.asInstanceOf[C]
    }
    def get[C<:AnyRef](implicit m: ClassTag[C]): Option[C] = {
      val result = this.apply[C]
      if (result ne null) Option(result) else None
    }
    def getOrElse[C<:AnyRef](defaultValue:C)(implicit m: ClassTag[C]): C = {
      val result = this.apply[C]
      if (result ne null) result else defaultValue
    }
    def getOrElseUpdate[C<:AnyRef](defaultValue: =>C)(implicit m: ClassTag[C]): C = {
      val result = this.apply[C]
      if (result ne null) result else {
        val value = defaultValue
        this += value
        value
      }
    }
   
    override def toString = values.mkString(" ")

  }

}

