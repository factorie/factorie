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

package cc.factorie

/** Provides member "attr" which is a map from class to an attribute value (instance of that class).
    For example: object foo extends Attr; foo.attr += "bar"; require(foo.attr[String] == "bar"); foo.attr.remove[String] */
trait Attr {
  object attr extends scala.collection.mutable.ListMap[Class[_],AnyRef] {
    def apply[C<:AnyRef]()(implicit m: Manifest[C]): C = this(m.erasure).asInstanceOf[C]
    def get[C<:AnyRef](implicit m: Manifest[C]): Option[C] = this.get(m.erasure).asInstanceOf[Option[C]]
    def getOrElse[C<:AnyRef](defaultValue:C)(implicit m: Manifest[C]): Option[C] = this.getOrElse(m.erasure, defaultValue).asInstanceOf[Option[C]]
    def getOrElseUpdate[C<:AnyRef](defaultValue:C)(implicit m: Manifest[C]): Option[C] = this.getOrElseUpdate(m.erasure, defaultValue).asInstanceOf[Option[C]]
    def +=[C<:AnyRef](value:C): C = { this(value.getClass) = value; value }
    def -=[C<:AnyRef](value:C): C = { super.-=(value.getClass); value }
    def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = super.-=(m.erasure)
  }
}

trait Attr2 {
  object attr {
    private var _attr: Array[AnyRef] = new Array[AnyRef](2)
    def length: Int = { var i = 0; while ((i < _attr.length) && (_attr(i) ne null)) i += 1; i }
    def setCapacity(cap:Int): Unit = { val ta = new Array[AnyRef](cap); System.arraycopy(_attr, 0, ta, 0, math.min(cap, math.min(cap, _attr.length))); ta }
    def growCapacity(cap:Int): Unit = if (cap > _attr.length) { val ta = new Array[AnyRef](cap); System.arraycopy(_attr, 0, ta, 0, _attr.length); ta }
    def increaseCapacity(incr:Int): Unit = { val ta = new Array[AnyRef](_attr.length+incr); System.arraycopy(_attr, 0, ta, 0, _attr.length); ta }
    def trim: Unit = { val l = length; if (l < _attr.length) setCapacity(l) }
    def +=[C<:AnyRef](value:C): C = {
      var i = 0
      while (i < _attr.length && _attr(i).getClass.isAssignableFrom(value.getClass))
        i += 1
      if (i == _attr.length)
        increaseCapacity(1)
      _attr(i) = value
      value
    }

  }
}