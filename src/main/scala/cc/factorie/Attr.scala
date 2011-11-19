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
    def +=[C<:AnyRef](value:C): C = { this(value.getClass) = value; value }
    def -=[C<:AnyRef](value:C): C = { super.-=(value.getClass); value }
    def remove[C<:AnyRef](implicit m: Manifest[C]): Unit = super.-=(m.erasure)
  }
}

