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

import scala.reflect.Manifest;
import scala.collection.immutable._;

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
 
 
/**
 * Heterogenously Typed Map, where each type has exactly one value of that
 * type. Example:
 * <p/>
 * <code>
 *  val map = TypeMap() + 3 + "test" + List(1,2) + List("2","3")
 *  assert(map.get[Int] == 3); // ok
 *  map.get[Float] // compile time error
 *  map.get[List[Int]] // List(1,2)
 *  map.get[List[String]] // List(2,3)
 * </code>
 * 
 * @author dlwh
 * mccallum@cs.umass.edu changed name to TypeMap in cc.factorie.
 */


class TypeMap[+T] (private val map: Map[String,Any]) {
  /** Get a value corresponding to the given type from the map.*/
  def get[U>:T](implicit m: Manifest[U]) = map.get(m.toString).asInstanceOf[Option[U]].get;
  
  /** Get a value corresponding to the given type from the map.*/
  def apply[U>:T]()(implicit m: Manifest[U]) = get[U];
  
  /** Add or replace a value with the corresponding *static* type to the map. */
  def +[U](x: U)(implicit m: Manifest[U]) = new TypeMap[T with U](map + (m.toString->x))
  
  /** 
   * Replace a value with the corresponding *static* type to the map.
   * This is mostly to keep the inferred type as short as possible.
   */
  def -+[U>:T](x:U)(implicit m: Manifest[U]) = new TypeMap[T] (map + (m.toString->x));
  
  /**
   * Applys f to this and combines the result with this map.
   */
  def and[U](f: TypeMap[T]=>U)(implicit m: Manifest[U]): TypeMap[T with U] = {
    val newPart = f(this); 
    this.+[U](newPart);
  }
  
  /** Concatenates two maps. values in "other" override values in "this" */
  def ++[U](other: TypeMap[U]): TypeMap[T with U] = {
    new TypeMap[T with U](this.map ++ other.map);
  }
  
  override def toString = map.mkString("TypeMap(",",",")");
}
 
/**
 * Utilities for constructing TypeMaps
 */
object TypeMap {
  /**
   * Construct an empty TypeMap. Note that this allows you to successfully
   * compile an extraction of "Any" from this map, but it won't succeed 
   * at run time.
   */
  def apply() = new TypeMap[Any](Map.empty);
  
  /**
   * Construct an TypeMap with one initial value
   */
  def apply[T](x:T)(implicit m: Manifest[T]) = new TypeMap[T](Map.empty + (m.toString->x))
  
  /**
   * Construct an TypeMap with two initial values
   */
  def apply[T,T2](x:T, x2:T2)(implicit m: Manifest[T], m2: Manifest[T2]) = {
    new TypeMap[T with T2](Map.empty ++ List( (m.toString->x) , (m2.toString->x2)))
  }
  
  /**
   * Construct an TypeMap with three initial values
   */
  def apply[T,T2,T3](x:T, x2:T2, x3: T3)(implicit m: Manifest[T], m2: Manifest[T2], m3: Manifest[T3]) = {
    new TypeMap[T with T2 with T3](Map.empty ++ List( (m.toString->x) , (m2.toString->x2), (m3.toString->x3)))
  }
}
 
