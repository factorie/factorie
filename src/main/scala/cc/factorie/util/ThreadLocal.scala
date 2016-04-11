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

import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConversions._

class ThreadLocal[T](thunk: => T) {
  private val _instances: scala.collection.concurrent.Map[Long, T] = new ConcurrentHashMap[Long, T]
  def get: T = _instances.getOrElseUpdate(Thread.currentThread().getId, thunk)
  def instances = _instances.values
}
