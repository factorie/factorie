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

/**
 * User: apassos
 * Date: 3/8/13
 * Time: 7:59 AM
 */
final class RWLock extends Serializable {
  private val _lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  @inline def readLock() { _lock.readLock().lock() }
  @inline def writeLock() { _lock.writeLock().lock() }
  @inline def readUnlock() { _lock.readLock().unlock() }
  @inline def writeUnlock() { _lock.writeLock().unlock() }
  @inline def withReadLock[T](value: => T) = {
    readLock()
    try { value } finally { readUnlock() }
  }
  @inline def withWriteLock[T](value: => T) = {
    writeLock()
    try { value } finally { writeUnlock() }
  }
}
