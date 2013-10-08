package cc.factorie.util

/**
 * User: apassos
 * Date: 3/8/13
 * Time: 7:59 AM
 */
final class RWLock {
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
