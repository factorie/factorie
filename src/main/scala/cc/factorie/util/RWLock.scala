package cc.factorie.util

/**
 * User: apassos
 * Date: 3/8/13
 * Time: 7:59 AM
 */
class RWLock {
  private val _lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  def readLock() { _lock.readLock().lock() }
  def writeLock() { _lock.writeLock().lock() }
  def readUnlock() { _lock.readLock().unlock() }
  def writeUnlock() { _lock.writeLock().unlock() }
}
