package cc.factorie.util

import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.concurrent.Map

class ThreadLocal[T](thunk: => T) {
  private val _instances: mutable.ConcurrentMap[Long, T] = new ConcurrentHashMap[Long, T]
  def get: T = _instances.getOrElseUpdate(Thread.currentThread().getId, thunk)
  def instances = _instances.values
}
