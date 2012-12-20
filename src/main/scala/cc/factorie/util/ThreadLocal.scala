package cc.factorie.util

import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

abstract class ThreadLocal[T] {

  def initialValue: T

  private val _instances: mutable.ConcurrentMap[Long, T] = new ConcurrentHashMap[Long, T]
  def get: T = _instances.getOrElseUpdate(Thread.currentThread().getId, initialValue)
  def instances = _instances.values
}
