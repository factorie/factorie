package cc.factorie.util

import java.util.concurrent.{Executors, Callable, ExecutorService}

/**
 * User: apassos
 * Date: 7/30/13
 * Time: 2:57 PM
 */
object Threading {
  import scala.collection.JavaConversions._
  def parForeach[In](xs: Iterable[In], numThreads: Int)(body: In => Unit): Unit = withThreadPool(numThreads)(p => parForeach(xs, p)(body))
  def parForeach[In](xs: Iterable[In], pool: ExecutorService)(body: In => Unit): Unit = {
    val futures = xs.map(x => javaAction(body(x)))
    pool.invokeAll(futures).toSeq
  }

  def parMap[In, Out](xs: Iterable[In], numThreads: Int)(body: In => Out): Iterable[Out] = withThreadPool(numThreads)(p => parMap(xs, p)(body))
  def parMap[In, Out](xs: Iterable[In], pool: ExecutorService)(body: In => Out): Iterable[Out] = {
    val futures = xs.map(x => javaClosure(body(x)))
    pool.invokeAll(futures).toSeq.map(_.get())
  }

  def javaAction(in: => Unit): Callable[Object] = new Callable[Object] { def call(): Object = {
    try {
      in
    } catch {
      case t: Throwable => t.printStackTrace(); throw new Error("Caught error in parallel computation", t)
    }
    null
  }}
  def javaClosure[A](in: => A): Callable[A] = new Callable[A] { def call(): A =  try { in } catch { case t: Throwable => t.printStackTrace(); throw new Error(t) } }

  def newFixedThreadPool(numThreads: Int) = Executors.newFixedThreadPool(numThreads)
  def withThreadPool[A](numThreads: Int)(body: ExecutorService => A) = {
    val pool = newFixedThreadPool(numThreads)
    try {
      body(pool)
    } finally pool.shutdown()
  }
}
