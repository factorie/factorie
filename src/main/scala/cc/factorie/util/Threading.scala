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

import java.util.concurrent.{Callable, ExecutorService, Executors}

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
