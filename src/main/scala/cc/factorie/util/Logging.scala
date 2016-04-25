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

import java.io._

/** Class for logging messages.  
    If you want to globally replace this mechanism with a different one, you can change 
    "type Logging = cc.factorie.util.Logging" 
    in cc/factorie/package.scala to something else, and recompile.  The logging method names used in the library are pretty standard
    @author Andrew McCallum  */
class Logger(val name:String, outputStream: => OutputStream = System.err, @volatile var level: Int = Logger.INFO) {
  def this(name:String, f:File) = this (name, new BufferedOutputStream(new FileOutputStream(f)))
  def this(name:String, s:String) = this (name, new File(s))
  /**  Try to parse the level string into an Int from System.getenv, but if it fails to parse, do nothing.  
       Return true if level was set, false otherwise. */
  setLevelFromEnv()
  def setLevelFromEnv(): Boolean = {
    val envLevelString = java.lang.System.getenv(name+".level")
    if (envLevelString != null) try { level = envLevelString.toInt; true } catch { case _: Throwable => false }
    else false
  }
  def setLevel(theLevel:Int): Unit = level = theLevel
  if (Logger.loggerMap.contains(name)) throw new Error("There is already a logger named "+name)
  Logger.loggerMap(name) = this
  var printWriter = new PrintWriter(outputStream)
  protected def out = printWriter
  def close() = out.close()
  def verbosePrefix(theLevel: Int): String = "["+theLevel+"] ("+Logger.sourceDescription(2)+") "
  def prefix(theLevel: Int): String = ""
  def log(theLevel: Int)(msg: => Any): Unit = {
    if (level >= theLevel) {
      out.println(prefix(theLevel) + msg.toString)
      out.flush()
    }
  }
  //def log(theLevel:Int, msg: => Any): Unit = log(theLevel)(msg) // For similarity to log4j
  def fatal(msg: =>Any): Unit = log(Logger.FATAL)(msg)
  def error(msg: =>Any): Unit = log(Logger.ERROR)(msg)
  def warn(msg: =>Any): Unit = log(Logger.WARN)(msg)
  def info(msg: =>Any): Unit = log(Logger.INFO)(msg)
  // Remove these because they are not in log4j
  //def config(msg: =>Any): Unit = log(Logger.CONFIG)(msg)
  //def fine(msg: =>Any): Unit = log(Logger.FINE)(msg)
  //def finer(msg: =>Any): Unit = log(Logger.FINER)(msg)
  //def finest(msg: =>Any): Unit = log(Logger.FINEST)(msg)
  def debug(msg: =>Any): Unit = log(Logger.DEBUG)(msg)
  def trace(msg: =>Any): Unit = log(Logger.TRACE)(msg)
}

object Logger {
  val NEVER = -1
  val FATAL = 10
  val ERROR = 20
  val WARN = 30
  val INFO = 40
  val CONFIG = 50
  val FINE = 60
  val FINER = 70
  val FINEST = 80
  val DEBUG = 90
  val TRACE = 100

  // TODO replace with collection.concurrent.TrieMap once getOrElseUpdate is
  // actually threadsafe (see https://issues.scala-lang.org/browse/SI-7943)
  val loggerMap = collection.mutable.HashMap[String,Logger]()
  val globalLogger = new Logger("cc.factorie", System.err, INFO)
  val neverLogger = new Logger("null", System.err, Int.MinValue) {
    override def log(theLevel: Int)(x: => Any): Unit = {}
  }
  // see comment on loggerMap above
  def logger(name:String) = synchronized{ loggerMap.getOrElseUpdate(name, new Logger(name, System.err, INFO)) }
  def getLogger(name:String) = logger(name) // Alias for similarity to log4j
  def getRootLogger = globalLogger // for similarity to log4j

  @noinline protected def sourceDescription(framesUp:Int): String = {
    val e = new Exception().getStackTrace()(framesUp+1)
    e.getFileName + ":" + e.getLineNumber
  }
}

/** Uses no per-instance memory, but slow because it does a hash lookup for each call on the logger. */
trait Logging {
  def logger: Logger = Logger.logger(this.getClass.getName)
}

/** Uses no per-instance memory, and is fast because it goes directly to the companion class to get a logger, 
    but it is the default logger for all "cc.factorie", and you can't set its level individually. */
trait GlobalLogging extends Logging {
  override def logger = Logger.globalLogger
}

/** Fast, but uses one machine word of memory per-instance. */
trait FastLogging extends Logging {
  override val logger = Logger.logger(this.getClass.getName)
}

object Timer {
  def time[A](name: String, function: () => A): A = {
    val t0 = System.currentTimeMillis()
    val res = function()
    println(name + ": " + (System.currentTimeMillis() - t0))
    res
  }

  var cumulativeTime = 0L
  val timeMap = collection.mutable.HashMap[String, Long]()
  def incrementTime[A](name: String, function: () => A): A = {
    val t0 = System.currentTimeMillis()
    val res = function()
    val time = System.currentTimeMillis() - t0
    cumulativeTime += time
    if (!timeMap.contains(name)) timeMap(name) = 0L
    timeMap(name) += time
    res
  }

  def reportAndClear() {
    println("Total time: " + cumulativeTime)
    timeMap.iterator.toSeq.sortBy[Long](a => -a._2).foreach(pair => {
      println(pair._1 +": " + (pair._2.toDouble / cumulativeTime) + " (" + pair._2 + ")")
    })
    cumulativeTime = 0
    timeMap.clear()
  }

}
