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

import java.io.File

import scala.collection.mutable
import scala.reflect.runtime.universe.{TypeTag, typeOf}

/** Concrete version is implemented as an inner class of @see CmdOptions.
    @author Andrew McCallum */
trait CmdOption[T] {
  def name: String
  def shortName: Char
  def helpMsg: String
  def valueName: String
  def defaultValue: T
  def value: T
  def setValue(v: T): Unit
  def hasValue: Boolean
  def invokedCount: Int
  def wasInvoked = invokedCount > 0
  def required: Boolean
  def parse(args:Seq[String], index:Int): Int
  def unParse: Seq[String] = {
    if (hasValue)
      value match {
        case a: Seq[_] => Seq(f"--$name%s=${a.mkString(",")}")
        case "" => Seq()
        case f:File => Seq(s"--$name=${f.getAbsolutePath}")
        case a: Any => Seq(f"--$name%s=${value.toString}%s")
      }

    else
      Seq()
  }
  override def hashCode = name.hashCode
  override def equals(other:Any) = name.equals(other)
}


/** A simple command-line option parsing tool.
    Example usage:
    <code>
    def main(args:Array[String]): Unit = {
      object opts extends CmdOptions {
        val train = new CmdOption("train", "eng.train", "CoNLL-format file from which to get training data.")
        val temperature = new CmdOption("temperature", 1.0, "Temperature for the sampler.")
        val iterations = new CmdOption("iterations", 15, "Number of iterations of training.")
      }
      opts.parse(args)
      // then later
      for (i <- 0 until opts.iterations.value) ...
    }
    </code>
    @author Andrew McCallum
 */
class CmdOptions {
  protected val opts = new mutable.HashMap[String,cc.factorie.util.CmdOption[_]]
  def apply(key: String) = opts(key)
  def get(key:String) = opts.get(key)
  def size = opts.size
  //def iterator = opts.iterator
  var strict = true
  def values = opts.values
  def +=(c:cc.factorie.util.CmdOption[_]): this.type = {
    if (opts.contains(c.name)) throw new Error("CmdOption "+c.name+" already exists.")
    opts(c.name) = c
    this
  }
  def -=(c:cc.factorie.util.CmdOption[_]): this.type = {
  	opts -= c.name
  	this
  }
  def error(msg:String): Unit = {
    System.err.println(msg)
    System.err.println(usageString)
    System.exit(-1)
  }
  def usageString: String = {
    val sb = new StringBuffer
    sb append "Usage: "
    opts.values.foreach(o => { if (o.hasValue) sb.append("--" + o.name+"="+o.valueName) else sb.append(o.name); sb.append(" ") })
    sb.toString
  }
  /** The arguments that were unqualified by dashed options. */
  private val _remaining = new mutable.ArrayBuffer[String]
  def remaining: Seq[String] = _remaining
  /** Parse sequence of command-line arguments. */
  def parse(args:Seq[String]): Unit = {
    //opts.values.foreach(o => o.invokedCount = 0) // Reset for each parse?  No, might want to parse multiple times.
    var index = 0
    while (index < args.length) {
      val origIndex = index
      var invoked = false
      val optsIter = opts.valuesIterator
      while (optsIter.hasNext && !invoked) {
        val opt = optsIter.next()
        index = opt.parse(args, index)
        invoked = index != origIndex
        assert(invoked || index == origIndex)
      }
      if (!invoked) {
        // Handle options not recognized by any CmdOption parse
        if (strict && args(index).startsWith("-")) error("Unrecognized option "+args(index))
        _remaining += args(index)
        index += 1
      }
    }
    opts.values.find(o => o.required && o.invokedCount == 0) match {
      case Some(o) => error("Required CmdOption --"+o.name+" was not provided.")
      case None =>
    }
  }

  /** Writes options from this instance into another instance of opts of a different type. */
  def writeInto[Opts <: CmdOptions](other:Opts):Opts = {
    other.opts ++= other.opts.keySet.intersect(this.opts.keySet).map { key =>
      key -> this.opts(key)
    }
    other
  }

  /** get options as a Seq[String] e.g. Seq("--l1=value", "--l2=value"...) **/
  def unParse: Seq[String] = values.toSeq.flatMap(_.unParse)

  class CmdOption[T:TypeTag](val name:String, val defaultValue:T, val valueName:String, val helpMsg:String, val required:Boolean, val shortName:Char) extends cc.factorie.util.CmdOption[T] {
    def this(name:String, defaultValue:T, valueName:String, helpMsg:String) = this(name, defaultValue, valueName, helpMsg, false, name.head)
    def this(name:String, defaultValue:T, valueName:String, helpMsg:String, required:Boolean) = this(name, defaultValue, valueName, helpMsg, required, name.head)
    def this(name:String, helpMsg:String) = this(name, null.asInstanceOf[T], null.asInstanceOf[String], helpMsg)
    def this(name:String, helpMsg:String, required:Boolean) = this(name, null.asInstanceOf[T], null.asInstanceOf[String], helpMsg, required)
    def this(name:String, shortName:Char, defaultValue:T, valueName:String, helpMsg:String, required:Boolean) = this(name, defaultValue, valueName, helpMsg, required, shortName)

    CmdOptions.this += this
    private def valueType = typeOf[T]
    private def matches(str:String):Boolean = str == ("--" + name) || str == ("-" + shortName)

    var _value = defaultValue

    def value:T = {
      _value
    }

    def setValue(v: T) { _value = v }

    def hasValue = !(valueType =:= typeOf[Nothing])

    var invokedCount = 0

    def parse(args: Seq[String], index: Int) =
     if(!hasValue && matches(args(index))) {
       invoke()
       invokedCount += 1
       index + 1
     } else if(args(index) == "--"+name || args(index) == "-"+shortName) {
       // support options like --file foo, or -f foo (or just --file or -f, in which case value is the defaultValue)
       var newIndex = index + 1
       // If the next args does not begin with regex "-.+" assume it is the value of this argument and parse it...
       if (newIndex < args.length && !(args(newIndex).startsWith("-") && args(newIndex).length > 1)) newIndex = parseValue(args, newIndex)
       else if (valueType =:= typeOf[Boolean]) setValue(true.asInstanceOf[T]) // for CmdOption[Boolean], invoking with no value arg defaults to true
       // ...otherwise the value will just remain the defaultValue
       invoke()
       invokedCount += 1
       newIndex
     } else if (args(index).startsWith("--"+name+"=")) {
       // support --file=foo
       // modified on 1/21/2012 to support --file=foo=bar --brian
       val rightOfEq = args(index).drop(name.length + 3)
       parseValue(List(rightOfEq), 0)
       invoke()
       invokedCount += 1
       index + 1
     } else index

    def invoke():Unit = {}

    /** Parses each of the supported value types of cmdoption, setting the value field and
      * returning the current index of the input args after processing. */
    protected def parseValue(args:Seq[String], index:Int):Int = {

//      println("parsing %s starting at index %s".format(args, index))
      var nextIndex = index
      //
      def processList[RT](converter:String => RT):List[RT] = if(args(index) contains ",") {
        args(index).split(",").toList.map(converter)
      } else {
        val arr = args.drop(index).takeWhile(s => !s.startsWith("-"))
//        println("found %s as the relevant subset" format arr)
        nextIndex += arr.size
        arr.toList.map(converter)
      }

      setValue(valueType match {
        case t if t =:= typeOf[List[String]] => processList(identity).asInstanceOf[T]
        case t if t =:= typeOf[List[Int]] => processList(_.toInt).asInstanceOf[T]
        case t if t =:= typeOf[List[Double]] => processList(_.toDouble).asInstanceOf[T]
        case t if t =:= typeOf[List[File]] => processList(FileUtils.fromString).asInstanceOf[T]
        case t if t =:= typeOf[Map[String, String]] => processList(identity).map{ itm =>
          val Array(k, v) = itm.split(":")
          k -> v
        }.toMap.asInstanceOf[T]
        case t if t =:= typeOf[Char] => args(index).head.asInstanceOf[T]
        case t if t =:= typeOf[File] => FileUtils.fromString(args(index)).asInstanceOf[T]
        case t if t =:= typeOf[String] => args(index).asInstanceOf[T]
        case t if t =:= typeOf[Short] => args(index).toShort.asInstanceOf[T]
        case t if t =:= typeOf[Int] => args(index).toInt.asInstanceOf[T]
        case t if t =:= typeOf[Long] => args(index).toLong.asInstanceOf[T]
        case t if t =:= typeOf[Double] => args(index).toDouble.asInstanceOf[T]
        case t if t =:= typeOf[Float] => args(index).toFloat.asInstanceOf[T]
        case t if t =:= typeOf[Boolean] => args(index).toBoolean.asInstanceOf[T]
        case otw => throw new Error("CmdOption does not handle values of type " + otw)
      })
      if (nextIndex == index) nextIndex += 1
      nextIndex
    }
  }
  override def toString: String = unParse.mkString("\n")
}

/** Default CmdOption collection that should be included in most CmdOptions. */
trait DefaultCmdOptions extends CmdOptions {
  new CmdOption("help", "", "STRING", "Print this help message.") {
    override def invoke() {
      DefaultCmdOptions.this.values.foreach(o => printf("%-30s%s%n", "--" + o.name, o.helpMsg))
      System.exit(0)
    }
  }
  new CmdOption("version", "", "STRING",  "Print version numbers.") {
    override def invoke {
      println("java version " + sys.props("java.version"))
      println("scala version " + util.Properties.versionNumberString)
      println("FACTORIE version " + "1.2-SNAPSHOT") //TODO obviously this is wrong
      System.exit(0)
    }
  }
  new CmdOption("config", "", "FILE", "Read command option values from a file") {
    override def invoke() {
      if (this.value != "") {
        import scala.io.Source
        val contents = Source.fromFile(new java.io.File(this.value)).mkString
        val args = contents.split("\\s+")
        DefaultCmdOptions.this.parse(args)
      }
    }
  }
}
