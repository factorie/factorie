/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
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
import scala.reflect.Manifest
import scala.collection.mutable.{HashMap,HashSet,ArrayBuffer}
import cc.factorie._

/** Concrete version is implemented as an inner class of @see CmdOptions. 
    @author Andrew McCallum */
trait CmdOption[T] {
  def name: String
  def shortName: Char
  def helpString: String
  def valueClass: Class[_]
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
        case a: Seq[_] => Seq(f"--$name%s") ++ a.map(_.toString)
        case "" => Seq()
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
  private val opts = new HashMap[String,cc.factorie.util.CmdOption[_]]
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
    opts.values.foreach(o => { if (o.hasValue) sb.append(o.name+"="+o.valueName) else sb.append(o.name); sb.append(" ") })
    sb.toString
  }
  /** The arguments that were unqualified by dashed options. */
  private val _remaining = new ArrayBuffer[String]
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
  /*object CmdOption {
    def apply[T](name:String, defaultValue:T, valueName:String, helpMsg:String)(implicit m:Manifest[T]): CmdOption[T] =
      new CmdOption[T](name, defaultValue, valueName, helpMsg)
    def apply[T](name:String, shortName:Char, defaultValue:T, valueName:String, helpMsg:String)(implicit m:Manifest[T]): CmdOption[T] =
      new CmdOption[T](name, shortName, defaultValue, valueName, helpMsg)
    def apply(name:String, helpMsg:String): CmdOption[Any] =
      new CmdOption[Any](name, helpMsg)
  }*/
  class CmdOption[T](val name:String, val helpMsg:String)(implicit m:Manifest[T]) extends cc.factorie.util.CmdOption[T] {
    // TODO Add "required" constructor argument when we have Scala 2.8
    def this(name:String, defaultValue:T, valueName:String, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, helpMsg)
      this.valueName = valueName
      value = defaultValue
      this.defaultValue = defaultValue
    }
    /*def this(name:String, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, defaultValue, { val fields = m.runtimeClass.getName.split("[^A-Za-z]+"); if (fields.length > 1) fields.last else fields.head }, helpMsg)
    }*/
    def this(name:String, shortName:Char, defaultValue:T, valueName:String, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, defaultValue, valueName, helpMsg)
      this.shortName = shortName
    }
    /*def this(name:String, shortName:Char, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, defaultValue, helpMsg)
      this.shortName = shortName
    }*/
    CmdOptions.this += this
    // TODO When we have Scala 2.8 default args, add a "shortName" one-char alternative here
    var shortName: Char = ' ' // space char indicates no shortName
    val valueManifest: Manifest[T] = m
    def valueClass: Class[_] = m.runtimeClass
    var valueName: String = null
    var defaultValue: T = _
    var value: T = _
    var invokedCount = 0
    def required = false
    def setValue(v: T) { value = v }
    def hasValue = valueClass != noValueClass
    def noValueClass = classOf[Any] // This is the value of m.runtimeClass if no type is specified for T in CmdOption[T].
    /** Attempt to match and process command-line option at position 'index' in 'args'.  
        Return the index of the next position to be processed. */
    def parse(args:Seq[String], index:Int): Int = {
      if (valueClass == noValueClass && args(index) == "--"+name || args(index) == "-"+shortName) {
        // support options like --help or -h (i.e. no arguments to option)
        invoke
        invokedCount += 1
        index + 1
      } else if (args(index) == "--"+name || args(index) == "-"+shortName) {
        // support options like --file foo, or -f foo (or just --file or -f, in which case value is the defaultValue)
        var newIndex = index + 1
        // If the next args does not begin with regex "-.+" assume it is the value of this argument and parse it...
        if (newIndex < args.length && !(args(newIndex).startsWith("-") && args(newIndex).length > 1)) newIndex = parseValue(args, newIndex)
        else if (valueClass == classOf[Boolean]) this.asInstanceOf[CmdOption[Boolean]].value = true // for CmdOption[Boolean], invoking with no value arg defaults to true
        // ...otherwise the value will just remain the defaultValue
        invoke
        invokedCount += 1
        newIndex
      } else if (args(index).startsWith("--"+name+"=")) {
        // support --file=foo
        // modified on 1/21/2012 to support --file=foo=bar --brian
        val rightOfEq = args(index).drop(name.size + 3)
        parseValue(List(rightOfEq), 0)
        invoke
        invokedCount += 1
        index + 1
      } else index
    }
    /** Called after this CmdOption has been matched and value has been parsed. */
    def invoke(): Unit = {}
    /** After we have found a match, request that argument(s) to command-line option be parsed. 
        Return the index position that should be processed next. 
        This method allows one option to possibly consume multiple args, (in contrast with parseValue(String).) */
    protected def parseValue(args:Seq[String], index:Int): Int = {
      //println("CmdOption    "+valueManifest)
      //val listIntManifest = Manifest.classType[List[Int]](classOf[List[Int]], Manifest.classType[Int](classOf[Int]))
      //println("Manifest     "+listIntManifest)
      //println("CmdOption == "+(valueManifest == listIntManifest))
      //println("typeArgs     "+(valueManifest.typeArguments))
      //println("typeArgs1 == "+((valueClass eq classOf[List[_]])))
      //if (valueManifest.typeArguments.size > 0) println("typeArgs2 == "+((valueManifest.typeArguments(0).runtimeClass eq classOf[Int])))
      //println("typeArgs ==  "+((valueClass eq classOf[List[_]]) && (valueManifest.typeArguments(0).runtimeClass eq classOf[Int])))
      if ((valueClass eq classOf[List[_]]) && (valueManifest.typeArguments(0).runtimeClass eq classOf[String])) {
        // Handle CmdOpt whose value is a List[String]
        if (args(index).contains(',')) {
          // Handle the case in which the list is comma-separated
          value = args(index).split(",").toList.asInstanceOf[T]
          index + 1
        } else {
          // Handle the case in which the list is space-separated
          var i = index
          val listValue = new scala.collection.mutable.ListBuffer[String]
          // Read arguments until we find another CmdOption, which must begin with either with regex "--" or "-.+"
          while (i < args.length && !args(i).startsWith("--") && !(args(i).startsWith("-") && args(i).length > 1)) {
            listValue += args(i)
            i += 1
          }
          value = listValue.toList.asInstanceOf[T]
          i
        }
      } else if ((valueClass eq classOf[List[_]]) && (valueManifest.typeArguments(0).runtimeClass eq classOf[Int])) {
        // Handle CmdOpt whose value is a List[String]
        if (args(index).contains(',')) {
          // Handle the case in which the list is comma-separated
          value = args(index).split(",").toList.map(_.toInt).asInstanceOf[T]
          index + 1
        } else {
          // Handle the case in which the list is space-separated
          var i = index
          val listValue = new scala.collection.mutable.ListBuffer[Int]
          // Read arguments until we find another CmdOption, which must begin with either with regex "--" or "-.+"
          while (i < args.length && !args(i).startsWith("--") && !(args(i).startsWith("-") && args(i).length > 1)) {
            listValue += args(i).toInt
            i += 1
          }
          value = listValue.toList.asInstanceOf[T]
          i
        }
      } else if ((valueClass eq classOf[List[_]]) && (valueManifest.typeArguments(0).runtimeClass eq classOf[Double])) {
        // Handle CmdOpt whose value is a List[String]
        if (args(index).contains(',')) {
          // Handle the case in which the list is comma-separated
          value = args(index).split(",").toList.map(_.toDouble).asInstanceOf[T]
          index + 1
        } else {
          // Handle the case in which the list is space-separated
          var i = index
          val listValue = new scala.collection.mutable.ListBuffer[Double]
          // Read arguments until we find another CmdOption, which must begin with either with regex "--" or "-.+"
          while (i < args.length && !args(i).startsWith("--") && !(args(i).startsWith("-") && args(i).length > 1)) {
            listValue += args(i).toDouble
            i += 1
          }
          value = listValue.toList.asInstanceOf[T]
          i
        }
      } else {
        parseValue(args(index))
        index + 1
      }
    }
    /** Parse a value from a single arg */
    protected def parseValue(valueStr:String): Unit = {
      // TODO Is there a better way to do this?
      if (valueClass eq classOf[Int]) value = Integer.parseInt(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Float]) value = java.lang.Float.parseFloat(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Double]) value = java.lang.Double.parseDouble(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Short]) value = java.lang.Short.parseShort(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Long]) value = java.lang.Long.parseLong(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Boolean]) value = java.lang.Boolean.parseBoolean(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Char]) value = valueStr.apply(0).asInstanceOf[T]
      else if (valueClass eq classOf[String]) value = valueStr.asInstanceOf[T]
      // Support comma-separated multiple values, e.g. --train=eng.train,eng.testa
      //else if (valueManifest <:< Manifest.classType[List[String]](classOf[List[String]], Manifest.classType[String](classOf[String]))) value = valueStr.split(",").toList.asInstanceOf[T] // Now handled above.
      //else if (valueManifest <:< Manifest.classType[List[Int]](classOf[List[Int]], Manifest.classType[Int](classOf[Int]))) value = valueStr.split(',').map(Integer.parseInt(_)).toList.asInstanceOf[T]
      else throw new Error("CmdOption does not handle value of type "+valueManifest)
      // TODO Add an option that will run the interpreter on some code
    }
    // TODO Format long help messages more nicely.
    def helpString: String = {
      val defaultValueString = defaultValue match { case d:Seq[_] => d.mkString(","); case _ => defaultValue.toString }
      if (valueClass != noValueClass) "--%-15s %s\n".format(name+"="+valueName, helpMsg+"  Default="+defaultValueString)
      else "--%-15s %s\n".format(name, helpMsg)
    }
  }
}

/** Default CmdOption collection that should be included in most CmdOptions. */
trait DefaultCmdOptions extends CmdOptions {
  new CmdOption("help", "", "STRING", "Print this help message.") {
    override def invoke = {
      DefaultCmdOptions.this.values.foreach(o => println(o.helpString))
      System.exit(0)
    }
  }
  new CmdOption("version", "", "STRING",  "Print version numbers.") {
    override def invoke {
      throw new Error("Not yet implemented.") // TODO How to manage version strings?
      //println("FACTORIE version "+factorieVersionString)
      // TODO How do I print the Scala and JVM version numbers?
      System.exit(0)
    }
  }
  new CmdOption("config", "", "FILE", "Read command option values from a file") {
    override def invoke {
      if (this.value != "") {
        import scala.io.Source
        val contents = Source.fromFile(new java.io.File(this.value)).mkString
        val args = contents.split("\\s+")
        DefaultCmdOptions.this.parse(args)
      }
    }
  }
}
