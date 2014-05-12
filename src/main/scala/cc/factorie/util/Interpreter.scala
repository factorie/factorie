/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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


/** The scala REPL available through an object main.
    Runs each of the command-line args as a scala command, then drops into an interpreter prompt. */
object Interpreter {
  def main(args:Array[String]): Unit = {
    val settings = new scala.tools.nsc.Settings
    settings.usejavacp.value = true
    val iloop = new scala.tools.nsc.interpreter.ILoop
    args.foreach(iloop.command(_))
    iloop.process(settings)
  }
}
