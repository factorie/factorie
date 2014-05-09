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

object ScriptingUtils {
  import scala.reflect.runtime._
  import scala.tools.reflect.ToolBox
  def eval[T](code: String, imports: Seq[String] = Seq("cc.factorie._")): T = {
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val tb = cm.mkToolBox()
    val script = imports.map("import " + _).mkString("\n") + "\n{\n" + code + "\n}"
    tb.eval(tb.parse(script)).asInstanceOf[T]
  }
  def main(args: Array[String]): Unit = {
    val res = eval[String](
      """
val tmp = "asd" + "foo"
tmp + "!"
      """)
    println(res)
    val res2 = eval[cc.factorie.optimize.GradientOptimizer]("import cc.factorie.optimize._; new AdaGrad(rate = 0.1)")
    println(res2)
    val res3 = eval[cc.factorie.optimize.GradientOptimizer]("new AdaGrad(rate = 0.1)", Seq("cc.factorie.optimize._"))
    println(res3)
  }
}
