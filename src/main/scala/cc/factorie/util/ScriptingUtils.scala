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
