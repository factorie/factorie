package cc.factorie.util

object ScriptingUtils {
  def eval[T](str: String): T = {
    import scala.reflect.runtime._
    import scala.tools.reflect.ToolBox
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val tb = cm.mkToolBox()
    val script = "import cc.factorie._; {" + str.split("\n").map("    " + _).mkString + "}"
    tb.eval(tb.parse(script)).asInstanceOf[T]
  }
  def main(args: Array[String]): Unit = {
    val res = eval[String](
      """
"asd" + "foo" + "!"
      """)
    println(res)
    val res2 = eval[cc.factorie.optimize.GradientOptimizer]("import cc.factorie.optimize._; new AdaGrad(rate = 0.1)")
    println(res2)
  }
}
