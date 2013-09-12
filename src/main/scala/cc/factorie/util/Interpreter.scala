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
