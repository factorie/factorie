import sbt._
import java.util.jar._
import java.io.File

class FactorieProject(info: ProjectInfo) extends DefaultProject(info) with RunRegex {
}


trait RunRegex {
  self: DefaultProject => 
	def getMainClassByRegex(str: String): Option[String] = getMainClassByRegex(str, mainCompileConditional)
	def getMainClassByRegex(str: String, compileConditional: CompileConditional): Option[String] = {
    import scala.util.matching.Regex
    def indent(s:String, i:Int):String = {stringWrapper(" ")+s}
    def printList(ls:List[String]) { for (l <- ls) { println(indent(l, 2))} }
    def printApps(apps:List[String]) = apps match {
      case x :: xs => { println("Several apps match:"); printList(apps) }
      case Nil     => println("No apps match regex")
    }

		val applications = compileConditional.analysis.allApplications.toList
    val apps = applications.filter{ ("(?i)"+str).r findFirstIn _ isDefined }
    apps match {
      case x :: Nil  => Some(x)
      case _         => {
        printApps(apps)
        None
      }
    }
	}

	protected def runByRegexAction = task { 
    args => args.toList match {
      case x :: xs =>  runTask(getMainClassByRegex(x.toString), runClasspath, xs) dependsOn(compile, copyResources)
      case _       =>  runTask(getMainClass(true), runClasspath, args) dependsOn(compile, copyResources)
    }
  }
	lazy val runs = runByRegexAction describedAs "select and run an application based on case-insensitive regex, e.g., runs mypack.*Myapp$ arg1 arg2"
}
