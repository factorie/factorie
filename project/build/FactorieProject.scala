import sbt._
import java.util.jar._
import java.io.File

class FactorieProject(info: ProjectInfo) extends DefaultProject(info) {
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  /// Syntax is: groupID % artifactID % revision [% configuration]
  override def libraryDependencies = Set(
    "fastutil" % "fastutil" % "5.1.5" from "http://obsearch.net/fastutil-5.1.5.jar",
    "junit" % "junit" % "4.4",
    "org.scalanlp" % "scalala" % "0.3.0",
    "org.scala-tools.testing" % "specs" % "1.6.1-2.8.0.Beta1-RC6"
    // "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"
  ) ++ super.libraryDependencies
}

