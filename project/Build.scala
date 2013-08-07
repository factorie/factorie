import sbt._
import Keys._

object Build extends sbt.Build {
  import Dependencies._

  lazy val overrideSettings = {
    lazy val publishSetting = publishTo <<= (version) {
      version: String =>
        def repo(name: String) = name at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/" + name
      val isSnapshot = version.trim.endsWith("SNAPSHOT")
      val repoName   = if(isSnapshot) "snapshots" else "releases"
      Some(repo(repoName))
    }
    
    lazy val credentialsSetting = credentials += {
      Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match {
        case Seq(Some(user), Some(pass)) =>
          Credentials("Sonatype Nexus Repository Manager", "iesl.cs.umass.edu", user, pass)
        case _ =>
          Credentials(Path.userHome / ".ivy2" / ".credentials")
      }
    }
  }

  lazy val myProject = Project("factorie", file("."))
    .settings(
      organization  := "cc.factorie",
      version       := "1.0.0-M4",
      scalaVersion  := "2.10.1",
      scalacOptions := Seq("-deprecation", "-unchecked", "-encoding", "utf8"),
      resolvers     ++= Dependencies.resolutionRepos,
      libraryDependencies ++= Seq(
        Compile.mongodb,
        Compile.akka,
        Compile.jregex,
        Compile.colt,
        Compile.compiler,
        Compile.junit,
        Test.scalatest
      )
    ) 
}

object Dependencies {
  val resolutionRepos = Seq(
    ScalaToolsSnapshots,
    "IESL repo"    at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public/",
    "Scala tools" at "https://oss.sonatype.org/content/groups/scala-tools",
    "Third party" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/"
    //"IESL rel  repo"    at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases/",
    //"IESL"    at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots/",
  )
  

  object Compile {
    val mongodb      = "org.mongodb"               % "mongo-java-driver"  % "2.11.1"
    val akka = "com.typesafe.akka" % "akka-actor_2.10" % "2.1.4"
    val jregex = "net.sourceforge.jregex" % "jregex" % "1.2_01"
    val colt = "org.jblas" % "jblas" % "1.2.3"
    val compiler = "org.scala-lang" % "scala-compiler" % "2.10.1"
    val junit        = "junit"                     %  "junit"             % "4.10"    
  }

  object Test {
    val scalatest    = "org.scalatest"             %  "scalatest_2.10"    % "1.9.1"    % "test"
  }
}




