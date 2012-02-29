import sbt._
import Keys._

object Build extends sbt.Build {
  import Dependencies._

  lazy val overrideSettings = {
    lazy val publishSetting = publishTo <<= (version) {
      version: String =>
        def repo(name: String) = name at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/" + name
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
      version       := "0.10.2-SNAPSHOT",
      scalaVersion  := "2.9.1",
      scalacOptions := Seq("-deprecation", "-unchecked", "-encoding", "utf8"),
      resolvers     ++= Dependencies.resolutionRepos,
      libraryDependencies ++= Seq(
        Compile.jdom,
        Compile.mongodb,
        Test.junit,
        Test.scalatest
      )
    ) 
}

object Dependencies {
  val resolutionRepos = Seq(
    ScalaToolsSnapshots,
    "IESL rel  repo"    at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/releases/",
    "IESL snap repo"    at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/snapshots/"
  )


  
  object Compile {
    val commonsIo    = "commons-io"                %  "commons-io"        % "2.0.1"
    val jettison     = "org.codehaus.jettison"     %  "jettison"          % "1.3"
    val jdom         = "org.jdom"                  %  "jdom"              % "1.1"
    val casbah       = "com.mongodb.casbah"        %% "casbah"            % "2.1.5-1"
    val mongodb      = "org.mongodb"               % "mongo-java-driver"  % "2.7.3"
  }

  object Test {
    val scalatest    = "org.scalatest"             %  "scalatest"         % "1.2"      % "test"
    val junit        = "junit"                     %  "junit"             % "4.8.1"    % "test"
  }
}




