import sbt._
import Keys._
import Scope.{GlobalScope, ThisScope}

object BuildSettings {
  val buildOrganization = "cc.factorie"
  val buildScalaVersion = "2.9.1"
  val buildVersion = "0.10.2-SNAPSHOT"

  val buildSettings = Defaults.defaultSettings ++
  Seq (
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version := buildVersion,
    parallelExecution := true,
    retrieveManaged := true,
    autoCompilerPlugins := true,
    externalResolvers <<= resolvers map { rs => Resolver.withDefaultResolvers(rs)},
    javacOptions ++= Seq("-Xlint:unchecked"),
    crossScalaVersions := Seq("2.9.1"),
    publishArtifact in (Compile, packageDoc) := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xcheckinit", "-encoding", "utf8"),
    shellPrompt := ShellPrompt.buildShellPrompt)
}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }

  val current = """\*\s+([^\s]+)""".r

  def gitBranches = ("git branch --no-color" lines_! devnull mkString)
  def hgBranch = ("hg branch" lines_! devnull mkString)

  val buildShellPrompt = {
    (state: State) => {
      val currBranch = hgBranch
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (currBranch, currProject, BuildSettings.buildVersion)
    }
  }
}

object FactorieBuild extends Build {
  import BuildSettings._

  val buildShellPrompt = ShellPrompt.buildShellPrompt
  val printClasspath = TaskKey[File]("print-class-path")

  def printCp = (target, fullClasspath in Compile, compile in Compile) map { (out, cp, analysis) =>
    println(cp.files.map(_.getName).mkString("\n"))
    println("----")
    println(analysis.relations.allBinaryDeps.toSeq.mkString("\n"))
    println("----")
    println(out)
    out
  }

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

    Defaults.defaultSettings ++ Seq(
      publishSetting, 
      credentialsSetting
    )
  }

  lazy val factorie:Project = Project(
    id = "factorie",
    base = file("."),
    settings = buildSettings ++ overrideSettings
  )
}




