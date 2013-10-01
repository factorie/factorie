
/*&

# Installation

FACTORIE is known to run on MacOS, Linux, and Windows with Cygwin, and is expected to run on any platform on which the Java Virtual Machine is available.

## Dependencies

FACTORIE requires Java >= 1.5 and Apache Maven >= 3.0. Maven will install the correct versions of all other dependencies for you, including Scala.

## Downloading Pre-compiled JAR

To use FACTORIE as a library, download the pre-compiled jar from https://github.com/factorie/factorie/releases. Adding this jar to the classpath of your Java project will allow you to use FACTORIE in that project.

If you have the FACTORIE source, you can also run mvn package, which will generate a jar in the target folder which you can then include in other projects.

## Checking out Source from GitHub

You can get the latest FACTORIE source code by cloning our repository on Github:

```
git clone https://github.com/factorie/factorie.git
```

## Compiling Development Version Source

To compile FACTORIE, tell Maven to compile the project from the root directory. If you just cloned the git repository into your current directory, then the commands would be:

```
cd factorie
mvn compile
```

This step may take several minutes because it must not only compile all of FACTORIE, but also download any dependencies, such as Scala. Maven will print many messages; no need to be concerned about them unless they start with [ERROR].

If you find you are running out of Java heap space or PermGen space, you may want to add "-Xms2g -Xmx2g -XX:MaxPermSize=256m" to your MAVEN_OPTS environment variable before running Maven:

```
export MAVEN_OPTS="$MAVEN_OPTS -Xms2g -Xmx2g -XX:MaxPermSize=256m"
```

This will tell Maven to run Java with 2GB heap space and up to 256MB PermGen space, which should be enough to compile FACTORIE.

## Running Test Suite

After compiling, you may want to run the unit tests. The following command will achieve this:

```
mvn test
```

If you are running the latest version from git, rather than a milestone release, then some tests may fail. If tests fail for a milestone release, please contact the mailing list: factorie-discuss@googlegroups.com

## Adding to a Maven Project

If you'd like to add FACTORIE as a dependency in a Maven-manged project, first add the FACTORIE repository in the repositories section of the project's pom.xml:

```
<repositories>
  ...
  <repository>
    <id>IESL Releases</id>
    <name>IESL Repo</name>
    <url>https://dev-iesl.cs.umass.edu/nexus/content/groups/public</url>
    <snapshots>
      <enabled>false</enabled>
    </snapshots>
    <releases>
      <enabled>true</enabled>
    </releases>
  </repository>
</repositories>
```

Then add FACTORIE as a dependency in the dependencies section:

```
<dependencies>
  ...
  <dependency>
    <groupId>cc.factorie</groupId>
    <artifactId>factorie</artifactId>
    <version>1.0.0-M8</version>
  </dependency>
<dependencies>
```

## NLP Resources JAR



## Questions, Bug Reports and Feature Requests

The FACTORIE mailing list at factorie-discuss@googlegroups.com is a good place to ask general questions and discuss broad points.

Please report specific problems with the installation, bugs in the code, or feature requests in the FACTORIE GitHub issues tracker at 
https://github.com/factorie/factorie/issues  

## Eclipse and IntelliJ IDEA Setup

To import FACTORIE as a project in Eclipse or IDEA, start by cloning the git repository. 

Eclipse (Kepler):
1. Install the Scala plugin for Eclipse through the Eclipse Marketplace. Once installed the plugin should prompt you to make sure Eclipse is configured properly to run Scala, e.g. you may want to add a line to your eclipse.ini file that increased the default amount of Java heap space that Eclipse runs with
2. If you want to manage git source control through Eclipse, make sure you have the latest version of the EGit plugin installed.
3. Before you can import a Maven Scala project, you’ll also need to install the m2eclipse-scala connector (the Maven plugin that comes with Eclipse doesn’t support Scala by default) by installing the connector from the following Eclipse plugin site: http://alchim31.free.fr/m2e-scala/update-site (via Help->Install New Software)
4. Now import Factorie as a Maven project via File->Import->Maven->Existing Maven Projects. Select the top-level  directory from your cloned version of FACTORIE, the directory containing the pom.xml file, as the root directory.
5. It should work! If Eclipse complains that Factorie is full of syntax errors (yet compiles fine) try cleaning (Project->Clean), rebuilding (should happen automatically after clean, otherwise Project->Build Project) and restarting Eclipse. Also make sure that you are in the Scala perspective.
6. To compile, right click on pom.xml and select Run As->Maven Build. Enter "compile" as the goal. Apply and run (in the future you can just do Run As->Maven Build)
7. To run different launchers defined in your pom.xml you will have to add new Maven run configurations. For e.g. a launcher called "LauncherExample", you would want to create a new run configuration for the project with the goal "scala:run" and parameter "Dlauncher" with value "LauncherExample"

IntelliJ IDEA (12 CE):
1. Select Import Project from the Quick Start menu.
2. Select the top-level directory from your cloned FACTORIE project, the directory containing the pom.xml file.
3. Select "Import project from external model" then highlight Maven as the type.
4. The root directory should be set to the directory containing the pom.xml, and select "Import Maven projects automatically"
5. Select the profile "nlp-jar-with-dependencies"
6. The FACTORIE Maven project should be automatically selected in the next window
7. Make sure the project SDK is set to the correct JDK on your machine, version >= 1.5. You may have to set your JDK home path if you have not yet configured your JDK in IntelliJ
8. Select the project name and location for the IDEA project to be stored. The defaults should be fine.
9. IDEA should detect that this is a git project and ask if you want to register it as such. If you'd like to manage git version control through IDEA you should say yes.
10. When IDEA asks whether you'd like to enable type-aware highlighting, it is probably best to say yes. 
11. To compile FACTORIE, do Build -> Make Project
12. To run different launchers defined in your pom.xml, go to Run -> Edit Configurations then click on the + in the top left to create a new Maven run configuration. For e.g. a launcher called "LauncherExample" you would want to add the command line "scala:run -Dlauncher=LauncherExample". The default working directory should be the project root, which is correct.

If you find you are running out of Java heap space or PermGen space when compiling, add the JVM arguments "-Xms2g -Xmx2g -XX:MaxPermSize=256m" in your run configuration.

## Coding Standards

We generally follow the Scala style guide suggestions in 
http://davetron5000.github.com/scala-style/index.html

For indentation use two spaces rather than tabs.

 */
