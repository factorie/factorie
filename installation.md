---
title: "Installation"
layout: default
weight: 1
group: doc
---

Installation
===

Requirements
---

FACTORIE has been successfully installed and tested on MacOS, Linux, and Windows with Cygwin. If you encounter difficulties installing FACTORIE please let us know at discuss@factorie.cs.umass.edu.

Compilation of FACTORIE relies on [Apache Maven](http://maven.apache.org/), a software project management and build tool. Maven automatically downloads the project dependencies, including additional Java packages and even the correct version of the Scala compiler.

# Software Requirements

* [Java](http://www.java.com/getjava/) >= 1.5
* [Apache Maven](http://maven.apache.org/) >= 2.2
* [Scala](http://www.scala-lang.org/) >= 2.9.1 -- Maven will install Scala for you, so no need to install separately.

Step-by-step Installation
---

1. Make sure you have all the required Maven software. Type `mvn -version` and verify that the Maven version is greater than 2.2 and that the Java version is greater than 1.5.

        $ mvn -version
        Apache Maven 2.2.1 (rdebian-1)
        Java version: 1.6.0_15
        Java home: /usr/lib/jvm/java-6-sun-1.6.0.15/jre
        ...

2. Obtain the FACTORIE source. You can get the most recent FACTORIE source directly from the git source code revision control system. Git is freely available from its download page.

        $ git clone https://github.com/factorie/factorie.git factorie
        $ cd factorie
        $ git checkout "factorie-{{ site.factorie_version }}"

3. Tell Maven to compile the project. In this step might take several minutes because not only must it compile all of FACTORIE, but it will also download many dependent packages. This step will print many messages; no need to be concerned about them unless they start with [ERROR]

        $ mvn compile

    After everything has compiled, run the Unit Tests (This should not take too long).

        $ mvn test

    Sometimes the tests fail if you are using the latest Mercurial version. However, if tests fail for a release version, please contact the mailing list.

Next we recommend reading the [tutorials](tutorial.html), and looking at the example code in `src/main/scala/cc/factorie/example`.

Factorie as a Library
---

If you want to use Factorie as a library, and have your code use it, there are two possible ways.

### Jar Dependency

Run the following command

    $ mvn package

This results in multiple files in the target directory, including `factorie-XX.X-STANDALONE.jar`. This file can be added to your classpath when compiling and running your Java code.

### As a Maven Dependency

In the pom.xml file for your project, add the following in the repositories section:

~~~xml
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
~~~
 
Then add the following in the dependencies section:

~~~xml
    <dependencies>
      <dependency>
        <groupId>cc.factorie</groupId>
        <artifactId>factorie</artifactId>
        <version>{{ site.factorie_version }}</version>
      </dependency>
    <dependencies>
~~~

### As an sbt Dependency

In your `build.sbt` file, add the following:

~~~
resolvers += "IESL Release" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"
libraryDependencies += "cc.factorie" % "factorie" % "{{ site.factorie_version }}"
~~~
