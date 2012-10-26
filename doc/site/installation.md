---
title: "factorie: Installation"
---

Installation
===

Requirements
---

FACTORIE has been successfully installed and tested on MacOS, Linux, and Windows with Cygwin. If you encounter difficulties installing FACTORIE please let us know at discuss@factorie.cs.umass.edu.

Compilation of FACTORIE relies on Apache Maven, a software project management and build tool. Maven automatically downloads the project dependencies, including additional Java packages and even the correct version of the Scala compiler.

# Software Requirements

* Java >= 1.5
* Apache Maven >= 2.2
* Scala >= 2.9.1 -- Maven will install Scala for you, so no need to install separately.

Step-by-step Installation
---

1. Make sure you have all the required Maven software. Type `mvn -version` and verify that the Maven version is greater than 2.2 and that the Java version is greater than 1.5.

        $ mvn -version
        Apache Maven 2.2.1 (rdebian-1)
        Java version: 1.6.0_15
        Java home: /usr/lib/jvm/java-6-sun-1.6.0.15/jre
        ...

2. Obtain the FACTORIE source. You can do this by downloading and unpacking the FACTORIE tar.gz available from the Download page.

        $ wget http://factorie.googlecode.com/files/factorie-1.0.0-M1-src.tar.gz
        $ tar xzf factorie-1.0.0-M1-src.tar.gz
        $ cd factorie-1.0.0-M1
        $ ls
        LICENSE.txt pom.xml src

    Alternatively you can get the most recent FACTORIE source directly from the Mercurial (hg) source code revision control system. Mercurial is freely available from its download page.

        $ hg clone https://factorie.googlecode.com/hg/ factorie
        $ cd factorie

3. Tell Maven to compile the project. In this step might take several minutes because not only must it compile all of FACTORIE, but it will also download many dependent packages. This step will print many messages; no need to be concerned about them unless they start with [ERROR]

        $ mvn compile

    After everything has compiled, run the Unit Tests (This should not take too long).

        $ mvn test

    Sometimes the tests fail if you are using the latest Mercurial version. However, if tests fail for a release version, please contact the mailing list.

Next we recommend looking at the example code in `src/main/scala/cc/factorie/example`. Some of these examples are documented on our Examples page.

Factorie as a Library
---

If you want to use Factorie as a library, and have your code use it, there are two possible ways.

### Jar Dependency

Run the following command

    $ mvn package

This results in multiple files in the target directory, including `factorie-XX.X-STANDALONE.jar`. This file can be added to your classpath when compiling and running your Java code.

### As a Maven Dependency

In the pom.xml file for your project, add the following in the repositories section:

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
 
Then add the following in the dependencies section:

    <dependencies>
      <dependency>
        <groupId>cc.factorie</groupId>
        <artifactId>factorie</artifactId>
        <version>1.0.0-M1</version>
      </dependency>
    <dependencies>