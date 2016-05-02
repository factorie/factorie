# FACTORIE 

[![Build Status](https://travis-ci.org/factorie/factorie.svg?branch=master)](https://travis-ci.org/factorie/factorie)

This directory contains the source of FACTORIE, a toolkit for probabilistic modeling based on imperatively-defined factor graphs. More information, see [the FACTORIE webpage](http://factorie.cs.umass.edu).

## Installation

Installation relies on Maven, version 3.  If you don't already have maven, install it from http://maven.apache.org/download.html.
Alternatively, you can use [sbt](http://scala-sbt.org) as outlined below (a script for running sbt comes bundled with Factorie).

To compile type

```
$ mvn compile
```

To accomplish the same with sbt, type

```
$ ./sbt compile
```

You might need additional memory. If so, for sbt type

```
export SBT_OPTS="$SBT_OPTS -Xmx1g"
```

and for Maven type:

```
export MAVEN_OPTS="$MAVEN_OPTS -Xmx1g -XX:MaxPermSize=128m"
```

To create a self-contained .jar, that contains FACTORIE plus all its dependencies, including the Scala runtime, type 

```
$ mvn -Dmaven.test.skip=true package -Pjar-with-dependencies
```

To accomplish the same with sbt, type

```
$ ./sbt assembly
```

To create a similar self-contained .jar that also contains all resources needed for NLP (including our lexicons and pre-trained model parameters), type  

```
$ mvn -Dmaven.test.skip=true package -Pnlp-jar-with-dependencies
```

To accomplish the same with sbt, type

```
$ ./sbt -J-Xmx2G with-nlp-resources:assembly

```
##Try out a simple example

To get an idea what a simple FACTORIE program might look like, open one of the class files in the tutorial package
```
$ ls src/main/scala/cc/factorie/tutorial
```
To run one of these examples using maven type
```
$ mvn scala:run -DmainClass=cc.factorie.tutorial.Grid
```


## Try out implemented NLP models

Then you can run some FACTORIE tools from the command-line. For example, you can run many natural language processing tools.

```
$ bin/fac nlp --wsj-forward-pos --conll-chain-ner
```

will launch an NLP server that will perform part-of-speech tagging and named entity recognition in its input.  The server listens for text on a socket, and spawns a parallel document processor on each request.  To feed it input, type in a separate shell

```
$ echo "I told Mr. Smith to take a job at IBM in Raleigh." | nc localhost 3228
```

You can also run a latent Dirichlet allocation (LDA) topic model. Assume that "mytextdir" is a directory name containing many plain text documents each in its own file.  Then typing 

```
$ bin/fac lda --read-dirs mytextdir --num-topics 20 --num-iterations 100
```

will run 100 iterations of a sparse collapsed Gibbs sampling on all the documents, and print out the results every 10 iterations. FACTORIE's LDA implementation is faster than MALLET's.

You can also train a document classifier. Assume that "sportsdir" and "politicsdir" are each directories that  contain plan text files in the categories sports and politics. Typing

```
$ bin/fac classify --read-text-dirs sportsdir,politicsdir --write-classifier mymodel.factorie
```

will train a log-linear by maximum likelihood (MaxEnt) and save it in the file "mymodel.factorie".

The above are simply a few simple command-line options.  Internally the FACTORIE library contains extensive and general facilities for factor graphs: data representation, model structure, inference, learning.
