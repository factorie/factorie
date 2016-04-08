---
title: "Part-of-speech Tagging"
layout: default
group: usersguide
weight: 750
---


FACTORIE Part-of-Speech Tagger
==========================

## About ##

A part-of-speech tagger (POS tagger) is a program that takes human-language text as input and attempts to automatically determine the grammatical POS tags (noun, verb, etc.) of each token in the text. We achieve this by training a simple probabilistic model that, given a token of text, predicts its part-of-speech given its form and context in the sentence. Our tagger uses a fast and accurate forward model based heavily on the one described in the following paper:

> Jinho D. Choi and Martha Palmer. 2012. [Fast and Robust Part-of-Speech Tagging Using Dynamic Model Selection](http://aclweb.org/anthology//P/P12/P12-2071.pdf). In Proceedings of the 50th Annual Meeting of the Association for Computational Linguistics (ACL12), pp. 363--367.

Unlike the above, our tagger does not perform dynamic model selection. It is trained using regularized AdaGrad, with l1, l2, learning rate, delta, feature count cutoff, and number of training iterations hyperparameters tuned via grid search using a development set.

We provide two pre-trained models, one trained on the Ontonotes English corpus and one trained on Penn Treebank WSJ sections 0-18. The default tagger in Factorie is the model trained on Ontonotes, a cross-domain dataset guaranteeing relatively high inter-annotator accuracy, called OntonotesForwardPosTagger.

Our tagger is both fast and accurate, processing more than 20K tokens/second and achieving 97.22% accuracy on WSJ sections 22-24.

## How to use ##

### From the command line ###

The easiest way to get started is by using our tagger through the FACTORIE NLP command line tool. Check out the [quick start guide](http://factorie.cs.umass.edu/usersguide/UsersGuide200QuickStart.html).

### As a Maven dependency ###

To use our tagger as a dependency in your Maven project, you will need to add the IESL repository:

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

Then include the dependencies for FACTORIE and our pre-trained POS models, if you choose not to train your own:

```
<dependencies>
  ...
  <dependency>
    <groupId>cc.factorie</groupId>
    <artifactId>factorie</artifactId>
    <version>1.2</version>
  </dependency>

  <dependency>
    <groupId>cc.factorie.app.nlp</groupId>
    <artifactId>all-models</artifactId>
    <version>1.2</version>
  </dependency>
<dependencies>
```

## Training your own models ##

You can also train your own POS tagger using FACTORIE. The following is an example script to train and test your own ForwardPosTagger on labeled data in whitespace-separated one-word-per-line format:

```bash
#!/bin/bash

memory=2g
modelname="ForwardPosTagger.factorie"

trainfile="--train-file=/path/to/training/data"
testfile="--test-file=/path/to/test/data"
save="--save-model=true"
model="--model=$modelname"

java -classpath factorie-jar-with-dependencies-1.0-SNAPSHOT.jar -Xmx$memory cc.factorie.app.nlp.pos.ForwardPosTrainer --owpl $trainfile $testfile $save $model
```

This will train a model on the given training data, testing accuracy on the given test data, and saving the trained model to a file called “ForwardPosTagger.factorie”. If you are training on a lot of data, you may need to increase the amount of memory allocated to the JVM. For example, to train on the Ontonotes corpus requires about 16GB.

The above will learn the parameters of the model using regularized AdaGrad, log loss and some default hyperparameters. You can change the default values, as well as specify entire directories of training data, etc. using the command line parameters listed below:

* model: Filename for the model (saving a trained model or reading a saved model)

* save-model: Whether to save the trained model

* test-file: Test data file

* train-file: Training data file

* test-dir: Directory containing test files

* train-dir: Directory containing training files

* test-files: Comma-separated list of training files

* train-files: Comma-separated list of training files

* owpl: Whether the data is in OWPL format or otherwise (Ontonotes) (default: false)

* l1: l1 regularization weight for AdaGradRDA (default: 0.000001)

* l2: l2 regularization weight for AdaGradRDA (default: 0.00001)

* rate: learning rate (default: 1.0)

* delta: learning rate decay (default: 0.1)

* cutoff: Discard features less frequent than this before training (default: 2)

* update-examples: Whether to update examples in later iterations during training (default: true)

* use-hinge-loss: Whether to use hinge loss or log loss during training (default: false)

* num-iterations: Number of passes over the data for training (default: 5)
 
You may want to perform hyperparameter optimization to find the right hyperparameters for your model. FACTORIE’s hyperparameter optimizer assumes access to a grid cluster engine
where it can use qsub to submit many parallel jobs testing different sets of hyperparameters. The ForwardPosOptimizer object contains good default ranges for optimizing the l1, l2, rate, delta, cutoff and number of training iterations for the POS tagger, spawning 200 jobs each with 16GB heap allocated to the JVM. For a more detailed explanation of FACTORIE’s hyperparameter optimization capabilities, see [the users guide] (http://factorie.cs.umass.edu/usersguide/UsersGuide490ParallelismAndHyperparameters.html).


