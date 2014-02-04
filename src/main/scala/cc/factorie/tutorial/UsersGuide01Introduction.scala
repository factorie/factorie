/*&

# FACTORIE User's Guide

Version 1.0

Andrew McCallum, Alexandre Passos, Sameer Singh,... 


# Introduction

[FACTORIE](http://factorie.cs.umass.edu) is a toolkit for deployable probabilistic modeling, implemented as a software library in [Scala](http://scala-lang.org).
It provides its users with a succinct language for creating [factor graphs](http://en.wikipedia.org/wiki/Factor_graph), estimating parameters and performing inference.

## Purpose and Capabilities

FACTORIE aims to provide a full-featured framework for probabilistic graphical models (both directed and undirected) that is both flexible for rapid prototyping and efficient at large scale for deployment in substantial applications.  
It supplies infrastructure for representing random variables, 
creating dependencies among them with factors, 
running a variety of inference procedures on simple or complex dependency structures,
and estimating parameters by various state-of-the-art methods. 

FACTORIE's key features include the following:

- It is object-oriented, enabling encapsulation, abstraction and inheritance in the definition of random variables, factors, inference and learning methods.
- It is scalable, with demonstrated success on problems with billions of variables and factors, and on models that have changing structure, such as case factor diagrams.  It has also been plugged into a database back-end, representing a new approach to probabilistic databases capable of handling many billions of variables.
- It is flexible, supporting multiple modeling and inference paradigms.  Its original emphasis was on conditional random fields, undirected graphical models, MCMC inference, online training, and discriminative parameter estimation.  However, it now also supports directed generative models (such as latent Dirichlet allocation), and has support for variational inference, including belief propagation and mean-field methods, as well as dual-decomposition.
- It supports smooth rise in users' learning effort as users descend in layers of abstraction---from command-line tools, to succinct scripting, to customized models and inference procedures.  FACTORIE is designed to avoid "impenetrable black-boxes."
- It is embedded into a general purpose programming language, providing model authors with familiar and extensive resources for implementing the procedural aspects of their solution, including the ability to beneficially mix data pre-processing, diagnostics, evaluation, and other book-keeping code in the same files as the probabilistic model specification.
- It allows the use of imperative (procedural) constructs to define the factor graph---an unusual and powerful facet that enables significant efficiencies and also supports the injection of both declarative and procedural domain knowledge into model design.
- The structure of generative models can be expressed as a program that describes the generative storyline by creating variables and specifying their parents.  
  The structure of undirected graphical models can be specified similarly by explicitly creating factors and specifying their neighboring variables.
  However, most commonly the creation of factors for relational data is defined in templates which contain functions that create the necessary factors in a Turing-complete imperative style.  
  This usage of imperative programming to define various aspects of factor graph construction and operation is an innovation originated in FACTORIE; we term this approach imperatively-defined factor graphs.  The above three methods for specifying relational factor graph structure can be mixed in the same model.

FACTORIE's limitations include the following:

- It does not yet have extensive support for inference and learning with continuous random variables.  Support for discrete random variables has been our main emphasis thus far.
- It has only minimal support for automatically selecting an appropriate inference method given a particular graphical model.  Our users instead specify which family of inference method they wish to use.
- It does not yet have convenient infrastructure for defining simple non-relational graphical models (such as the [Sprinkler/Rain/Grass example](http://en.wikipedia.org/wiki/Bayesian_network)).  Our emphasis thus far has been on large relational data.  
- It does not yet have a simple declarative syntax for specifying model structure.  It has an extremely flexible mechanism for definition model structure directly in Scala.  A simpler front-end syntax for beginners will be added in the future. 
- It is not yet connected to a tool for directly producing graphs or other visualizations.
For further discussion of FACTORIE's comparison to other related tools see [Tutorial010SimilarTools.scala.html].

FACTORIE comes with pre-built model structures and command-line tools for:

- classification (including document classification, with MaxEnt, NaiveBayes, SVMs and DecisionTrees)
- linear regression
- linear-chain conditional random fields
- topic modeling (including latent Dirichlet allocation, as well as several variants)
- natural language processing, including many standard NLP pipeline components, such as tokenization, sentence segmentation, part-of-speech tagging, named entity recognition, dependency parsing and within-document coreference.

FACTORIE has been successfully applied to many tasks, including:

- cross-document entity resolution on 100 million mentions, parallelized and distributed [(Wick, Singh, McCallum, ACL, 2012)](http://cs.umass.edu/%7Esameer/files/hierar-coref-acl12.pdf)
- within-document co-reference, supervised [(Zheng, Vilnis, Singh, Choi, McCallum, CoNLL, 2013)](http://ciir-publications.cs.umass.edu/getpdf.php?id=1119)
- parallel/distributed belief propagation
- transition-based and graph-based dependency parsing
- relation extraction, distantly supervised
- schema matching
- ontology alignment
- parallelized latent Dirichlet allocation.



## First Examples

Here are three brief examples providing a brief sense of FACTORIE usage.

### Topic Modeling, Document Classification, and NLP on the Command-line

FACTORIE comes with a pre-built implementation of the [latent Dirichlet allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) topic model. 
If "mytextdir" is a directory name containing many plain text documents each in its own file, then typing 
```
$ bin/fac lda --read-dirs mytextdir --num-topics 20 --num-iterations 100
```
will run 100 iterations of a sparse collapsed Gibbs sampling on all the documents, and print out the results every 10 iterations. 
FACTORIE's LDA implementation is faster than [MALLET](http://mallet.cs.umass.edu)'s.

You can also train a document classifier. 
If "sportsdir" and "politicsdir" are each directories that contain plan text files in the categories sports and politics, then typing
```
$ bin/fac classify --read-text-dirs sportsdir politicsdir --write-classifier mymodel.factorie
```
will train a log-linear classifier by maximum likelihood (same as maximum entropy) and save it in the file "mymodel.factorie".

If you also have the Maven-supplied factorie-nlp-resources JAR in your classpath, you can run many natural language processing tools.  For example,

```
$ bin/fac nlp --wsj-forward-pos --transition-based-parser --conll-chain-ner
```

will launch an NLP server that will perform part-of-speech tagging, dependency parsing and named entity recognition on its input.  
The server listens for text on a socket, and spawns a parallel document processor on each request.  
To feed it input, type in a separate shell

```
$ echo "Mr. Jones took a job at Google in New York.  He and his Australian wife moved from New South Wales on 4/1/12." | nc localhost 3228
```

which then produces the output:

```
1       1       Mr.             NNP     2       nn      O
2       2       Jones           NNP     3       nsubj   U-PER
3       3       took            VBD     0       root    O
4       4       a               DT      5       det     O
5       5       job             NN      3       dobj    O
6       6       at              IN      3       prep    O
7       7       Google          NNP     6       pobj    U-ORG
8       8       in              IN      7       prep    O
9       9       New             NNP     10      nn      B-LOC
10      10      York            NNP     8       pobj    L-LOC
11      11      .               .       3       punct   O

12      1       He              PRP     6       nsubj   O
13      2       and             CC      1       cc      O
14      3       his             PRP$    5       poss    O
15      4       Australian      JJ      5       amod    U-MISC
16      5       wife            NN      6       nsubj   O
17      6       moved           VBD     0       root    O
18      7       from            IN      6       prep    O
19      8       New             NNP     9       nn      B-LOC
20      9       South           NNP     10      nn      I-LOC
21      10      Wales           NNP     7       pobj    L-LOC
22      11      on              IN      6       prep    O
23      12      4/1/12          NNP     11      pobj    O
24      13      .               .       6       punct   O
```


### Univariate Gaussian

The following code creates a model for holding factors that connect random variables for holding mean and variance with 1000 samples from a Gaussian.
Then it re-estimates by maximum likelihood the mean and variance from the sampled data. 

```
package cc.factorie.tutorial
object ExampleGaussian extends App {
  import cc.factorie._                             // The base library
  import cc.factorie.directed._                    // Factors for directed graphical models
  implicit val model = DirectedModel()             // Define the "model" that will implicitly store the new factors we create
  implicit val random = new scala.util.Random(0)   // Define a source of randomness that will be used implicitly in data generation below 
  val mean = new DoubleVariable(10)                // A random variable for holding the mean of the Gaussian
  val variance = new DoubleVariable(1.0)           // A random variable for holding the variance of the Gaussian
  println("true mean %f variance %f".format(mean.value, variance.value))
  // Generate 1000 new random variables from this Gaussian distribution
  //  "~" would mean just "Add to the model a new factor connecting the parents and the child"
  //  ":~" does this and also assigns a new value to the child by sampling from the factor
  val data = for (i <- 1 to 1000) yield new DoubleVariable :~ Gaussian(mean, variance) 
  // Set mean and variance to values that maximize the likelihood of the children
  Maximize(mean)
  Maximize(variance)
  println("estimated mean %f variance %f".format(mean.value, variance.value))
}
```

### Linear-chain Conditional Random Field for part-of-speech tagging

The following code declares data, model, inference and learning for a linear-chain CRF for part-of-speech tagging.

```
object ExampleLinearChainCRF extends App {
  import cc.factorie._            // The base library: variables, factors
  import cc.factorie.la           // Linear algebra: tensors, dot-products, etc.
  import cc.factorie.optimize._   // Gradient-based optimization and training
  // Declare random variable types
  // A domain and variable type for storing words
  object TokenDomain extends CategoricalDomain[String]
  class Token(str:String) extends CategoricalVariable(str) { def domain = TokenDomain }
  // A domain and variable type for storing part-of-speech tags
  object LabelDomain extends CategoricalDomain[String]
  class Label(str:String, val token:Token) extends LabeledCategoricalVariable(str) { def domain = LabelDomain }
  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]
  // Create random variable instances from data
  val data = List("See/V Spot/N run/V", "Spot/N is/V a/DET big/J dog/N", "He/N is/V fast/J") // Just a toy amount of data for this example
  val labelSequences = for (sentence <- data) yield new LabelSeq ++= sentence.split(" ").map(s => { val a = s.split("/"); new Label(a(1), new Token(a(0)))})
  // Define a model structure
  val model = new Model with Parameters {
    // Two families of factors, where factor scores are dot-products of sufficient statistics and weights.  (The weights will set in training below.)
    val markov = new DotFamilyWithStatistics2[Label,Label] { val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size)) }
    val observ = new DotFamilyWithStatistics2[Label,Token] { val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.size)) }
    // Given some variables, return the collection of factors that neighbor them.
    def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => labels.map(label => new observ.Factor(label, label.token)) ++ labels.sliding(2).map(window => new markov.Factor(window.head, window.last))
    }
  }
  // Learn parameters
  val trainer = new BatchTrainer(model.parameters, new ConjugateGradient)
  trainer.trainFromExamples(labelSequences.map(labels => new LikelihoodExample(labels, model, InferByBPChainSum)))
  // Inference on the same data.  We could let FACTORIE choose the inference method, 
  // but here instead we specify that is should use max-product belief propagation specialized to a linear chain
  labelSequences.foreach(labels => BP.inferChainMax(labels, model))
  // Print the learned parameters on the Markov factors.
  println(model.markov.weights)
  // Print the inferred tags
  labelSequences.foreach(_.foreach(l => println("Token: " + l.token.value + " Label: " + l.value)))
}
```

## History

Andrew McCallum began designing and developing FACTORIE in April 2008 as an effort to build an alternative to his [MALLET](http://mallet.cs.umass.edu) toolkit that could represent arbitrarily-structured factor graphs.
An additional goal was to demonstrate the benefits of mixing declarative and imperative styles in probabilistic programming [(McCallum, Schultz, Singh)](http://people.cs.umass.edu/~mccallum/papers/factorie-nips09.pdf).
Initial development emphasized discrete data, undirected graphical models, MCMC inference and learning by [SampleRank](http://www.cs.umass.edu/%7Emwick/MikeWeb/Publications_files/wick09samplerank.pdf)

By spring 2009 FACTORIE was hosting research experiments in entity resolution, and was extended to directed graphical models and variational inference.  

In 2010 FACTORIE gained significant infrastructure for representing sequences, spans and tensors, as well as support for gradient based-optimization by conjugate gradient and LBFGS.
By this time the toolkit was mature enough to support a wide variety of research---the experiments of over ten publications were implemented with FACTORIE.
The interfaces for variables, factors, factor sufficient statistics, factor templates and models were made more general.  
Sebastian Riedel, Sameer Singh and Michael Wick all joined McCallum in contributing to FACTORIE, and version 0.9.0 was released.

By 2011-2012 FACTORIE was supporting extensive experiments in natural language processing, including state-of-the-art entity resolution and relation extraction, as well as parallel-distributed belief propagation on general graphs. 
Serialization and [MongoDB](http://www.mongodb.org) interfaces were added.  
A general interface for probabilistic inference was improved, including classes for marginals and summaries.
The support for efficient tensors was improved.
Alexandre Passos and Luke Vilnis began contributing extensively.
Version 0.10 was released January 2011.  Version 1.0 milestone 1 was released in fall 2012.

Work in 2013 focused on parameter estimation and preparations for the version 1.0 release.
A flexible and efficient interface for parameter estimation was refined to separate `GradientOptimizers` (which take a gradient and change parameters, including traditional methods like LBFGS and modern online methods such as AdaGrad), `Examples` (which take data and produce a gradient), and `Trainers` (which take an optimizer and examples and schedule the updates, batch or online, serial or parallel, HogWild, etc).
Core interfaces for variables, factors, models and inference were minor adjusted for clarity.
Writing user guide documentation began in earnest in summer 2013.  
In addition, core natural language processing infrastructure was vastly expanded 
with the addition of state-of-the-art tokenization, sentence segmentation, part-of-speech, NER, dependency parser, and within-document coreference.

FACTORIE version 1.0 is anticipated to be released in November 2013.



## Relationship to other toolkits

### Machine learning toolkits

Although at its core FACTORIE is a toolkit for graphical models, since classification, clustering and regression can be expressed as trivial graphical models, FACTORIE can also be used as a tool for traditional non-structured machine learning.

[MALLET](http://mallet.cs.umass.edu) provides facilities for classification, finite-state (linear chain) conditional random fields, and a few topic models.
However, it was not designed to support arbitrarily-structured graphical models.  FACTORIE strives to supersede MALLET in all ways.
The [GRMM](http://mallet.cs.umass.edu/grmm/) extension to MALLET, written by Charles Sutton, does support graphical models, but not with as flexible an architecture; and in any case it is no longer being developed or supported. 

[SciKit-Learn](http://scikit-learn.org) provides an extensive set of
tools for classification, regression, clustering, dimensionality
reduction and model selection.  It is more mature than FACTORIE, and
it is easily integratable with matplotlib, a graphics package, so it
can produce high-quality graphs directly.  However, it does not support
graphical models.  SciKit-Learn is implemented in Python; in 
many cases it is less efficient than FACTORIE, which is JVM-based.

[Weka](http://www.cs.waikato.ac.nz/ml/weka/) is a collection of
machine learning algorithms, mostly for classification and
regression. Weka comes with a convenient GUI for visualization and
experimentation with small datasets, but it is not focused on large
sparse problems like factorie. It also does not support graphical
models.

[Apache Mahout](http://mahout.apache.org/) provides a set of
large-scale machine learning algorithms implemented on top of Apache
Hadoop's MapReduce system. Factorie is mostly a single-machine
framework, working at smaller scale but benefitting from the locality
efficiency that arise form this.

[LibLinear](http://www.csie.ntu.edu.tw/~cjlin/liblinear/) is a C
library for fast training of linear support vector machines and
l1-regularized logistic regression. Factorie has an implementation of
liblinear's l1-regularized algorithm, and supports reading and writing
of libsvm-style training examples for classification and regression.



### Probabilistic programming and graphical models toolkits

[Infer.NET](http://research.microsoft.com/en-us/um/cambridge/projects/infernet/) is a language for describing graphical models in C# and F#, which compiles inference down to CLR bytecode before execution. It is mostly focused on bayesian models with continuous random variables, and uses EP as its main inference algorithm. Factorie provides more general machine learning utilities, has a full nlp pipeline, and is mostly focused on discrete graphical models or other models in which inference can only be done by sampling and factors have to be lazily defined.

Figaro...

[Alchemy](http://ai.cs.washington.edu/projects/alchemy) is a library for stastistical relational learning, mostly structured around Markov Logic Networks. While MLNs are representable in factorie, its main focus is on other kinds of model. Factorie also provides many tools for machine learning and natural language processing which do not fit in the statistical relational learning framework.

PEBL...

[BUGS](http://www.mrc-bsu.cam.ac.uk/bugs/) is an automatic Gibbs sampler generator for Bayesian models. It works by defining a domain-specific language to write a model down, and then using known subroutines to automatically Gibbs sample all variables. While factorie does not have as many automatic sampling routines as BUGS, it allows for many other sampling styles, including annealing for maximization, as well as implementing belief propagation and other inference algorithms.

Church...



### Natural language processing toolkits

OpenNLP...

GATE...

OpenCalais...

Stanford Core NLP...



### Scalable, parallel and distributed machine learning toolkits

Apache Mahout...

GraphLab...

OptiML...

[Vowpal Wabbit](https://github.com/JohnLangford/vowpal_wabbit/wiki) is
a very efficient library for doing binary classification and, via
learning reductions, some other more complex models, such as neural
networks and topic models. VW runs on a single machine or distributed,
and is written in heavily optimized C code. While factorie implements
many variants of algorithms which VW also implements, the focuses of
the libraries are very different, as factorie prefers to have
in-memory data structures for easy access to and analysis of data,
while VW works by streaming things from disk. Factorie is also more
flexible when it comes to how the parameters are stored and how
learning is made, though it also supports VW-style feature hashing.

*/
