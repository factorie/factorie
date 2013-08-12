/*&

# Introduction

[FACTORIE](http://factorie.cs.umass.edu) is a toolkit for deployable probabilistic modeling, implemented as a software library in [Scala](http://scala-lang.org).
It provides its users with a succinct language for creating [factor graphs](http://en.wikipedia.org/wiki/Factor_graph), estimating parameters and performing inference.

## Purpose and Capabilities

FACTORIE aims to provide a full-featured framework for probabilistic graphical models (both directed and undirected) that is both flexible and scalable.  
Thus it supplies infrastructure for representing random variables, 
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
- It does not yet have a simple declarative syntax for specifying model structure.  It has an extremely flexible mechanism for definition model structure directly in Scala; a simpler front-end syntax for beginners will be added in the future. 
- It is not yet connected to a tool for directly producing graphs or other visualizations.
For further discussion of FACTORIE's comparison to other related tools see [Tutorial010SimilarTools.scala.html].

FACTORIE comes with pre-built model structures for:

- classification (including document classification, with MaxEnt, NaiveBayes, SVMs and DecisionTrees)
- linear regression
- linear-chain conditional random fields
- topic modeling (including latent Dirichlet allocation, as well as several variants)
- natural language processing, including many standard NLP pipeline components: tokenization, sentence segmentation, part-of-speech tagging, named entity recognition, dependency parsing.

FACTORIE has been successfully applied to various tasks, including:

- cross-document entity resolution (on 100 million mentions, parallelized and distributed)
- within-document co-reference, supervised
- parallel/distributed belief propagation
- transition-based and graph-based dependency parsing
- relation extraction, distantly supervised
- schema matching
- ontology alignment
- parallelized latent Dirichlet allocation.



## First Examples

Before descending into details, here are three brief examples providing a flavor of FACTORIE usage.

### Topic Modeling and Document Classification on the Command-line

FACTORIE come with a pre-built implementation of the [latent Dirichlet allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) topic model. 
Assume that "mytextdir" is a directory name containing many plain text documents each in its own file.  Then typing 
```
$ bin/fac lda --read-dirs mytextdir --num-topics 20 --num-iterations 100
```
will run 100 iterations of a sparse collapsed Gibbs sampling on all the documents, and print out the results every 10 iterations. FACTORIE's LDA implementation is faster than [MALLET](http://mallet.cs.umass.edu)'s.

You can also train a document classifier. Assume that "sportsdir" and "politicsdir" are each directories that  contain plan text files in the categories sports and politics. Typing
```
$ bin/fac classify --read-text-dirs sportsdir politicsdir --write-classifier mymodel.factorie
```
will train a log-linear classifier by maximum likelihood (same as maximum entropy) and save it in the file "mymodel.factorie".


### Univariate Gaussian
*/

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
  Maximize(mean, variance)
  println("estimated mean %f variance %f".format(mean.value, variance.value))
}

/*&

### Linear-chain Conditional Random Field for part-of-speech tagging

The following code declares data, model, inference and learning for a linear-chain CRF for part-of-speech tagging.
 */

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
    override def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => labels.map(label => new observ.Factor(label, label.token)) ++ labels.sliding(2).map(window => new markov.Factor(window.head, window.last))
    }
    def factors(v:Var) = throw new Error("This model does not implement unrolling from a single variable.")
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
