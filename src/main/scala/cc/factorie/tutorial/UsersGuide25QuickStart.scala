/*& Quick Start */
/*&
## First Examples

Here are three examples providing a brief sense of FACTORIE usage.

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
17      6       moved           VBD     0       rooat    O
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

```scala
package cc.factorie.tutorial
object ExampleGaussian extends App {
  import cc.factorie._                             // The base library
  import cc.factorie.directed._                    // Factors for directed graphical models
  import cc.factorie.variable_
  import cc.factorie.model._
  import cc.factorie.infer._
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

```scala
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
  class Label(str:String, val token:Token) extends LabeledCategoricalVariable(str){
    def domain = LabelDomain
  }
  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]
  // Create random variable instances from data (a toy amount of data for this example)
  val data = List("See/V Spot/N run/V", "Spot/N is/V a/DET big/J dog/N", "He/N is/V fast/J") 
  val labelSequences = for (sentence <- data) yield new LabelSeq ++= sentence.split(" ").map(s => {
   val a = s.split("/")
   new Label(a(1), new Token(a(0)))
  })
  // Define a model structure
  val model = new Model with Parameters {
    // Two families of factors, where factor scores are dot-products of sufficient statistics and weights.
    // (The weights will set in training below.)
    val markov = new DotFamilyWithStatistics2[Label,Label] { 
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
    }
    val observ = new DotFamilyWithStatistics2[Label,Token] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.size))
    }
    // Given some variables, return the collection of factors that neighbor them.
    def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => 
        labels.map(label => new observ.Factor(label, label.token))
        ++ labels.sliding(2).map(window => new markov.Factor(window.head, window.last))
    }
  }
  // Learn parameters
  val trainer = new BatchTrainer(model.parameters, new ConjugateGradient)
  trainer.trainFromExamples(labelSequences.map(labels => new LikelihoodExample(labels, model, InferByBPChain)))
  // Inference on the same data.  We could let FACTORIE choose the inference method, 
  // but here instead we specify that is should use max-product belief propagation
  // specialized to a linear chain
  labelSequences.foreach(labels => BP.inferChainMax(labels, model))
  // Print the learned parameters on the Markov factors.
  println(model.markov.weights)
  // Print the inferred tags
  labelSequences.foreach(_.foreach(l => println(s"Token: ${l.token.value} Label: ${l.value}")))
}
```
*/
