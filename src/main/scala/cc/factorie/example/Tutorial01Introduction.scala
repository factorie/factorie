package cc.factorie.example
import cc.factorie._


object TutorialIntroduction {
  def main(args:Array[String]): Unit = { println("TutorialIntroduction") }
  
  /*&

FACTORIE is a toolkit for deployable probabilistic modeling, implemented as a software library in Scala.
It provides its users with a succinct language for creating factor graphs, estimating parameters and performing inference.

Its key features include the following:

- It is object-oriented, enabling encapsulation, abstraction and inheritance in the definition of random variables, factors, inference and learning methods.
- It is scalable, with demonstrated success on problems with billions of variables and factors, and on models that have changing structure, such as case factor diagrams.  It has also been plugged into a database back-end, representing a new approach to probabilistic databases capable of handling many billions of variables.
- It is flexible, supporting multiple modeling and inference paradigms.  Its original emphasis was on conditional random fields, undirected graphical models, MCMC inference, online training, and discriminative parameter estimation.  However, it now also supports directed generative models (such as latent Dirichlet allocation), and has support for variational inference, including belief propagation and mean-field methods.
- It supports straightforward descent in layers of abstraction---from command-line tools, to succinct scripting, to customized models and inference procedures---no impenetrable black-boxes.
- It is embedded into a general purpose programming language, providing model authors with familiar and extensive resources for implementing the procedural aspects of their solution, including the ability to beneficially mix data pre-processing, diagnostics, evaluation, and other book-keeping code in the same files as the probabilistic model specification.
- It allows the use of imperative (procedural) constructs to define the factor graph---an unusual and powerful facet that enables significant efficiencies and also supports the injection of both declarative and procedural domain knowledge into model design.
- The structure of generative models can be expressed as a program that describes the generative storyline by creating variables and specifying their parents.  
  The structure undirected graphical models can be specified similarly by explicitly creating factors and specifying their neighboring variables.
  However, most commonly the creation of factors for relational data is defined in templates which contain functions that create the necessary factors in a Turing-complete imperative style.  
  This usage of imperative programming to define various aspects of factor graph construction and operation is an innovation originated in FACTORIE; we term this approach imperatively-defined factor graphs.  The above three methods for specifying relational factor graph structure can be mixed in the same model.

FACTORIE comes with pre-built models for:

- classification (including document classification, with MaxEnt, NaiveBayes, SVMs and DecisionTrees)
- linear regression
- linear-chain conditional random fields
- topic modeling (including latent Dirichlet allocation, as well as several variants)
- natural language processing, including many standard NLP pipeline components: tokenization, sentence segmentation, part-of-speech tagging, named entity recognition, dependency parsing
- topic models, including latent Dirichlet allocation and multiple variants 

FACTORIE has been successfully applied to various tasks, including:
- cross-document entity resolution, on 100k mentions, parallelized and distributed
- within-document coreference, supervised
- relation extraction, distantly supervised
- schema matching
- ontology alignment
- topics-over-time

This series of tutorials provides a detailed description of the FACTORIE framework.  
We begin however, with three examples   

Before descending into details, here are three examples to give you briefly a flavor of FACTORIE usage.

First, an example command-line tool.  The following reads textual documents from three directories corresponding to three classes, 
creates a bag-of-words feature vector representation, does a 50/50 train/test split,
trains a maximum-entropy classifier, prints results on the training and test data, then saves the trained classifier

fac classify --read-text-dirs sports politics arts --trainer MaxEnt --training-portion 0.5 --write-classifier my-classifier 

The following code declares data, model, inference and learning for a linear-chain CRF for part-of-speech tagging.
 */

  import cc.factorie._            // The base library: variables, factors
  import cc.factorie.la._         // Linear algebra
  import cc.factorie.optimize._   // Gradient-based optimization and training
  // Declare random variable types
  object TokenDomain extends CategoricalDomain[String]
  class Token(str:String) extends CategoricalVariable(str) { def domain = TokenDomain }
  object LabelDomain extends CategoricalDomain[String]
  class Label(str:String, val token:Token) extends LabeledCategoricalVariable(str) { def domain = LabelDomain }
  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]
  // Create random variables from data
  val data = List("See/V Spot/N run/V", "Spot/N is/V a/DET big/J dog/N", "He/N is/V fast/J")
  val labelSequences = for (sentence <- data) yield new LabelSeq ++= sentence.split(" ").map(s => { val a = s.split("/"); new Label(a(0), new Token(a(1)))})
  // Define a model
  val model = new Model {
    // Two families of factors, where factor scores are dot-products of sufficient statistics and weights (which will be trained below)
    val markov = new DotFamilyWithStatistics2[Label,Label] { lazy val weights = new DenseTensor2(LabelDomain.size, LabelDomain.size) }
    val observ = new DotFamilyWithStatistics2[Label,Token] { lazy val weights = new DenseTensor2(LabelDomain.size, TokenDomain.size) }
    override def families = Seq(markov, observ)
    // Given some variables, return the collection of factors that neighbor them.
    override def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => labels.map(label => new observ.Factor(label, label.token)) ++ labels.sliding(2).map(window => new markov.Factor(window.head, window.last))
    }
    def factors(v:Var) = throw new Error("This model does not implement unrolling from a single variable.")
  }
  // Learn parameters
  val trainer = new BatchTrainer(model, new ConjugateGradient)
  trainer.trainFromExamples(labelSequences.map(labels => new LikelihoodExample(labels, InferByBPChainSum)))
  // Inference on the same data.  We could let FACTORIE choose the inference method, 
  // but here instead we specify that is should use max-product belief propagation specialized to a linear chain
  labelSequences.foreach(labels => BP.inferChainMax(labels, model))
  // Print the learned parameters on the Markov factors.
  println(model.markov.weights)
  
}
