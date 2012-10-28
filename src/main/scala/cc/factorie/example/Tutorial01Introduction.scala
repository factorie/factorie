package cc.factorie.example


object TutorialIntroduction {
  
  /*&

FACTORIE is a toolkit for deployable probabilistic modeling, implemented as a software library in Scala.
It provides its users with a succinct language for creating relational factor graphs, estimating parameters and performing inference.
Its key features include the following:

- It is object-oriented, enabling encapsulation, abstraction and inheritance in the definition of random variables, factors, inference and learning methods.
- It is scalable, with demonstrated success on problems with billions of variables and factors, and on models that have changing structure, such as case factor diagrams.  It has also been plugged into a database back-end, representing a new approach to probabilistic databases capable of handling many billions of variables.
- It is flexible, supporting multiple modeling and inference paradigms.  Its original emphasis was on conditional random fields, undirected graphical models, MCMC inference, online training, and discriminative parameter estimation.  However, it now also supports directed generative models (such as latent Dirichlet allocation), and has support for variational inference, including belief propagation and mean-field methods.
- It is embedded into a general purpose programming language, providing model authors with familiar and extensive resources for implementing the procedural aspects of their solution, including the ability to beneficially mix data pre-processing, diagnostics, evaluation, and other book-keeping code in the same files as the probabilistic model specification.
- It allows the use of imperative (procedural) constructs to define the factor graph---an unusual and powerful facet that enables significant efficiencies and also supports the injection of both declarative and procedural domain knowledge into model design.
- The structure of generative models can be expressed as a program that describes the generative storyline by creating variables and specifying their parents.  
  The structure undirected graphical models can be specified similarly by explicitly creating factors and specifying their neighboring variables.
  However, most commonly the creation of factors for relational data is defined in templates which contain functions that create the necessary factors in a Turing-complete imperative style.  
  This usage of imperative programming to define various aspects of factor graph construction and operation is an innovation originated in FACTORIE; we term this approach imperatively-defined factor graphs.  The above three methods for specifying relational factor graph structure can be mixed in the same model.

FACTORIE has been successfully applied to various tasks in natural language processing and information integration, including
- named entity recognition
- entity resolution
- relation extraction
- parsing
- schema matching
- ontology alignment
- latent-variable generative models, including latent Dirichlet allocation.

The following code declares data, model, inference and learning for a linear-chain CRF for word segmentation.
Detailed explanations for this code are given later in the tutorial.
Here we intend to provide merely a flavor.
*/

  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.optimize._
  // Declare random variable types
  object TokenDomain extends CategoricalDomain[String]
  class Token(str:String) extends CategoricalVariable(str) { def domain = TokenDomain }
  class Label(value:Boolean, val token:Token) extends LabeledBooleanVariable(value) with ChainLink[Label,Sentence]
  class Sentence extends Chain[Sentence,Label]
  // Create random variables for data
  val data = List("See him run", "One fine day", "Into thin air")
  val sentences = for (s <- data) yield new Sentence ++= s.map(c => new Label(true, new Token(c.toString)))
  // Define a model
  val model = new Model {
    val markov = new DotFamilyWithStatistics2[Label,Label] { lazy val weights = new DenseTensor2(BooleanDomain.size, BooleanDomain.size)}
    val observ = new DotFamilyWithStatistics2[Label,Token] { lazy val weights = new DenseTensor2(BooleanDomain.size, TokenDomain.size)}
    def factors(v:Variable) = v match {
      case l:Label => List(new markov.Factor(l, l), new observ.Factor(l, l.token))
      case _ => Nil
    }
  }
  // Learn parameters
  val trainer = new BatchTrainer(model, new ConjugateGradient)
  trainer.processExamples(sentences.map(l => new LikelihoodExample(l.asSeq, InferByBPChainSum)))
  // Inference on test data, letting FACTORIE choose the inference method.
  sentences.foreach(s => Maximize(s.asSeq, model))
  
/*&    
Sponsors
---

Research and development of FACTORIE is supported in part by the UMass Center for Intelligent Information Retrieval; in part by Google, in part by the National Science Foundation under NSF grant #IIS-0803847, #IIS-0326249 and #CNS-0551597; in part by Army prime contract number W911NF-07-1-0216 and University of Pennsylvania subaward number 103-548106; and in part by SRI International subcontract #27-001338 and ARFL prime contract #FA8750-09-C-0181. Any opinions, findings and conclusions or recommendations expressed in this material are the authors' and do not necessarily reflect those of the sponsors.
*/
}