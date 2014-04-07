
Changelog
===

New in version 1.0.0-RC1
---

* Overall
    - Improved tutorials and documentation
    - Switched many classifiers and factors to score using left-multiplication which gives >3x speedup in many cases
    - Refactored usage of Var/Value type members, making Assignments nice to use, among other things
    - Moved many files into separate subpackages, new "Factorie" object provides default imports
    - Added automated performance testing of various models
    - Simplified labeled variables by removing several varieties

* NLP
    - Renaming of many NLP components
    - Simplified Spans with no self-types
    - Refactored Spans and Tags
    - New Phrase classes that generalizes mentions
    - Performance improvements for parsing
    - Fixes and accuracy improvements for POS tagging
    - Fixes, refactoring, improvements for mention finding
    - Fixes to mention entity type prediction
    - Fixes to tokenizer, coreference
    - Fixes and improved features / accuracy for NER
    - Cleanup of ACE and Ontonotes loaders
    - Efficiency improvements and fixes to app.chain command line tool

* Classifiers
    - New app.classify.backend package with enhanced and simplified support for GLMs, etc.
    - Fix to squared epsilon insensitive loss

* Learning
    - Many efficiency improvements to online and batch optimizers
    - Fix to BackTrackLineOptimizer that greatly speeds up BFGS and CG
    - API for initialization/finalization of weights added to GradientOptimizers
    - Speedup to parallel trainers by avoiding excess locking

* Inference
    - Greatly improved efficiency for inference and learning in chains using ChainModel
    - Big refactoring/cleanup and fixes to BP

* Linear Algebra
    - Fixes and performance improvements to many tensor operations
    - Fixes and speed/safety improvements to smart tensor accumulators

* Serialization
    - Added version numbers and IDs to Cubbie serialization
    - Added buffering for speed improvements

New in version 1.0.0-M7:
---

* Overall
    - Removed deprecated code
    - Improved tutorials and documentation
    - Improved command line tools

* NLP
    - New tokenizers and sentence segmenter
    - Reworked DocumentAnnotator annotation pipeline
    - Parallel LDA implementation
    - Improved NER
    - Conll2000 loader
    - Support for loading NER3 models from classpath (NER3 requires dependency on factorie-nlp-resources-ner project)
    - Added support for word embeddings in NER3
    - Bugfixes and improvement to mention finders, new NerAndPronounMentionFinder

* Learning
    - Efficiency improvements to accumulators, trainers, and weights maps
    - Small bugfixes to OnlineTrainer and hyperparameter optimization

* Inference
    - Changed Infer API
    - Bugfix to dual decomposition

New in version 1.0.0-M6:
---

* Overall
    - Website hosted on github
    - removing deprecated code

* NLP
    - much improved mention annotators
    - classifier-based mention entity type predictor
    - deprecated annotators removed: DepParser1, WithinDocCoref1
    - CorefGazetteers removed
    - new pronoun lexicons
    - bugfixes and improvements to coreference

* Learning
    - removing broken optimizers
    - bugfix in SampleRank when proposals have same score

New in version 1.0.0-M5:
---

* Overall
    - Move to Scala 2.10.1
    - Migration to github
    - Better handling of Implicits
    - Hyperparameter optimization
    - support for conditional dependencies as profiles in pom
    - improved tutorials

* NLP
    - Command line interface (see README.txt)
    - Documents contain Sections
    - Support for reading models from a variety of sources (classpath, files, urls, etc.)
    - Default annotators that load models from the classpath
    - Overhauled lexicons handling
    - new annotators for mention type, gender, number, etc.
    - better support for OntoNotes and all its annotations (parsing, coreference, etc)
    - better support for ACE and relations
    - much improved parse-based mention finding
    - improvements to Tokenizers and Segmenters
    - addition of SparseLDA
    - more unification of data structures across different tasks
    - bugfixes and speed improvements

* Variables and values
    - Refactoring of Assignment

* Inference
    - support for arbitrary number of neighbors in MPLP
    - bugfixes and speed improvements

* Learning
    - regularized dual averaging (RDA) added
    - exponentiated gradient optimizer

* Serialization
    - major bugfixes and speed improvements

* Linear Algebra
    - bugfixes and major speed improvements

New in version 1.0.0-M4:
---

* Variables and values
    - Top of the variable hierarchy, Variable, renamed to Var.
    - Spring cleaning of many variables and domains, including
    (a) replacing Var.isContant with trait VarWithConstantValue
    (b) removing cascadeUnroll, (c) moving ~ and :~ methods to
    cc.factorie.directed.
    - DiscreteValue and CategoricalValue no longer have a "domain"
    member, (similarly to TensorVar values).
    - CategoricalVariable now throws an error if its initial value
    is not found or placed into its domain.

* Model and Templates
    - The way parameters are created and stored has been centralized.
    New trait Weight is a TensorVar with Tensor value.  New traits TensorSet and
    WeightsSet store a collection of Weights (usually one Tensor per factor family)
    used to store parameters.  New trait WeightsMap stores a set of Tensors
    separate from the Weights' Tensor values, but which are looked up by Weights as
    keys.  These are typically used to store sufficient statistics and gradients.
    - Models no longer have weights by default.  Inherit from Parameters
    to provide "def parameters: WeightsSet".  For example, many places you
    previously had "TemplateModel" will now need "TemplateModel with Parameters".
    - The syntax for creating weights inside a DotFamily or DotTemplate has changed.
    Rather than "lazy val weights = new DenseTensor1(mydomain.size)" instead
    "val weights = Weights(DenseTensor1(mydomain.size))"

* Inference
    - There is no more optional Summary argument to Infer.infer: if
    one wants to specialize inference based on something the recommended
    way to do so is by storing this state in an instance of an object
    which implements Infer.
    - Marginals have been specialized. Most Summaries now are only expected
    to return Marginal1s over single variables or FactorMarginals, which
    represent factor expected sufficient statistics (for training).
    - New MAPSummary, which can be constructed from an Assignment and which allows
    for training using any kind of MAP inference algorithm.
    - Mew MAP inference algorithm, MPLP.

* Example, Trainer, Optimizer
    - The optimize package has been reworked to fit in better with the new
    way of storing weights. Now Example.accumulateExampleInto no longer
    gets passed a model. The Trainers also no longer need models, but just
    their weightsSets, which can be obtained from model.parameters if the
    model has Parameters.  Hence previous calls to "BatchTrainer(model, new AdaGrad)"
    must be changed to "BatchTrainer(model.parameters, new AdaGrad)"
    - The trainers have been renamed for clarity. We now have two parallel
    batch trainers: ParallelBatchTrainer, which locks the accumulator, and
    works better with examples which take a long time to compute (things
    which run inference, for example), and ThreadLocalBatchTrainer which
    keeps a thread-local gradient and works best for classifiers and other
    models with very fast "inference". Likewise there are two online
    trainers: ParallelOnlineTrainer which uses read-write locks on the
    weights, and SynchronizedOptimizerOnlineTrainer, which locks the
    optimizer.
    - There are many changes to the optimizers as well. Now we have a
    specific type of optimizer called GradientStep, which all support
    things like MIRA, adaptive learning rates, and averaging. We also have
    optimizers which are not GradientSteps but support more interesting
    online optimization algorithms, such as the AdaGradRDA, which does
    l1/l2 regularized adagrad dual averaging, Pegasos, and
    L2RegularizedConstantRate, which do l2 regularization.
    - SampleRankTrainer moved to cc.factorie.optimize.
    - New framework for linear objective functions, along with Examples
    for multiclass/binary classification and multi/univariate regression.

* NLP
    - New DocumentAnnotator infrastructure; automatically invokes prerequisites.
    - New DocumentAnnotators for tokenization, lemmatization, part-of-speech tagging,
    mention chunking, dependency parsing.
    - Various label domains now constant, e.g., PTBPosDomain, ConllNerDomain.
    PosLabel removed and replaced by PTBPosLabel.
    - Move Lexicon from app.chain to app.nlp and make more efficient for single
    word entries.
    - New interface for querying WordNet data, including synsets and lemmatization.
    - LoadOntonotes5 updated for correct format.
    - New method Token.stringNormalized allows multiple string transformations to
    coexist.
    - app.nlp.NLP is a new command-line socket-based server for processing text
    with a sequence of DocumentAnnotators.
    - LoadPlainText no longer performs token or sentence segmentation, relying
    on a DocumentAnnotator to do that.
    - New part-of-speech tagger app.nlp.POS3 is a fast feedforward tagger
    with good accuracy.
    - Three new dependency parsers, all of which support the DocumentAnnotator API.
    DepParser1 is a simple proof-of-concept projective shift-reduce dependency parser.
    DepParser2 is a state-of-the-art non-projective shift-reduce dependency parser.
    GraphProjectiveParser is a first-order projective dependency parser.

* Serialization
    - Domains and models can be serialized and deserialized in an order-independent manner.
    - Serialization support for many different types of tensors.

* Linear algebra
    - Tensor trait hierarchy refactoring: explicit trait SparseTensor, singleton tensors
    now implement the appropriate binary/indexed sparse tensor trait, added parent trait
    Outer2Tensor to share efficient operations for outer products.
    - Performance improvements to sparse tensors.

New in version 1.0.0-M3:
---

* Documentation
  - improved existing tutorials
  - new tutorial on Inference and Learning
  - better TUI
  - better comments and error messages
  - Parser Demo
  - site can be generated at the users' end

* Models and Templates
  - support for feature hashing
  - Massive renaming of Variables and Domains

* NLP
  - Classifier based POS tagger
  - added port of ClearNLP tokenizer/segmenter
  - Faster Bibtex parser
  - REST API for Parsers

* Inference
  - support efficient inference for ChainModels
  - Sampler can return a DiffList of all the changes
  - bugfixes in MHSampler
  - BP logZ implemented to enable likelihood learning

* Optimization and Training
  - Removed redundant SampleRank
  - Added Pegasos. Pseudo-likelihood, Contrastive Divergence, StructSVM, AdaGrad
  - new ClassifierTrainer to support all types of losses, trainers and optimizers
  - better multi-threaded support
  - bugfixes and efficiency improvements

* Tensors
  - speed enhacements and bugfixes
  - more operations implemented
  - new tests for Tensors

* Serialization
  - all new serialization based on Cubbies

New in version 1.0.0-M2:
---

* Documentation
  - markdown based website, the source for which is checked into the repository
  - Tutorial on Domains
  - more assertions throughout the code (including tutorials)
  - better Tutorial prettifier

* Models and Templates
  - Factors can provide statistics and scores on any Assignment and valueTensors
  - trait Model independent of context, ModelWithContext[C] can unroll given any context

* NLP
  - Abstracted dependency parser prediction for easily dropping in alternative classifiers.
  - Bootstrapping for improved dependency parser training.

* Inference
  - BPSummary is more efficient, includes an abstract version

* Optimization and Training
  - Pieces are now Examples, Learners are Trainers
  - MaxlikelihoodExample is efficient in computing constraints
  - SampleRankExample replaces old trainer, almost as efficient

* Classifiers
    - Added DecisionTree, AdaBoost, SVM classifiers in app.classify

* Tensors
  - Filled in more of the missing cases in Tensors
  - Fixed indexing bugs in a few Tensor types
  - OuterTensors that efficiently represent the outer product between Tensors

* Serialization
  - gzip support

New in version 1.0.0-M1:
---

* Models and Templates

  - All templates are now Models
  - Models are now parameterized by the type of things they can score
  - It is possible to write code that does not deduplicate factors

* NLP
  - new Ontonotes Loader
  - new Nonprojective Dependency parser

* Inference
  - Summary class now maintains the marginals, and is common to Samplers and BP
  - Reimplementation of BP to be more efficient

* Optimization & Training
  - efficient L2-regularized SVM training
  - integration with app.classify
  - support for parallel batch and online training with a Piece API
  - support for Hogwild (including Hogwild SampleRank)

* Tensors
  - all new la package that replaces the earlier Vector classes with Tensors
  - Tensors can be multi-dimensional, with implementations that independently choose sparsity/singleton for each dimension
  - weights and features now use Tensors

* Serialization
  - Serialization exists in a different class

* Misc
  - Added Tutorials to walkthrough model construction
  - Cleaned examples so that they work (added a test that makes sure they do)

New in version 0.10.2:
---

* NLP
  - Customized forward-backward and viterbi for chain models
  - changes to the coreference data structures that support hierarchical models
  - new data loaders
  - models can be loaded from JARs (POS model in IESL Nexus)
  - initial dependency parser

* BP
  - Refactoring to be faster and cleaner interface, with bugfixes
  - Caching of scores and values
  - MaxProduct works even when multiple MAP states
  - TimingBP to compare performance of the different variants of BP in the codebase
  - maxMarginal with threshold, to support PR curves
  - some initial parallelization

* Max likelihood training
  - convenience constructors for selecting which families to update
  - pieces can use families for inference that are not updated

* Trainer that uses Stochastic gradient descent

* Cubbie
  - new united interface for serialization/persistence (including mongodb support)

* Hierarchical Coref Model
  - added model that supports arbitrarily deep and wide hierarchy of entites, aka Wick, Singh, McCallum, ACL 2012

* Gzip saving/loading of models
* Data loaders for bibtex, dblp, etc.
* Better support for limitedValues and sparse domains on factors
* Code cleanup, including deletion of inner/outer factors

New in version 0.10.1:
---
  
* Many renames, new features and refactors; the list below is partially complete.

* Initial support for sparse value iteration in factor/families

* Data representation for app.nlp like Tokens, ParseTrees, Spans, Sentences, etc.

* Initial version for POS, NER, within-doc coref for app.nlp

* Additional vectors that mix sparse and dense representations (SparseOuterVector) in factorie.la

* Added Families that represent sets of factors. Templates are a type of Family now.

* Initial support for MaxLikelihood and Piecewise Training using the new BP framework

* Added a more flexible, modular BP framework

* DiscreteVector and CategoricalVector

  The old names "DiscretesValue", "DiscretesVariable", etc were
deemed too easily misread (easy to miss the little "s" in the middle)
and have been renamed "DiscreteVectorValue", "DiscreteVectorVariable",
etc.

* Factors independent of Templates

* Models independent of Templates

* Redesigned cc.factorie.generative package

New in version 0.10.0:
---

* Variable 'value' methods:

  All variables must now have a 'value' method with return type
'this.Value'. By default this type is Any. If you want to override
use the VarAndValueType trait, which sets the covariant types
'VariableType' and 'ValueType'. 'Value' is magically defined from
these to be psuedo-invariant.

  The canonical representation of DiscreteVariable (and
CategoricalVariable) values used to be an Int. Now it is a
DiscreteValue (or CategoricalValue) object, which is a wrapper around
an integer (and its corresponding categorical value). These objects
are created automatically in the DiscreteDomain (or
CategoricalDomain), and are guaranteed to be unique for each integer
value, and thus can be compared by pointer equality.

  For example, if 'label' is a CategoricalVariable[String]
label.value is a CategoricalValue.
label.intValue == label.value.index, is an integer
label.categoryValue == label.value.category, is a String

* Discrete variables and vectors

  DiscreteValues has been renamed DiscretesValue. Similarily there are
now classes DiscretesVariable, CategoricalsValue and
CategoricalsVariable. These plural names refer to vector values and
their variables. For example, CategoricalsVariable is a superclass of
the BinaryFeatureVectorVariable.

  The singular DiscreteValue, DiscreteVariable, CategoricalValue and
CategoricalVariable hold single values (i.e. which could be mapped to
single integers), but are subclasses their plural counterparts, with
values that are singleton vectors.

  The domain of the plural types (i.e. vectors, not necessarily
singleton vectors) are DiscretesDomain and CategoricalsDomain. The
length of these vectors are determined by an inner DiscreteDomain or
CategoricalDomain. Hence to create a domain for vectors of length 10:

    new DiscretesDomain {
      val dimensionDomain = new DiscreteDomain { def count = 10 }
    }

* TrueSetting renamed to TargetValue

  Now that all variables have a 'value', the name 'setting' is
deprecated. Also, "true" and "truth" were deemed confusable with
boolean values, and are now deprecated. The preferred alternative is
"target". Hence, the "TrueSetting" trait has been renamed
"TargetValue", and various methods renamed:
setToTruth => setToTarget
valueIsTruth => valueIsTarget
trueIntValue => targetIntValue

* Domains:

  Previously there was a one-to-one correspondence between variable
classes and domains; the variable looked up its domain in a global
hashtable whose keys were the variable classes. Furthermore Domain
objects were often created for the user auto-magically. This scheme
lacked flexibility and was sometimes confusing. The one-to-one
correspondence has now been removed. The 'domain' method in Variable
is now abstract. Some subclasses of Variable define this method, such
as RealVariable; others still leave it abstract. For example, in
subclasses of DiscreteVariable and CategoricalVariable you must define
the 'domain' method. In these cases you must also create your domain
objects explicitly. Thus we have sacrificed a little brevity for
clarity and flexibility. Here is an example of typical code for
creating class labels:

    object MyLabelDomain extends CategoricalDomain[String]
    class MyLabel(theValue:String) extends CategoricalVariable(theValue) {
      def domain = MyLabelDomain
    }

  or

    class MyLabel(theValue:String, val domain = MyLabelDomain) extends CategoricalVariable(theValue)

  The type argument for domains used to be the variable class; now it is
the 'ValueType' type of the domain (and its variables).

  Templates now automatically gather the domains of the neighbor
variables. VectorTemplates also gather the domains of their
statistics values. [TODO: Discuss the dangers of this automatic
mechanism and consider others mechanisms.]



* Template statistics:

  Previously the constructor arguments of Stat objects were Variables.
They have now been changed to Variable *values* instead. Furthermore,
whereas the old Template.statistics method took as arguments a list
of variables, the new Template.statistics method takes a "Values"
object, which is a simple Tuple-style case class containing variable values.

  For example, old code:

    new Template2[Label,Label] extends DotStatistics1[BooleanVariable] {
      def statistics(y1:Label, y2:Label) =
        Stat(new BooleanVariable(y1.intValue == y2.intValue)
    }

  might be re-written as:

    new Template2[Label,Label] extends DotStatistics1[BooleanValue] {
      def statistics(values:Values) = Stat(values._1 == values._2)
    }

* VectorTemplate

  VectorStatistics1, VectorStatistics2, VectorStatistics3 used to take
VectorVar type arguments. They now take DiscretesValue type
arguments. The method 'statsize' has been renmed
'statisticsVectorLength' for clarity.

* Generative modeling package

  The probability calculations and sampling routines are no longer
implemented in the variable, but in templates instead. Each
GeneratedVar must have a value "generativeTemplate" and a method
"generativeFactor". Many changes have been made to the generative
modeling package, but they are not yet finished or usable. The code
is being checked in now in order to facilitate others' work on the
undirected models.

New in Version 0.9.0:
---

Rudimentary optimize package includes ConjugateGradient and
LimitedMemoryBFGS.

LogLinearMaximumLikelihood sets parameters by BFGS on likelihood
gradient calculated by belief propagation on trees. Additional
inference methods to come soon.

Belief propagation now works.

Variables no longer use their own "set" method to initialize their
values. This means that if you are relying on "override def set" to
do some coordination during object initialization, you must separately
set up this coordination in your own constructors.

Rename Factor neighbor variables from "n1" to "_1" to better match
Scala's Tuples.

Support for generative models has been completely overhauled, and is
now in its own separate package: cc.factorie.generative.

Many variables have been renamed to better match standard names in
statistics, including EnumVariable => CategoricalVariable.

New in Version 0.8.1:
---