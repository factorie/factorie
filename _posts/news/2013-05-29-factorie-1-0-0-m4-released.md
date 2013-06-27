---
title: Factorie 1.0.0-M4 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M4 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is the first milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M4.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M4.jar)**
* Source files: [factorie-1.0.0-M4-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M4-src.tar.gz)

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
