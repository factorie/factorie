/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
/*& Optimization */
/*&

# Optimization

Much modern machine learning finds parameters by setting up an optimization
problem that minimizes error on the training data while penalizing model complexity
with a regularization term. Formally, these optimization problems take the form:

`\[ \theta^* = \mathop{\arg\min}_\theta \sum_i \ell(x_i, y_i, \theta) + R(\theta) \]`

Where `\[ \theta \]` is a parameter vector, `\[ \ell \]` is the per-example loss function,
and `\[ R \]` is the regularizer. FACTORIE includes an optimization package specialized for
these problems. Learning problems can be solved in batch mode or online, sequentially or
in parallel, and with many different choices of regularization and parameter update rules.

The main traits involved in the optimization package are as follows:

## Parameters and Weights

In FACTORIE, the parameter vector `\[ \theta ]\` is represented by Weights and Parameters objects. A Weights object
defines a mutable slot containing a single parameter tensor. Since models often define multiple groups
of parameters, corresponding to different types of factors or feature templates, anything
which can be optimized in FACTORIE extends the Parameters trait, which serves as a container
for passing sets of Weights to optimizers and trainers. This trait also allows for easy
definition of individual Weights objects.

Concretely, Parameters provides several overloaded constructor methods for Weights objects.
These methods take a default initialization tensor and define slots which are registered to
the containing Parameters object:

```scala
class MyModel extends Parameters {
  val bias = Weights(new DenseTensor1(labelSize))
  val observations = Weights(new DenseTensor2(featureSize, labelSize))
}
```

To allow for efficient regularized online learning with sparse gradients, some optimizers
need to control how predictions are computed and weights updated (for example, by storing
weights as a combination of a scalar multiplier and a base array, or storing a sum of observed
gradients which is sparsified during prediction). In FACTORIE this is implemented by having
optimizers swap these alternate tensor implementations into the model's Weights' slots.

## Example

In FACTORIE an Example is an object which can compute an objective function
and its gradient (or a subgradient), at the level of a single piece of training data (corresponding to
one of the `\[ \ell ]\` functions in the learning objective). FACTORIE defines Examples for
maximum-likelihood training of CRFs, structured perceptron, structured SVM,
pseudolikelihood, pseudomax, generalized linear models, learning to rank,
matrix completion, and many others.

One of the main advantages of FACTORIE's framework is that it is easy to
write new Examples and take advantage of many advanced optimizers and trainers.

## GradientOptimizer

Most optimizers in FACTORIE operate on gradients (although there are some standalone classes
for non-gradient-based learning methods such as LibLinear SVM). The base trait for all
such optimizers is GradientOptimizer.

Optimizers in FACTORIE are responsible for incorporating regularization, since naively
including the optimizer gradient in the Example would be impractically inefficient for online learning,
and even in batch mode is incapable of achieving sparsity for l1 regularization.

FACTORIE has many already implemented optimizers, such as AdaGrad, MIRA,
AdaGradRDA, RDA, Pegasos, L2-regularized SGD, LBFGS, conjugate gradient,
and batch gradient descent with line search.

Many of these optimizers support parameter averaging out-of-the-box when
it is possible to do so.

## Trainer

While Examples compute gradients, and GradientOptimizers use
gradients to update Weights, the Trainer is the glue connecting these two. In FACTORIE,
Trainers are responsible for evaluating the Examples, ordering them, coordinating parallelism,
and implementing logging.

FACTORIE has many online trainers, including the serial OnlineTrainer, the
locking ParallelOnlineTrainer (which locks the Weights tensors), the locking
SynchronizedOptimizerOnlineTrainer (which locks the optimizer, which may store other
mutable state that requires thread safety), and the
non-locking HogwildTrainer. Similarly, there is the serial BatchTrainer,
the ParallelBatchTrainer which keeps a single gradient vector, and the
ThreadLocalBatchTrainer which keeps a per-thread gradient vector.

## Convenience methods

Given a set of Examples, FACTORIE has convenience methods that include
reasonable defaults for the ther components of a learning pipeline. Calling the methods
Trainer.onlineTrain or Trainer.batchTrain provides a strong baseline learning method
in most cases.

## Learning Graphical Models

Learning in FACTORIE is more often than not done via the optimization
package described above. When learning graphical models the most convenient
approach is to use LikelihoodExamples, coupled with an appropriate inference
method, and pass the examples to Trainer.onlineTrain. This approach is designed for
maximum likelihood learning, but depending on the choice of inference method it is
equivalent to structured perceptron or structured SVM learning. However, there are
convenient Examples for those methods as well.

When training models in which inference is intractable, it can be more appropriate
to use the PseudoLikelihood- and PseudoMaxExamples, or the
ContrastiveDivergenceExample.

FACTORIE also includes support for SampleRank, an efficient learning algorithm for
models using sampling based inference, which interleaves parameter updates between steps
of sampling. To use SampleRank simply create SampleRankExamples
and pass them on to a Trainer, or use the SampleRankTrainer directly.

Semi-supervised methods such as posterior regularization can be applied by using the
SemisupervisedExample, which takes a constrained and unconstrained inference method
(corresponding to some form of partial supervision), and trains the base model so that
unconstrained inference matches the constrained model.

## Sample Code

Now we will apply the FACTORIE optimization framework to the concrete task of learning a linear-chain
conditional random field, a type of model used for part-of-speech tagging, named-entity recognition,
or noun phrase chunking. The linear chain will be our running example, but generalizes to any other type
of graphical model.

First we need to set up a basic model, features, and single training example:
*/
package cc.factorie.tutorial

object UsersGuide70LearningAndOptimization extends App {
  import cc.factorie._
  import variable._
  import cc.factorie.app.nlp._
  import cc.factorie.app.chain._
  import cc.factorie.optimize.{SynchronizedOptimizerOnlineTrainer, Trainer, SampleRankTrainer}
  import cc.factorie.infer.{GibbsSampler, InferByBPChain}
  import cc.factorie.optimize.OnlineTrainer

  implicit val random = new scala.util.Random(0)

  object LabelDomain extends CategoricalDomain[String]
  class Label(val token: Token, s: String) extends LabeledCategoricalVariable(s) {
    def domain = LabelDomain
  }
  object FeaturesDomain extends CategoricalVectorDomain[String]
  class Features(val token: Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
  }

  object model extends ChainModel[Label, Features, Token](
    LabelDomain,
    FeaturesDomain,
    l => l.token.attr[Features],
    l => l.token,
    t => t.attr[Label])

  // The Document class implements documents as sequences of sentences and tokens.
  val document = new Document("The quick brown fox jumped over the lazy dog.")
  val tokenizer = new app.nlp.segment.DeterministicTokenizer
  tokenizer.process(document)
  val segmenter = new app.nlp.segment.DeterministicSentenceSegmenter
  segmenter.process(document)
  assertStringEquals(document.tokenCount, "10")
  assertStringEquals(document.sentenceCount, "1")

  // Let's assign all tokens the same label for the sake of simplicity
  document.tokens.foreach(t => t.attr += new Label(t, "A"))
  // Let's also have another possible Label value to make things interesting
  LabelDomain.index("B")
  // Let's also initialize features for all tokens
  document.tokens.foreach(t => {
    val features = t.attr += new Features(t)
    // One feature for the token's string value
    features += "W=" + t.string.toLowerCase
    // And one feature for its capitalization
    features += "IsCapitalized=" + t.string(0).isUpper.toString
  })
/*&
Now we can demonstrate several ways to learn parameters for this model:
*/
  // This example calculates the maximum likelihood (CRF) gradient for an example
  val example = new optimize.LikelihoodExample(document.tokens.toSeq.map(_.attr[Label]), model, InferByBPChain)

  // We can take advantage of the Trainer object's default methods for learning
  Trainer.onlineTrain(model.parameters, Seq(example))

  // Or we can put together a learning pipeline from scratch:
  val optimizer0 = new optimize.AdaGrad()
  val trainer = new OnlineTrainer(model.parameters, optimizer0)

  trainer.trainFromExamples(Seq(example))

  // We can also learn using alternative objectives, such as SampleRank
  val sampler = new GibbsSampler(model, HammingObjective)
  val sampleRankExamples = document.tokens.toSeq.map(t => new optimize.SampleRankExample(t.attr[Label], sampler))
  Trainer.onlineTrain(model.parameters, sampleRankExamples, optimizer = optimizer0)
}
/*&
Many other choices of inference, optimizers, and examples can be swapped into the above format
to optimize different training objectives with minimal code changes.
 */