---
title: "Learning and Optimization"
layout: default
group: usersguide
weight: 80
---


# Optimization

Much modern machine learning finds parameters by setting up an optimization
problem that trades off low error on the training data with having a
simple predictor that generalizes well. These optimization problems
are usually of the form:

`\[ \theta^* = \mathop{\arg\min}_\theta \sum_i \ell(x_i, y_i, \theta) + R(\theta) \]`

FACTORIE includes an optimization package directly designed to solve
optimization problems of the form above, with many variants of batch and
online algorithms, which can work sequentially or in parallel, and
supporting many different types of regularization.

The main traits involved in the optimization package are as follows.

## Parameters

Anything which can be optimized in FACTORIE extends the Parameters trait.
This simply allows for easy definition of Weights, and passing them to
the optimizers and trainers when necessary.

## Weights

To allow for efficient regularized online sparse updates some optimizers
need to control how predictions are computed. In FACTORIE this is
encapsulated by having optimizers rewrite the model's weights.

To allow that, the tensors which are going to be optimized have to be
placed inside special slots designed for this. Here is an example model:

```scala
class MyModel extends Parameters {
  val bias = Weights(new DenseTensor1(labelSize))
  val observations = Weights(new DenseTensor2(featureSize, labelSize))
}
```

then to use the weights, just do bias.value or observations.value and
things will work.

## Example

In FACTORIE an Example is an object which can compute an objective function
and its gradient (or a subgradient). FACTORIE has Examples defined for
maximum-likelihood training of CRFs, structured perceptron, structured SVM,
pseudolikelihood, pseudomax, generalized linear models, learning to rank,
matrix completion, and many others.

One of the main advantages of FACTORIE's framework is that it is easy to
write Examples and take advantage of many advanced optimizers and trainers.

## GradientOptimizer

Most optimizers in FACTORIE operate on gradients. The base class for all
such optimizers is GradientOptimizer.

Optimizers in FACTORIE are responsible for incorporating regularization,
since in online learning each type of regularization often requires
different optimizers.

FACTORIE has many already implemented optimizers, such as AdaGrad, MIRA,
AdaGradRDA, RDA, Pegasos, L2-regularized SGD, LBFGS, conjugate gradient,
and batch gradient descent with line search.

Many of these optimizers support parameter averaging out-of-the-box when
it is possible to do so.

## Trainer

While an Example is responsible for computing a gradient and a
GradientOptimizer for updating the model's Weights with respect to that
gradient, the Trainer is the glue connecting these two. In FACTORIE Trainers
are responsible for deciding when to evaluate the Examples, in which order,
for logging, and for parallelism.

FACTORIE has many online trainers, including the serial OnlineTrainer, the
locking ParallelOnlineTrainer (which locks the tensors), the locking
SynchronizedOptimizerOnlineTrainer (which locks the optimizer), and the
non-locking HogwildTrainer. Similarly, there is the serial BatchTrainer,
the ParallelBatchTrainer which keeps a single gradient vector, and the
ThreadLocalBatchTrainer which keeps a per-thread gradient vector.

## Convenience methods

Given a set of Examples, FACTORIE has convenience methods to pick
reasonable defaults for all optimization decisions. Simply calling
Trainer.onlineTrain or Trainer.batchTrain will do the right thing
in most cases.

# Learning

Learning in FACTORIE is more often than not done via the optimization
package described above. When learning graphical models the most convenient
approach is to use LikelihoodExamples, coupled with your favorite inference
method, and give them to Trainer.onlineTrain. This approach can do max
likelihood, but it can also do the structured perceptron or structured SVM,
though there are convenient examples for those as well.

When training models in which inference is infeasible the PseudoLikelihood
and PseudoMax examples can be very useful, or the
ContrastiveDivergenceExample.

FACTORIE also has support for SampleRank, a very efficient learning algorithm
which performs parameter updates while the variables are being sampled. To
use SampleRank simply create SampleRankExamples and pass them on to a
Trainer, or use the SampleRankTrainer directly.


