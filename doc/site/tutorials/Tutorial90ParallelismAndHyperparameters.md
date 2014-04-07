---
title: "Parallelism"
layout: default
group: tutorial
weight: 100
---


Parallelism and Hyperparameter Optimization Tutorial
====================================================

Factorie has support for parallelism and distributed computation in some parts
which are often bottlenecks when working with machine learning and natural
language processing systems.

As a general rule, if some object doesn't look like it accesses global mutable
state it doesn't, and can be safely used from one thread while other threads do
other computations with no major drawbacks. At the same time, individual factorie
objects shouldn't be used from multiple threads unless explicitly specified in a
comment.

In this tutorial we will go over the main parts of factorie which explicitly
support parallelism, and how to use them efficiently. We will also cover factorie's
distributed computation facilities for hyperparameter optimization.


Often in feature-based models for classification or general structured linear models
a big fraction of the training and testing time is spent doing feature extraction
and creating the domain (see tutorial 010 on variables and domains), which is
responsible for mapping from user-friendly names (usually strings) to the integers
internally used to index into factorie tensors.

The most commonly used factorie domain is the CategoricalDomain

```scala
package cc.factorie.tutorial
object Tutorial90ParallelismAndHyperparameters extends App {
  import cc.factorie._
  import cc.factorie.app.nlp.{ Document, Token }
  import cc.factorie.app.chain.ChainModel
  import cc.factorie.app.nlp.segment.{ DeterministicSentenceSegmenter, DeterministicTokenizer }
  import cc.factorie.optimize.Trainer
  import cc.factorie.variable.{ LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain }
  import cc.factorie.infer.InferByBPChain
  implicit val random = new scala.util.Random(0)

  val c = new CategoricalDomain[String]()

```

The categorical domain maps from categories (which are usually strings but can be
any scala type) to integers. It is fully thread-safe: a single domain object
can be accessed from many threads with no additional set up required.

The one exception is that when the domain is being frozen no other thread should
be writing to it.

```scala
  for (feature <- (0 until 1000).par) {
    // calling .index on a domain will add the category to the domain if it's not present,
    // and return its index. It is fine to do this from many threads at once.
    c.index(feature.toString)
  }

```

Other objects which also use CategoricalDomains as their back ends, such as the
CategoricalVectorDomain are then also thread-safe.

Also for all DocumentAnnotators calling annotator.process in multiple threads
is safe.


Another big bottleneck in machine learning and natural language processing systems
is parameter estimation (learning). Factorie has automatic support for many
thread-based parallelism styles when doing parameter estimation. We will set up
a parameter estimation problem and then go over the main facilities factorie
provides for parallelizing it. For more information on parameter estimation and the
optimize package see the relevant tutorials on learning and optimization.

Here we create a simple chain model and one document, with some labels and features.


```scala
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
  val document = new Document("The quick brown fox jumped over the lazy dog.")
  DeterministicTokenizer.process(document)
  DeterministicSentenceSegmenter.process(document)
  document.tokens.foreach(t => t.attr += new Label(t, "A"))
  LabelDomain.index("B")
  document.tokens.foreach(t => {
    val features = t.attr += new Features(t)
    features += "W=" + t.string.toLowerCase
    features += "IsCapitalized=" + t.string(0).isUpper.toString
  })
  val example = new optimize.LikelihoodExample(document.tokens.toSeq.map(_.attr[Label]), model, InferByBPChain)

```

Though we only have one training example we can still use factorie's parallel
training facilities, and they should perform normally.

As seen in the learning tutorial, the main way of training a model with in batch
model (that is, accumulating the gradients of all training examples and giving
them to the optimizer all together) is by using Trainer.batchTrain

```scala
  Trainer.batchTrain(model.parameters, Seq(example))
```

Indeed, because there are no safety issues, Trainer.batchTrain uses parallelism
by default. To disable it, call

```scala
  Trainer.batchTrain(model.parameters, Seq(example), useParallelTrainer = false)
```

It is also possible to control how many threads will be used in the parallel
training process.

```scala
  Trainer.batchTrain(model.parameters, Seq(example), nThreads = 2)
```

By default factorie will use one learning thread per processor in your machine.

The default ParallelBatchTrainer stores one gradient tensor in memory and synchronizes
updates to it. Factorie also provides the ThreadLocalBatchTrainer, which keeps a
thread-local gradient tensor and uses no synchronization at all while computing
the gradients. It can be used like any other trainer. See the optimization tutorial
for more information.

Parallelism is also available in the online trainers, but it's disabled by default.
So, doing

```scala
  Trainer.onlineTrain(model.parameters, Seq(example))
```

will not use parallelism, and it has to be enabled explicitly, as in

```scala
  Trainer.onlineTrain(model.parameters, Seq(example), useParallelTrainer = true)
```

The default ParallelOnlineTrainer tries to keep the weights safe. It read locks
the weights tensors when doing predictions and write locks them when doing updates.

Sometimes this is not the desired behavior, and hence we provide other parallel
online trainers. These are the SynchronizedOptimizerOnlineTrainer, which does not
lock the weight vectors but synchronizes all accesses to the optimizer, and the
HogwildTrainer, which uses no locks at all, and should be used only if one is
willing to pay the cost of race-conditions or if one is implementing one's own
locking system on top of factorie. To see how to use your own trainers see the
optimization tutorial.


Finally, factorie provides facilities for distributed or locally parallel
hyperparameter optimization.

The main object which drives the hyperparameter optimization process is a
HyperparameterSearcher. To specify one we need to first define some command-line
options to be optimized.

```scala
  import cc.factorie.util.CmdOptions
  object opts extends CmdOptions {
    val dummy1 = new CmdOption("dummy1", "A", "STRING", "Doesn't mean anything")
    val dummy2 = new CmdOption("dummy2", 0.1, "DOUBLE", "Doesn't mean much either")
  }
```

Once we have the command-line options we need to creat Hyperparameter objects,
which are templates for how each hyperparameter can take values.

```scala
  import cc.factorie.util.{ HyperParameter, SampleFromSeq, UniformDoubleSampler }
  val d1 = new HyperParameter(opts.dummy1, new SampleFromSeq(Seq("A", "B", "C")))
  val d2 = new HyperParameter(opts.dummy2, new UniformDoubleSampler(0, 1))
```

Finally, one needs an executor, which is a function which will take a list of
strings, generated from those hyperparameters, and return a Future[Double].

Factorie doesn't care how this Future is computed. The simplest (but not that
useful) option is to just compute it right away and return a successful future

```scala
  import scala.concurrent.Future
  val executor0 = (a: Array[String]) => Future.successful(1.0)
```

A more interesting strategy is to return a future which will execute each training
job in parallel in the same machine

```scala
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.future
  val executor1 = (a: Array[String]) => future { 1.0 }
```

The HyperparameterSearcher polls the futures every once in a while to see when they
finish. It doesn't wait for them all to finish, which might never happen in distributed
cases. This is not a big problem because factorie uses random search for hyperparameter
optimization, and hence which jobs succeed and which fail does not really matter.

So you also need to specify how long to wait until completion, how many jobs do you
want to start, and for how many of those jobs do you wait until they finish.

Now we are ready to optimize the hyperparameters.

```scala
  val hyp = new cc.factorie.util.HyperParameterSearcher(opts, Seq(d1, d2), executor1, numTrials = 10, numToFinish = 5, secondsToSleep = 1)
  val optimizeArgs = hyp.optimize()
  assertStringEquals(optimizeArgs.length, "2")
}
```

args is a string containing the two winning hyperparameter values. We don't know
which because the optimization is non deterministic and we're always returning the
same value in this example.

Of course, optimizing the hyperparameters on the same machine is not always desirable.
For this reason, Factorie provides two executor classes, QSubExecutor and SSHActorExecutor,
which allow you to run jobs in other machines.

Both classes operate similarly: you give them some configuration options about the
distribution process itself and the name of a class which extends HyperparameterMain,
and that class's main function will be run on each machine, results will be serialized
to disk or through sockets, and the optimizer will proceed as desired.

QSubExecutor runs jobs in a job queue using a qsub command which can be seen in its source.
It is easy to provide an alternative job queue running script, by extending JobQueueExecutor.

SSHActorExecutor will take a username, a list of machines, and a directory, and will ssh
as that username into each machine, cd into the directory, and run jobs from there. Each
machine will only run one job at a time, so if your machines can handle more than that just
pass their names more than once in the list.

Since this tutorial should be runnable without distributed environments being properly set
up we can't run either of those classes. To see an example of how to use QSubExecutor see
for instance the DepParser2Optimizer object.
