package cc.factorie.tutorial
import cc.factorie._
import variable._
import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import cc.factorie.optimize.{SynchronizedOptimizerOnlineTrainer, Trainer, SampleRankTrainer}
import cc.factorie.infer.{GibbsSampler, InferByBPChain}

object Tutorial60Learning {
  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    /*& Here we set up a simple linear chain CRF, such as the one used for part-of-speech tagging,
     * named-entity recognition, or noun phrase chunking. It will be our running example in this
     * tutorial, but most of the things we'll discuss generalize all across factorie.
     **/
    object LabelDomain extends CategoricalDomain[String]
    class Label(val token: Token, s: String) extends LabeledCategoricalVariable(s) {
      def domain = LabelDomain
    }
    object FeaturesDomain extends CategoricalVectorDomain[String]
    class Features(val token: Token) extends BinaryFeatureVectorVariable[String] {
      def domain = FeaturesDomain
    }

    /*& The ChainModel class implements a default model for linear chains.
     * It by default implements all the factor templates one expects from a linear chain model,
     * with the exception that the (label, label, features) template is optional.
     *
     * As we saw in the Model tutorial, a model in factorie is an object that
     * can take some variables and return some factors, which know how to score
     * assignments of those variables. So models store weightsSet, and things like that.
     *
     * To construct a chain model you need to give it a few things. First, the domains
     * of the labels and the token features. Then, functions that can take a label to
     * its feature vector, a label to its token, and a token to its label. These functions
     * are used by the ChainModel class to generate factors.
     **/
    object model extends ChainModel[Label,  Features, Token](
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
     * Now that we have one sentence and a model we can start to do inference in it.
     *
     * Inference in factorie is abstracted in objects that implement the Infer trait,
     * which has one method, infer, that takes a sequence of variables, a model, and an
     * optional Summary, and returns a Summary object with the result of inference.
     *
     * A Summary object has two essential pieces of information in it: it allows you to
     * compute the normalization constant of the model (useful for learning), and it can
     * return marginal distributions over subsets of variables in your model. It also
     * knows how to turn these marginals into expected sufficient statistics for learning.
     *
     * The most commonly used Infer objects are those in the BP package.
     **/

    val summary = InferByBPChain.infer(document.tokens.toSeq.map(_.attr[Label]), model)
    assertStringEquals(summary.logZ, "6.931471805599453")
    assertStringEquals(summary.marginal(document.tokens.head.attr[Label]).proportions, "Proportions(0.49999999999999994,0.49999999999999994)")

    /*&
     * Aside from InferByBPChainSum, which knows how to run forward-backward on chain models
     * factorie has MaximizeByBPChain, which runs viterbi, InferByBPTreeSum, which runs BP on
     * trees, and InferByBPLoopy which runs loopy belief propagation.
     *
     * For more inference tools see the Inference tutorial.
     *
     *
     * One of the main uses of inference is in learning.
     *
     * The learning infrastructure in factorie is made of three components which can
     * be mixed and matched at will.
     *
     * An Example abstracts over a piece of data and knows how to compute objectives
     * and gradients.
     *
     * GradientOptimizers know how to update a model's weightsSet given gradients and values.
     *
     * In the middle between Examples and GradientOptimizers we have Trainers, which control
     * when the gradients are evaluated, where they are stored, the degree of parallelism, etc.
     **/



    /*&
     * The most common example one uses is the LikelihoodExample, which computes the value and the
     * gradient for maximum likelihood training. Here's how to construct one for this sentence
     * using Factorie's BP inferencer.
     **/
    val example = new optimize.LikelihoodExample(document.tokens.toSeq.map(_.attr[Label]), model, InferByBPChain)

    /*& In this tutorial let's use the AdaGrad optimizer, which is efficient and has
     * per-coordinate learning rates but is not regularized
     **/
    val optimizer0 = new optimize.AdaGrad()

    /*&
     * To learn a model the simplest way is to call Trainer.onlineTrain. This will optimize the
     * parameters with AdaGrad and parameter averaging using SGD.
     **/
    Trainer.onlineTrain(model.parameters, Seq(example), optimizer=optimizer0)

    // Factorie also supports batch learning. Note that regularization is built into the optimizer
    val optimizer1 = new optimize.LBFGS with optimize.L2Regularization
    optimizer1.variance = 10000.0
    Trainer.batchTrain(model.parameters, Seq(example), optimizer=optimizer1)

    /*&
     * Factorie also supports other batch trainers. The ParallelBatchTrainer keeps a per-thread
     * gradient vector and aggregates all gradients before sending them to the optimizer, while
     * the SynchronizedBatchTrainer keeps a single gradient vector and locks it.
     *
     * Also note that all online optimizers can be used with the batch trainers, but not the
     * other way around.
     *
     * One can also not call Trainer.onlineTrain or Trainer.batchTrain and create a Trainer
     * explicitly. A Trainer is a policy object which is responsible for deciding which gradients
     * are evaluated, when, and in which threads, and when these gradients are given to the optimizers.
     *
     * All trainers inherit from Trainer. To create a trainer, simply instantiate it.
     **/
    val trainer = new SynchronizedOptimizerOnlineTrainer(model.parameters, optimizer0)
    /*&
     * To use the trainer we can either call .trainFromExamples or call things in a loop:
     **/
    trainer.trainFromExamples(Seq(example))
    // or
    while (!trainer.isConverged) {
      trainer.processExamples(Seq(example))
    }
    /*&
     * Some trainers might require some tear down after training for optimal usage. See the implementation
     * of Trainer.train for an example of how to do so.
     *
     *
     * Factorie also has many other optimizers. Usually AdaGrad with ParameterAveraging is a good bet,
     * but often one wants something more specific.
     *
     * There are two main regularized online optimizers in factorie: AdaGradRDA, which uses regularized
     * dual averaging and supports l1 and l2 regularization, and L2RegularizedConstantRate, which supports
     * only l2 regularization. Neither of these optimizers support parameter averaging.
     *
     * For more information see the java docs of the cc.factorie.optimize package.
     **/

    // Now we can run inference and see that we have learned
    val summary2 = InferByBPChain(document.tokens.map(_.attr[Label]).toIndexedSeq, model)
    assertStringEquals(summary2.logZ, "48.63607808729122")
    assertStringEquals(summary2.marginal(document.tokens.head.attr[Label]).proportions, "Proportions(0.9999308678897892,6.913211020966629E-5)")

    /*&
     * Factorie also has support for more efficient learning algorithms than traditional
     * inference-based batch and online methods.
     *
     * The main such method is SampleRank, which runs a sampler and updates the weightsSet
     * while the sampler explores the posterior to make the model's predictions match an
     * arbitrary loss function.
    **/
    val sampler = new GibbsSampler(model, HammingObjective)
    val sampleRankExamples = document.tokens.toSeq.map(t => new optimize.SampleRankExample(t.attr[Label], sampler))
    Trainer.onlineTrain(model.parameters, sampleRankExamples, optimizer=optimizer0)
    // SampleRank comes with its own trainer, however, for ease of use
    val trainer2 = new SampleRankTrainer(model.parameters, sampler, optimizer0)
    trainer2.processContexts(document.tokens.toSeq.map(_.attr[Label]))

    /*&
     * Finally, there are many other useful examples in factorie. The LinearMulticlassExample
     * implements generalized linear models for many classification loss functions, for example, and the
     * DominationLossExample knows how to do learning to rank.
     **/
  }
}
