/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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

package cc.factorie.app.classify
import cc.factorie.app.classify.backend._
import cc.factorie.infer._
import cc.factorie.la.{SingletonBinaryTensor1, Tensor1}
import cc.factorie.optimize._
import cc.factorie.variable._

/** A record of the result of applying a Classifier to a variable. */
class Classification[V<:DiscreteVar](val _1:V, score:Tensor1) extends MulticlassClassification(score) with DiscreteMarginal1[V] {
  def bestValue = _1.domain.apply(bestLabelIndex)
}

// Classifiers

/** Performs iid prediction of a DiscreteVar. */
trait Classifier[L<:DiscreteVar] {
  // Get classification record without changing the value of the label
  def classification(v:L): Classification[L]
  def classifications(labels: Iterable[L]): Seq[Classification[L]] = labels.toSeq.map(label => classification(label))
  // Get classification record and also set the label to its best scoring value 
  def classify[L2<:L with MutableDiscreteVar](v:L2): Classification[L] = { val c = classification(v); v := c.bestLabelIndex; c }
  def classify(labels: Iterable[L with MutableDiscreteVar]): Seq[Classification[L]] = labels.toSeq.map(l => classify(l))
  def bestLabelIndex(v:L): Int = classification(v).bestLabelIndex
  // TODO It might be nice to have a weighted version of this.  We could do this with a LabelList. :-) -akm
  def accuracy(labels:Iterable[L with LabeledDiscreteVar]): Double = {
    var correct = 0.0; var total = 0.0
    labels.foreach(label => { total += 1.0; if (bestLabelIndex(label) == label.target.intValue) correct += 1.0 })
    correct / total
  } 
}

/** A Classifier in which the "input, observed" object to be classified is a VectorVar (with value Tensor1). */
trait VectorClassifier[V<:DiscreteVar, Features<:VectorVar] extends Classifier[V] with MulticlassClassifier[Tensor1] {
  def labelToFeatures: V=>Features
}

/** A VectorClassifier in which the score for each class is a dot-product between the observed feature vector and a vector of parameters.
    Examples include NaiveBayes, MultivariateLogisticRegression, LinearSVM, and many others.
    Counter-examples include KNearestNeighbor. */
class LinearVectorClassifier[L<:DiscreteVar,F<:VectorVar](numLabels:Int, numFeatures:Int, val labelToFeatures:L=>F) extends LinearMulticlassClassifier(numLabels, numFeatures) with VectorClassifier[L,F] with Serializable {
  def classification(v:L): Classification[L] = new Classification(v, predict(labelToFeatures(v).value))
  override def bestLabelIndex(v:L): Int = predict(labelToFeatures(v).value).maxIndex
}


// Classifier trainers

/** An object that can create and train a VectorClassifier given labeled training data. */
trait VectorClassifierTrainer {
  def train[L<:LabeledDiscreteVar,F<:VectorVar](labels:Iterable[L], l2f:L=>F): VectorClassifier[L,F]
}

/** An object that can create and train a LinearVectorClassifier (or train a pre-existing LinearVectorClassifier) given labeled training data. */
trait LinearVectorClassifierTrainer extends VectorClassifierTrainer {
  /** Create a new LinearVectorClassifier, not yet trained. */
  protected def newClassifier[L<:LabeledDiscreteVar,F<:VectorVar](labelDomainSize:Int, featureDomainSize:Int, l2f:L=>F): LinearVectorClassifier[L,F] = new LinearVectorClassifier(labelDomainSize, featureDomainSize, l2f)
  /** Create, train and return a new LinearVectorClassifier */
  def train[L<:LabeledDiscreteVar,F<:VectorVar](labels:Iterable[L], l2f:L=>F): LinearVectorClassifier[L,F] = train(newClassifier(labels.head.domain.size, l2f(labels.head).domain.dimensionSize, l2f), labels, l2f)
  /** Train (and return) an already-created (perhaps already partially-trained) LinearVectorClassifier. */
  def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], l2f:L=>F): C
}

/** A LinearVectorClassifierTrainer that uses the cc.factorie.optimize package to estimate parameters. */
class OptimizingLinearVectorClassifierTrainer(
  val optimizer: GradientOptimizer,
  val useParallelTrainer: Boolean,
  val useOnlineTrainer: Boolean,
  val objective: OptimizableObjectives.Multiclass,
  val maxIterations: Int,
  val miniBatch: Int,
  val nThreads: Int)(implicit random: scala.util.Random) extends LinearVectorClassifierTrainer
{
  // TODO This is missing weights on Examples.  I think passing a Seq[Double] is error prone, and am tempted to go back to LabelList. -akm
  /** Create a sequence of Example instances for obtaining the gradients used for training. */
  def examples[L<:LabeledDiscreteVar,F<:VectorVar](classifier:LinearVectorClassifier[L,F], labels:Iterable[L], l2f:L=>F, objective:OptimizableObjectives.Multiclass): Seq[Example] =
    labels.toSeq.map(l => new PredictorExample(classifier, l2f(l).value, l.target.intValue, objective))
    
  /** Train the classifier to convergence, calling the diagnostic function once after each iteration.
      This is the base method called by the other simpler train methods. */
  def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], l2f:L=>F, diagnostic:C=>Unit): C = {
    Trainer.train(parameters=classifier.parameters, examples=examples(classifier, trainLabels, l2f, objective), maxIterations=maxIterations, evaluate = ()=>diagnostic(classifier), optimizer=optimizer, useParallelTrainer=useParallelTrainer, useOnlineTrainer=useOnlineTrainer, miniBatch=miniBatch, nThreads=nThreads)
    classifier
  }
  /** Return a function suitable for passing in as the diagnostic to train which prints the accuracy on the testLabels */
  def defaultTestDiagnostic[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:LinearVectorClassifier[L,F], trainLabels:Iterable[L], testLabels:Iterable[L]): C=>Unit =
    (c:C) => println(f"Test accuracy: ${classifier.accuracy(testLabels.asInstanceOf[Iterable[L with LabeledDiscreteVar]])}%1.4f")
  /** Return a function suitable for passing in as the diagnostic to train which prints the accuracy on the trainLabels and the testLabels */
  def defaultTrainAndTestDiagnostic[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:LinearVectorClassifier[L,F], trainLabels:Iterable[L], testLabels:Iterable[L]): C=>Unit =
    (c:LinearVectorClassifier[L,F]) => println(f"Train accuracy: ${classifier.accuracy(trainLabels.asInstanceOf[Iterable[L with LabeledDiscreteVar]])}%1.4f\nTest  accuracy: ${classifier.accuracy(testLabels.asInstanceOf[Iterable[L with LabeledDiscreteVar]])}%1.4f")
  /** Train the classifier to convergence, calling a test-accuracy-printing diagnostic function once after each iteration. */
  def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], testLabels:Iterable[L], l2f:L=>F): C =
    train(classifier, trainLabels, l2f, defaultTestDiagnostic(classifier, trainLabels, testLabels))
  /** Train the classifier to convergence, calling no diagnostic function. */
  def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], l2f:L=>F): C = {
    train(classifier, trainLabels, l2f, (c:LinearVectorClassifier[L,F]) => ())
    classifier
  }
}

/** An OptimizingLinearVectorClassifierTrainer pre-tuned with default arguments well-suited to online training, operating on the gradient of one Example at a time. */
class OnlineOptimizingLinearVectorClassifierTrainer(
  useParallel:Boolean = false,
  optimizer: GradientOptimizer = new AdaGrad with ParameterAveraging,
  objective: OptimizableObjectives.Multiclass = OptimizableObjectives.sparseLogMulticlass,
  maxIterations: Int = 3,
  miniBatch: Int = -1,
  nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends OptimizingLinearVectorClassifierTrainer(optimizer, useParallel, useOnlineTrainer = true, objective, maxIterations, miniBatch, nThreads)

/** An OptimizingLinearVectorClassifierTrainer pre-tuned with default arguments well-suited to batch training, operating on all the gradients of the Examples together. */
class BatchOptimizingLinearVectorClassifierTrainer(useParallel:Boolean = true,
  optimizer: GradientOptimizer = new LBFGS with L2Regularization,
  objective: OptimizableObjectives.Multiclass = OptimizableObjectives.sparseLogMulticlass,
  maxIterations: Int = 200,
  nThreads: Int = Runtime.getRuntime.availableProcessors())(implicit random: scala.util.Random)
  extends OptimizingLinearVectorClassifierTrainer(optimizer, useParallel, useOnlineTrainer = false, objective, maxIterations, -1, nThreads)

/** An OptimizingLinearVectorClassifierTrainer pre-tuned with default arguments well-suited to training an L2-regularized linear SVM. */
class SVMLinearVectorClassifierTrainer(nThreads: Int = 1, l2: Double = 0.1)(implicit random: scala.util.Random) extends OptimizingLinearVectorClassifierTrainer(optimizer=null, useParallelTrainer=false, useOnlineTrainer=false, objective=null, miniBatch= -1, maxIterations= -1, nThreads= -1) {
  val baseTrainer = new backend.SVMMulticlassTrainer(nThreads, l2)
  override def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], l2f:L=>F, diagnostic:C=>Unit): C = {
    baseTrainer.baseTrain(classifier, trainLabels.map(_.target.intValue).toSeq, trainLabels.map(l2f(_).value).toSeq, trainLabels.map(l => 1.0).toSeq, c => ())
    classifier
  }
}

/** Creates a trained naive Bayes classifier by counting feature occurrences, smoothed with pseudo-counts (m-Estimates).
    Note that contrary to tradition, this naive Bayes classifier does not include a "bias" weight P(class); it only includes the feature weights, P(feature|class).
    If you want a "bias" weight you must include in your data a feature that always has value 1.0. */
class NaiveBayesClassifierTrainer(pseudoCount:Double = 0.1) extends LinearVectorClassifierTrainer {
  val baseTrainer = new backend.NaiveBayes(pseudoCount)
  def train[C<:LinearVectorClassifier[L,F],L<:LabeledDiscreteVar,F<:VectorVar](classifier:C, trainLabels:Iterable[L], l2f:L=>F): C = {
    baseTrainer.baseTrain(classifier, trainLabels.map(_.target.intValue).toSeq, trainLabels.map(l2f(_).value).toSeq, trainLabels.map(l => 1.0).toSeq, c => ())
    classifier
  }
}


// Decision trees.  Just one simple example so far. -akm

class DecisionTreeClassifier[L<:DiscreteVar,F<:VectorVar](val tree:DTree, val labelToFeatures:L=>F) extends VectorClassifier[L,F] {
  def classification(label:L): Classification[L] = new Classification(label, predict(labelToFeatures(label).value))
  def predict(features: Tensor1) = DTree.score(features, tree)
}

class DecisionTreeClassifierTrainer(treeTrainer: DecisionTreeTrainer)(implicit random: scala.util.Random) extends VectorClassifierTrainer {
  def train[L<:LabeledDiscreteVar,F<:VectorVar](labels:Iterable[L], l2f:L=>F): DecisionTreeClassifier[L,F] = {
    val labelSize = labels.head.domain.size
    val instances = labels.toSeq.map(label => DecisionTreeTrainer.Instance(l2f(label).value, new SingletonBinaryTensor1(labelSize, label.target.intValue), 1.0))
    val dtree = treeTrainer.train(instances)
    new DecisionTreeClassifier(dtree, l2f)
  }
}

class ID3DecisionTreeClassifier(implicit random: scala.util.Random) extends DecisionTreeClassifierTrainer(new ID3DecisionTreeTrainer)
class C45DecisionTreeClassifier(implicit random: scala.util.Random) extends DecisionTreeClassifierTrainer(new C45DecisionTreeTrainer)
