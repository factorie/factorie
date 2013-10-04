/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.chain

import java.io.{FileWriter, BufferedWriter, FileInputStream, FileOutputStream}
import scala.util.Random
import cc.factorie._
import cc.factorie.optimize.{LikelihoodExample, OnlineTrainer}
import cc.factorie.util.DefaultCmdOptions
import cc.factorie.variable._
import scala.io.Source
import scala.Some
import cc.factorie.infer.{InferByBPChain, MaximizeByBPChain}

object ChainOpts extends DefaultCmdOptions {

  val writeSequences = new CmdOption("write-sequences", "sequences", "FILE", "Filename in which to save the sequences' labels and features.")

  // provide either these 3
  val readSequences = new CmdOption("read-sequences", "sequences", "FILE", "Filename from which to read the sequences' labels and features in one-line-per-token format.")
  val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the sequences that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
  val crossValidation = new CmdOption("cross-validation", 1, "N", "The number of folds for cross-validation (DEFAULT=1)")

  // or these 2
  val readTrainingSequences = new CmdOption("read-training-sequences", "sequences", "FILE", "Filename from which to read the training sequences' labels and features.")
  val readTestingSequences = new CmdOption("read-testing-sequences", "sequences", "FILE", "Filename from which to read the testing sequences' labels and features.")

  val readBinaryFeatures = new CmdOption("read-binary-features", true, "true|false", "If true, features will be binary as opposed to counts.  Default is true.")

  val readTextEncoding = new CmdOption("read-text-encoding", "UTF-8", "ENCODING", "The name of the encoding to use, e.g. UTF-8.")

  val writeClassifications = new CmdOption("write-classifications", "classifications", "FILE", "Filename in which to save the classifications.")

  val writeChainModel = new CmdOption("write-chain-model", "chain-model", "FILE", "Filename in which to save the chain model.")
  val readChainModel = new CmdOption("read-chain-model", "chain-model", "FILE", "Filename from which to read the chain model.")

  val localRandomSeed = new CmdOption("random-seed", -1, "N", "The random seed for randomly selecting a proportion of the instance list for training")

  val trainer = new CmdOption("trainer", "OnlineTrainer", "ChainTrainer", "Scala expression providing ChainTrainer class.") //todo implement this CLA
  // TODO Consider enabling the system to use multiple ChainTrainers at the same time, and compare results

  val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.") //todo implement this CLA
}

object FeaturesDomain extends CategoricalVectorDomain[String]
object LabelDomain extends CategoricalDomain[String]

class FeatureChain extends Chain[FeatureChain, Features]

trait Features extends CategoricalVectorVar[String] with Observation[Features] with ChainLink[Features, FeatureChain] {
  val features:Iterable[String]
  val label:Label
  val string = "N/A"
  override def domain = FeaturesDomain
}

object Features {
  def apply(features:Iterable[String], label:Label):Features = ChainOpts.readBinaryFeatures.value match {
    case true => new BinaryFeatures(features, label)
    case false => new NonBinaryFeatures(features, label)
  }
}

class BinaryFeatures(val features:Iterable[String], val label:Label) extends BinaryFeatureVectorVariable[String](features) with Features {
  label.featOpt = Some(this)
}
class NonBinaryFeatures(val features:Iterable[String], val label:Label) extends FeatureVectorVariable[String](features) with Features {
  label.featOpt = Some(this)
}

class Label(value:String) extends LabeledCategoricalVariable[String](value) {
  var featOpt:Option[Features] = None
  def features:Features = featOpt match {
    case Some(feature) => feature
    case None => throw new Exception("Uninitialized Features")
  }
  def token = features
  override def domain = LabelDomain
}

object Chain {

  def main(args: Array[String]): Unit = {

    val startTime = System.currentTimeMillis()

    
    ChainOpts.parse(args)

    if(ChainOpts.trainer.wasInvoked) {
      throw new NotImplementedError("Specifying a trainer isn't yet implemented")
    }

    if(ChainOpts.evaluator.wasInvoked) {
      throw new NotImplementedError("Specifying an evaluator isn't yet implemented")
    }

    // local random seed
    implicit val random = new Random(ChainOpts.localRandomSeed.value)

    def processInstances(filename:String):Iterable[FeatureChain] = {
      val src = Source.fromFile(filename)

      val featureChains =  src.getLines().toSeq.map(_.split("\\s+").toList).split(_.nonEmpty).map{ chains =>
        new FeatureChain() ++= chains.collect{ case labelString :: featureString =>
          Features(featureString, new Label(labelString))
        }.toIterable
      }
      src.close()
      featureChains.toIterable
    }

    // todo share across this and classify
    val (trainingLabels, testingLabels) = if(Seq(ChainOpts.readSequences, ChainOpts.trainingPortion).map(_.wasInvoked).reduce(_ && _)) {
      processInstances(ChainOpts.readSequences.value) match {
        case labels if ChainOpts.trainingPortion == 1.0 => labels -> Seq()
        case labels => labels.shuffle.split(ChainOpts.trainingPortion.value)
      }
    } else if(Seq(ChainOpts.readTrainingSequences, ChainOpts.readTestingSequences).map(_.wasInvoked).reduce(_ && _)) {
      processInstances(ChainOpts.readTrainingSequences.value) -> processInstances(ChainOpts.readTestingSequences.value)
    } else {
      throw new IllegalArgumentException("Invalid argument combination, supply either a read sequence and training portion or a pair of training and testing sequences.")
    }

    val model = new ChainModel[Label, Features, Features](LabelDomain, FeaturesDomain, _.features, _.token, _.label)



    if(ChainOpts.readChainModel.wasInvoked) {
      val f = new FileInputStream(ChainOpts.readChainModel.value)
      model.deserialize(f)
      f.close()
    } else {
      val examples = trainingLabels.map{ fc => new LikelihoodExample(fc.map{f:Features => f.label}, model, InferByBPChain)}

      val trainer = new OnlineTrainer(model.parameters, maxIterations = 1)

      trainer.trainFromExamples(examples)
    }

    var totalTokens = 0.0
    var correctTokens = 0.0

    testingLabels.foreach{ fc =>
      val res = MaximizeByBPChain.infer(fc.map{f:Features => f.label}, model, null)
      res.setToMaximize(null) // sets each label to it's maximum value
      totalTokens += fc.size
      correctTokens += HammingObjective.accuracy(fc.map{f:Features => f.label}) * fc.size
    }

    if(ChainOpts.writeClassifications.wasInvoked) {
      val classificationResults = new BufferedWriter(new FileWriter(ChainOpts.writeClassifications.value))
      testingLabels.foreach{ featureChain =>
        featureChain.foreach{ link =>
          classificationResults.write("%s %s".format(link.label.categoryValue, link.features.mkString(" ")))
          classificationResults.newLine()
        }
        classificationResults.newLine()
      }
      classificationResults.flush()
      classificationResults.close()
    }

    println("Overall accuracy: " + (correctTokens / totalTokens))

    println("Total elapsed time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "sec")

    if(ChainOpts.writeChainModel.wasInvoked) {
      val f = new FileOutputStream(ChainOpts.writeChainModel.value)
      model.serialize(f)
      f.flush()
      f.close()
    }

  }
}