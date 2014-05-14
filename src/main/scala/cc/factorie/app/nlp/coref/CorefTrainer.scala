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
package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.util.{Trackable, HyperparameterMain, TimingCollector, Trackers}
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.load.LoadConll2011
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}

/** Trainers for Coreference Systems*/
trait ForwardCorefTrainerOpts extends CorefTrainerOpts{
  val numPositivePairsTrain = new CmdOption("prune-train", 2, "INT", "number of positive pairs before pruning instances in training")
  val numPositivePairsTest = new CmdOption("prune-test", 100, "INT", "number of positive pairs before pruning instances in testing")
  val numThreads = new CmdOption("num-threads", 4, "INT", "Number of threads to use")
  val featureComputationsPerThread = new CmdOption("feature-computations-per-thread", 2, "INT", "Number of feature computations per thread to run in parallel during training")
  val numTrainingIterations = new CmdOption("num-training-iterations", 4, "INT", "Number of passes through the training data")
  val useMIRA = new CmdOption("use-mira", false, "BOOLEAN", "Whether to use MIRA as an optimizer")
  val saveFrequency = new CmdOption("save-frequency", 1, "INT", "how often to save the model between epochs")
  val trainPortionForTest = new CmdOption("train-portion-for-test", 0.1, "DOUBLE", "When testing on train, what portion to use.")
  val mergeFeaturesAtAll = new CmdOption("merge-features-at-all", true, "BOOLEAN", "Whether to merge features")
  val conjunctionStyle = new CmdOption("conjunction-style", "NONE", "NONE|HASH|SLOW", "What types of conjunction features to use - options are NONE, HASH, and SLOW (use slow string-based conjunctions).")
  val entityLR = new CmdOption("entity-left-right",false,"BOOLEAN","whether to do entity-based pruning in lr search")
  val slackRescale = new CmdOption("slack-rescale",2.0,"FLOAT","recall bias for hinge loss")
  val useEntityType = new CmdOption("use-entity-type",true,"BOOLEAN","whether to use entity type info")
  val mergeAppositions = new CmdOption("merge-appositions",false,"BOOLEAN","whether to merge appositions as a rule")
  val usePronounRules = new CmdOption("use-pronoun-rules",false,"BOOLEAN","whether to do deterministic assigning of pronouns and not consider pronouns for training")
  val trainSeparatePronounWeights = new CmdOption("separate-pronoun-weights",true,"BOOLEAN","train a separate weight vector for pronoun-pronoun comparison")
  val numCompareToTheLeft = new CmdOption("num-compare-to-the-left",75,"INT","number of mentions to compare to the left before backing off to only looking at non-pronouns and those in entities (only used if entityLR == true)")
  val learningRate = new CmdOption("learning-rate",1.0,"FLOAT","learning rate")
  val serialize = new CmdOption("serialize", "ForwardCoref.factorie", "FILE", "Filename in which to serialize classifier.")
  val deserialize = new CmdOption("deserialize", "", "FILE", "Filename from which to deserialize classifier.")
  val useAverageIterate = new CmdOption("use-average-iterate", true, "BOOLEAN", "Use the average iterate instead of the last iterate?")
}

object ForwardCorefTrainer extends CorefTrainer{
  object opts extends ForwardCorefTrainerOpts

  def evaluateParameters(args: Array[String]): Double = {
    opts.parse(args)
    val conjunctionStyle = opts.conjunctionStyle.value match {
      case "NONE" => ConjunctionOptions.NO_CONJUNCTIONS
      case "HASH" => ConjunctionOptions.HASH_CONJUNCTIONS
      case "SLOW" => ConjunctionOptions.SLOW_CONJUNCTIONS
      case s => sys.error("Unknown conjunction style: " + s)
    }

    val lr = if (conjunctionStyle == ConjunctionOptions.HASH_CONJUNCTIONS) new ForwardCorefImplicitConjunctions else new ForwardCoref

    val options = lr.options
    //options that get serialized with the model
    options.setParameterOptions(opts)
    options.setConfig("useEntityType",opts.useEntityType.value)
    options.setConfig("trainSeparatePronounWeights",opts.trainSeparatePronounWeights.value)
    // options which affect only learning

    println("** Arguments")
    val ignoreOpts = Set("config", "help", "version")
    for (o <- opts.values.toSeq.sortBy(_.name); if !ignoreOpts(o.name)) println(o.name + " = " + o.value)

    val timer = new TimingCollector()
    Trackers += timer

    val rng = new scala.util.Random(opts.randomSeed.value)
    val loadTrain = !opts.deserialize.wasInvoked
    val (trainDocs,testDocs) =  if(opts.useGoldBoundaries.value ) makeTrainTestDataGoldBoundaries(opts.trainFile.value,opts.testFile.value,loadTrain)
    else makeTrainTestData(opts.trainFile.value,opts.testFile.value,options, loadTrain, opts.useNerMentions.value)

    addGenderNumberLabeling(trainDocs,testDocs)
    println(timer.timings)


    if (opts.deserialize.wasInvoked){

      val lr = if(opts.deserialize.value == "NerForwardCoref.factorie") new NerForwardCoref()
               else if (opts.deserialize.value == "ParseForwardCoref.factorie") new ParseForwardCoref()
               else new ForwardCoref()

      //copy over options that are tweakable at test time
      println("deserializing from " + opts.deserialize.value)

      lr.deserialize(opts.deserialize.value)  //note that this may overwrite some of the options specified at the command line.  The idea is that there are certain options that must be consistent
      //between train and test. These options were serialized with the model, and set when you deserialize the model.

      //However, there are some options that are safe to change at test time. Just to be extra sure, we set this manually back
      lr.options.setConfig("usePronounRules",options.usePronounRules) //this is safe to tweak at test time if you train separate weights for all the pronoun cases

      lr.model.MentionPairFeaturesDomain.freeze()
      //Add Cached Mention Features
      for(doc <- testDocs; mention <- doc.coref.mentions) mention.attr += new MentionCharacteristics(mention)

      lr.doTest(testDocs, WordNet, "Test")
    }
    else{
      lr.train(trainDocs, testDocs, WordNet, rng, opts.saveFrequency.wasInvoked,opts.saveFrequency.value,opts.serialize.value, opts.learningRate.value)
      println(timer.timings)
      if (opts.serialize.wasInvoked)
        lr.serialize(opts.serialize.value)
    }


    if (opts.writeConllFormat.value)
      writeConllOutput(testDocs)

    val accuracy = 0.0
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(accuracy,opts.targetAccuracy.value.toDouble)

    accuracy
  }
}

object StructuredCorefTrainer extends CorefTrainer{
  object ProbCorefTrainerOpts extends CorefTrainerOpts{
    val maxMentDistance = new CmdOption("prune-train", 2, "INT", "number of positive pairs before pruning instances in training")
    val numPositivePairsTest = new CmdOption("prune-test", 100, "INT", "number of positive pairs before pruning instances in testing")
    val numThreads = new CmdOption("num-threads", 4, "INT", "Number of threads to use")
    val numTrainingIterations = new CmdOption("num-training-iterations", 20, "INT", "Number of iterations to use for training")
    val saveFrequency = new CmdOption("save-frequency", 4, "INT", "how often to save the model between epochs")
    val learningRate = new CmdOption("learning-rate",1.0,"FLOAT","learning rate")
    val featureSet = new CmdOption("feature-set","lexical","LEXICAL|CONVENTIONAL","Feature set to use for this model")
    val l1 = new CmdOption("l1", .0001, "INT", "l1 regularizer for adaGradRDA")
    val useAverageIterate = new CmdOption("use-average-iterate", true, "BOOLEAN", "Use the average iterate instead of the last iterate?")
    val serialize = new CmdOption("serialize", "StructuredCoref.factorie","FILE","Filename in which to serialize classifier.")
    val deserialize = new CmdOption("deserialize", "", "FILE", "Filename from which to deserialize classifier.")
  }
  val opts = ProbCorefTrainerOpts
  def evaluateParameters(args:Array[String]):Double = {
    opts.parse(args)
    //two varibles taken from ForwardCoreferenceTrainer
    val rng = new scala.util.Random(opts.randomSeed.value)
    val loadTrain = !opts.deserialize.wasInvoked
    val coreferenceSystem = new StructuredCoref
    val options = coreferenceSystem.options
    options.featureSet="lexical"
    options.learningRate = opts.learningRate.value
    options.l1 = opts.l1.value
    options.useAverageIterate = opts.useAverageIterate.value
    options.useAdaGradRDA = false
    options.numTrainingIterations = opts.numTrainingIterations.value
    options.useGoldBoundaries = opts.useGoldBoundaries.value
    options.useNERMentions = opts.useNerMentions.value
    println("** Arguments")
    val ignoreOpts = Set("config", "help", "version")
    for (o <- opts.values.toSeq.sortBy(_.name); if !ignoreOpts(o.name)) println(o.name + " = " + o.value)

    println("Loading Documents")
    val (trainDocs,testDocs) =   if(options.useGoldBoundaries) makeTrainTestDataGoldBoundaries(opts.trainFile.value,opts.testFile.value,loadTrain)
    else makeTrainTestData(opts.trainFile.value, opts.testFile.value,options, loadTrain, opts.useNerMentions.value)

    addGenderNumberLabeling(trainDocs,testDocs)

    var accuracy = 0.0
    if (opts.deserialize.wasInvoked){
      //copy over options that are tweakable at test time
      println("deserializing from " + opts.deserialize.value)
      val testSystem = if(opts.deserialize.value == "NerStructuredCoref.factorie") new NerStructuredCoref()
      else if (opts.deserialize.value == "ParseStructuredCoref.factorie") new ParseStructuredCoref()
      else new StructuredCoref()
      testSystem.deserialize(opts.deserialize.value)  //note that this may overwrite some of the options specified at the command line.  The idea is that there are certain options that must be consistent
      //between train and test. These options were serialized with the model, and set when you deserialize the model.
      options.featureSet = "lexical"
      testSystem.model.MentionPairFeaturesDomain.freeze()
      accuracy = testSystem.doTest( testDocs, WordNet,"Test")
      testSystem
    }else{
      accuracy = coreferenceSystem.train(trainDocs,testDocs, WordNet, rng,opts.saveFrequency.wasInvoked,opts.saveFrequency.value,opts.serialize.value, opts.learningRate.value)
      if (opts.serialize.wasInvoked && !opts.deserialize.wasInvoked)
        coreferenceSystem.serialize(opts.serialize.value)
    }

    if(opts.writeConllFormat.value)
      writeConllOutput(testDocs)
    testDocs.head.tokens.foreach{t => println(coreferenceSystem.tokenAnnotationString(t))}
    accuracy
  }
}

trait CorefTrainerOpts extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions{
  val trainFile = new CmdOption("train", "src/main/resources/conll-train-clean.txt", "STRING", "File with training data")
  val testFile = new CmdOption("test", "src/main/resources/conll-test-clean.txt", "STRING", "File with testing data")
  val useExactEntTypeMatch = new CmdOption("use-exact-entity-type-match", true, "BOOLEAN", "whether to require exact alignment between NER annotation and NP annotation")
  val useGoldBoundaries = new CmdOption("use-gold-boundaries",false,"BOOLEAN","whether to use gold parse boundaries + gold mention boundaries")
  val mentionAlignmentShiftWidth = new CmdOption("alignment-width",0,"INT","tolerance on boundaries when aligning detected mentions to gt mentions")
  val portion = new CmdOption("portion", 1.0, "DOUBLE", "Portion of corpus to load.")
  val useNerMentions = new CmdOption("use-ner-mentions", false, "BOOLEAN", "Whether to use NER mentions instead of noun phrase mentions")
  val randomSeed = new CmdOption("random-seed", 0, "INT", "Seed for the random number generator")
  val writeConllFormat = new CmdOption("write-conll-format", true, "BOOLEAN", "Write CoNLL format data.")
}
/** Classes shared by both coref systems*/
abstract class CorefTrainer extends HyperparameterMain with Trackable{
  def evaluateParameters(args: Array[String]): Double

  def opts:CorefTrainerOpts

  def addGenderNumberLabeling(trainDocs:Seq[Document], testDocs:Seq[Document]){
    |**("Adding Training Gender Labels")
    if(trainDocs ne null){
      for (doc <- trainDocs; mention <- doc.targetCoref.mentions) {
        NounPhraseGenderLabeler.process(mention.phrase)
        NounPhraseNumberLabeler.process(mention.phrase)
        DeterministicNounPhraseTypeLabeler.process(mention.phrase)
      }

    for (doc <- trainDocs; mention <- doc.coref.mentions) {
      NounPhraseGenderLabeler.process(mention.phrase)
      NounPhraseNumberLabeler.process(mention.phrase)
      DeterministicNounPhraseTypeLabeler.process(mention.phrase)
    }
    }
    for (doc <- testDocs; mention <- doc.coref.mentions) {
      NounPhraseGenderLabeler.process(mention.phrase)
      NounPhraseNumberLabeler.process(mention.phrase)
      DeterministicNounPhraseTypeLabeler.process(mention.phrase)
    }
    **|
  }

  def makeTrainTestDataGoldBoundaries(trainFile: String, testFile: String, loadTrain: Boolean): (Seq[Document],Seq[Document]) = {
    var trainDocs: Seq[Document] = null
    if (loadTrain){
      val allTrainDocs = LoadConll2011.loadWithParse(trainFile)
      trainDocs = allTrainDocs.take((allTrainDocs.length*opts.portion.value).toInt)
      for(doc <- trainDocs; mention <- doc.getTargetCoref.mentions){
        assert(mention.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")
        doc.coref.addMention(mention.phrase).phrase.attr += mention.phrase.attr[OntonotesPhraseEntityType]
      }
      println("Train: "+trainDocs.length+" documents, " + trainDocs.map(d => d.coref.mentions.size).sum.toFloat / trainDocs.length + " mentions/doc")
    }
    val allTestDocs  =  LoadConll2011.loadWithParse(testFile)
    val testDocs = allTestDocs.take((allTestDocs.length*opts.portion.value).toInt)
    for(doc <- testDocs; mention <- doc.getTargetCoref.mentions){
      assert(mention.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")
      doc.coref.addMention(mention.phrase).phrase.attr += mention.phrase.attr[OntonotesPhraseEntityType]
    }
    println("Test : "+ testDocs.length+" documents, " + testDocs.map(d => d.coref.mentions.size).sum.toFloat / testDocs.length + " mention/doc")

    (trainDocs,testDocs)
  }

  def makeTrainTestData(trainFile: String, testFile: String, options: CorefOptions, loadTrain: Boolean, useNerMentions: Boolean): (Seq[Document],Seq[Document]) = {
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    if (useNerMentions) {
      map(classOf[NerTag]) = () => ConllChainNer
    }
    var trainDocs: Seq[Document] = null
    if(loadTrain){
      val allTrainDocs = LoadConll2011.loadWithParse(trainFile, loadSingletons = false)
      val unalignedTrainDocs = allTrainDocs.take((allTrainDocs.length*opts.portion.value).toInt)
      trainDocs = MentionAlignment.makeLabeledData(unalignedTrainDocs,null,options.useEntityType, options, map.toMap)
      println("Train: "+trainDocs.length+" documents, " + trainDocs.map(d => d.targetCoref.mentions.size).sum.toFloat / trainDocs.length + " mentions/doc")
    }

    val testDocs: Seq[ Document] = {
      val allTestDocs = LoadConll2011.loadWithParse(testFile, loadSingletons = false)
      val unalignedTestDocs = allTestDocs.take((allTestDocs.length*opts.portion.value).toInt)
      MentionAlignment.makeLabeledData(unalignedTestDocs,null,options.useEntityType, options, map.toMap)
    }
    println("Test : "+ testDocs.length+" documents, " + testDocs.map(d => d.targetCoref.mentions.size).sum.toFloat / testDocs.length + " mention/doc")
    if(!useNerMentions){
      val labeler = NounPhraseEntityTypeLabeler
      if (loadTrain)  for (doc <- trainDocs; mention <- doc.coref.mentions) labeler.process(mention.phrase)
      for (doc <- testDocs; mention <- doc.coref.mentions) labeler.process(mention.phrase)
    }
    (trainDocs,testDocs)
  }

  def writeConllOutput(testDocs:Seq[Document]){
    val conllFormatPrinter = new CorefConllOutput
    val conllFormatFilt = new java.io.PrintStream(new java.io.File("eval-test.filtpred"))
    testDocs.foreach(d => conllFormatPrinter.printConll2011Format(d.getCoref, conllFormatFilt,withSingletons = false))
    conllFormatFilt.flush()
    conllFormatFilt.close()

    val conllFormatNonFilt = new java.io.PrintStream(new java.io.File("eval-test-key.filtgold"))
    testDocs.foreach{d => d.targetCoref.removeSingletons(); conllFormatPrinter.printConll2011Format(d.getTargetCoref, conllFormatNonFilt,withSingletons = false)}
    conllFormatNonFilt.flush()
    conllFormatNonFilt.close()
  }

}

object StructuredCorefOptimizer{
  def main(args: Array[String]) {
    val opts = StructuredCorefTrainer.ProbCorefTrainerOpts
    opts.parse(args)
    opts.serialize.setValue("")
    val l1 = cc.factorie.util.HyperParameter(opts.l1, new cc.factorie.util.SampleFromSeq(List(0.000005,0.00005,0.0005,.0001,.00001)))
    val rate = cc.factorie.util.HyperParameter(opts.learningRate, new cc.factorie.util.SampleFromSeq(List(0.1,0.5,0.8,1,1.2)))

    val qs = new cc.factorie.util.QSubExecutor(40, "cc.factorie.app.nlp.coref.StructuredCorefTrainer")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, rate), qs.execute, 40, 18, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best lr: " + opts.learningRate.value)
    println("Running best configuration...")
    opts.serialize.setValue("StructuredCoref.factorie")
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 6.hours)
    println("Done")
  }
}

object ForwardCorefOptimizer{
  def main(args: Array[String]) {
    val opts = ForwardCorefTrainer.opts
    opts.parse(args)
    opts.serialize.setValue("")

    val l1 = cc.factorie.util.HyperParameter(opts.numTrainingIterations, new cc.factorie.util.SampleFromSeq(List(1,2,3,4,5)))
    val rate = cc.factorie.util.HyperParameter(opts.learningRate, new cc.factorie.util.SampleFromSeq(List(0.1,0.5,0.8,1,1.2,1.5)))

    val qs = new cc.factorie.util.QSubExecutor(40, "cc.factorie.app.nlp.coref.ForwardCorefTrainer")
    val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(l1, rate), qs.execute, 40, 22, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best rate: " + opts.learningRate.value + " best l1: " + opts.numTrainingIterations.value)
    opts.serialize.setValue("ForwardCoref.factorie")
    println("Running best configuration...")
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 6.hours)
    println("Done")
  }
}