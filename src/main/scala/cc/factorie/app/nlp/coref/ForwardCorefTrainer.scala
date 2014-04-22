package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import cc.factorie.util.HyperparameterMain
import cc.factorie.app.nlp.coref.mention._
import cc.factorie.app.nlp.phrase._ //{NounPhraseGenderLabeler,NounPhraseNumberLabeler,NounPhraseEntityTypeLabeler}
import cc.factorie.app.nlp.load.LoadConll2011

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 1:01 PM
 */


trait ForwardCorefTrainerOpts extends CorefTrainerOpts{
  val numPositivePairsTrain = new CmdOption("prune-train", 2, "INT", "number of positive pairs before pruning instances in training")
  val numPositivePairsTest = new CmdOption("prune-test", 100, "INT", "number of positive pairs before pruning instances in testing")
  val numThreads = new CmdOption("num-threads", 4, "INT", "Number of threads to use")
  val featureComputationsPerThread = new CmdOption("feature-computations-per-thread", 2, "INT", "Number of feature computations per thread to run in parallel during training")
  val numTrainingIterations = new CmdOption("num-training-iterations", 4, "INT", "Number of passes through the training data")
  val writeConllFormat = new CmdOption("write-conll-format", false, "BOOLEAN", "Write CoNLL format data.")
  val useAverageIterate = new CmdOption("use-average-iterate", true, "BOOLEAN", "Use the average iterate instead of the last iterate?")
  val useMIRA = new CmdOption("use-mira", false, "BOOLEAN", "Whether to use MIRA as an optimizer")
  val saveFrequency = new CmdOption("save-frequency", 4, "INT", "how often to save the model between epochs")
  val trainPortionForTest = new CmdOption("train-portion-for-test", 0.1, "DOUBLE", "When testing on train, what portion to use.")
  val mergeFeaturesAtAll = new CmdOption("merge-features-at-all", true, "BOOLEAN", "Whether to merge features")
  val conjunctionStyle = new CmdOption("conjunction-style", "NONE", "NONE|HASH|SLOW", "What types of conjunction features to use - options are NONE, HASH, and SLOW (use slow string-based conjunctions).")
  val entityLR = new CmdOption("entity-left-right",false,"BOOLEAN","whether to do entity-based pruning in lr search")
  val slackRescale = new CmdOption("slack-rescale",2.0,"FLOAT","recall bias for hinge loss")
  val useEntityType = new CmdOption("use-entity-type",true,"BOOLEAN","whether to use entity type info")
  val mergeAppositions = new CmdOption("mergeAppositions",false,"BOOLEAN","whether to merge appositions as a rule")
  val usePronounRules = new CmdOption("use-pronoun-rules",false,"BOOLEAN","whether to do deterministic assigning of pronouns and not consider pronouns for training")
  val trainSeparatePronounWeights = new CmdOption("separate-pronoun-weights",false,"BOOLEAN","train a separate weight vector for pronoun-pronoun comparison")
  val numCompareToTheLeft = new CmdOption("num-compare-to-the-left",75,"INT","number of mentions to compare to the left before backing off to only looking at non-pronouns and those in entities (only used if entityLR == true)")
  val learningRate = new CmdOption("learning-rate",1.0,"FLOAT","learning rate")
  val mentionDetection = new CmdOption("mention-detection", "PARSE", "NER|PARSE|GOLD", "Which mention detection framework to use.  Each coref model is trained using a different mention detection paradigm. It's best to use the mention detection used for training.")
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
    println()

    val rng = new scala.util.Random(opts.randomSeed.value)
    val loadTrain = !opts.deserialize.wasInvoked
    val (trainDocs,testDocs) =  if(opts.useNonGoldBoundaries.value ) makeTrainTestDataNonGold(opts.trainFile.value,opts.testFile.value,options, loadTrain, opts.useNerMentions.value)
    else makeTrainTestData(opts.trainFile.value,opts.testFile.value, loadTrain)

    if(loadTrain)
      for (doc <- trainDocs; mention <- doc.coref.mentions) {
        NounPhraseGenderLabeler.process(mention.phrase)
        NounPhraseNumberLabeler.process(mention.phrase)
      }

    for (doc <- testDocs; mention <- doc.coref.mentions) {
      NounPhraseGenderLabeler.process(mention.phrase)
      NounPhraseNumberLabeler.process(mention.phrase)
    }

    val mentPairClsf =
      if (opts.deserialize.wasInvoked){
        val lr = new ForwardCoref()

        //copy over options that are tweakable at test time
	      println("deserializing from " + opts.deserialize.value)
        lr.deserialize(opts.deserialize.value)  //note that this may overwrite some of the options specified at the command line.  The idea is that there are certain options that must be consistent
        //between train and test. These options were serialized with the model, and set when you deserialize the model.

        //However, there are some options that are safe to change at test time. Just to be extra sure, we set this manually back
        lr.options.setConfig("usePronounRules",options.usePronounRules) //this is safe to tweak at test time if you train separate weights for all the pronoun cases

        lr.model.MentionPairFeaturesDomain.freeze()
        lr.doTest(testDocs, WordNet, "Test")
        lr
      }
      else{
        lr.train(trainDocs, testDocs, WordNet, rng, opts.saveFrequency.wasInvoked,opts.saveFrequency.value,opts.serialize.value, opts.learningRate.value)
        lr
      }

    if (opts.serialize.wasInvoked && !opts.deserialize.wasInvoked)
      mentPairClsf.serialize(opts.serialize.value)

    //if (opts.writeConllFormat.value) {
      val conllFormatPrinter = new CorefScorer
      val conllFormatGold = new java.io.PrintStream(new java.io.File("eval-test.filtpred"))
      testDocs.foreach(d => conllFormatPrinter.printConll2011Format(d.getCoref, conllFormatGold,true))
      conllFormatGold.flush()
      conllFormatGold.close()
   /*
      val conllFormatGold2 = new java.io.PrintStream(new java.io.File("conll-test.nonfilteredgold"))
      testDocs.foreach(d => printConll2011Format(d, testTrueMaps(d.name), conllFormatGold2))
      conllFormatGold2.flush()
      conllFormatGold2.close()
    }     */
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
  }
  val opts = ProbCorefTrainerOpts
  def evaluateParameters(args:Array[String]):Double = {
    opts.parse(args)
    //two varibles taken from ForwardCoreferenceTrainer
    val rng = new scala.util.Random(opts.randomSeed.value)
    val loadTrain = !opts.deserialize.wasInvoked
    val coreferenceSystem = new StructuredCoref
    val options = coreferenceSystem.options

    //options.learningRate = opts.learningRate.value
    options.numTrainingIterations = opts.numTrainingIterations.value

    println("Options Created")
    println("Learning Rate: " +options.learningRate)
    println("Loading Training Docs")
    val train = "src/main/resources/conll-train-clean.txt"
    val test = "src/main/resources/conll-test-clean.txt"
    val (trainDocs,testDocs) =   if(options.useNonGoldBoundaries) makeTrainTestDataNonGold(opts.trainFile.value,opts.testFile.value,options, loadTrain, opts.useNerMentions.value)
    else makeTrainTestData(opts.trainFile.value, opts.testFile.value, loadTrain)

    if(loadTrain)
      for (doc <- trainDocs; mention <- doc.coref.mentions) {
        NounPhraseGenderLabeler.process(mention.phrase)
        NounPhraseNumberLabeler.process(mention.phrase)
      }

    for (doc <- testDocs; mention <- doc.coref.mentions) {
      NounPhraseGenderLabeler.process(mention.phrase)
      NounPhraseNumberLabeler.process(mention.phrase)
    }
    var accuracy = 0.0
    if (opts.deserialize.wasInvoked){
      //copy over options that are tweakable at test time
      println("deserializing from " + opts.deserialize.value)
      coreferenceSystem.deserialize(opts.deserialize.value)  //note that this may overwrite some of the options specified at the command line.  The idea is that there are certain options that must be consistent
      //between train and test. These options were serialized with the model, and set when you deserialize the model.

      coreferenceSystem.model.MentionPairFeaturesDomain.freeze()
      val acc = coreferenceSystem.doTest( testDocs, WordNet,"Test")
      accuracy = acc
      coreferenceSystem
    }else{
      val acc = coreferenceSystem.train(trainDocs,testDocs, WordNet, rng,opts.saveFrequency.wasInvoked,opts.saveFrequency.value,opts.serialize.value, opts.learningRate.value)
      accuracy = acc
      if (opts.serialize.wasInvoked && !opts.deserialize.wasInvoked)
        coreferenceSystem.serialize(opts.serialize.value)
    }
    accuracy
  }
}
