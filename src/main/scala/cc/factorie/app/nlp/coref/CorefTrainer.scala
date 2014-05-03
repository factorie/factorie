package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap, Document}
import cc.factorie.util.coref.GenericEntityMap
import cc.factorie.app.nlp.coref.mention._
import cc.factorie.app.nlp.load.LoadConll2011
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import cc.factorie.util.{Trackable, HyperparameterMain}
import java.util.concurrent.ExecutorService
import cc.factorie.app.nlp.phrase._
import java.util.Calendar

/**
* @author Cellier
*/

trait CorefTrainerOpts extends cc.factorie.util.DefaultCmdOptions with cc.factorie.app.nlp.SharedNLPCmdOptions{
  val trainFile = new CmdOption("train", "conll-train-clean.txt", "STRING", "File with training data")
  val testFile = new CmdOption("test", "conll-test-clean.txt", "STRING", "File with testing data")
  val useExactEntTypeMatch = new CmdOption("use-exact-entity-type-match", true, "BOOLEAN", "whether to require exact alignment between NER annotation and NP annotation")
  val useNonGoldBoundaries = new CmdOption("use-nongold-boundaries",true,"BOOLEAN","whether to use non-gold mention boundaries")
  val mentionAlignmentShiftWidth = new CmdOption("alignment-width",0,"INT","tolerance on boundaries when aligning detected mentions to gt mentions")
  val portion = new CmdOption("portion", 1.0, "DOUBLE", "Portion of corpus to load.")
  val serialize = new CmdOption("serialize", "N/A", "FILE", "Filename in which to serialize classifier.")
  val deserialize = new CmdOption("deserialize", "N/A", "FILE", "Filename from which to deserialize classifier.")
  val useNerMentions = new CmdOption("use-ner-mentions", false, "BOOLEAN", "Whether to use NER mentions instead of noun phrase mentions")
  val randomSeed = new CmdOption("random-seed", 0, "INT", "Seed for the random number generator")
}

trait CorefTrainer extends HyperparameterMain with Trackable{
  def evaluateParameters(args: Array[String]): Double

  def opts:CorefTrainerOpts

  //object opts extends CorefTrainerOpts

  def addGenderNumberLabeling(trainDocs:Seq[Document], testDocs:Seq[Document]){
    |**("Adding Training Gender Labels")
    if(trainDocs ne null)
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
    **|


    for (doc <- testDocs; mention <- doc.coref.mentions) {
      NounPhraseGenderLabeler.process(mention.phrase)
      NounPhraseNumberLabeler.process(mention.phrase)
      DeterministicNounPhraseTypeLabeler.process(mention.phrase)
    }
  }

  def makeTrainTestData(trainFile: String, testFile: String, loadTrain: Boolean): (Seq[Document],Seq[Document]) = {
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
    val testDocs = allTestDocs.take(/*(allTestDocs.length*opts.portion.value).toInt*/3)
    for(doc <- testDocs; mention <- doc.getTargetCoref.mentions){
      assert(mention.phrase.attr[OntonotesPhraseEntityType] ne null,"missing entity type")
      doc.coref.addMention(mention.phrase).phrase.attr += mention.phrase.attr[OntonotesPhraseEntityType]
    }
    println("Test : "+ testDocs.length+" documents, " + testDocs.map(d => d.coref.mentions.size).sum.toFloat / testDocs.length + " mention/doc")

    (trainDocs,testDocs)
  }

  def makeTrainTestDataNonGold(trainFile: String, testFile: String, options: Coref1Options, loadTrain: Boolean, useNerMentions: Boolean): (Seq[Document],Seq[Document]) = {
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    if (useNerMentions) {
      map(classOf[NerTag]) = () => ConllChainNer
    } else {
      map(classOf[Mention]) = () => ParseBasedMentionFinding
    }

    val trainDocsAll = if(loadTrain) LoadConll2011.loadWithParse(trainFile) else null
    val trainDocs = if(loadTrain) trainDocsAll.take((trainDocsAll.length*opts.portion.value).toInt) else null


    val testDocsAll = LoadConll2011.loadWithParse(testFile)
    val testDocs = testDocsAll.take((testDocsAll.length*opts.portion.value).toInt)


    //TODO: ReAdd Mention Alignment when the time is right before pushing to factorie
    //val (trainDocs) = if(loadTrain) MentionAlignment.makeLabeledData(trainFile,null,opts.portion.value,options.useEntityType, options, map.toMap) else (null,null)
    //val (testDocs) = MentionAlignment.makeLabeledData(testFile,null,opts.portion.value,options.useEntityType, options, map.toMap)


    if(!useNerMentions){
      //val labeler = MentionPhraseEntityTypeLabeler
      val labeler = NounPhraseEntityTypeLabeler
      if (loadTrain)  for (doc <- trainDocs; mention <- doc.coref.mentions) labeler.process(mention.phrase)
      for (doc <- testDocs; mention <- doc.coref.mentions) labeler.process(mention.phrase)
    }
    (trainDocs,testDocs)
  }
}
