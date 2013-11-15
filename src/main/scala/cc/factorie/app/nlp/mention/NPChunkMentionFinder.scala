package cc.factorie.app.nlp.mention

import java.io.{File, FileOutputStream, FileInputStream}
import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase.{ChunkerOpts, CRFChunker}
import cc.factorie.app.nlp.load.{BILOUChunkTag}
import cc.factorie.app.nlp.coref.{ForwardCoref, Coref1Options, MentionAlignment, ConllCorefLoader}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import cc.factorie.app.nlp.coref.ForwardCorefTrainer.opts


/**
 * User: cellier
 * Date: 10/28/13
 * Time: 11:24 PM
 * Finite State machine implementation to grab NP spans
 * TODO: MentionEntityType / run ner over them
 */

class ChunkBasedMentionList extends MentionList

object ChunkBasedMentionFinder extends DocumentAnnotator {
  var chunk = initChunker(new CRFChunker)
  def initChunker(chunker: CRFChunker):CRFChunker = {
    chunker.deserialize(new FileInputStream(new java.io.File("2011NestedTrainedChunkerModel.factorie")))
    chunker
  }
  def prereqAttrs = Seq(classOf[Token])
  def postAttrs = Seq(classOf[ChunkBasedMentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  val PersonLexicon = new lexicon.UnionLexicon("MentionEntityTypePerson", lexicon.PersonPronoun, lexicon.PosessiveDeterminer)

  /* TODO: Find a way to include these better */
  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS.contains(t.posTag.categoryValue.toUpperCase)

  def getNPChunkSpans(document: Document): Seq[(String,TokenSpan)] ={
    val spans = ArrayBuffer[(String,TokenSpan)]()
    document.sections.foreach{s => s.tokens.map{
      t =>
        if (t.attr[BILOUChunkTag].categoryValue != "O") {
          if(t.attr[BILOUChunkTag].categoryValue == "U-NP") spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, 1))
          else if(t.attr[BILOUChunkTag].categoryValue == "B-NP"){
            if(t.hasNext) {
              var lookFor = t.next
              while (lookFor.hasNext && lookFor.attr[BILOUChunkTag].categoryValue.matches("(I)-NP")) lookFor = lookFor.next
              spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, lookFor.next.positionInSection - t.positionInSection))
              //while(lookFor.next.hasNext && lookFor.attr[BILOUChunkTag].categoryValue.matches("(I|L)-NP")) lookFor = lookFor.next
              //spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, lookFor.next.positionInSection - t.positionInSection))

            } else  spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, 1))
          }
        }  else {
          if ( t.string.length > 2 && !t.containsLowerCase && upperCase.findFirstIn(t.string).nonEmpty && (t.getNext ++ t.getPrev).exists(i => i.containsLowerCase)) {
            spans += ("ORG" -> new TokenSpan(s, t.positionInSection, 1))
          } else if (t.posTag.categoryValue == "NNP") {
            spans += ("MISC" -> new TokenSpan(s, t.positionInSection, 1))
          }
        }
      }
    }
    spans.toSeq
  }

  def getChunkMentions(document: Document): Seq[Mention] = {
    getNPChunkSpans(document).map{labelSpan =>
      val label = labelSpan._1
      val s = labelSpan._2
      val m = new Mention(s, s.length-1)
      if(label.startsWith("U") && s.tokens.length == 1 && isPersonalPronoun(s.tokens(0))){
        m.attr += new MentionType(m, "PRO")
        m.attr += new MentionEntityType(m,"PERSON")
      } /*
      else if(s.tokens.count(t => isCommonNoun(t)) > 0){
        m.attr += new MentionType(m, "NOM")
        m.attr += new MentionEntityType(m,"PERSON")
      }
      else if(s.tokens.count(t => isProperNoun(t)) > 0){
        m.attr += new MentionType(m, "NAM")
        if(s.tokens.count(t => PersonLexicon.contains(s))>0) m.attr += new MentionEntityType(m,"PERSON")
        else m.attr += new MentionEntityType(m,"ORG")
      }
      //else if mention includes a    */
      else {m.attr += new MentionType(m, "NAM")
        m.attr += new MentionEntityType(m,"MISC")
      }
      m
    }
  }

  def process(document: Document) = {
    chunk.process(document)
    val mentions = getChunkMentions(document)
    document.attr += new MentionList() ++= mentions.sortBy(m => (m.head.stringStart, m.length))
    document
  }

  def testMentions(trainFile: String, testFile: String, options: Coref1Options) = {
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    map(classOf[MentionList]) = () => ChunkBasedMentionFinder

    val (trainDocs,trainMap) = MentionAlignment.makeLabeledData(trainFile,null,opts.portion.value,options.useEntityType, options, map.toMap)
    val (testDocs,testMap) = MentionAlignment.makeLabeledData(testFile,null,opts.portion.value,options.useEntityType, options, map.toMap)

  }

  def tagBILOUtoMentions(sentence: Sentence, mentions: ArrayBuffer[Mention]){
    sentence.tokens.map(t => t.attr += new BILOUChunkTag(t,"O"))
    //Negative length for descending sort
    mentions.sortBy(m => m.tokens.length).foreach{mention =>
      //if(mention.forall(t => t.attr[BILOUChunkTag].categoryValue == "O"))
      mention.tokens.map{t =>
        if(t.attr[BILOUChunkTag].categoryValue == "O"){
          if(mention.tokens.length == 1)  t.attr += new BILOUChunkTag(t, "U-NP")
          else if(t == mention.head) t.attr += new BILOUChunkTag(t, "B-NP")
          else if(t == mention.last) t.attr += new BILOUChunkTag(t,"L-NP")
          else t.attr += new BILOUChunkTag(t,"I-NP")
        }
      }
    }
  }
}



object CreateChunkingTrainingData{
  def main(args: Array[String]){
    implicit val random = new scala.util.Random(0)
    val opts = new ChunkerOpts
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)
    val trainDocs = ConllCorefLoader.loadWithParse(opts.trainFile.value)
    val testDocs = ConllCorefLoader.loadWithParse(opts.testFile.value)
    println("Train: " + trainDocs.length+" documents, " + trainDocs.map(d => d.attr[MentionList].length).sum.toFloat / trainDocs.length + " mentions/doc")
    println("Test: " + testDocs.length+" documents, " + testDocs.map(d => d.attr[MentionList].length).sum.toFloat / testDocs.length + " mentions/doc")

    val trainSentencesMentions = trainDocs.flatMap(d => d.attr[MentionList].groupBy(mention=>mention.sentence))
    val testSentencesMentions = testDocs.flatMap(d => d.attr[MentionList].groupBy(mention=>mention.sentence))
    trainSentencesMentions.map(s => ChunkBasedMentionFinder.tagBILOUtoMentions(s._1,s._2))
    testSentencesMentions.map(s => ChunkBasedMentionFinder.tagBILOUtoMentions(s._1,s._2))
    val trainSentences = trainSentencesMentions.map(_._1)
    val testSentences = testSentencesMentions.map(_._1)
    println("Mentions Tagged")
    val p = new java.io.PrintWriter(new File("src/main/resources/taggedSmallerMentions.txt"))
    try {
      testSentences.foreach(_.tokens.map{t =>  p.println(t.string + " " + t.attr[PennPosTag].categoryValue + " " + t.attr[BILOUChunkTag].categoryValue)})
    } finally { p.close() }

    println("Starting Training")
    //val chunk = new CRFChunker
    //println("TrainSentences: " + trainSentences.length)
    //println("TestSentences: " + testSentences.length)
    //chunk.train(trainSentences, testSentences)
    //if (opts.saveModel.value) chunk.serialize(new FileOutputStream(new File(opts.modelFile.value)))
  }
  def mentionprint(args:Array[String]){
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    map(classOf[MentionList]) = () => ChunkBasedMentionFinder
    val options = new ForwardCoref().options
    //options that get serialized with the model
    val train = ConllCorefLoader.loadWithParse(opts.trainFile.value)
    train.map(s => println(s.sentences.mkString))
    options.setConfig("useEntityType",opts.useEntityType.value)
    options.setConfig("trainSeparatePronounWeights",opts.trainSeparatePronounWeights.value)
    // options which affect only learning
    options.useAverageIterate = opts.useAverageIterate.value
    options.numTrainingIterations = opts.numTrainingIterations.value
    options.trainPortionForTest = opts.trainPortionForTest.value
    options.useEntityLR = opts.entityLR.value
    options.saveFrequency =  opts.saveFrequency.value
    options.numThreads = opts.numThreads.value
    options.featureComputationsPerThread = opts.featureComputationsPerThread.value
    options.pruneNegTrain = opts.numPositivePairsTrain.value > 0
    options.pruneNegTest = opts.numPositivePairsTest.value > 0
    options.numPositivePairsTrain = opts.numPositivePairsTrain.value
    options.numPositivePairsTest = opts.numPositivePairsTest.value
    options.useExactEntTypeMatch = opts.useExactEntTypeMatch.value
    options.slackRescale = opts.slackRescale.value
    options.mentionAlignmentShiftWidth = opts.mentionAlignmentShiftWidth.value
    options.useNonGoldBoundaries = opts.useNonGoldBoundaries.value
    options.mergeMentionWithApposition = opts.mergeAppositions.value
    options.setConfig("usePronounRules",opts.usePronounRules.value)
    options.numCompareToTheLeft = opts.numCompareToTheLeft.value
    // options still in flux
    options.mergeFeaturesAtAll = opts.mergeFeaturesAtAll.value
    options.learningRate = opts.learningRate.value

    val (trainDocs,trainMap) = MentionAlignment.makeLabeledData(opts.trainFile.value,null,opts.portion.value,options.useEntityType, options, map.toMap)
    val (testDocs,testMap) = MentionAlignment.makeLabeledData(opts.trainFile.value,null,opts.portion.value,options.useEntityType, options, map.toMap)

  }
}



