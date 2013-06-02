package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.mention.{MentionList, Mention, Entity}
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Document, Token}
import cc.factorie.app.strings.Stopwords
import cc.factorie.{FeatureVectorVariable, Parameters, CategoricalTensorDomain}
import cc.factorie.la.{Tensor1, WeightsMapAccumulator, DenseTensor1}
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.util.coref.{CorefEvaluator, GenericEntityMap}
import cc.factorie.util.{DefaultCmdOptions, BinarySerializer, DoubleAccumulator}
import java.util.concurrent.Callable
import cc.factorie.optimize._
import java.io.File

/**
 * User: apassos
 * Date: 5/30/13
 * Time: 10:07 PM
 */

object WithinDocCoref1 {
  val properSet = Set("NNP", "NNPS")
  val nounSet = Seq("NN", "NNS")
  val posSet = Seq("POS")
  val posTagsSet = Set("PRP", "PRP$", "WP", "WP$")
  val relativizers = Set("who", "whom", "which", "whose", "whoever", "whomever", "whatever", "whichever", "that")

  val maleHonors = Set("mr", "mister")
  val femaleHonors = Set("ms", "mrs", "miss", "misses")
  val neuterWN = Set("artifact", "location", "group")
  val singPron = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluPron = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singDet = Set("a ", "an ", "this ")
  val pluDet = Set("those ", "these ", "some ")

  val malePron = Set("he", "him", "his", "himself")
  val femalePron = Set("she", "her", "hers", "herself")
  val neuterPron = Set("it", "its", "itself", "this", "that", "anything", "something",  "everything", "nothing", "which", "what", "whatever", "whichever")
  val personPron = Set("you", "your", "yours", "i", "me", "my", "mine", "we", "our", "ours", "us", "myself", "ourselves", "themselves", "themself", "ourself", "oneself", "who", "whom", "whose", "whoever", "whomever", "anyone", "anybody", "someone", "somebody", "everyone", "everybody", "nobody")

  val allPronouns = maleHonors ++ femaleHonors ++ neuterWN ++ malePron ++ femalePron ++ neuterPron ++ personPron

  def namGender(m: Mention, corefGazetteers: CorefGazetteers): Char = {
    val fullhead = m.span.phrase.trim.toLowerCase
    var g = 'u'
    val words = fullhead.split("\\s")
    if (words.length == 0) return g

    val word0 = words.head
    val lastWord = words.last

    var firstName = ""
    var honor = ""
    if (corefGazetteers.honors.contains(word0)) {
      honor = word0
      if (words.length >= 3)
        firstName = words(1)
    } else if (words.length >= 2) {
      firstName = word0
    } else {
      firstName = word0
    }

    // determine gender using honorifics
    if (maleHonors.contains(honor))
      return 'm'
    else if (femaleHonors.contains(honor))
      return 'f'

    // determine from first name
    if (corefGazetteers.maleFirstNames.contains(firstName))
      g = 'm'
    else if (corefGazetteers.femaleFirstNames.contains(firstName))
      g = 'f'
    else if (corefGazetteers.lastNames.contains(lastWord))
      g = 'p'

    if (corefGazetteers.cities.contains(fullhead) || corefGazetteers.countries.contains(fullhead)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    if (corefGazetteers.orgClosings.contains(lastWord)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    g
  }

  def nomGender(m: Mention, wn: WordNet): Char = {
    val fullhead = m.span.phrase.toLowerCase
    if (wn.isHypernymOf("male", fullhead))
      'm'
    else if (wn.isHypernymOf("female", fullhead))
      'f'
    else if (wn.isHypernymOf("person", fullhead))
      'p'
    else if (neuterWN.exists(wn.isHypernymOf(_, fullhead)))
      'n'
    else
      'u'
  }


  def proGender(m: Mention): Char = {
    val pronoun = m.span.phrase.toLowerCase
    if (malePron.contains(pronoun))
      'm'
    else if (femalePron.contains(pronoun))
      'f'
    else if (neuterPron.contains(pronoun))
      'n'
    else if (personPron.contains(pronoun))
      'p'
    else
      'u'
  }


  def strongerOf(g1: Char, g2: Char): Char = {
    if ((g1 == 'm' || g1 == 'f') && (g2 == 'p' || g2 == 'u'))
      g1
    else if ((g2 == 'm' || g2 == 'f') && (g1 == 'p' || g1 == 'u'))
      g2
    else if ((g1 == 'n' || g1 == 'p') && g2 == 'u')
      g1
    else if ((g2 == 'n' || g2 == 'p') && g1 == 'u')
      g2
    else
      g2
  }

  def truthEntityMap(mentions: MentionList) = {
    val map = new GenericEntityMap[Mention]
    mentions.foreach(m => map.addMention(m, map.numMentions.toLong))
    val entities = mentions.groupBy(_.attr[Entity])
    entities.flatMap(_._2.sliding(2)).foreach(p => {
      if (p.size == 2) map.addCoreferentPair(p(0), p(1))
    })
    map
  }

  object CorefOptions extends DefaultCmdOptions {
    val train = new CmdOption("train", "conll-train-clean.txt", "STRING", "An ontonotes training file")
    val test = new CmdOption("test", "conll-train-clean.txt", "STRING", "An ontonotes testing file")
    val wordnet = new CmdOption("wordnet", "wordnet/", "STRING", "The path to wordnet files")
    val gazetteers = new CmdOption("gazetteers", "factorie-nlp-resources/src/main/resources/cc/factorie/app/nlp/lexicon/", "STRING", "Path to the gazetteers")
    val trainPortion = new CmdOption("trainPortion", 1.0, "STRING", "Fraction of train / test data to use")
    val testPortion = new CmdOption("testPortion", 1.0, "STRING", "Fraction of train / test data to use")
    val model = new CmdOption("model-file", "coref-model", "STRING", "File to which to save the model")
    val iterations = new CmdOption("iterations", 2, "INT", "Number of iterations for training")
    val batchSize = new CmdOption("batch-size", 30, "INT", "Number of documents to tokenize at a time")
  }
  def main(args: Array[String]) {
    CorefOptions.parse(args)
    println("Loading data")
    val allTrain = ConllCorefLoader.loadWithParse(CorefOptions.train.value)
    val allTest = ConllCorefLoader.loadWithParse(CorefOptions.test.value)
    val trainDocs = allTrain.take((allTrain.length*CorefOptions.trainPortion.value).toInt)
    val testDocs = allTest.take((allTest.length*CorefOptions.testPortion.value).toInt)
    println("Training on " + trainDocs.length + " with " + (trainDocs.map(_.attr[MentionList].length).sum/trainDocs.length.toFloat) + " mentions per document")
    println("Loading wordnet")
    val wordnet = new WordNet(CorefOptions.wordnet.value)
    println("Loading gazetteers")
    val gazetteers = new CorefGazetteers(CorefOptions.gazetteers.value)
    println("Training")
    val coref = new WithinDocCoref1(wordnet, gazetteers)
    coref.train(trainDocs, testDocs, CorefOptions.iterations.value, CorefOptions.batchSize.value)
    println("Serializing")
    coref.serialize(CorefOptions.model.value)
  }
}


class WithinDocCoref1(wn: WordNet, val corefGazetteers: CorefGazetteers) extends cc.factorie.app.nlp.DocumentAnnotator {
  self =>
  object domain extends CategoricalTensorDomain[String] { dimensionDomain.maxSize = 5e5.toInt }
  class CorefModel extends Parameters {
    val weights = Weights(new DenseTensor1(domain.dimensionDomain.maxSize))
  }
  val model = new CorefModel
  var threshold = 0.0

  def prereqAttrs = Seq(classOf[PTBPosLabel], classOf[MentionList])
  def postAttrs = Seq(classOf[GenericEntityMap[Mention]])

  def process1(document: Document) = {
    val facMents = document.attr[MentionList].toSeq
    val ments = facMents.map(m => new LRCorefMention(m, m.start, m.sentence.indexInSection, wn, corefGazetteers))
    val out = new GenericEntityMap[Mention]
    ments.foreach(m => out.addMention(m.mention, out.numMentions.toLong))
    for (i <- 0 until ments.size) {
      val m1 = ments(i)
      val bestCand = processOneMention(ments, i)
      if (bestCand > -1) {
        out.addCoreferentPair(ments(i).mention, ments(bestCand).mention)
      }
    }
    document.attr += out
    document
  }

  private class LRCorefMention(val mention: Mention, val tokenNum: Int, val sentenceNum: Int, wordNet: WordNet, val corefGazetteers: CorefGazetteers) extends cc.factorie.Attr {
    import WithinDocCoref1._
    val _head =  mention.document.asSection.tokens(mention.headTokenIndex)
    def headToken: Token = _head
    def parentEntity = mention.attr[Entity]
    def headPos = headToken.posLabel.categoryValue
    def span = mention.span

    def document = mention.document

    var cache = new LRFeatureCache(this, wordNet, corefGazetteers)
    def clearCache() = cache = new LRFeatureCache(this, wordNet, corefGazetteers)
    def isPossessive = posSet.contains(headPos)
    def isNoun = nounSet.contains(headPos)
    def isProper = properSet.contains(headPos)
    def isPRO = posTagsSet.contains(headPos)
    def isRelativeFor(other: LRCorefMention) =
      (relativizers.contains(cache.lowerCaseHead) &&
        ((other.span.head == other.span.last.next) ||
          ((other.span.head == span.last.next(2) && span.last.next.string.equals(","))
            || (other.span.head == span.last.next(2) && span.last.next.string.equals(",")))))


    def areRelative(m2: LRCorefMention): Boolean = isRelativeFor(m2) || m2.isRelativeFor(this)

    def areAppositive(m2: LRCorefMention): Boolean = {
        ((m2.isProper || isProper)
          && ((m2.span.last.next(2) == span.head && m2.span.last.next.string.equals(","))
              || (span.last.next(2) == m2.span.head && span.last.next.string.equals(","))))
      }


  }

  private class LRFeatureCache(m: LRCorefMention, wordNet: WordNet, corefGazetteers: CorefGazetteers) {
    lazy val hasSpeakWord = corefGazetteers.hasSpeakWord(m.mention, 2)
    lazy val wnLemma = wordNet.lemma(m.headToken.string, "n")
    lazy val wnSynsets = wordNet.synsets(wnLemma).toSet
    lazy val wnHypernyms = wordNet.hypernyms(wnLemma)
    lazy val wnAntonyms = wnSynsets.flatMap(_.antonyms()).toSet
    lazy val nounWords: Set[String] =
        m.span.tokens.filter(_.posLabel.categoryValue.startsWith("N")).map(t => t.string.toLowerCase).toSet
    lazy val lowerCaseHead: String = m.span.phrase.toLowerCase
    lazy val headPhraseTrim: String = m.span.phrase.trim
    lazy val nonDeterminerWords: Seq[String] =
      m.span.tokens.filterNot(_.posLabel.categoryValue == "DT").map(t => t.string.toLowerCase)
    lazy val initials: String =
        m.span.tokens.map(_.string).filterNot(corefGazetteers.orgClosings.contains(_)).filter(t => t(0).isUpper).map(_(0)).mkString("")
    lazy val predictEntityType: String = "" // TODO: have predicted entity type in factorie somehow. m.attr[PredictedEntityType].str
    lazy val demonym: String = corefGazetteers.demonymMap.getOrElse(headPhraseTrim, "")

    lazy val capitalization: Char = {
        if (m.span.length == 1 && m.span.head.positionInSentence == 0) 'u' // mention is the first word in sentence
            val s = m.span.value.filter(_.posLabel.categoryValue.startsWith("N")).map(_.string.trim)
            if (s.forall(_.forall(_.isUpper))) 'a'
            else if (s.forall(t => t.head.isLetter && t.head.isUpper)) 't'
            else 'f'
      }
    lazy val gender: Char = {
      if (m.isProper) {
        WithinDocCoref1.namGender(m.mention, corefGazetteers)
      } else if (m.isPossessive) {
        val gnam = WithinDocCoref1.namGender(m.mention, corefGazetteers)
        val gnom = WithinDocCoref1.nomGender(m.mention, wordNet)
        if (gnam == 'u' && gnom != 'u') gnom else gnam
      } else if (m.isNoun) {
        WithinDocCoref1.nomGender(m.mention, wordNet)
      } else if (m.isPRO) {
        WithinDocCoref1.proGender(m.mention)
      } else {
        'u'
      }
    }
    lazy val number: Char = {
      val fullhead = lowerCaseHead
      if (WithinDocCoref1.singPron.contains(fullhead)) {
        's'
      } else if (WithinDocCoref1.pluPron.contains(fullhead)) {
        'p'
      } else if (WithinDocCoref1.singDet.exists(fullhead.startsWith)) {
        's'
      } else if (WithinDocCoref1.pluDet.exists(fullhead.startsWith)) {
        'p'
      } else if (m.isProper) {
        if (!fullhead.contains(" and ")) {
          's'
        } else {
          'u'
        }
      } else if (m.isNoun || m.isPossessive) {
          val maybeSing = if (corefGazetteers.isSingular(fullhead)) true else false
          val maybePlural = if (corefGazetteers.isPlural(fullhead)) true else false

          if (maybeSing && !maybePlural) {
            's'
          } else if (maybePlural && !maybeSing) {
            'p'
          } else if (m.headPos.startsWith("N")) {
            if (m.headPos.endsWith("S")) {
              'p'
            } else {
              's'
            }
          } else {
            'u'
          }
      } else {
        'u'
      }
    }
    lazy val acronym: Set[String] = {
      if (m.span.length == 1)
          Set.empty
        else {
          val alt1 = m.span.value.map(_.string.trim).filter(_.exists(_.isLetter)) // tokens that have at least one letter character
          val alt2 = alt1.filterNot(t => Stopwords.contains(t.toLowerCase)) // alt1 tokens excluding stop words
          val alt3 = alt1.filter(_.head.isUpper) // alt1 tokens that are capitalized
          val alt4 = alt2.filter(_.head.isUpper)
          Seq(alt1, alt2, alt3, alt4).map(_.map(_.head).mkString.toLowerCase).toSet
        }
    }
  }

  private class LeftToRightCorefFeatures extends FeatureVectorVariable[String] { def domain = self.domain; override def skipNonCategories = true }
  private def bin(value: Int, bins: Seq[Int]): Int = math.signum(value) * (bins :+ Int.MaxValue).indexWhere(_ > math.abs(value))

  private def getFeatures(mention1: LRCorefMention, mention2: LRCorefMention) = {
    val f = new LeftToRightCorefFeatures

    f += "BIAS"
    f += "gmc" + mention1.cache.gender + "" +  mention2.cache.gender
    f += "nms" + mention1.cache.number + "" + mention2.cache.number
    if (mention1.cache.nonDeterminerWords == mention2.cache.nonDeterminerWords) f += "hms"
    else f += "hmsf"
    f += "mt1" + mention1.headPos
    f += "mt2" + mention2.headPos
    if (!mention1.cache.nounWords.intersect(mention2.cache.nounWords).isEmpty) f += "pmhm"
    else f += "pmhmf"
    if (mention1.cache.lowerCaseHead.contains(mention2.cache.lowerCaseHead) || mention2.cache.lowerCaseHead.contains(mention1.cache.lowerCaseHead))
      f += "sh"
    else f += "shf"
    if (mention1.cache.wnSynsets.exists(mention2.cache.wnSynsets.contains))
      f += "asyn"
    else f += "asynf"
    if (mention1.cache.wnSynsets.exists(mention2.cache.wnAntonyms.contains))
      f += "aan"
    else f += "aanf"
    if (mention1.cache.wnSynsets.exists(mention2.cache.wnHypernyms.contains) || mention2.cache.wnSynsets.exists(mention1.cache.wnHypernyms.contains))
      f += "ahyp"
    else f += "ahypf"
    if (mention1.cache.wnHypernyms.exists(mention2.cache.wnHypernyms.contains)) f += "hsh"
    else f += "hshf"
    if (mention1.areAppositive(mention2))
      f += "aA"
    else f += "aAf"
    if (mention1.cache.hasSpeakWord && mention2.cache.hasSpeakWord)
      f += "bs"
    else f += "bsf"
    if (mention1.areRelative(mention2))
      f += "rpf"
    else f += "rpff"
    f += "mtpw" + (if (mention2.isPRO) mention2.headPos + mention1.headToken.string else mention2.headPos + mention1.headPos)
    f += "etm" + mention1.cache.predictEntityType+mention2.cache.predictEntityType
    f += "lhp" + mention1.headToken.string+mention2.headToken.string
    if (mention1.span.sentence == mention2.span.sentence)
      f += "ss"

    if (mention1.span.head.string.toLowerCase == mention2.span.head.string.toLowerCase)
      f += "bM"
    else f += "bMf"
    if (mention1.span.last.string.toLowerCase == mention2.span.last.string.toLowerCase)
      f += "eM"
    else f += "eMf"
    if (mention1.span.head.string == mention2.span.head.string)
      f += "bMc"
    else f += "bMcf"
    if (mention1.span.last.string == mention2.span.last.string)
      f += "eMc"
    else f += "eMcf"
    if (mention1.isPRO)
      f += "pa"
    else f += "paf"

    val binTokenSentenceDistances = false
    val sdist = bin(mention1.sentenceNum - mention2.sentenceNum, 1 to 10)
    if (binTokenSentenceDistances) for (sd <- 1 to sdist) f += "sd" + sd
    else f += "sd" + sdist
    val tdist = bin(mention1.tokenNum - mention2.tokenNum, Seq(1, 2, 3, 4, 5, 10, 20, 50, 100, 200))
    if (binTokenSentenceDistances) for (td <- 1 to tdist) f += "td" + td
    else f += "td" + tdist
    if (mention1.cache.demonym != "" && mention1.cache.demonym == mention2.cache.demonym) f += "dM"
    else f += "dMf"
    f += "cap" + mention1.cache.capitalization +"_" +  mention2.cache.capitalization
    f += "hpos" + mention2.headToken.posLabel.value + "_" + mention1.headToken.posLabel.value
    f += "am" + mention1.cache.acronym.exists(mention2.cache.acronym.contains)

    f
  }

  private def processOneMention(orderedMentions: Seq[LRCorefMention], mentionIndex: Int): Int = {
    val m1 = orderedMentions(mentionIndex)
    var bestCand = -1
    var bestScore = Double.MinValue

    var j = mentionIndex - 1
    var numPositivePairs = 0
    while (j >= 0 && (numPositivePairs < 100)) {
      val m2 = orderedMentions(j)

      val cataphora = m2.isPRO && !m1.isPRO

      if (!cataphora) {
        val candFeats = getFeatures(orderedMentions(mentionIndex), orderedMentions(j))

        val score = model.weights.value dot candFeats.value
        if (score > threshold) {
          numPositivePairs += 1
          if (bestScore <= score) {
            bestCand = j
            bestScore = score
          }
        }
      }
      j -= 1
    }
    bestCand
  }

  private def oneDocExample(doc: Document): Seq[Example] = {
    val mentions = doc.attr[MentionList].map(m => new LRCorefMention(m, m.start, m.sentence.indexInSection, wn, corefGazetteers))
    val examplesList = collection.mutable.ListBuffer[Example]()
    for (anaphorIndex <- 0 until mentions.length) {
      val m1 = mentions(anaphorIndex)
      var numAntecedent = 0
      var i = anaphorIndex - 1
      while (i >= 0 && numAntecedent < 2) {
        val m2 = mentions(i)
        val label = m1.parentEntity == m2.parentEntity
        if (label) numAntecedent += 1
        if (!(m2.isPRO && !m1.isPRO) && (label || (numAntecedent < 3))) {
          examplesList += new LinearBinaryExample(model.weights,
            getFeatures(m1, m2).value.asInstanceOf[Tensor1],
            if (label) 1 else -1,
            LinearObjectives.hingeScaledBinary(negSlackRescale=3.0))
        }
        i -= 1
      }
    }
    examplesList
  }

  private def generateTrainingExamples(docs: Seq[Document], nThreads: Int): Seq[Example] = {
    def docToCallable(doc: Document) = new Callable[Seq[Example]] { def call() = oneDocExample(doc) }
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    import collection.JavaConversions._
    val result = pool.invokeAll(docs.map(docToCallable)).flatMap(_.get)
    pool.shutdown()
    result
  }

  private def evaluate(name: String, docs: Seq[Document], batchSize: Int, nThreads: Int) {
    def docToCallable(doc: Document) = new Callable[Document] { def call() = process1(doc) }
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    import collection.JavaConversions._
    val b3Score = new cc.factorie.util.coref.CorefEvaluator.Metric
    val mucScore = new cc.factorie.util.coref.CorefEvaluator.Metric
    val batched = docs.grouped(batchSize).toSeq
    for (batch <- batched) {
      val docs = pool.invokeAll(batch.map(docToCallable(_))).map(_.get)
      docs.foreach(doc => {
        val trueMap = WithinDocCoref1.truthEntityMap(doc.attr[MentionList])
        val predMap = doc.attr[GenericEntityMap[Mention]]
        val b3 = CorefEvaluator.BCubedNoSingletons.evaluate(predMap, trueMap)
        val muc = CorefEvaluator.MUC.evaluate(predMap, trueMap)
          b3Score.microAppend(b3)
          mucScore.microAppend(muc)
      })
    }
    pool.shutdown()
    println("          PRECISION RECALL F1")
    println(f"$name%s  B3  ${100*b3Score.precision}%2.2f ${100*b3Score.recall}%2.2f ${100*b3Score.f1}%2.2f ")
    println(f"$name%s MUC  ${100*mucScore.precision}%2.2f ${100*mucScore.recall}%2.2f ${100*mucScore.f1}%2.2f ")
  }

  def train(docs: Seq[Document], testDocs: Seq[Document], trainIterations: Int, batchSize: Int, nThreads: Int = 2) {
    val rng = new scala.util.Random(0)
    val opt = new cc.factorie.optimize.AdaGrad with ParameterAveraging
    val trainer = new OnlineTrainer(model.parameters, opt, maxIterations=trainIterations)
    for (it <- 0 until trainIterations) {
      var batch = 0
      val batches = rng.shuffle(docs).grouped(batchSize).toSeq
      for (documents <- batches) {
        println("Generating training examples batch "+ batch + " of " + batches.length)
        val examples = generateTrainingExamples(documents, nThreads)
        println("Trainining ")
        trainer.logEveryN = examples.length-1
        for (i <- 0 until 2) trainer.processExamples(examples)
        batch += 1
      }
      domain.freeze()
      opt.setWeightsToAverage(model.parameters)
      println("Iteration " + it)
      evaluate("TRAIN MICRO", docs.take((docs.length*0.1).toInt), batchSize, nThreads)
      evaluate("TEST  MICRO", testDocs, batchSize, nThreads)
      opt.unSetWeightsToAverage(model.parameters)
    }
    opt.setWeightsToAverage(model.parameters)
    // TODO: calibrate the threshold of the model to balance precision and recall
  }

  import cc.factorie.util.CubbieConversions._
  def serialize(file: String)  { BinarySerializer.serialize(model, domain.dimensionDomain, new File(file)) }
  def deSerialize(file: String) { BinarySerializer.deserialize(model, domain.dimensionDomain, new File(file)) }
}

