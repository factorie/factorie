package cc.factorie.app.nlp.coref

import cc.factorie.{FeatureVectorVariable, Parameters, CategoricalTensorDomain}
import cc.factorie.optimize._
import cc.factorie.la.{Tensor1, DenseTensor1}
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.mention.{MentionList, Mention, Entity}
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.util.coref.{CorefEvaluator, GenericEntityMap}
import cc.factorie.util.{DefaultCmdOptions, BinarySerializer}
import java.util.concurrent.Callable
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
  val proSet = Set("PRP", "PRP$", "WP", "WP$")
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

  // a guess at the gender of a nominal mention
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
      if (words.length >= 3) firstName = words(1)
      else if (words.length >= 2) firstName = word0
      else firstName = word0
    }

    // determine gender using honorifics
    if (maleHonors.contains(honor)) return 'm'
    else if (femaleHonors.contains(honor)) return 'f'

    // determine from first name
    if (corefGazetteers.maleFirstNames.contains(firstName)) g = 'm'
    else if (corefGazetteers.femaleFirstNames.contains(firstName)) g = 'f'
    else if (corefGazetteers.lastNames.contains(lastWord)) g = 'p'

    if (corefGazetteers.cities.contains(fullhead) || corefGazetteers.countries.contains(fullhead)) {
      if (g.equals("m") || g.equals("f") || g.equals("p")) return 'u'
      g = 'n'
    }

    if (corefGazetteers.orgClosings.contains(lastWord)) {
      if (g.equals("m") || g.equals("f") || g.equals("p")) return 'u'
      g = 'n'
    }

    g
  }

  // a guess at a gender of a mention which is a common noun
  def nomGender(m: Mention, wn: WordNet): Char = {
    val fullhead = m.span.phrase.toLowerCase
    if (wn.isHypernymOf("male", fullhead)) 'm'
    else if (wn.isHypernymOf("female", fullhead)) 'f'
    else if (wn.isHypernymOf("person", fullhead)) 'p'
    else if (neuterWN.exists(wn.isHypernymOf(_, fullhead))) 'n'
    else 'u'
  }

  // a guess at a gender of a pronominal mention
  def proGender(m: Mention): Char = {
    val pronoun = m.span.phrase.toLowerCase
    if (malePron.contains(pronoun)) 'm'
    else if (femalePron.contains(pronoun)) 'f'
    else if (neuterPron.contains(pronoun)) 'n'
    else if (personPron.contains(pronoun)) 'p'
    else 'u'
  }

  // contructs a ground-truth entity map from a set of Mentions
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
    val wordnet = new CmdOption("wordnet", "wordnet/", "STRING", "The path to wordnet directory, which should contain a subdirectory called 'dict'") // TODO Make this instead set System Property cc.factorie.app.nlp.wordnet.WordNet to "file:"+this.value
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
    val wordnet = new WordNet(new File(CorefOptions.wordnet.value)) // TODO Should instead get it globally
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
  // We want to start training before reading all features, so we need to depend only on the domain's max size
  object model extends Parameters { val weights = Weights(new DenseTensor1(domain.dimensionDomain.maxSize)) }

  def prereqAttrs = Seq(classOf[PTBPosLabel], classOf[MentionList]) // how to specify that we need entity types?
  def postAttrs = Seq(classOf[GenericEntityMap[Mention]])

  def process1(document: Document) = {
    val facMents = document.attr[MentionList].toSeq
    val ments = facMents.map(m => new LRCorefMention(m, m.start, m.sentence.indexInSection, wn, corefGazetteers))
    val out = new GenericEntityMap[Mention]
    ments.foreach(m => out.addMention(m.mention, out.numMentions.toLong))
    for (i <- 0 until ments.size) {
      val bestCand = processOneMention(ments, i)
      if (bestCand > -1) {
        out.addCoreferentPair(ments(i).mention, ments(bestCand).mention)
      }
    }
    document.attr += out
    document
  }

  def train(docs: Seq[Document], testDocs: Seq[Document], trainIterations: Int, batchSize: Int, nThreads: Int = 2) {
    val rng = new scala.util.Random(0)
    val opt = new cc.factorie.optimize.AdaGrad with ParameterAveraging
    // since the main bottleneck is generating the training examples we do that in parallel and train sequentially
    val trainer = new OnlineTrainer(model.parameters, opt, maxIterations=trainIterations)
    for (it <- 0 until trainIterations) {
      val batches = rng.shuffle(docs).grouped(batchSize).toSeq
      for (batch <- 0 until batches.length; documents = batches(batch)) {
        println("Generating training examples batch "+ batch + " of " + batches.length)
        val examples = generateTrainingExamples(documents, nThreads)
        println("Training ")
        trainer.logEveryN = examples.length-1
        for (i <- 0 until 2) trainer.processExamples(examples)
      }
      domain.freeze()
      opt.setWeightsToAverage(model.parameters)
      println("Iteration " + it)
      // we don't evaluate on the whole training set because it feels wasteful, still it's nice to see some results
      evaluate("TRAIN MICRO", docs.take((docs.length*0.1).toInt), batchSize, nThreads)
      evaluate("TEST  MICRO", testDocs, batchSize, nThreads)
      opt.unSetWeightsToAverage(model.parameters)
    }
    opt.setWeightsToAverage(model.parameters)
  }

  import cc.factorie.util.CubbieConversions._
  def serialize(file: String)  { BinarySerializer.serialize(model, domain.dimensionDomain, new File(file)) }
  def deSerialize(file: String) { BinarySerializer.deserialize(model, domain.dimensionDomain, new File(file)) }

  private class LRCorefMention(val mention: Mention, val tokenNum: Int, val sentenceNum: Int, wordNet: WordNet, val corefGazetteers: CorefGazetteers) {
    import WithinDocCoref1._
    def parentEntity = mention.attr[Entity]
    def headPos = mention.headToken.posLabel.categoryValue
    def span = mention.span

    // All the code which follows is essentially cached per-mention information used to compute features.
    // All the pairwise features are simple combinations of these per-mention features, so having them here
    // removes them from the inner loop.
    def isPossessive = posSet.contains(headPos)
    def isNoun = nounSet.contains(headPos)
    def isProper = properSet.contains(headPos)
    def isPRO = proSet.contains(headPos)
    val hasSpeakWord = corefGazetteers.hasSpeakWord(mention, 2)
    val wnLemma = wordNet.lemma(mention.headToken.string, "n")
    val wnSynsets = wordNet.synsets(wnLemma).toSet
    val wnHypernyms = wordNet.hypernyms(wnLemma)
    val wnAntonyms = wnSynsets.flatMap(_.antonyms()).toSet
    val nounWords: Set[String] =
        span.tokens.filter(_.posLabel.categoryValue.startsWith("N")).map(t => t.string.toLowerCase).toSet
    val lowerCasePhrase: String = span.phrase.toLowerCase
    val lowerCaseHead: String = mention.headToken.string.toLowerCase
    val lowerCaseFirst: String = span.head.string.toLowerCase
    val headPhraseTrim: String = span.phrase.trim
    val nonDeterminerWords: Seq[String] =
      span.tokens.filterNot(_.posLabel.categoryValue == "DT").map(t => t.string.toLowerCase)
    val predictEntityType: String = mention.attr[EntityType].categoryValue
    val demonym: String = corefGazetteers.demonymMap.getOrElse(headPhraseTrim, "")

    val capitalization: Char = {
        if (span.length == 1 && span.head.positionInSentence == 0) 'u' // mention is the first word in sentence
            val s = span.value.filter(_.posLabel.categoryValue.startsWith("N")).map(_.string.trim)
            if (s.forall(_.forall(_.isUpper))) 'a'
            else if (s.forall(t => t.head.isLetter && t.head.isUpper)) 't'
            else 'f'
      }
    val gender: Char = {
      if (isProper) {
        WithinDocCoref1.namGender(mention, corefGazetteers)
      } else if (isPossessive) {
        val gnam = WithinDocCoref1.namGender(mention, corefGazetteers)
        val gnom = WithinDocCoref1.nomGender(mention, wordNet)
        if (gnam == 'u' && gnom != 'u') gnom else gnam
      } else if (isNoun) {
        WithinDocCoref1.nomGender(mention, wordNet)
      } else if (isPRO) {
        WithinDocCoref1.proGender(mention)
      } else {
        'u'
      }
    }
    val number: Char = {
      val fullhead = lowerCasePhrase
      if (WithinDocCoref1.singPron.contains(fullhead)) {
        's'
      } else if (WithinDocCoref1.pluPron.contains(fullhead)) {
        'p'
      } else if (WithinDocCoref1.singDet.exists(fullhead.startsWith)) {
        's'
      } else if (WithinDocCoref1.pluDet.exists(fullhead.startsWith)) {
        'p'
      } else if (isProper) {
        if (!fullhead.contains(" and ")) {
          's'
        } else {
          'u'
        }
      } else if (isNoun || isPossessive) {
          val maybeSing = if (corefGazetteers.isSingular(fullhead)) true else false
          val maybePlural = if (corefGazetteers.isPlural(fullhead)) true else false

          if (maybeSing && !maybePlural) {
            's'
          } else if (maybePlural && !maybeSing) {
            'p'
          } else if (headPos.startsWith("N")) {
            if (headPos.endsWith("S")) {
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
    val possibleAcronyms: Set[String] = {
      if (span.length == 1)
          Set.empty
        else {
          val alt1 = span.value.map(_.string.trim).filter(_.exists(_.isLetter)) // tokens that have at least one letter character
          val alt2 = alt1.filterNot(t => Stopwords.contains(t.toLowerCase)) // alt1 tokens excluding stop words
          val alt3 = alt1.filter(_.head.isUpper) // alt1 tokens that are capitalized
          val alt4 = alt2.filter(_.head.isUpper)
          Seq(alt1, alt2, alt3, alt4).map(_.map(_.head).mkString.toLowerCase).toSet
        }
    }

    // These are some actual feature functions. Not sure where else to put them.
    def isRelativeFor(other: LRCorefMention) =
      (relativizers.contains(lowerCasePhrase) &&
        ((other.span.head == other.span.last.next) ||
          ((other.span.head == span.last.next(2) && span.last.next.string.equals(","))
            || (other.span.head == span.last.next(2) && span.last.next.string.equals(",")))))


    def areRelative(m2: LRCorefMention): Boolean = isRelativeFor(m2) || m2.isRelativeFor(this)

    def areAppositive(m2: LRCorefMention): Boolean = {
        ((m2.isProper || isProper)
          && ((m2.span.last.next(2) == span.head && m2.span.last.next.string.equals(","))
              || (span.last.next(2) == m2.span.head && span.last.next.string.equals(","))))
      }

    def lowerCasePhraseMatch(mention2: LRCorefMention) =
      lowerCasePhrase.contains(mention2.lowerCasePhrase) || mention2.lowerCasePhrase.contains(lowerCasePhrase)

    def areHyp(mention2: LRCorefMention) =
      wnSynsets.exists(mention2.wnHypernyms.contains) || mention2.wnSynsets.exists(wnHypernyms.contains)
  }

  private class LeftToRightCorefFeatures extends FeatureVectorVariable[String] { def domain = self.domain; override def skipNonCategories = true }
  private def bin(value: Int, bins: Seq[Int]): Int = math.signum(value) * (bins :+ Int.MaxValue).indexWhere(_ > math.abs(value))

  private def getFeatures(mention1: LRCorefMention, mention2: LRCorefMention) = {
    val f = new LeftToRightCorefFeatures

    f += "BIAS"
    f += "Genders:" + mention1.gender + "" +  mention2.gender
    f += "Numbers:" + mention1.number + "" + mention2.number
    f += (if (mention1.nonDeterminerWords == mention2.nonDeterminerWords)  "sameNDwords" else "~sameNDWords")
    f += "pos1:" + mention1.headPos
    f += "pos2:" + mention2.headPos
    f += (if (!mention1.nounWords.intersect(mention2.nounWords).isEmpty) "shareNouns" else "~shareNouns")
    f += (if (mention1.lowerCasePhraseMatch(mention2)) "phraseMatch" else "~phraseMatch")
    f += (if (mention1.wnSynsets.exists(mention2.wnSynsets.contains)) "Synonyms" else "~Synonyms")
    f += (if (mention1.wnSynsets.exists(mention2.wnAntonyms.contains)) "Antonyms" else "~Antonyms")
    f += (if (mention1.areHyp(mention2)) "AreHypernyms" else "~AreHypernyms")
    f += (if (mention1.wnHypernyms.exists(mention2.wnHypernyms.contains)) "ShareHypernyms" else "~ShareHypernyms")
    f += (if (mention1.areAppositive(mention2)) "AreAppositive" else "~AreAppositive")
    f += (if (mention1.hasSpeakWord && mention2.hasSpeakWord) "BothSpeak" else "~BothSpeak")
    f += (if (mention1.areRelative(mention2)) "AreRelative" else "~AreRelative")
    f += "PosPronoun:" + mention2.headPos + (if (mention2.isPRO) mention1.mention.headToken.string else mention1.headPos)
    f += "PredictedEntityTypes:" + mention1.predictEntityType+mention2.predictEntityType
    f += "HeadWords:" + mention1.mention.headToken.string+mention2.mention.headToken.string
    f += (if (mention1.span.sentence == mention2.span.sentence) "SameSentence" else "~SameSentence")
    f += (if (mention1.lowerCaseFirst == mention2.lowerCaseFirst) "BeginMatchLC" else "~BeginMatchLC")
    f += (if (mention1.lowerCaseHead == mention2.lowerCaseHead) "EndMatchLC" else "~EndMatchLC")
    f += (if (mention1.span.head.string == mention2.span.head.string) "BeginMatch" else "~BeginMatch")
    f += (if (mention1.span.last.string == mention2.span.last.string) "EndMatch" else "~EndMatch")
    f += (if (mention1.isPRO) "1IsPronoun" else "~1IsPronoun")
    f += (if (mention1.demonym != "" && mention1.demonym == mention2.demonym) "DemonymMatch" else "~DemonymMatch")
    f += "Capitalizations:" + mention1.capitalization +"_" +  mention2.capitalization
    f += "HeadPoss:" + mention2.mention.headToken.posLabel.value + "_" + mention1.mention.headToken.posLabel.value
    f += (if (mention1.possibleAcronyms.exists(mention2.possibleAcronyms.contains)) "AcronymMatch" else "~AcronymMatch")

    val sdist = bin(mention1.sentenceNum - mention2.sentenceNum, 1 to 10)
    for (sd <- 1 to sdist) { f += "SentenceDistance>=" + sd}
    val tdist = bin(mention1.tokenNum - mention2.tokenNum, Seq(1, 2, 3, 4, 5, 10, 20, 50, 100, 200))
    for (td <- 1 to tdist) f += "TokenDistance>=" + td

    f
  }

  // processing one mention at testing time
  private def processOneMention(orderedMentions: Seq[LRCorefMention], mentionIndex: Int): Int = {
    val m1 = orderedMentions(mentionIndex)
    var bestCand = -1
    var bestScore = Double.MinValue
    var j = mentionIndex - 1
    var numPositivePairs = 0
    while (j >= 0 && (numPositivePairs < 100)) {
      val m2 = orderedMentions(j)
      if (!m2.isPRO || m1.isPRO) { // we try to link if either the later mention is a pronoun or the earlier one isn't
        val score = model.weights.value dot getFeatures(orderedMentions(mentionIndex), orderedMentions(j)).value
        if (score > 0.0) {
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

  // processing one document at training time
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
    try {
      import collection.JavaConversions._
      pool.invokeAll(docs.map(docToCallable)).flatMap(_.get)
    } finally {
      pool.shutdown()
    }
  }

  private def evaluate(name: String, docs: Seq[Document], batchSize: Int, nThreads: Int) {
    import CorefEvaluator.Metric
    val ceafEEval = new CorefEvaluator.CeafE()
    val ceafMEval = new CorefEvaluator.CeafM()
    def docToCallable(doc: Document) = new Callable[(Metric,Metric,Metric,Metric,Metric)] { def call() = {
      process1(doc)
      val trueMap = WithinDocCoref1.truthEntityMap(doc.attr[MentionList])
      val predMap = doc.attr[GenericEntityMap[Mention]]
      val b3 = CorefEvaluator.BCubedNoSingletons.evaluate(predMap, trueMap)
      val muc = CorefEvaluator.MUC.evaluate(predMap, trueMap)
      val ce = ceafEEval.evaluate(predMap, trueMap)
      val cm = ceafMEval.evaluate(predMap, trueMap)
      val bl = CorefEvaluator.Blanc.evaluate(predMap, trueMap)
      (b3,muc,ce,cm,bl)
    }}
    val pool = java.util.concurrent.Executors.newFixedThreadPool(nThreads)
    try {
      import collection.JavaConversions._
      val b3Score = new CorefEvaluator.Metric
      val mucScore = new CorefEvaluator.Metric
      val ceafE = new CorefEvaluator.Metric
      val ceafM = new CorefEvaluator.Metric
      val blanc = new CorefEvaluator.Metric
      val batched = docs.grouped(batchSize).toSeq
      for (batch <- batched) {
        val results = pool.invokeAll(batch.map(docToCallable(_))).map(_.get)
        results.foreach(eval => {
          b3Score.microAppend(eval._1)
          mucScore.microAppend(eval._2)
          ceafE.microAppend(eval._3)
          ceafM.microAppend(eval._4)
          blanc.macroAppend(eval._5)
        })
      }
      println("                   PR    RE   F1")
      println(f"$name%s    B3  ${100*b3Score.precision}%2.2f ${100*b3Score.recall}%2.2f ${100*b3Score.f1}%2.2f ")
      println(f"$name%s   MUC  ${100*mucScore.precision}%2.2f ${100*mucScore.recall}%2.2f ${100*mucScore.f1}%2.2f ")
      println(f"$name%s   C-E  ${100*ceafE.precision}%2.2f ${100*ceafE.recall}%2.2f ${100*ceafE.f1}%2.2f ")
      println(f"$name%s   C-M  ${100*ceafM.precision}%2.2f ${100*ceafM.recall}%2.2f ${100*ceafM.f1}%2.2f ")
      println(f"$name%s BLANC  ${100*blanc.precision}%2.2f ${100*blanc.recall}%2.2f ${100*blanc.f1}%2.2f ")
    } finally {
      pool.shutdown()
    }
  }
}

