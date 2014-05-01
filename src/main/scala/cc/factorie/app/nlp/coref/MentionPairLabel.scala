package cc.factorie.app.nlp.coref

import cc.factorie.la.{Tensor1, SparseTensor, GrowableSparseBinaryTensor1}
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable}
import scala.collection.mutable
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.ner.OntonotesEntityTypeDomain

/** A binary feature vector for the features of a mention pair.
    Here, mention1 is the mention to the right.
    @author Alexandre Passos
 */
class MentionPairFeatures(val model: CorefModel, val mention1:Mention, val mention2:Mention, mentions: Seq[Mention], options: Coref1Options) extends BinaryFeatureVectorVariable[String] {
 // def this(label:MentionPairLabel) = this(label.model,label.mention1,label.mention2,label.mentions,label.options)
  {
    val t = new GrowableSparseBinaryTensor1(domain.dimensionDomain)
    t.sizeHint(if (options.conjunctionStyle == ConjunctionOptions.SLOW_CONJUNCTIONS) 650 else 70)
    set(t)(null)
  }
  def domain = model.MentionPairFeaturesDomain
  override def skipNonCategories = true
  val features = this
  var basicFeatureCalculated = false
  var conjunctionCalculated = false
  val mergeableFeatures = collection.mutable.Set[String]()
  def bin(value: Int, bins: Seq[Int]): Int = math.signum(value) * (bins :+ Int.MaxValue).indexWhere(_ > math.abs(value))
  val pfx =  mentType(mention1) +":" + mentType(mention2)
  def mentType(mention:Mention): String = if (mention.phrase.isPronoun) "pro" else "non"

  def addFeature(f: String) {
    if(options.trainSeparatePronounWeights){
      features += pfx + "-" +  f
    }else
      features += f
  }



  computeFeatures()
  def computeConjunctionFeatures() {
    if (basicFeatureCalculated && !conjunctionCalculated) {
      if (options.conjunctionStyle == ConjunctionOptions.SLOW_CONJUNCTIONS) {
        val activeDomainSize = features.value.activeDomainSize
        val basicFeats = features.value.asInstanceOf[SparseTensor]._indices
        //Note: this doesnt quite work with hash domains
        for (a <- 0 until activeDomainSize - 1) {
          for (b <- a + 1 until activeDomainSize) {
            val sb = new StringBuilder
            sb.append(basicFeats(a)); sb.append("_&&_"); sb.append(basicFeats(b))
            addFeature(sb.toString())
          }
        }
      }
      conjunctionCalculated = true
    }
  }
  def computeFeatures() { computeBasicFeatures();}// computeConjunctionFeatures() }

  def addMergeableFeature(f: String) {
    if (options.mergeFeaturesAtAll) {
      assert(mergeableFeatures ne null)
      assert(f ne null)
      mergeableFeatures += f
    }
    addFeature(f)
  }

  lazy val mergeableAllFeatures = mergeableFeatures

  def proWordHead: String = {
    val m1c = mention1.attr[MentionCharacteristics] 
    val m2c = mention2.attr[MentionCharacteristics] 
    val e1 = if (m2c.isPRO) mention2.phrase.headToken.string else m2c.predictEntityType
    val e2 = if (m1c.isPRO) mention1.phrase.headToken.string else m1c.predictEntityType
    e1 + "&&" + e2
  }
  
  def entityTypeMatch: String = {
    val m1c = mention1.attr[MentionCharacteristics] 
    val m2c = mention2.attr[MentionCharacteristics] 
    val t1 = m2c.predictEntityType
    val t2 = m1c.predictEntityType
    if (t1 == OntonotesEntityTypeDomain.O || t2 == OntonotesEntityTypeDomain.O)
      "unknown"
    else if (t1 == t2)
      "true"
    else
      "false"
  }

  def acronymMatch: Char = {
    val m1 = mention1.attr[MentionCharacteristics]
    val m2 = mention2.attr[MentionCharacteristics]
    if (mention1.phrase.length == 1 && mention2.phrase.length > 1) {
      if (m2.acronym.contains(mention1.phrase.string.trim.toLowerCase)) 't' else 'f'
    } else if (mention1.phrase.length > 1 && mention2.phrase.length == 1) {
      if (m1.acronym.contains(mention2.phrase.string.trim.toLowerCase)) 't' else 'f'
    } else
      'u'
  }


  def computeBasicFeatures() {
    val m1 = if(mention1.attr[MentionCharacteristics] eq null){ mention1.attr += new MentionCharacteristics(mention1); mention1.attr[MentionCharacteristics]} else mention1.attr[MentionCharacteristics]
    val m2 = if(mention2.attr[MentionCharacteristics] eq null){ mention2.attr += new MentionCharacteristics(mention1); mention2.attr[MentionCharacteristics]} else mention2.attr[MentionCharacteristics]
    if (basicFeatureCalculated) return


    addMergeableFeature("BIAS")
    addMergeableFeature("gmc" + m1.gender + "" +  m2.gender)
    addMergeableFeature("nms" + m1.number + "" + m2.number)
    if (m1.nonDeterminerWords == m2.nonDeterminerWords)
      addMergeableFeature("hms")
    else addMergeableFeature("hmsf")
    addMergeableFeature("mt1" + m1.headPos) // although mention1 is the anaphor and mention2 is the antecedent, to match Roth's implementation, m1MentionType returns the type of mention2
    addFeature("mt2" + m2.headPos)
    if (!m1.nounWords.intersect(m2.nounWords).isEmpty)
      addMergeableFeature("pmhm")
    else addMergeableFeature("pmhmf")
    if (m1.lowerCaseHead.contains(m2.lowerCaseHead) || m2.lowerCaseHead.contains(m1.lowerCaseHead))
      addMergeableFeature("sh")
    else addMergeableFeature("shf")
    if (CorefFeatures.canBeAliases(mention1, mention2)) addMergeableFeature("sapetc") else addMergeableFeature("soonAliasPredETypeCached:false")
    if (m1.wnSynsets.exists(m2.wnSynsets.contains))
      addMergeableFeature("asyn")
    else addMergeableFeature("asynf")
    if (m1.wnSynsets.exists(m2.wnAntonyms.contains))
      addMergeableFeature("aan")
    else addMergeableFeature("aanf")
    if (m1.wnSynsets.exists(m2.wnHypernyms.contains) || m2.wnSynsets.exists(m1.wnHypernyms.contains))
      addMergeableFeature("ahyp")
    else addMergeableFeature("ahypf")
    if (m1.wnHypernyms.exists(m2.wnHypernyms.contains)) addMergeableFeature("hsh")
    else addMergeableFeature("hshf")
    if (CorefFeatures.areAppositive(mention1, mention2))
      addMergeableFeature("aA")
    else addMergeableFeature("aAf")
    if (m1.hasSpeakWord && m2.hasSpeakWord)
      addMergeableFeature("bs")
    else addMergeableFeature("bsf")
    if (CorefFeatures.areRelative(mention1, mention2))
      addMergeableFeature("rpf")
    else addMergeableFeature("rpff")
    for (cm <- CorefFeatures.countCompatibleMentionsBetween(mention1, mention2, mentions.toSeq)) addMergeableFeature("cmc" + cm)
    addMergeableFeature("mtpw" + (if (m2.isPRO) m2.headPos + mention1.phrase.headToken.string else m2.headPos + m1.headPos))
    addMergeableFeature("pwhe" + proWordHead)
    addMergeableFeature("etm" + entityTypeMatch)
    addMergeableFeature("lhp" + CorefFeatures.headWordsCross(mention1, mention2, model))
    if (mention1.phrase.sentence == mention2.phrase.sentence) addMergeableFeature("ss") // false values of this feature are not included in Roth's system
    CorefFeatures.matchingTokensRelations(mention1, mention2).foreach(r => addMergeableFeature("apr" + r))

    if (mention1.phrase.head.string.toLowerCase == mention2.phrase.head.string.toLowerCase)
      addMergeableFeature("bM")
    else addMergeableFeature("bMf")
    if (mention1.phrase.last.string.toLowerCase == mention2.phrase.last.string.toLowerCase)
      addMergeableFeature("eM")
    else addMergeableFeature("eMf")
    if (mention1.phrase.head.string == mention2.phrase.head.string)
      addMergeableFeature("bMc")
    else addMergeableFeature("bMcf")
    if (mention1.phrase.last.string == mention2.phrase.last.string)
      addMergeableFeature("eMc")
    else  addMergeableFeature("eMcf")
    if (m1.isPRO) addMergeableFeature("pa") else addMergeableFeature("paf")

    val binTokenSentenceDistances = false
    val sdist = bin(mention1.phrase.sentence.indexInSection - mention2.phrase.sentence.indexInSection, 1 to 10)
    if (binTokenSentenceDistances) for (sd <- 1 to sdist) addMergeableFeature("sd" + sd)
    else addMergeableFeature("sd" + sdist)
    val tdist = bin(mention1.phrase.start - mention2.phrase.start, Seq(1, 2, 3, 4, 5, 10, 20, 50, 100, 200))
    if (binTokenSentenceDistances) for (td <- 1 to tdist) addMergeableFeature("td" + td)
    else addMergeableFeature("td" + tdist)
    if (m1.demonym != "" && m1.demonym == m2.demonym) addMergeableFeature("dM") else addMergeableFeature("dMf")
    addMergeableFeature("cap" + m1.capitalization +"_" +  m2.capitalization)
    addMergeableFeature("hpos" + mention2.phrase.headToken.posTag.value + "_" + mention1.phrase.headToken.posTag.value)
    addMergeableFeature("am" + acronymMatch)
    basicFeatureCalculated = true
  }
}

class MentionPairLabel(val model: PairwiseCorefModel, val mention1:Mention, val mention2:Mention, mentions: Seq[Mention], val initialValue: Boolean, options: Coref1Options) extends LabeledCategoricalVariable(if (initialValue) "YES" else "NO")  {
  def domain = model.MentionPairLabelDomain
  def features:MentionPairFeatures = new MentionPairFeatures(model, mention1, mention2, mentions, options)
  //lazy val features =
}




object LexicalCounter {
  // Used for definiteness ablations
  val dets1 = Set("the", "a", "an")
  val dets2 = Set("some", "all", "more", "no", "one", "two", "three", "any", "other", "many", "such", "both")
  val dems = Set("this", "that", "these", "those")
  val poss = Set("his", "their", "its", "our", "her", "my", "your")
  val mods = Set("new", "last", "former", "public", "political", "vice");
  val nats = Set ("china", "u.s.", "foreign", "taiwan", "israeli", "national", "american", "palestinian", "chinese", "federal", "japan")
  val roles = Set("mr.", "reporter", "president")
  val defaultCutoff = 20

  def countWordTypes(nonPronouns: Seq[MentionCharacteristics],specificWordFunc: (MentionCharacteristics) => String, cutoff:Int = defaultCutoff): DefaultHashMap[String,Int] = {
    countAndPrune(nonPronouns.map(specificWordFunc),cutoff)
  }

  def countLexicalItems(mentionList:Seq[Mention],trainDocs:Seq[Document], cutoff: Int):LexicalCounter = {
    val nonPronouns = mentionList.filter(!_.attr[MentionCharacteristics].isPRO)
    // HEAD WORDS
    val allHeadWordsInTrain = nonPronouns.map(_.attr[MentionCharacteristics].lowerCaseHead)
    val headWordCounts = countAndPrune(allHeadWordsInTrain, cutoff)
    // FIRST WORDS
    val allFirstWordsInTrain = nonPronouns.flatMap(mention => {
      val words = mention.phrase.tokens
      if (words.size > 1) Seq[String](words(0).string.toLowerCase) else Seq[String]()
    })
    val firstWordCounts = countAndPrune(allFirstWordsInTrain, cutoff)

    // LAST WORDS
    val allLastWordsInTrain = nonPronouns.flatMap(mention => {
      val words = mention.phrase.tokens
      if (words.size > 1 && mention.phrase.last.position - 1 != mention.phrase.start) Seq[String](words(words.size - 1).string.toLowerCase) else Seq[String]()
    })
    val lastWordCounts = countAndPrune(allLastWordsInTrain, cutoff)
    // PENULTIMATE WORDS
    val allPenultimateWordsInTrain = nonPronouns.flatMap(mention => {
      val words = mention.phrase.tokens
      if (words.size > 2) Seq[String](words(words.size - 2).string.toLowerCase) else Seq[String]()
    })
    val penultimateWordCounts = countAndPrune(allPenultimateWordsInTrain, cutoff)
    // SECOND WORDS
    val allSecondWordsInTrain = nonPronouns.flatMap(mention => {
      val words = mention.phrase.tokens
      if (words.size > 3) Seq[String](words(1).string.toLowerCase) else Seq[String]()
    })
    val secondWordCounts = countAndPrune(allSecondWordsInTrain, cutoff)
    // PRECEDING WORDS
    val allPrecedingWordsInTrain = mentionList.map(m => getTokenAtOffset(m.phrase.tokens(0),-1).toLowerCase)
    val precedingWordCounts = countAndPrune(allPrecedingWordsInTrain, cutoff)
    // FOLLOWING WORDS
    val allFollowingWordsInTrain = mentionList.map(mention => getTokenAtOffset(mention.phrase.last,+1).toLowerCase)
    val followingWordCounts = countAndPrune(allFollowingWordsInTrain, cutoff)
    // PRECEDING BY 2 WORDS
    val allPrecedingBy2WordsInTrain = mentionList.map(m=> getTokenAtOffset(m.phrase.tokens(0),-2).toLowerCase)
    val precedingBy2WordCounts = countAndPrune(allPrecedingBy2WordsInTrain, cutoff)
    // FOLLOWING BY 2 WORDS
    val allFollowingBy2WordsInTrain = mentionList.map(mention => getTokenAtOffset(mention.phrase.last,+1).toLowerCase)
    val followingBy2WordCounts = countAndPrune(allFollowingBy2WordsInTrain, cutoff)
    // GOVERNOR WORDS
    //val allGovernorWordsInTrain = mentionList.map(mention => mention.governor.toLowerCase)
    //val governorWordCounts = countAndPrune(allGovernorWordsInTrain, cutoff);

    // PREFIXES AND SUFFIXES
    val allPrefixCounts = new DefaultHashMap[String,Int](0)
    val allSuffixCounts = new DefaultHashMap[String,Int](0)
    val allShapeCounts = new DefaultHashMap[String,Int](0)
    val allClassCounts = new DefaultHashMap[String,Int](0)

    for (trainDoc <- trainDocs; sentence <- trainDoc.sentences; word <- sentence) {
      val text = word.string
      allShapeCounts(cc.factorie.app.strings.stringShape(text,2)) += 1//incrementCount(NerExample.shapeFor(word), 1.0)
      allClassCounts(classFor(word))+=1
      if (text.size >= 1) {
        allPrefixCounts(text.substring(0, 1))+=1
        allSuffixCounts(text.substring(text.length - 1))+=1
      }
      if (text.size >= 2) {
        allPrefixCounts(text.substring(0, 2)) += 1
        allSuffixCounts(text.substring(text.size - 2))+=1
      }
      if (text.size >= 3) {
        allPrefixCounts(text.substring(0, 2)) += 1
        allSuffixCounts(text.substring(text.size - 2))+=1
      }
    }
    prune(allPrefixCounts,cutoff)
    prune(allSuffixCounts,cutoff)
    prune(allShapeCounts,cutoff)
    prune(allClassCounts,cutoff)
    //println(allPrefixCounts.size + " prefixes: top 100 = " + GUtil.getTopNKeysSubCounter(allPrefixCounts, 100).toString)
    //Logger.logss(allSuffixCounts.size + " suffixes: top 100 = " + GUtil.getTopNKeysSubCounter(allSuffixCounts, 100).toString)
    //Logger.logss(allShapeCounts.size + " shapes: top 100 = " + GUtil.getTopNKeysSubCounter(allShapeCounts, 100).toString)
    //Logger.logss(allClassCounts.size + " classes: top 100 = " + GUtil.getTopNKeysSubCounter(allClassCounts, 100).toString)
    new LexicalCounter(headWordCounts, firstWordCounts, lastWordCounts, /*penultimateWordCounts, secondWordCounts*/ precedingWordCounts, followingWordCounts, /*precedingBy2WordCounts, followingBy2WordCounts,  allPrefixCounts, allSuffixCounts, */allShapeCounts,allClassCounts)
  }
  private def countAndPrune(words: Seq[String], cutoff: Int): DefaultHashMap[String,Int] = {
    val counts = new DefaultHashMap[String,Int](0)
    words.foreach(key=>counts(key) += 1)
    counts.foreach{case (key,value) => if(value < cutoff) counts.remove(key)}
    counts
  }

  private def prune(counts: DefaultHashMap[String,Int],cutoff:Int):DefaultHashMap[String,Int]={
    counts.foreach{case (key,value) => if(value < cutoff) counts.remove(key)}
    counts
  }

  def getTokenAtOffset(token: Token,offset:Int): String = { val t = token.next(offset); "W@"+offset+(if (t ne null) t.string else null) }

  def classFor(word:Token):String = {
    val sb = new StringBuilder
    val wlen = word.string.length()
    val numCaps = (word.string: Seq[Char]).count(_.isUpper)
    val hasDigit = word.containsDigit
    val hasDash = word.string.contains('-')
    val hasLower = numCaps < wlen
    val ch0 = word.string(0)
    val lowered = word.string.toLowerCase
    if (Character.isUpperCase(ch0)|| Character.isTitleCase(ch0)) {
      if (numCaps == 1) {
        sb.append("-INITC")
      } else {
        sb.append("-CAPS")
      }
    } else if (!Character.isLetter(ch0) && numCaps > 0) {
      sb.append("-CAPS")
    } else if (hasLower) {
      sb.append("-LC")
    }

    if (hasDigit) {
      sb.append("-NUM")
    }
    if (hasDash) {
      sb.append("-DASH")
    }
    if (lowered.endsWith("s") && wlen >= 3) {
      // here length 3, so you don't miss out on ones like 80s
      val ch2 = lowered.charAt(wlen - 2)
      // not -ess suffixes or greek/latin -us, -is
      if (ch2 != 's' && ch2 != 'i' && ch2 != 'u') {
        sb.append("-s")
      }
    } else if (word.string.length() >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
      if (lowered.endsWith("ed")) {
        sb.append("-ed")
      } else if (lowered.endsWith("ing")) {
        sb.append("-ing")
      } else if (lowered.endsWith("ion")) {
        sb.append("-ion")
      } else if (lowered.endsWith("er")) {
        sb.append("-er")
      } else if (lowered.endsWith("est")) {
        sb.append("-est")
      } else if (lowered.endsWith("ly")) {
        sb.append("-ly")
      } else if (lowered.endsWith("ity")) {
        sb.append("-ity")
      } else if (lowered.endsWith("y")) {
        sb.append("-y")
      } else if (lowered.endsWith("al")) {
        sb.append("-al")
      }
    }
    sb.toString()
  }

}
class LexicalCounter(val headWordCounts: DefaultHashMap[String,Int],
                     val firstWordCounts: DefaultHashMap[String,Int] = null,
                     val lastWordCounts: DefaultHashMap[String,Int] = null,
                     //val penultimateWordCounts: DefaultHashMap[String,Int],
                     //val secondWordCounts: DefaultHashMap[String,Int],
                     val precedingWordCounts: DefaultHashMap[String,Int] = null,
                     val followingWordCounts: DefaultHashMap[String,Int] = null,
                     //val precedingBy2WordCounts: DefaultHashMap[String,Int],
                     //val followingBy2WordCounts: DefaultHashMap[String,Int],
                     //val commonGovernorWordCounts: DefaultHashMap[String,Int],
                     //val prefixCounts: DefaultHashMap[String,Int],
                     //val suffixCounts: DefaultHashMap[String,Int],
                     val shapeCounts: DefaultHashMap[String,Int] = null,
                     val classCounts: DefaultHashMap[String,Int] = null
                      ) extends Serializable {
}

class DefaultHashMap[String,Int](val defaultValue: Int) extends mutable.HashMap[String,Int] {
  override def default(key:String) = defaultValue
}

