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

import cc.factorie.la.{SparseTensor, GrowableSparseBinaryTensor1}
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable}
import scala.collection.mutable
import cc.factorie.app.nlp.{Token}

/** Contains two possible mention sets:
 *    Lexical & Conventional
 *      Conventional - String Match, Gender Cross, Head word / entity Type etc
 *      Lexical - Anaphoricity Detection if mention1 == mention2, else Lexical features for the pair
 *  A binary feature vector for the features of a mention pair.
    Here, mention1 is the mention to the right. */
class MentionPairFeatures(val model: CorefModel, val mention1:Mention, val mention2:Mention, mentions: Seq[Mention], options: CorefOptions) extends BinaryFeatureVectorVariable[String] {
  {
    val t = new GrowableSparseBinaryTensor1(domain.dimensionDomain)
    val sizeBoundary = if (options.featureSet == "conventional"){
      if (options.conjunctionStyle == ConjunctionOptions.SLOW_CONJUNCTIONS) 650
      else 70 //Count of features needed plus any neighbor merges
    } else{
      if (options.conjunctionStyle == ConjunctionOptions.PRON_CONJUNCTIONS) 40
      else 16
    }
    t.sizeHint(sizeBoundary)
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

  computeFeatures()

  def computeFeatures() {
    if(options.featureSet == "lexical")
      computeLexicalFeatures()
    else computeConventionalFeatures()
  }

  def addFeature(f: String) {
    if(options.trainSeparatePronounWeights){
      features += pfx + "-" +  f
    }else features += f
  }


  def addFeatureWithPronConjunction(featLabel: String) {
    addFeature(featLabel)
    addFeature(featLabel + "C=" + mention1.attr[MentionCharacteristics].canonicalizedPronounOrType)
    if (mention1 != mention2) addFeature("P=" + mention2.attr[MentionCharacteristics].canonicalizedPronounOrType)
  }

  def addMergeableFeature(f: String) {
    if (options.mergeFeaturesAtAll) {
      assert(mergeableFeatures ne null)
      assert(f ne null)
      mergeableFeatures += f
    }
    addFeature(f)
  }

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

  lazy val mergeableAllFeatures = mergeableFeatures

  def computeConventionalFeatures() {
    val m1 = if(mention1.attr[MentionCharacteristics] eq null){ mention1.attr += new MentionCharacteristics(mention1); mention1.attr[MentionCharacteristics]} else mention1.attr[MentionCharacteristics]
    val m2 = if(mention2.attr[MentionCharacteristics] eq null){ mention2.attr += new MentionCharacteristics(mention2); mention2.attr[MentionCharacteristics]} else mention2.attr[MentionCharacteristics]
    if (basicFeatureCalculated) return

    addMergeableFeature("BIAS")
    addMergeableFeature("gmc" + m1.gender + "" +  m2.gender)
    addMergeableFeature("nms" + m1.number + "" + m2.number)
    if (m1.nonDeterminerWords == m2.nonDeterminerWords)
      addMergeableFeature("hms")
    else addMergeableFeature("hmsf")
    addMergeableFeature("mt1" + m1.headPos)
    addFeature("mt2" + m2.headPos)
    if (!m1.nounWords.intersect(m2.nounWords).isEmpty)
      addMergeableFeature("pmhm")
    else addMergeableFeature("pmhmf")
    if (m1.lowerCaseString.contains(m2.lowerCaseString) || m2.lowerCaseString.contains(m1.lowerCaseString))
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
    addMergeableFeature("pwhe" + CorefFeatures.proWordHead(mention1,mention2))
    addMergeableFeature("etm" + CorefFeatures.entityTypeMatch(mention1,mention2))
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
    addMergeableFeature("am" + CorefFeatures.acronymMatch(mention1,mention2))
    basicFeatureCalculated = true
  }

  def computeLexicalFeatures(): Unit = {
    val m1 = if(mention1.attr[MentionCharacteristics] eq null){ mention1.attr += new MentionCharacteristics(mention1); mention1.attr[MentionCharacteristics]} else mention1.attr[MentionCharacteristics]
    val m2 = if(mention2.attr[MentionCharacteristics] eq null){ mention2.attr += new MentionCharacteristics(mention2); mention2.attr[MentionCharacteristics]} else mention2.attr[MentionCharacteristics]
    if (basicFeatureCalculated) return

    features += "Bias" //+ currMention.mType
    val newEntity = mention1 == mention2
    addFeatureWithPronConjunction("Len=" + mention1.phrase.tokens.size + "|NE=" + newEntity)
    val counts = model.CorefTokenFrequencies.counter
    addFeatureWithPronConjunction("HdWd=" + returnWord(mention1.phrase.headToken,counts,counts.headWords) + "|NE=" + newEntity)
    addFeatureWithPronConjunction("First=" + returnWord(mention1.phrase(0),counts,counts.firstWords) + "|NE=" + newEntity)
    addFeatureWithPronConjunction("Last=" + returnWord(mention1.phrase.tokens.last,counts,counts.lastWords) + "|NE=" + newEntity)
    addFeatureWithPronConjunction("Class=" + returnWordForm(mention1.phrase(0),counts) + "|NE=" +newEntity)
    addFeature("Pos=" + mention1.phrase.sentence.indexInSection + "|NE=" + newEntity)
    if(!newEntity){
      features += "PrevLen=" + mention2.phrase.tokens.size
      addFeatureWithPronConjunction("PrevHead=" + returnWord(mention2.phrase.headToken,counts,counts.headWords))
      addFeatureWithPronConjunction("PrevHeadShape=" + returnShape(mention2.phrase.headToken,counts))
      addFeatureWithPronConjunction("PrevFirst=" + returnWord(mention2.phrase(0),counts,counts.firstWords))
      addFeatureWithPronConjunction("PrevLast=" + returnWord(mention2.phrase.last,counts,counts.lastWords))
      addFeatureWithPronConjunction("PrevPrec=" + returnWord(TokenFreqs.getTokenAtOffset(mention2.phrase(0),-1),counts,counts.precContext))
      addFeatureWithPronConjunction("PrevFollow=" + returnWord(TokenFreqs.getTokenAtOffset(mention2.phrase.last,+1),counts,counts.followContext))

      //Pair Features
      var dist = mention1.phrase.sentence.indexInSection - mention2.phrase.sentence.indexInSection
      if(dist <10)  addFeature("sent_dist=" + dist.toString)
      dist = mention1.phrase.start - mention2.phrase.start
      if(dist < 10)  addFeature("mention_dist=" + dist.toString)

      if(m1.lowerCaseString == m2.lowerCaseString) addFeature("String_Match")
      else addFeature("No_String_Match")
      if(m1.lowerCaseHead == m2.lowerCaseHead) addFeature("Head_Match")
      else addFeature("No_Head_Match")

      addFeature("curr-type" + m1.predictEntityType + "|link-type" + m2.predictEntityType)
      addFeature("gmc1" + m1.genderIndex + "|gmc2"+m2.genderIndex)
    }
  }
  private def returnWord(token: Token, counter: TopTokenFrequencies, category: DefaultHashMap[String,Int]): String = if(token == null) "NA" else counter.containsToken(category,token)
  private def returnShape(token: Token, counter: TopTokenFrequencies): String =  counter.containsString(counter.shapes,cc.factorie.app.strings.stringShape(token.string, 2))
  private def returnWordForm(token: Token,counter: TopTokenFrequencies): String = counter.containsString(counter.wordForm,TokenFreqs.getWordClass(token))
}

class MentionPairLabel(val model: PairwiseCorefModel, val mention1:Mention, val mention2:Mention, mentions: Seq[Mention], val initialValue: Boolean, options: CorefOptions) extends LabeledCategoricalVariable(if (initialValue) "YES" else "NO")  {
  def domain = model.MentionPairLabelDomain
  def genFeatures():MentionPairFeatures = new MentionPairFeatures(model, mention1, mention2, mentions, options)
}


class TopTokenFrequencies(val headWords: DefaultHashMap[String,Int],
                          val firstWords: DefaultHashMap[String,Int] = null,
                          val lastWords: DefaultHashMap[String,Int] = null,
                          val precContext: DefaultHashMap[String,Int] = null,
                          val followContext: DefaultHashMap[String,Int] = null,
                          val shapes: DefaultHashMap[String,Int] = null,
                          val wordForm: DefaultHashMap[String,Int] = null, default: Int = 20) {
  def this(nonPronouns: Seq[Mention],typesOfCounts: Seq[String], default:Int) = this(
    if(typesOfCounts.contains("Head")) TokenFreqs.countWordTypes(nonPronouns,(t) => t.phrase.headToken.string.toLowerCase,default) else null,
    if(typesOfCounts.contains("First")) TokenFreqs.countWordTypes(nonPronouns,(t) => t.phrase.tokens(0).string.toLowerCase,default)else null,
    if(typesOfCounts.contains("Last")) TokenFreqs.countWordTypes(nonPronouns,(t) => t.phrase.last.string.toLowerCase,default)else null,
    if(typesOfCounts.contains("Prec")) TokenFreqs.countWordTypes(nonPronouns,(t) => TokenFreqs.getTokenStringAtOffset(t.phrase.tokens(0),-1).toLowerCase,default)else null,
    if(typesOfCounts.contains("Follow")) TokenFreqs.countWordTypes(nonPronouns,(t) => TokenFreqs.getTokenStringAtOffset(t.phrase.last,1).toLowerCase,default)else null,
    if(typesOfCounts.contains("Shape")) TokenFreqs.countWordTypes(nonPronouns,(t) => cc.factorie.app.strings.stringShape(t.phrase.string,2),default)else null,
    if(typesOfCounts.contains("WordForm")) TokenFreqs.countWordTypes(nonPronouns,(t) => TokenFreqs.getWordClass(t.phrase.headToken),default)else null)


  //If this token is not a top token, fall back on using pos tag
  def containsToken(lexicon: DefaultHashMap[String,Int],token: Token): String = {
    if(lexicon.contains(token.string.toLowerCase)) token.string.toLowerCase
    else token.posTag.categoryValue
  }

  def containsString(lexicon: DefaultHashMap[String,Int],tokenString: String): String = if(lexicon.contains(tokenString)) tokenString else ""
}


object TokenFreqs{
  def countWordTypes(nonPronouns: Seq[Mention],specificWordFunc: (Mention) => String, cutoff: Int): DefaultHashMap[String,Int] = {
    countAndPrune(nonPronouns.map(specificWordFunc),cutoff)
  }

  private def countAndPrune(words: Seq[String], cutoff: Int): DefaultHashMap[String,Int] = {
    val counts = new DefaultHashMap[String,Int](0)
    words.foreach(key=>counts(key) += 1)
    counts.foreach{case (key,value) => if(value < cutoff) counts.remove(key)}
    counts
  }

  def getTokenAtOffset(token: Token,offset: Int): Token = { val t = token.next(offset); if (t ne null) t else null }
  def getTokenStringAtOffset(token: Token,offset: Int): String = { val t = token.next(offset); if (t ne null) t.string else ""}

  def getWordClass(word: Token):String = {
    val sb = new StringBuilder
    if (word.isCapitalized) {
      if (word.containsLowerCase) sb.append("Cap-Mix")
      else sb.append("Cap")
    }
    if (word.isDigits) sb.append("Num")
    else if (word.containsDigit) sb.append("Num-Mix")
    if (word.string.contains('-')) sb.append("Dash")
    if (word.string.contains('s') && word.string.length() >= 3) sb.append("-S")
    else if (word.string.length() >= 5){
      val lowerCase = word.string.toLowerCase
      if (lowerCase.endsWith("ed")) sb.append("-ed")
      else if (lowerCase.endsWith("ing")) sb.append("-ing")
      else if (lowerCase.endsWith("ion")) sb.append("-ion")
      else if (lowerCase.endsWith("er")) sb.append("-er")
      else if (lowerCase.endsWith("est")) sb.append("-est")
      else if (lowerCase.endsWith("ly")) sb.append("-ly")
      else if (lowerCase.endsWith("ity")) sb.append("-ity")
      else if (lowerCase.endsWith("y")) sb.append("-y")
      else sb.append("-none")
    }
    sb.toString()
  }
}

class DefaultHashMap[String,Int](val defaultValue: Int) extends mutable.HashMap[String,Int] {
  override def default(key:String) = defaultValue
}

