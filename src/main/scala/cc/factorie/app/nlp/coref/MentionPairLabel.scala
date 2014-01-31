package cc.factorie.app.nlp.coref

import cc.factorie.la.{Tensor1, SparseTensor, GrowableSparseBinaryTensor1}
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable}
import cc.factorie.app.nlp.coref.mention.{CorefFeatures, CorefMention}

/**
 * User: apassos
 * Date: 6/27/13
 * Time: 12:23 PM
 */

//here, mention1 is the mention to the right
class MentionPairFeatures(val model: PairwiseCorefModel, val mention1: CorefMention, val mention2: CorefMention, mentions: Seq[CorefMention], options: Coref1Options) extends BinaryFeatureVectorVariable[String] {
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
  def mentType(ment: CorefMention): String = if(ment.isPRO) "pro" else "non"

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
  def computeFeatures() { computeBasicFeatures(); computeConjunctionFeatures() }

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
    val e1 = if (mention2.isPRO) mention2.headToken.string else mention2.predictEntityType
    val e2 = if (mention1.isPRO) mention1.headToken.string else mention1.predictEntityType
    e1 + "&&" + e2
  }
  def entityTypeMatch: String = {
    val t1 = mention2.predictEntityType
    val t2 = mention1.predictEntityType
    if (t1.equals("unknown") || t2.equals("unknown"))
      "unknown"
    else if (t1.equals(t2))
      "true"
    else
      "false"
  }

  def acronymMatch: Char = {
    if (mention1.span.length == 1 && mention2.span.length > 1) {
      if (mention2.acronym.contains(mention1.span.phrase.trim.toLowerCase)) 't' else 'f'
    } else if (mention1.span.length > 1 && mention2.span.length == 1) {
      if (mention1.acronym.contains(mention2.span.phrase.trim.toLowerCase)) 't' else 'f'
    } else
      'u'
  }


  def computeBasicFeatures() {
    if (basicFeatureCalculated) return

    addMergeableFeature("BIAS")
    addMergeableFeature("gmc" + mention1.gender + "" +  mention2.gender)
    addMergeableFeature("nms" + mention1.number + "" + mention2.number)
    if (mention1.nonDeterminerWords == mention2.nonDeterminerWords)
      addMergeableFeature("hms")
    else addMergeableFeature("hmsf")
    addMergeableFeature("mt1" + mention1.mType) // although mention1 is the anaphor and mention2 is the antecedent, to match Roth's implementation, m1MentionType returns the type of mention2
    addFeature("mt2" + mention2.mType)
    if (!mention1.nounWords.intersect(mention2.nounWords).isEmpty)
      addMergeableFeature("pmhm")
    else addMergeableFeature("pmhmf")
    if (mention1.lowerCaseHead.contains(mention2.lowerCaseHead) || mention2.lowerCaseHead.contains(mention1.lowerCaseHead))
      addMergeableFeature("sh")
    else addMergeableFeature("shf")
    if (CorefFeatures.canBeAliases(mention1, mention2)) addMergeableFeature("sapetc") else addMergeableFeature("soonAliasPredETypeCached:false")
    if (mention1.wnSynsets.exists(mention2.wnSynsets.contains))
      addMergeableFeature("asyn")
    else addMergeableFeature("asynf")
    if (mention1.wnSynsets.exists(mention2.wnAntonyms.contains))
      addMergeableFeature("aan")
    else addMergeableFeature("aanf")
    if (mention1.wnSynsets.exists(mention2.wnHypernyms.contains) || mention2.wnSynsets.exists(mention1.wnHypernyms.contains))
      addMergeableFeature("ahyp")
    else addMergeableFeature("ahypf")
    if (mention1.wnHypernyms.exists(mention2.wnHypernyms.contains)) addMergeableFeature("hsh")
    else addMergeableFeature("hshf")
    if (CorefFeatures.areAppositive(mention1, mention2))
      addMergeableFeature("aA")
    else addMergeableFeature("aAf")
    if (mention1.hasSpeakWord && mention2.hasSpeakWord)
      addMergeableFeature("bs")
    else addMergeableFeature("bsf")
    if (CorefFeatures.areRelative(mention1, mention2))
      addMergeableFeature("rpf")
    else addMergeableFeature("rpff")
    for (cm <- CorefFeatures.countCompatibleMentionsBetween(mention1, mention2, mentions)) addMergeableFeature("cmc" + cm)
    addMergeableFeature("mtpw" + (if (mention2.isPRO) mention2.mType + mention1.headToken.string else mention2.mType + mention1.mType))
    addMergeableFeature("pwhe" + proWordHead)
    addMergeableFeature("etm" + entityTypeMatch)
    addMergeableFeature("lhp" + CorefFeatures.headWordsCross(mention1, mention2, model))
    if (mention1.span.sentence == mention2.span.sentence) addMergeableFeature("ss") // false values of this feature are not included in Roth's system
    CorefFeatures.matchingTokensRelations(mention1, mention2).foreach(r => addMergeableFeature("apr" + r))

    if (mention1.span.head.string.toLowerCase == mention2.span.head.string.toLowerCase)
      addMergeableFeature("bM")
    else addMergeableFeature("bMf")
    if (mention1.span.last.string.toLowerCase == mention2.span.last.string.toLowerCase)
      addMergeableFeature("eM")
    else addMergeableFeature("eMf")
    if (mention1.span.head.string == mention2.span.head.string)
      addMergeableFeature("bMc")
    else addMergeableFeature("bMcf")
    if (mention1.span.last.string == mention2.span.last.string)
      addMergeableFeature("eMc")
    else  addMergeableFeature("eMcf")
    if (mention1.isPRO)
      addMergeableFeature("pa")
    else  addMergeableFeature("paf")

    val binTokenSentenceDistances = false
    val sdist = bin(mention1.sentenceNum - mention2.sentenceNum, 1 to 10)
    if (binTokenSentenceDistances) for (sd <- 1 to sdist) addMergeableFeature("sd" + sd)
    else addMergeableFeature("sd" + sdist)
    val tdist = bin(mention1.tokenNum - mention2.tokenNum, Seq(1, 2, 3, 4, 5, 10, 20, 50, 100, 200))
    if (binTokenSentenceDistances) for (td <- 1 to tdist) addMergeableFeature("td" + td)
    else addMergeableFeature("td" + tdist)
    if (mention1.demonym != "" && mention1.demonym == mention2.demonym) addMergeableFeature("dM") else addMergeableFeature("dMf")
    addMergeableFeature("cap" + mention1.capitalization +"_" +  mention2.capitalization)
    addMergeableFeature("hpos" + mention2.headToken.posTag.value + "_" + mention1.headToken.posTag.value)
    addMergeableFeature("am" + acronymMatch)
    basicFeatureCalculated = true
  }
}

class MentionPairLabel(val model: PairwiseCorefModel, val mention1: CorefMention, val mention2: CorefMention, mentions: Seq[CorefMention], val initialValue: Boolean, options: Coref1Options) extends LabeledCategoricalVariable(if (initialValue) "YES" else "NO")  {
  def domain = model.MentionPairLabelDomain
  val features = new MentionPairFeatures(model, mention1, mention2, mentions, options)
}


