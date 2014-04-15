package cc.factorie.app.nlp.coref

import cc.factorie.la.{Tensor1, SparseTensor, GrowableSparseBinaryTensor1}
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable}
import cc.factorie.app.nlp.coref.mention.{CorefFeatures,MentionCharacteristics}

/** A binary feature vector for the features of a mention pair.
    Here, mention1 is the mention to the right.
    @author Alexandre Passos
 */
class MentionPairFeatures(val model: PairwiseCorefModel, val mention1:Mention, val mention2:Mention, mentions: Seq[Mention], options: Coref1Options) extends BinaryFeatureVectorVariable[String] {
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
  def mentType(mention:Mention): String = if (mention.attr[MentionCharacteristics].isPRO) "pro" else "non"

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
    if (t1.equals("unknown") || t2.equals("unknown"))
      "unknown"
    else if (t1.equals(t2))
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
    val m1 = mention1.attr[MentionCharacteristics]
    val m2 = mention2.attr[MentionCharacteristics]
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
    for (cm <- CorefFeatures.countCompatibleMentionsBetween(mention1, mention2, mentions)) addMergeableFeature("cmc" + cm)
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
  val features = new MentionPairFeatures(model, mention1, mention2, mentions, options)
}


