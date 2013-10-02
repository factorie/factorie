package cc.factorie.app.nlp.relation

import cc.factorie._
import app.nlp.hcoref.PairwiseMention
import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.load.{ACEMentionSpan,ACEMentionSpanList,ACEMentionIdentifiers}
import collection.mutable.{HashMap, ArrayBuffer}
import cc.factorie.util.Attr
import cc.factorie.variable._
import scala.Some

/**
 * @author sameer, brian, sebastian
 */

object RelationVariables {
  var fewFeatures: Boolean = false

  val NoneLabel = "NONE"

  class RelationMentions extends SetVariable[RelationMention]

  object RelationLabelDomain extends CategoricalDomain[String] {
    lazy val None = value(NoneLabel)
  }

  RelationLabelDomain += NoneLabel

  class RelationLabel(labelStr: String, val mention: RelationMention) extends LabeledCategoricalVariable(labelStr) {
    def domain = RelationLabelDomain
  }

  class RelationMention(val arg1: PairwiseMention, val arg2: PairwiseMention, val relationType: String, val relationSubType: Option[String]) extends ArrowVariable(arg1, arg2) with Attr {
    val arg1Features = new ArgFeatures(arg1, true)
    val arg2Features = new ArgFeatures(arg2, false)
    val features = new Features(this)

    def computeFeatures() = {
      //arg1Features.compute
      //arg2Features.compute
      features.compute
      FeatureNormalizer += features
    }
  }

  object RelationArgFeaturesDomain extends CategoricalDomain[String]

  class ArgFeatures(val arg: PairwiseMention, val first: Boolean) extends BinaryFeatureVectorVariable[String] {
    def domain = RelationArgFeaturesDomain

    def compute() = {
      this += "BIAS"
      // TODO compute relation features using "first" and "arg"
      // TODO convert Lexicons (from refectorie.proj.jntinf) to app.chain.Lexicon
      for (tok <- arg.tokens) {
        this += "POS_" + tok.posLabel.categoryValue
        if (tok.string(0).isLower)
          this += "STEM_" + tok.string.replaceAll("\\s+", " ").take(5)
        //for (lex <- Lexicons.getMemberships(tok.string.replaceAll("\\s+", " ").toLowerCase))
        //  this += "TOK-LEX-" + lex
      }
//      for (lex <- Lexicons.getMemberships(arg.phrase.replaceAll("\\s+", " ").toLowerCase))
//        this += "PHRASE-LEX-" + lex
      this += "HEAD_POS_" + arg.headToken.posLabel.categoryValue
//      for (lex <- Lexicons.getMemberships(arg.headToken.string.replaceAll("\\s+", " ").toLowerCase))
//        this += "HEAD_LEX-" + lex
    }
  }

  object RelationFeaturesDomain extends CategoricalDomain[String]

  case class PathEdge(from: Token, to: Token, label: String, headToModifier: Boolean) {
    def directedLabel = if (headToModifier) "-" + label + "->" else "<-" + label + "-"
  }

  def shortestPath(from: Token, to: Token, visited: Set[Token] = Set.empty): Option[Seq[PathEdge]] = {
    if (from == to) Some(Seq.empty)
    else {
      val paths = new ArrayBuffer[Seq[PathEdge]]
      try {
        val parent = from.parseParent
        if (!visited(parent)) {
          for (path <- shortestPath(parent, to, visited + from)) {
            val edge = PathEdge(from, parent, from.parseLabel.categoryValue, false)
            paths += edge +: path
          }
        }
      } catch {
        case _: Throwable =>
      }
      for (child <- from.parseChildren; if !visited(child)) {
        for (path <- shortestPath(child, to, visited + from)) {
          val edge = PathEdge(from, child, child.parseLabel.categoryValue, true)
          paths += edge +: path
        }
      }
      if (paths.isEmpty) None else Some(paths.minBy(_.size))
    }
  }

  def bin(value: Int, bins: Seq[Int]) = {
    math.signum(value) * (bins :+ Int.MaxValue).indexWhere(_ > math.abs(value))
  }

  def normalizedWord(token: Token) = {
    if (token.string(0).isUpper) "UPPER" else token.string
  }

  object FeatureNormalizer extends FeatureVectorVariable[String] {
    def domain = RelationFeaturesDomain

    def +=(feats: FeatureVectorVariable[String]) =
      for (i <- feats.value.activeDomain.toSeq)
        this.value.+=(i, feats.value(i)) // TODO Use Tensor.+= to make this much faster

    def normalize(feats: FeatureVectorVariable[String]) =
      for (i <- feats.value.activeDomain.toSeq)
        feats.value.update(i, feats.value(i) / value(i)) // TODO Use Tensor./= to make this much faster
  }

  class Features(val mention: RelationMention) extends FeatureVectorVariable[String] {
    def domain = RelationFeaturesDomain

    def addTokenFeatures(tok: Token, prefix: String) {
      if (tok.string(0).isLower) {
        //this += prefix + "W_" + tok.string.replaceAll("\\s+", " ").toLowerCase
        //this += prefix + "STEM_" + tok.string.replaceAll("\\s+", " ").toLowerCase.take(5)
      }
      this += prefix + "POS_" + tok.posLabel.categoryValue
    }

    override def +=(elt: String) = update(domain.index(elt), 1.0)(null)


    def tokenIsPossessive(token: Token) = {
      token.posLabel.categoryValue == "PRP$" || token.string.endsWith("'s")
    }

    def null2opt[T](t: T) = if (t == null) None else Some(t)

    def clean(string: String) = string.replaceAll("\\s+", " ")

    def computeZHOUFeatures() {
      val arg1 = mention.arg1
      val arg2 = mention.arg2
      val m1 = arg1.tokens.slice(0, arg1.headToken.position - arg1.start + 1)
      val m2 = arg2.tokens.slice(0, arg2.headToken.position - arg2.start + 1)
      val sentence = arg1.sentence
      val (left, right, forward) = if (arg1.headToken.position < arg2.headToken.position) (m1, m2, true) else (m2, m1, false)
      val inBetweenMentions = sentence.tokens.slice(left.last.positionInSentence, right.head.positionInSentence)

      if (left.head.sentence != right.last.sentence) {
        this += "IN DIFFERENT SENTENCES!"
        return
      }

      //Words
      this += "HM1" + arg1.headToken.string.replaceAll("\\s+", " ")
      this += "HM2" + arg2.headToken.string.replaceAll("\\s+", " ")
      if (!fewFeatures)
        this += "HM12 " + arg1.headToken.string.replaceAll("\\s+", " ") + "-" + arg2.headToken.string.replaceAll("\\s+", " ")
      if (inBetweenMentions.size == 0) this += "WBNULL"
      if (!fewFeatures) {
        if (inBetweenMentions.size == 1) this += "WBFL " + inBetweenMentions.head.string.replaceAll("\\s+", " ")
        if (inBetweenMentions.size > 1) {
          this += "WBF " + inBetweenMentions.head.string.replaceAll("\\s+", " ") + "-" + forward
          this += "WBL " + inBetweenMentions.last.string.replaceAll("\\s+", " ") + "-" + forward
        }
        if (inBetweenMentions.size > 2) {
          for (t <- inBetweenMentions) {
            this += "WBO " + t.string
          }
        }
      }
      this += "BMF1 " + null2opt(left.head.prev).map(_.string.replaceAll("\\s+", " ")) + "-" + forward
      this += "BML1 " + null2opt(left.head.prev).flatMap(t => null2opt(t.prev)).map(_.string.replaceAll("\\s+", " ")) + "-" + forward
      this += "BMF2 " + null2opt(left.head.next).map(_.string.replaceAll("\\s+", " ")) + "-" + forward
      this += "BML2 " + null2opt(left.head.next).flatMap(t => null2opt(t.next)).map(_.string.replaceAll("\\s+", " ")) + "-" + forward

      if (!fewFeatures) {
        for (t <- m1) {
          this += "WM1 " + t.string.replaceAll("\\s+", " ")
        }
        for (t <- m2) {
          this += "WM2 " + t.string.replaceAll("\\s+", " ")
        }
      }

      //mention level
      this += "ML12 " + arg1.attr[ACEMentionIdentifiers] + "-" + arg2.attr[ACEMentionIdentifiers]

      //overlap
      val mentionsInSentence = sentence.document.attr[ACEMentionSpanList].filter(_.sentence == sentence)
      val mentionsInBetween = mentionsInSentence.filter(m =>
        m.head.positionInSentence > left.last.positionInSentence && m.last.positionInSentence < right.head.positionInSentence)
      this += "#MB " + mentionsInBetween.size
      this += "#WB " + inBetweenMentions.size

      //todo: should this look at the restricted mentions m1 and m2?
      if (arg1.start <= arg2.start && arg1.end >= arg2.end || arg2.start <= arg1.start && arg2.end >= arg1.end) {
        val (outside, inside, arg1ContainsArg2) =
          if (arg1.start <= arg2.start && arg1.end >= arg2.end) (arg1, arg2, true) else (arg2, arg1, false)
        if (arg1ContainsArg2) {
          this += "M1 < M2"
          if (!fewFeatures)
            this += "M1 < M2 && HM12 " + arg1.headToken.string + "-" + arg2.headToken.string

        } else {
          this += "M2 < M1"
          if (!fewFeatures)
            this += "M2 < M1 && HM12 " + arg1.headToken.string + "-" + arg2.headToken.string
        }
        //todo conjoin with entity features.
      }
    }

    def compute() = computeZHOUFeatures

    def compute2() = {
      val m1 = mention.arg1
      val m2 = mention.arg2
      val sentence = m1.sentence
      val (left, right, forward) = if (m1.headToken.positionInSentence < m2.headToken.positionInSentence) (m1, m2, true) else (m2, m1, false)
      val inBetweenHeads = sentence.tokens.slice(left.headToken.positionInSentence + 1, if (right.head.positionInSentence > left.headToken.positionInSentence) right.head.positionInSentence else right.headToken.positionInSentence)
      val headDistance = inBetweenHeads.size

      this += "Bias " + forward + "-" + bin(headDistance, Seq(0, 5, 10, 50))

      if (headDistance < 3) {
        //features for nested arguments
        //"X of Y" etc.
        if (m1.start <= m2.start && m1.end >= m2.end || m2.start <= m1.start && m2.end >= m1.end) {
          val (outside, inside, arg1ContainsArg2) =
            if (m1.start <= m2.start && m1.end >= m2.end) (m1, m2, true) else (m2, m1, false)

          val inBetweenTags = inBetweenHeads.map(_.posLabel.categoryValue).mkString(" ")
          val config = "-" + arg1ContainsArg2 + "-" + forward
          val inBetweenTagsAndConfig = "BETW: " + inBetweenTags + config
          this += inBetweenTagsAndConfig
          this += inBetweenTagsAndConfig + " ARG1 POSS: " + tokenIsPossessive(m1.headToken)
          this += inBetweenTagsAndConfig + " ARG2 POSS: " + tokenIsPossessive(m2.headToken)
          this += inBetweenTagsAndConfig + " ARG1 TAG: " + m1.headToken.posLabel.categoryValue
          this += inBetweenTagsAndConfig + " ARG2 TAG: " + m2.headToken.posLabel.categoryValue
          this += inBetweenTagsAndConfig + " ARG1 WORD: " + m1.phrase.replaceAll("\n", " ")
          this += inBetweenTagsAndConfig + " ARG2 WORD: " + m2.phrase.replaceAll("\n", " ")
          this += inBetweenTagsAndConfig + " ARG TAGS: " + m1.headToken.posLabel.categoryValue + " " + m2.headToken.posLabel.categoryValue

        }
      } else {
      }
      //path features
      for (path <- shortestPath(m1.headToken, m2.headToken)) {
        if (path.size <= 3) {
          this += "PATH " + path.map(_.directedLabel).mkString(" ")
          //          this += "PATH LX " + path.map( edge => )
        }

      }


    }

    def compute1() = {
      val m1 = mention.arg1
      val m2 = mention.arg2
      val sentence = m1.sentence
      //val id1 = "%s %s %s".format(m1.document.name, m1.start, m1.end) //
      //val id2 = "%s %s %s".format(m2.document.name, m2.start, m2.end) //
      //this += "KILLER " + id1 + " " + id2
      //this += "TRUE_LABEL_" + mention.label.target.categoryValue
      this += "BIAS"
      val t1 = m1.attr[ACEMentionIdentifiers]
      val t2 = m2.attr[ACEMentionIdentifiers]
      this += "MTYPE-" + t1
      this += "MTYPE-" + t2
      this += "MTYPEs-%s-%s".format(t1, t2)
      if (m1.attr.contains[ACEMentionIdentifiers]) {
        val t1 = m1.attr[ACEMentionIdentifiers].ldcType
        val t2 = m2.attr[ACEMentionIdentifiers].ldcType
        this += "LDCTYPE-" + t1
        this += "LDCTYPE-" + t2
        this += "LDCTYPEs-%s-%s".format(t1, t2)
      }
      //if (edu.umass.cs.iesl.jntinf.coref.CorefGazetteers.usStates.contains(m1.string.trim.toLowerCase)) this += "M1-US-STATE"
      //if (obsolete.CorefGazetteers.usStates.contains(m2.string.trim.toLowerCase)) this += "M2-US-STATE"
      // TODO compute relation features using m1 and m2
      // Order of arguments / overlap
      if (m1.start <= m2.start && m1.end >= m2.end) {
        this += "ARG1_CONTAINS_ARG2"
        if (m1.start == m2.start) this += "ARG2_IS_LEFT_CHILD"
        if (m1.end == m2.end) this += "ARG2_IS_RIGHT_CHILD"
        // intermediate tokens
        /*
        for (tok <- sentence.tokens.slice(m1.start, m2.start)) {
          addTokenFeatures(tok, "NESTED_BEF_TOK_")
          addTokenFeatures(tok, "ARG2_NESTED_BEF_TOK_")
        }
        for (tok <- sentence.tokens.slice(m2.end, m1.end)) {
          addTokenFeatures(tok, "NESTED_AFT_TOK_")
          addTokenFeatures(tok, "ARG2_NESTED_AFT_TOK_")
        }
        */
      }
      if (m2.start <= m1.start && m2.end >= m1.end) {
        this += "ARG2_CONTAINS_ARG1"
        if (m2.start == m1.start) this += "ARG1_IS_LEFT_CHILD"
        if (m2.end == m1.end) this += "ARG1_IS_RIGHT_CHILD"
        // intermediate tokens
        /*
        for (tok <- sentence.tokens.slice(m2.start, m1.start)) {
          addTokenFeatures(tok, "NESTED_BEF_TOK_")
          addTokenFeatures(tok, "ARG1_NESTED_BEF_TOK_")
        }
        for (tok <- sentence.tokens.slice(m1.end, m2.end)) {
          addTokenFeatures(tok, "NESTED_AFT_TOK_")
          addTokenFeatures(tok, "ARG1_NESTED_AFT_TOK_")
        }
        */
      }
      if (m2.start >= m1.end) {
        this += "ARG1_IS_BEFORE"
        // intermediate tokens
        if (m2.start - m1.end - 1 < 3) {
          for (tok <- sentence.tokens.slice(m1.end, m2.start)) {
            addTokenFeatures(tok, "ARG1_IS_BEFORE_TOK_")
          }
        }
        /*
        for (tok <- sentence.tokens.slice(m1.end, m2.start)) {
          addTokenFeatures(tok, "ARG1_IS_BEFORE_TOK_")
          addTokenFeatures(tok, "NOT_NEST_TOK_")
        }
        */
      }
      if (m1.start >= m2.end) {
        this += "ARG2_IS_BEFORE"
        // intermediate tokens
        if (m1.start - m2.end - 1 < 3) {
          for (tok <- sentence.tokens.slice(m2.end, m1.start)) {
            addTokenFeatures(tok, "ARG2_IS_BEFORE_TOK_")
          }
        }
        /*
        for (tok <- sentence.tokens.slice(m2.end, m1.start)) {
          addTokenFeatures(tok, "ARG2_IS_BEFORE_TOK_")
          addTokenFeatures(tok, "NOT_NEST_TOK_")
        }
        */
      }
      this += "DISTANCE_" + bin(m1.headToken.positionInSentence - m2.headToken.positionInSentence, Seq(0, 1, 2, 3, 4, 5, 10, 20))

      for (path <- shortestPath(m1.headToken, m2.headToken)) {
        //this += "PATH " + path.map(_.directedLabel).mkString(" ")
        this += "PATH LENGTH " + path.size
        for (edge <- path) {
          this += "PATH_EDGE_" + edge.directedLabel
        }
        /*
        for (window <- path.sliding(2).map(_.toArray)) {
          if (window.length >= 2)
            this += "PATH_2GRAM_" + window(0).directedLabel + " " + normalizedWord(window(0).to) + window(1).directedLabel
        }
        */
      }
    }

  }

  def normalizeFeatures(doc: Document): Unit =
    doc.attr[RelationMentions].iterator foreach (FeatureNormalizer normalize _.features)

  // add the relation variables that don't appear yet
  def addAllVars(doc: Document): Unit = {
    val docRelations = doc.attr.getOrElseUpdate(new RelationMentions)
    val mentions = doc.attr[ACEMentionSpanList]
    var total = 0
    var added = 0
    mentions.foreach(_.attr.getOrElseUpdate(new RelationMentions))
    for (m1 <- mentions; m2 <- mentions; if m1 != m2 &&
                                            m1.head.sentence == m2.last.sentence &&
                                            m1.last.sentence == m2.head.sentence) {
      total += 1
      // check whether it already exists
      val rmentions1 = m1.attr[RelationMentions]
      val rmentions2 = m2.attr[RelationMentions]
      val exists: Boolean = rmentions1.exists(rm => rm.arg1 == m1 && rm.arg2 == m2)
      assert(exists == rmentions2.exists(rm => rm.arg1 == m1 && rm.arg2 == m2))
      //todo: play around with < 100
      if (!exists && m1.headToken.sentence == m2.headToken.sentence && math.abs(m1.start - m2.start) < 100) {
        //} && !(m1.start > m2.end)) {
        //} else {
        // add!
        //if ((m1.start <= m2.start && m1.end >= m2.end) || (m2.start <= m1.start && m2.end >= m1.end)) {
        val rm = new RelationMention(m1, m2, NoneLabel, None)
        docRelations.add(rm)(null)
        rmentions1.add(rm)(null)
        rmentions2.add(rm)(null)
        added += 1
        //}
      }
    }
    // also, compute all the features
    docRelations.foreach(_.computeFeatures)
  }

  /*
  def countLocalFeatures(docs: Seq[Document]): Tensor = {
    val counts = Tensor.newSparse(REModel.lt.weights.value)
    for (doc <- docs) {
      for (rm: RelationMention <- doc.attr[RelationMentions].members) {
        val d = new DiffList
        rm.label.setToTarget(d)
        REModel.lt.factors(rm.label).foreach(f => counts += f.currentStatistics)
        rm.label.setCategory(NoneLabel)(d)
        REModel.lt.factors(rm.label).foreach(f => counts += f.currentStatistics)
        d.undo
      }
    }
    counts
  }*/


}
