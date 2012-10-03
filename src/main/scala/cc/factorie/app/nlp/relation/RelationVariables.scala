package cc.factorie.app.nlp.relation

import cc.factorie._
import app.nlp.coref.PairwiseMention
import app.nlp.{Token, Document}
import collection.mutable.ArrayBuffer

/**
 * @author sameer, brian, sebastian
 * @date 12/22/11
 */

object RelationVariables {

  val NoneLabel = "NONE"

  class RelationMentions extends SetVariable[RelationMention]

  object RelationLabelDomain extends CategoricalDomain[String]

  RelationLabelDomain += NoneLabel

  class RelationLabel(labelStr: String, val mention: RelationMention) extends LabeledCategoricalVariable(labelStr) {
    def domain = RelationLabelDomain
  }

  class RelationMention(val arg1: PairwiseMention, val arg2: PairwiseMention, val labelStr: String) extends ArrowVariable(arg1, arg2) with Attr {
    val arg1Features = new ArgFeatures(arg1, true)
    val arg2Features = new ArgFeatures(arg2, false)
    val features = new Features(this)
    val label = new RelationLabel(labelStr, this)

    def computeFeatures = {
      //arg1Features.compute
      //arg2Features.compute
      features.compute
      FeatureNormalizer += features
    }
  }

  object RelationArgFeaturesDomain extends CategoricalDomain[String]

  class ArgFeatures(val arg: PairwiseMention, val first: Boolean) extends BinaryFeatureVectorVariable[String] {
    def domain = RelationArgFeaturesDomain

    def compute = {
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
      for (child <- from.parseChildren; if (!visited(child))) {
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
      for (i <- feats.tensor.activeDomain.toSeq)
        this.tensor.+=(i, feats.tensor(i)) // TODO Use Tensor.+= to make this much faster

    def normalize(feats: FeatureVectorVariable[String]) =
      for (i <- feats.tensor.activeDomain.toSeq)
        feats.tensor.update(i, feats.tensor(i) / tensor(i)) // TODO Use Tensor./= to make this much faster
  }

  class Features(val mention: RelationMention) extends FeatureVectorVariable[String] {
    def domain = RelationFeaturesDomain

    def addTokenFeatures(tok: Token, prefix: String) {
      if (tok.string(0).isLower) {
        //this += prefix + "W_" + tok.string.replaceAll("\\s+", " ").toLowerCase
        this += prefix + "STEM_" + tok.string.replaceAll("\\s+", " ").toLowerCase.take(5)
      }
      this += prefix + "POS_" + tok.posLabel.categoryValue
    }

    override def +=(elt: String) = if (!tensor.contains(domain.index(elt))) this.+=(elt, 1.0)  // was: elt.update(elt, 1.0)

    def compute = {
      val m1 = mention.arg1
      val m2 = mention.arg2
      val sentence = m1.sentence
      this += "BIAS"
      // TODO compute relation features using m1 and m2
      // Order of arguments / overlap
      if (m1.start <= m2.start && m1.end >= m2.end) {
        this += "ARG1_CONTAINS_ARG2"
        // intermediate tokens
        for (tok <- sentence.tokens.slice(m1.start, m2.start)) {
          addTokenFeatures(tok, "NESTED_BEF_TOK_")
          addTokenFeatures(tok, "ARG2_NESTED_BEF_TOK_")
        }
        for (tok <- sentence.tokens.slice(m2.end, m1.end)) {
          addTokenFeatures(tok, "NESTED_AFT_TOK_")
          addTokenFeatures(tok, "ARG2_NESTED_AFT_TOK_")
        }
      }
      if (m2.start <= m1.start && m2.end >= m1.end) {
        this += "ARG2_CONTAINS_ARG1"
        // intermediate tokens
        for (tok <- sentence.tokens.slice(m2.start, m1.start)) {
          addTokenFeatures(tok, "NESTED_BEF_TOK_")
          addTokenFeatures(tok, "ARG1_NESTED_BEF_TOK_")
        }
        for (tok <- sentence.tokens.slice(m1.end, m2.end)) {
          addTokenFeatures(tok, "NESTED_AFT_TOK_")
          addTokenFeatures(tok, "ARG1_NESTED_AFT_TOK_")
        }
      }
      if (m2.start > m1.end) {
        this += "ARG1_IS_BEFORE"
        // intermediate tokens
        for (tok <- sentence.tokens.slice(m1.end, m2.start)) {
          addTokenFeatures(tok, "ARG1_IS_BEFORE_TOK_")
          addTokenFeatures(tok, "NOT_NEST_TOK_")
        }
      }
      if (m1.start > m2.end) {
        this += "ARG2_IS_BEFORE"
        // intermediate tokens
        for (tok <- sentence.tokens.slice(m2.end, m1.start)) {
          addTokenFeatures(tok, "ARG2_IS_BEFORE_TOK_")
          addTokenFeatures(tok, "NOT_NEST_TOK_")
        }
      }
      this += "DISTANCE_" + bin(m1.headToken.indexInSentence - m2.headToken.indexInSentence, Seq(0, 1, 2, 3, 4, 5, 10, 20))

      for (path <- shortestPath(m1.headToken, m2.headToken)) {
        this += "PATH " + path.map(_.directedLabel).mkString(" ")
        this += "PATH LENGTH " + path.size
        for (edge <- path) {
          this += "PATH_EDGE_" + edge.directedLabel
        }
        for (window <- path.sliding(2).map(_.toArray)) {
          if (window.length >= 2)
            this += "PATH_2GRAM_" + window(0).directedLabel + " " + normalizedWord(window(0).to) + window(1).directedLabel
        }
      }
    }

  }

  def normalizeFeatures(doc: Document): Unit =
    doc.attr[RelationMentions].foreach(m => FeatureNormalizer.normalize(m.features))

  // add the relation variables that don't appear yet
  def addAllVariables(doc: Document): Unit = {
    val relations = doc.attr.getOrElseUpdate(new RelationMentions)
    val mentions = doc.spansOfClass[PairwiseMention]
    var total = 0
    var added = 0
    for (m1 <- mentions; m2 <- mentions; if (m1 != m2 && m1.sentence == m2.sentence)) {
      total += 1
      // check whether it already exists
      val rmentions1 = m1.attr.getOrElseUpdate(new RelationMentions)
      val rmentions2 = m2.attr.getOrElseUpdate(new RelationMentions)
      val exists: Boolean = rmentions1.exists(rm => rm.arg1 == m1 && rm.arg2 == m2)
      assert(exists == rmentions2.exists(rm => rm.arg1 == m1 && rm.arg2 == m2))
      if (!exists) {
        val rm = new RelationMention(m1, m2, NoneLabel)
        relations.add(rm)(null)
        rmentions1.add(rm)(null)
        rmentions2.add(rm)(null)
        added += 1
      }
    }
    // also, compute all the features
    relations.foreach(_.computeFeatures)
  }

}
