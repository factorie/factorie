package cc.factorie.app.nlp.parse.nonproj

import cc.factorie._
import app.nlp._

import collection.mutable.HashSet
import scala.annotation.tailrec

object ParserSupport {
    object ParserUtils {
      lazy val testFeatureSpec = io.Source.fromURL(this.getClass.getResource("/parser-features.json")).getLines().mkString("\n")
      lazy val featureGenerators: Seq[DependencyFeatureGenerator] = LoadParserFeatureSpec.fromJSON(testFeatureSpec)
    }
    
    object ParserConstants {
      val SHIFT  = 0
      val REDUCE = 1
      val PASS   = 2

      val LEFT  = 0
      val RIGHT = 1
      val NO    = 2
      
      val ROOT_ID = 0

      val TRAINING   = 0
      val PREDICTING = 1
      val BOOSTING   = 2
    }
    
    class DepToken(
        val form: String,
        val lemma: String,
        val pos: String,
        val thisIdx: Int,
        val state: ParseState) {
      var head: DepArc = null
      def hasHead: Boolean = head ne null

      def setHead(headToken: DepToken, label: String) {
        head = new DepArc(headToken, label)
        if (thisIdx < head.depToken.thisIdx) state.leftmostDeps(head.depToken.thisIdx) = thisIdx
        else state.rightmostDeps(head.depToken.thisIdx) = thisIdx
      }
      
      def leftmostDependent(): DepToken = {
        val i = state.leftmostDeps(thisIdx)
        if (i == -1) state.nullToken
        else state.sentenceTokens(i)
      }
      
      def rightmostDependent(): DepToken = {
        val i = state.rightmostDeps(thisIdx)
        if (i == -1) state.nullToken
        else state.sentenceTokens(i)
      }
    
      @tailrec final def isDescendentOf(that: DepToken): Boolean = {
        if (!hasHead) false
        else if (this.head.depToken == that) true
        else this.head.depToken.isDescendentOf(that)
      }
      
      override def toString = "(f: %s, l: %s, p: %s)".format(form, lemma, pos)
    
    }

  case class DepArc(depToken: DepToken, label: String)

  class ParseState(var stack: Int,
                   var input: Int,
                   val reducedIds: HashSet[Int],
                   sentence: Sentence) {

    private def depToken(token: Token, idx: Int, state: ParseState) = new DepToken(form = token.string, lemma = token.lemmaString, pos = token.posLabel.categoryValue, thisIdx=idx, state=state)
    val rootToken = new DepToken(form = "<ROOT>-f",  lemma = "<ROOT>-m", pos = "<ROOT>-p", thisIdx = 0, state=this)
    val nullToken = new DepToken(form = "<NULL>-f",  lemma = "<NULL>-m", pos = "<NULL>-p", thisIdx = -1, state=this)
    val sentenceTokens = (Seq(rootToken) ++ sentence.tokens.zipWithIndex.map(t => depToken(t._1, t._2+1, this))).toArray

    val leftmostDeps = Array.fill[Int](sentenceTokens.size)(-1)
    val rightmostDeps = Array.fill[Int](sentenceTokens.size)(-1)
    
    def inputToken(offset: Int): DepToken = {
      val i = input + offset
      if (i < 0 || sentenceTokens.size - 1 < i) nullToken
      else sentenceTokens(i)
    }

    def lambdaToken(offset: Int): DepToken = {
      val i = stack + offset
      if (i < 0 || sentenceTokens.size - 1 < i) nullToken
      else sentenceTokens(i)
    }

    def stackToken(offset: Int): DepToken = {
      if (offset == 0)
        return sentenceTokens(stack)

      var off = math.abs(offset)
      var dir = if (offset < 0) -1 else 1
      var i = stack + dir
      while (0 < i && i < input) {
        if (!reducedIds.contains(i)) {
          off -= 1
          if (off == 0)
            return sentenceTokens(i)
        }
        i += dir
      }
      nullToken
    }
  }

  case class ParseDecision(action: String) {
    val Array(lrnS, srpS, label) = action.split(" ")
    val leftOrRightOrNo = lrnS.toInt
    val shiftOrReduceOrPass = srpS.toInt
  }

  val defaultCategory = "-1 -1 N"
  class ParseDecisionVariable(targetDecision: ParseDecision, val state: ParseState, val domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) extends LabeledCategoricalVariable(targetDecision.action) {
    def this(state: ParseState, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) = this(new ParseDecision(defaultCategory), state, domain, featureDomain)
    val features = new NonProjDependencyParserFeatures(this, featureDomain)
    features ++= ParserUtils.featureGenerators.map(_.apply(state))
  }

  class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable, val domain: CategoricalDimensionTensorDomain[String]) extends BinaryFeatureVectorVariable[String] {
    override def skipNonCategories = domain.dimensionDomain.frozen
  }
    
}
