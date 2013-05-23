package cc.factorie.app.nlp.parse.nonproj

import cc.factorie._
import app.nlp._

import collection.mutable.HashSet

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
    
    object DepToken {
      def apply(token: Token, idx: Int, state: ParseState) = new DepToken(form = token.string, lemma = token.lemmaString, pos = token.posLabel.categoryValue, thisIdx=idx, state=state)
      // eclipse didn't like "lazy" here
      val root = new DepToken(form = "<ROOT>-f",  lemma = "<ROOT>-m", pos = "<ROOT>-p", thisIdx = 0)
      val nullToken = new DepToken(form = "<NULL>-f",  lemma = "<NULL>-m", pos = "<NULL>-p", thisIdx = -1)
    }
    class DepToken(
        val form: String,
        val lemma: String,
        val pos: String,
        val thisIdx: Int,
        var head: DepArc = null,
        var state: ParseState = null) {
    
      def setHead(headToken: DepToken, label: String) {
        head = new DepArc(headToken, label)
        if (thisIdx < head.depToken.thisIdx)
          state.leftmostDeps(head.depToken.thisIdx) = thisIdx
        else
          state.rightmostDeps(head.depToken.thisIdx) = thisIdx
      }
      
      def leftmostDependent(): DepToken = {
        val i = state.leftmostDeps(thisIdx)
        if (i == -1)
          DepToken.nullToken
        else
          state.sentenceTokens(i)
      }
      
      def rightmostDependent(): DepToken = {
        val i = state.rightmostDeps(thisIdx)
        if (i == -1)
          DepToken.nullToken
        else
          state.sentenceTokens(i)
      }
    
      def hasHead: Boolean = head ne null
    
      def isDescendentOf(that: DepToken): Boolean = {
        assert(that ne null)
        var ptr = this
        while (ptr.head != null && ptr != DepToken.root) {
          ptr = ptr.head.depToken
          if (ptr == that)
            return true
        }
        return false
      }
      
      override def toString = "(f: %s, l: %s, p: %s)".format(form, lemma, pos)
    
    }

  case class DepArc(depToken: DepToken, label: String)

  class ParseState(var stack: Int,
                   var input: Int,
                   val reducedIds: HashSet[Int],
                   val sentenceTokens: Array[DepToken]) {

    sentenceTokens.foreach(_.state = this)

    def this(tokens: Array[DepToken]) = this(0, 1, HashSet[Int](), tokens)

    val leftmostDeps = Array.fill[Int](sentenceTokens.size)(-1)
    val rightmostDeps = Array.fill[Int](sentenceTokens.size)(-1)
    
    def inputToken(offset: Int): DepToken = {
      val i = input + offset
      if (i < 0 || sentenceTokens.size - 1 < i)
        return DepToken.nullToken
      else
        sentenceTokens(i)
    }

    def lambdaToken(offset: Int): DepToken = {
      val i = stack + offset
      if (i < 0 || sentenceTokens.size - 1 < i)
        return DepToken.nullToken
      else
        sentenceTokens(i)
    }

    def stackToken(offset: Int): DepToken = {
      // TODO: check for correctness (wait on Jinho)
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
      return DepToken.nullToken
    }

    override def toString = "[ParseState: %d, %d]" format (stack, input)
    
  }

  class ParseDecision(val action: String) {
    val Array(lrnS, srpS, label) = action.split(" ")
    val leftOrRightOrNo = lrnS.toInt
    val shiftOrReduceOrPass = srpS.toInt
    import ParserConstants._
    def shift_?  = shiftOrReduceOrPass == SHIFT
    def reduce_? = shiftOrReduceOrPass == REDUCE
    def pass_?   = shiftOrReduceOrPass == PASS
    def left_?   = leftOrRightOrNo == LEFT
    def right_?  = leftOrRightOrNo == RIGHT
    override def toString = {
      val s = new StringBuffer
      s append "["
      s append "Decision: "
      s append (leftOrRightOrNo match {
        case LEFT  => "left"
        case RIGHT => "right"
        case NO    => "no"
        case -1    => "TEST"
      })
      s append "-"
      s append (shiftOrReduceOrPass match {
        case SHIFT  => "shift"
        case REDUCE => "reduce"
        case PASS   => "pass"
        case -1     => "TEST"
      })
      s append (" label: " + label)
      s append "]"
      s.toString
    }
  }

  val defaultCategory = "-1 -1 N"
  class ParseDecisionVariable(targetDecision: ParseDecision, state: ParseState, val domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) extends LabeledCategoricalVariable(targetDecision.action) {
    def this(state: ParseState, domain: CategoricalDomain[String], featureDomain: CategoricalDimensionTensorDomain[String]) = this(new ParseDecision(defaultCategory), state, domain, featureDomain)
    val features = new NonProjDependencyParserFeatures(this, featureDomain)
    features ++= ParserUtils.featureGenerators.map(_.apply(state))
  }

  class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable, val domain: CategoricalDimensionTensorDomain[String]) extends BinaryFeatureVectorVariable[String] {
    override def skipNonCategories = domain.dimensionDomain.frozen
  }
    
}
