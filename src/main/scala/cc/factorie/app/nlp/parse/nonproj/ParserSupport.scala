package cc.factorie.app.nlp.parse.nonproj

import cc.factorie._
import app.nlp._

import collection.mutable.HashSet

object ParserSupport {
    object ParserUtils {

      lazy val testFeatureSpec = io.Source.fromURL(ClassLoader.getSystemResource("parser-features.json")).getLines().mkString("\n")
      
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
      
    }
    
    object DepToken {
      def apply(token: Token) = new DepToken(token, form = token.string, lemma = token.attr[Lemma].lemma, pos = token.posLabel.categoryValue)
      //TODO: will it be a problem to have this in a separate document?
      //TODO: eclipse didn't like "lazy" here
      val root = new DepToken(new Token(new Document("Root Dummy Document", ""), "<ROOT>"), form = "<ROOT>-f",  lemma = "<ROOT>-m", pos = "<ROOT>-p")
      val nullToken = new DepToken(new Token(new Document("Null Dummy Document", ""), "<NULL>"), form = "<NULL>-f",  lemma = "<NULL>-m", pos = "<NULL>-p")
    }
    class DepToken(
        var token: Token,
        val form: String,
        val lemma: String,
        val pos: String,
        var head: DepArc = new DepArc(null, null, -1),
        var lmDepIdx: Int = null.asInstanceOf[Int],
        var rmDepIdx: Int = null.asInstanceOf[Int],
        var thisIdx: Int = null.asInstanceOf[Int],
        var state: ParseState = null) {
    
      def setHead(headToken: DepToken, label: String, idx: Int) {
        head.depToken = headToken
        head.label = label
        head.parentIdx = idx
        updateHeadLmDependents(thisIdx)
        updateHeadRmDependents(thisIdx)
      }
      
      def updateHeadLmDependents(idx: Int) {
        if (idx < lmDepIdx) {
          lmDepIdx = idx
          if (hasHead())
	        head.depToken.updateHeadLmDependents(idx)
        }
      }
      
      def updateHeadRmDependents(idx: Int) {
        if (rmDepIdx < idx) {
          rmDepIdx = idx
          if (hasHead())
            head.depToken.updateHeadRmDependents(idx)
        }
      }
    
      def hasHead(): Boolean = head.depToken != null
    
      def isDescendentOf(that: DepToken): Boolean = {
        var ptr = this
        while (ptr != null && ptr != DepToken.root) {
          ptr = ptr.head.depToken
          if (ptr == that)
            return true
        }
        return false
      }
      
      def leftmostDependent(): DepToken = {
        if (lmDepIdx == null.asInstanceOf[Int])
          DepToken.nullToken
        else
          state.sentenceTokens(lmDepIdx)
      }
      
      def rightmostDependent(): DepToken = {
        if (rmDepIdx == null.asInstanceOf[Int])
          DepToken.nullToken
        else
          state.sentenceTokens(rmDepIdx)
      }
      
      override def toString = "(%s, f: %s, l: %s, p: %s)".format(token.string, form, lemma, pos)
    
    }
    
    class DepArc(var depToken: DepToken, var label: String, var parentIdx: Int) {
      override def toString = (parentIdx, label).toString
    }
    
    class ParseState(
        var stack: Int,
        var input: Int,
        var reducedIds: HashSet[Int],
        var sentenceTokens: Array[DepToken]) {
      
      sentenceTokens.foreach(_.state = this)
      
      def this(tokens: Array[DepToken]) = this(0, 1, HashSet[Int](), tokens)
    
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
    
    case class ParseDecision(var leftOrRightOrNo: Int, var shiftOrReduceOrPass: Int, var label: String) {
    
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
        })
        s append "-"
        s append (shiftOrReduceOrPass match {
          case SHIFT  => "shift"
          case REDUCE => "reduce"
          case PASS   => "pass"
        })
        
        s append (" label: " + label)
        s append "]"
        
        s.toString()
      }
    
    }
    
    // define variables and domains
    object DecisionDomain extends CategoricalDomain[ParseDecision] {
      this += new ParseDecision(-1, -1, "")
      lazy val defaultCategory = this.head.category
    }
    class ParseDecisionVariable(targetDecision: ParseDecision, state: ParseState) extends LabeledCategoricalVariable(targetDecision) {
      def this(state: ParseState) = this(DecisionDomain.defaultCategory, state)
      def domain = DecisionDomain
      val features = new NonProjDependencyParserFeatures(this)
      features ++= ParserUtils.featureGenerators.map(_.apply(state)) //.map(s => new Hash[String](s.apply(state)))  //.map(_.apply(state))  //.map(s => new Hash[String](s))
    }
    
    object NonProjParserFeaturesDomain extends StringHashDomain(100000)
    class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable) extends HashingBinaryFeatureVectorVariable[String] {
      override def domain = NonProjParserFeaturesDomain
    }
    
}
