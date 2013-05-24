package cc.factorie.app.nlp.parse.nonproj

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.parse.ParseTreeLabelDomain
import cc.factorie._
import cc.factorie.app.nlp.pos.PTBPosLabel
import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.util.parsing.json.JSON
import scala.annotation.tailrec

trait Parser extends DocumentAnnotator {
  case class ParseDecision(action: String) {
    val Array(lrnS, srpS, label) = action.split(" ")
    val leftOrRightOrNo = lrnS.toInt
    val shiftOrReduceOrPass = srpS.toInt
  }
  object labelDomain extends CategoricalDomain[String]
  val defaultCategory = "-1 -1 N"
  labelDomain += defaultCategory
  class ParseDecisionVariable(targetDecision: ParseDecision, val state: ParseState) extends LabeledCategoricalVariable(targetDecision.action) {
    def domain = labelDomain
    val features = new NonProjDependencyParserFeatures(this)
    features ++= featureGenerators.map(_.apply(state))
  }
  object featuresDomain extends CategoricalDimensionTensorDomain[String]
  class NonProjDependencyParserFeatures(val decisionVariable: ParseDecisionVariable) extends BinaryFeatureVectorVariable[String] {
    def domain = featuresDomain
    override def skipNonCategories = domain.dimensionDomain.frozen
  }

  def trainFromVariables(vs: Seq[ParseDecisionVariable])
  def classify(v: ParseDecisionVariable): ParseDecision

  lazy val testFeatureSpec = io.Source.fromURL(this.getClass.getResource("/parser-features.json")).getLines().mkString("\n")
  lazy val featureGenerators: Seq[DependencyFeatures.DependencyFeatureGenerator] = DependencyFeatures.fromJSON(testFeatureSpec)

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

  def generateDecisions(ss: Seq[Sentence], mode: Int): Seq[ParseDecisionVariable] = {
    ss.flatMap(s => {
      val oracle: NonProjectiveOracle = {
        if (mode == ParserConstants.TRAINING) new NonprojectiveGoldOracle(s)
        else new NonprojectiveBoostingOracle(s, classify)
      }
      new NonProjectiveShiftReduce(oracle.predict).parse(s)
      oracle.instances
    })
  }
  def boosting(ss: Seq[Sentence], addlVs: Seq[ParseDecisionVariable] = Seq.empty[ParseDecisionVariable]) = trainFromVariables(addlVs ++ generateDecisions(ss, ParserConstants.BOOSTING))
  def process1(doc: Document) = { doc.sentences.foreach(process(_)); doc }
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence], classOf[PTBPosLabel])
  def postAttrs = Seq(classOf[ParseTree])
  def process(s: Sentence): Sentence = {
    val parse = s.attr.getOrElseUpdate(new ParseTree(s))
    new NonProjectiveShiftReduce(predict = classify).parse(s).zipWithIndex.map(dt => {
      parse.setParent(dt._2, dt._1._1)
      parse.label(dt._2).set(ParseTreeLabelDomain.index(dt._1._2))(null)
    })
    s
  }

  class NonProjectiveShiftReduce(val predict: ParseDecisionVariable => ParseDecision) {
    import ParserConstants._
    def parse(s: Sentence) = {
      val state = new ParseState(0, 1, collection.mutable.HashSet[Int](), s)
      while(state.input < state.sentenceTokens.length) {
        if (state.stack < 0)
          noShift(state)
        else {
          val label = predict(new ParseDecisionVariable(ParseDecision(defaultCategory), state))
          if (label.leftOrRightOrNo == LEFT) {
            if (state.stack == ROOT_ID) noShift(state)
            else if (state.inputToken(0).isDescendentOf(state.stackToken(0))) noPass(state)
            else if (label.shiftOrReduceOrPass == REDUCE) leftReduce(label.label, state)
            else leftPass(label.label, state)
          }
          else if (label.leftOrRightOrNo == RIGHT) {
              if (state.stackToken(0).isDescendentOf(state.inputToken(0))) noPass(state)
              else if (label.shiftOrReduceOrPass == SHIFT) rightShift(label.label, state)
              else rightPass(label.label, state)
          }
          else {
              if (label.shiftOrReduceOrPass == SHIFT) noShift(state)
              else if (label.shiftOrReduceOrPass == REDUCE && state.stackToken(0).hasHead) noReduce(state)
              else noPass(state)
          }
        }
      }
      state.sentenceTokens.drop(1).map(dt => if (dt.hasHead) (dt.head.depToken.thisIdx-1, dt.head.label) else (-1,""))
    }

    private def passAux(state: ParseState): Unit = {
      var i = state.stack - 1
      while (i >= 0) {
        if (!state.reducedIds.contains(i)) {
            state.stack = i
            return
        }
        i -= 1
      }
      state.stack = i
    }

    private def leftArc(label: String, state: ParseState)  { state.stackToken(0).setHead(state.inputToken(0), label) }
    private def rightArc(label: String, state: ParseState) { state.inputToken(0).setHead(state.stackToken(0), label) }

    private def shift(state: ParseState)  { state.stack = state.input; state.input += 1 }
    private def reduce(state: ParseState) { state.reducedIds.add(state.stack); passAux(state) }
    private def pass(state: ParseState)   { passAux(state: ParseState) }

    private def noShift(state: ParseState)  { shift(state) }
    private def noReduce(state: ParseState) { reduce(state) }
    private def noPass(state: ParseState)   { pass(state) }
    private def leftReduce(label: String, state: ParseState) { leftArc(label, state);  reduce(state) }
    private def leftPass(label: String, state: ParseState)   { leftArc(label, state);  pass(state)   }
    private def rightShift(label: String, state: ParseState) { rightArc(label, state); shift(state)  }
    private def rightPass(label: String, state: ParseState)  { rightArc(label, state); pass(state)   }
  }

  trait NonProjectiveOracle {
    import ParserConstants._
    val sentence: Sentence
    def predict(state: ParseDecisionVariable): ParseDecision

    var instances = new ArrayBuffer[ParseDecisionVariable] { override val initialSize = 100 }
    def getSimpleDepArcs = sentence.parse.parents.map(_ + 1).zip(sentence.parse.labels.map(_.value.category))
    def getDepArcs = { Seq((-1, "<ROOT-ROOT>")) ++ getSimpleDepArcs.map { case (i: Int, l: String) => (i, l) } }
    val goldHeads = getDepArcs

    def getGoldDecision(state: ParseState): ParseDecision = {
      val shiftOrReduceOrPass =
        getGoldLRN(state) match {
          case LEFT  => if (shouldGoldReduce(hasHead=true, state=state)) REDUCE else PASS
          case RIGHT => if (shouldGoldShift(state=state)) SHIFT else PASS
          case _ => {
            if (shouldGoldShift(state=state)) SHIFT
            else if (shouldGoldReduce(hasHead=false, state=state)) REDUCE
            else PASS
          }
        }
      new ParseDecision(getGoldLRN(state) + " " + shiftOrReduceOrPass + " " + getGoldLabel(state))
    }

    def getGoldLabel(state: ParseState): String = {
      if (goldHeads(state.stack)._1 == state.input) goldHeads(state.stack)._2
      else if (goldHeads(state.input)._1 == state.stack) goldHeads(state.input)._2
      else "N"
    }

    def getGoldLRN(state: ParseState): Int = {
      if (goldHeads(state.stack)._1 == state.input) LEFT
      else if (goldHeads(state.input)._1 == state.stack) RIGHT
      else NO
    }

    def shouldGoldShift(state: ParseState): Boolean = {
      if (goldHeads(state.input)._1 < state.stack) return false
      else
        for (i <- (state.stack - 1) until 0 by -1) if (!state.reducedIds.contains(i)) {
          if (goldHeads(i)._1 == state.input)
            return false
        }
      true
    }

    def shouldGoldReduce(hasHead: Boolean, state: ParseState): Boolean = {
      if (!hasHead && !state.stackToken(0).hasHead)
        return false
      for (i <- (state.input + 1) until state.sentenceTokens.length)
        if (goldHeads(i)._1 == state.stack)
          return false

      true
    }
  }

  class NonprojectiveGoldOracle(val sentence: Sentence) extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val decision = getGoldDecision(decisionVariable.state)
      instances += new ParseDecisionVariable(decision, decisionVariable.state)
      decision
    }
  }

  class NonprojectiveBoostingOracle(val sentence: Sentence, basePredict: ParseDecisionVariable => ParseDecision) extends NonProjectiveOracle {
    def predict(decisionVariable: ParseDecisionVariable): ParseDecision = {
      val label = new ParseDecisionVariable(getGoldDecision(decisionVariable.state), decisionVariable.state)
      instances += label
      basePredict(label)
    }
  }

  object DependencyFeatures {
    val locationAbbrevs = collection.mutable.HashMap(
      "S_LAMBDA" -> "l",
      "S_STACK"  -> "s",
      "S_BETA"   -> "b",
      "R_H"      -> "h",     // head
      "R_LMD"    -> "lmd",   // left-most dependent
      "R_RMD"    -> "rmd"    // right-most dependent
    )
    val formAbbrevs = collection.mutable.HashMap(
      "F_FORM"   -> "f",
      "F_LEMMA"  -> "m",
      "F_POS"    -> "p",
      "F_DEPREL" -> "d",
      "F_LNPL"   -> "lnpl", // left-nearest punctuation of lambda
      "F_RNPL"   -> "rnpl", // right-nearest punctuation of lambda
      "F_LNPB"   -> "lnpb", // left-nearest punctuation of beta
      "F_RNPB"   -> "rnpb"  // right-nearest punctuation of beta
    )
    val locationFns: HashMap[String, (Int) => (ParseState) => DepToken] = HashMap(
      "b"   -> ((offset: Int) => (state: ParseState) => state.inputToken(offset)),
      "l"   -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset)),
      "s"   -> ((offset: Int) => (state: ParseState) => state.stackToken(offset)),
      "l_h" -> ((_: Int) => (state: ParseState) => state.lambdaToken(0)),
      "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).leftmostDependent),
      "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).leftmostDependent),
      "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).rightmostDependent),
      "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).rightmostDependent)
    )

    val formFns = HashMap(
      "f"   -> ((t: DepToken) => t.form),
      "m"   -> ((t: DepToken) => t.lemma),
      "p"   -> ((t: DepToken) => t.pos),
      "d"   -> ((t: DepToken) => if (!t.hasHead) "null" else t.head.label),
      "b0"  -> ((t: DepToken) => if (t.pos == -2) "null" else (t.state.lambdaToken(0) eq t.state.sentenceTokens(1)).toString),
      "b1"  -> ((t: DepToken) => (t.state.stackToken(0) eq t.state.sentenceTokens.last).toString),
      "b2"  -> ((t: DepToken) => (t.state.input - t.state.stack == 1).toString)
    )

    def generators(locationOffsetAndForm: String): (ParseState => String) = {
      val LocationOffsetAndForm = """([a-z_]*)[+]?([-0-9]*):([a-z]*[0-9]?)""".r
      locationOffsetAndForm match {
        case LocationOffsetAndForm(location, offset, form) => {
          val locationFn = locationFns(location)(if (offset == "") 0 else offset.toInt)
          (state: ParseState) => location + offset + ":" + formFns(form)(locationFn(state))
        }
        case _ => throw new Error("Couldn't parse location and form from feature generator string.")
      }
    }

    abstract class DependencyFeatureGenerator extends (ParseState => String)
    class SingletonDependencyFeatureGenerator(f: String) extends DependencyFeatureGenerator {
      lazy val featureFn = generators(f)
      def apply(s: ParseState): String = featureFn(s)
    }
    class CompositeDependencyFeatureGenerator(gens: Seq[DependencyFeatureGenerator]) extends DependencyFeatureGenerator {
      def apply(s: ParseState) = gens.map(_.apply(s)).mkString("|")
    }
    private def stripJSONComments(s: String) = s.split("\n").map(_.split("#").head).mkString("\n")
    def fromJSON(source: String) = {
      val someJson = JSON.parseFull(stripJSONComments(source))
      val featureSpec = someJson match {
        case map: Some[Map[String, List[List[String]]]] => map.get("features")
        case _ => throw new Error()
      }
      featureSpec.map(fs => {
        val fGens = fs.map(f => new SingletonDependencyFeatureGenerator(f))
        if (fGens.length > 1) new CompositeDependencyFeatureGenerator(fGens)
        else fGens.head
      })
    }
  }

  class DepToken(val form: String, val lemma: String, val pos: String, val thisIdx: Int, val state: ParseState) {
    var head: DepArc = null
    def hasHead: Boolean = head ne null

    def setHead(headToken: DepToken, label: String) {
      head = new DepArc(headToken, label)
      if (thisIdx < head.depToken.thisIdx) state.leftmostDeps(head.depToken.thisIdx) = thisIdx
      else state.rightmostDeps(head.depToken.thisIdx) = thisIdx
    }
    def leftmostDependent: DepToken = {
      val i = state.leftmostDeps(thisIdx)
      if (i == -1) state.nullToken
      else state.sentenceTokens(i)
    }
    def rightmostDependent: DepToken = {
      val i = state.rightmostDeps(thisIdx)
      if (i == -1) state.nullToken
      else state.sentenceTokens(i)
    }
    @tailrec final def isDescendentOf(that: DepToken): Boolean = {
      if (!hasHead) false
      else if (this.head.depToken == that) true
      else this.head.depToken.isDescendentOf(that)
    }
  }

  case class DepArc(depToken: DepToken, label: String)

  class ParseState(var stack: Int, var input: Int, val reducedIds: HashSet[Int], sentence: Sentence) {
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
}
