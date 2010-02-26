package cc.factorie.example

import cc.factorie.util.Implicits._
import scala.io.Source
import java.io.File
import collection.mutable.{HashSet, ArrayBuffer}
import cc.factorie._
import application.LabeledTokenSeqs
import application.LabeledTokenSeqs.LabeledTokenSeq

/**
 * SpanNER provides an alternative representation for sequential labelling tasks. In addition to labelling
 * individual tokens with BIO tags, it maintains a span-based representation that can be used for model and
 * objective templates.
 * @author Sebastian Riedel  
 */
object SpanNER {

  //the BIO tags
  val O = "O"
  val I = "I"
  val B = "B"

  //mapping from span labels to BIO labels
  def insideLabel(spanLabel: String) = I + "-" + spanLabel

  def beginLabel(spanLabel: String) = B + "-" + spanLabel

  def outside = O

  //mapping from BIO labels to span labels, if possible
  def spanLabel(bioLabel: String) = if (O == bioLabel) None else Some(bioLabel.substring(2))

  //test bio labels
  def isBegin(bioLabel: String) = bioLabel.startsWith(B)

  def isInside(bioLabel: String) = bioLabel.startsWith(I)

  //synchronize the domains of span and token labels (so that for each B/I-X there is an X,
  //and for each X there are B/I-X labels
  def syncLabelDomains: Unit = {
    val dLabel = Domain[Label]
    val dSpanLabel = Domain[SpanLabel]
    dLabel.iterator.filter(l => !(l equals O)).map(spanLabel(_).get).foreach(dSpanLabel(_))
    dSpanLabel.iterator.map(beginLabel(_)).foreach(dLabel(_))
    dSpanLabel.iterator.map(insideLabel(_)).foreach(dLabel(_))
  }

  //The token representing one word
  class Token(word: String, features: Seq[String], labelString: String)
      extends LabeledTokenSeqs.Token[Label, Token](word, features) {
    val label: Label = new Label(labelString, this)

    override def toString = word
  }


  // The label for one word
  class Label(tag: String, token: Token) extends LabeledTokenSeqs.Label[Token, Label](tag, token) {

    //the span that is covering this label if the label value is not "O"
    var span: Option[Span] = None

    //we forbid to call set methods directly
    //todo: I want to forbid calling set or setByIndex directly on this label, but since label
    //is uncoordinatedcategorical this won't work. 
    //    override def setByIndex(index: Int)(implicit d:DiffList) {
    //      error("Changing token labels directly is forbidden. Use span methods instead.")
    //    }

    //this method should only be called within span operations
    private[SpanNER] def setLabel(label: String)(implicit d: DiffList) {
      val index = domain.index(label)
      super.setByIndex(index)
    }
  }


  // The label for one span, true label is stored here
  class SpanLabel(labelname: String, val span: Span)(d: DiffList) extends TypedCategoricalVariable[String] {
    super.setByIndex(domain(labelname))(d)

    def label: String = value

    // change label and coordinate spans
    override def setByIndex(index: Int)(implicit d: DiffList) = {
      super.setByIndex(index)
      span.coordinateTokenLabels(d)
    }

    override def toString = if (index < 0) "N/A" else value
  }

  class Span(sentence: Seq[Token], initStart: Int, initLength: Int, labelStr: String)(implicit d: DiffList)
      extends SpanVariable[Token](sentence, initStart, initLength)(d) {
    override def diffIfNotPresent = true

    val label = new SpanLabel(labelStr, this)(null)
    coordinateTokenLabels(d)

    //def sentence = seq.asInstanceOf[Sentence]

    override def toString = {if (present) "" else "!"} + "Span(" + this.phrase + "=" + this.label.toString + ")"

    /**
     * Brute force coordination of all token labels to be B-Label I-Label ... I-Label
     */
    def coordinateTokenLabels(implicit d: DiffList) = {
      this(0).label.set(beginLabel(label.value))(d)
      this.drop(1).foreach(_.label.set(insideLabel(label.value))(d))
    }

    /**
     * Prepend n tokens to this span.
     */
    override def prepend(n: Int)(implicit d: DiffList) = {
      val result = super.prepend(n)(d)
      this(0).label.set(beginLabel(label.value))(d)
      for (i <- start + 1 until start + n + 1) sentence(i).label.setLabel(insideLabel(label.value))
      result
    }

    override def trimEnd(n: Int)(implicit d: DiffList) = {
      for (i <- end until end + n) sentence(i).label.setLabel(O)(d)
      super.trimEnd(n)(d)
    }

    override def trimStart(n: Int)(implicit d: DiffList) = {
      for (i <- start until start + n) sentence(i).label.setLabel(O)(d)
      val result = super.trimStart(n)(d)
      this(0).label.set(beginLabel(label.value))(d)
      result
    }

    override def setLength(l: Int)(implicit d: DiffList) = {
      val result = super.setLength(l)(d)
      coordinateTokenLabels(d)
      result
    }

    /**
     * Merges two spans to one. In case spans are not adjacent this also fills up the gap between them
     * todo: if there are other spans between these two this method will lead to inconsistency
     */
    def merge(that: Span, useLabelOfThis: Boolean)(implicit d: DiffList) = {
      val (first, last) = {if (that.start < this.start) (that, this) else (this, that)}
      //      println("Merging: " + (first,last))
      val until = last.end
      last.delete(d)
      first.append(until - first.end)(d)
      //      println("Appended: " + (until - first.end))
      //      println("After append: " + first)
      first.label.set((if (useLabelOfThis) this.label.value else that.label.value))(d)
      //      println("After label change: " + first)
    }


    /**
     * This sets all labels to the 'O' state
     */
    override def delete(implicit d: DiffList) = {
      iterator.foreach(_.label.set(O)(d))
      super.delete(d)
    }

    override def append(n: Int)(implicit d: DiffList) = {
      for (i <- end + 1 until end + n + 1) sentence(i).label.setLabel(insideLabel(label.value))(d)
      super.append(n)
    }
  }


  class Sentence extends VariableSeqWithSpans[Token, Span] {
    type SpanType = Span

    /**
     * A labelled Span of tokens that can coordinate its label with the labels of its tokens. Note that this
     * coordination assumes that spans are never moved into an overlapping state.
     */

  }

  /**
   * A possible move for the proposer to choose
   */
  case class Choice(probability: Double, change: DiffList => Unit) {
    /**
     * For representing the difflist, model and true score of a choice
     */
    case class ScoredDiffList(model: Model, objective: Model) {
      val difflist = new DiffList
      change(difflist)
      val (modelScore, trueScore) = difflist.scoreAndUndo(model, objective)

      override def toString = modelScore + "," + trueScore + difflist.mkString("{", ",", "}")
    }

    /**
     * Return the difflist plus model score/objective score for this choice
     */
    def scoredDiffList(model: Model, objective: Model) = ScoredDiffList(model, objective)

    def asProposal(model: Model, objective: Model): Proposal = {
      val sd = ScoredDiffList(model, objective)
      Proposal(sd.difflist, sd.modelScore, sd.trueScore, probability)
    }

    def asProposalWithScoreAcceptance(model: Model, objective: Model, temperature: Double): Proposal = {
      val sd = ScoredDiffList(model, objective)
      Proposal(sd.difflist, sd.modelScore, sd.trueScore, sd.modelScore / temperature)
    }
  }


  class SpanSampler(val model: Model, obj: Model) extends ProposalSampler[Label] {
    var weightCreate: Double = 0.5
    var weightChangeLabel: Double = 1.0
    var weightDelete: Double = 2.0
    var weightChangeLength: Double = 5.0
    val maxLengthChange = 1

    temperature = 0.001

    var numSamples = 0

    var debug = false

    def postAcceptanceHook(logAcceptanceProb: Double, d: DiffList): Unit = {
      //super.postAcceptanceHook(logAcceptanceProb,d)
      println("accepted " + d)
    }

    def objective: Model = obj

    //only process uppercase words
    override def preProcessHook(context: Label) = {
      val token = context.token
      if (token.isCapitalized) context else null
    }

    //override def proposalHook(proposal: Proposal) = null

    def proposals(label: Label) = {
      //if (filter(label)) return 0.0;
      val token = label.token
      val choices = new ArrayBuffer[Choice]
      val sentence: Sentence = token.seq.asInstanceOf[Sentence]
      if (debug) {
        println("Sentence: " + sentence.mkString(" "))
        println("Token: " + token.word)
        println("Token feats: " + token.values.mkString(","))
        println("Spans before: " + sentence.spans.mkString("{", ",", "}"))
        println("Labelled Before: " + sentence.map(t => t.word + "/" + t.label.value).mkString(" "))
      }
      choices += Choice(0.0, diff => {})

      // Choice 1: create a new span
      if (label.value == O) { //(spans.length == 0) {
        // assert(label.index equals NERGlobal.outsideStateIndex)
        for (label <- Domain[SpanLabel]) {
          choices += Choice(weightCreate, diff => {
            new Span(sentence, token.position, 1, label)(diff)
          })
        }
      } else {
        val spans = sentence.spansStartingAt(token.position).toSeq
        //  assert(spans.length != 0)
        //   assert(spans.length == 1)
        if (spans.length > 1) error("Overlapping spans not allowed!")
        if (spans.length > 0) {
          val span = spans.first
          val spanLabel = span.label
          // assert(spanLabel.label equals NERGlobal.label2SpanLabel(label.label))
          //Choice 2: delete the span
          choices += Choice(weightDelete, diff => {span.delete(diff)})
          //Choice 3: change the label of the span
          for (label <- Domain[SpanLabel]) {
            if (!(label equals spanLabel.label))
              choices += Choice(weightChangeLabel, diff => {
                spanLabel.set(label)(diff)
              })
          }
          //Choice 4: change the length of the span
          // trim, trimStart
          ///*
          if (span.length > 1) {
            choices += Choice(weightChangeLength, diff => {span.trimEnd(1)(diff)})
            //choices += Choice(weightChangeLength, diff => {span.trimStart(1)(diff)})
          }
          // append
          if (!span.isAtEnd && span.last.label.next.value == O)
          //      if (!span.isAtEnd && sentence.spans(span.successor(1).position).toSeq.length == 0)
            choices += Choice(weightChangeLength, diff => {span.append(1)(diff)})
          // merge with adjacent span and try both my and his label (shouldn't we try all labels here?)
          if (!span.isAtEnd && sentence(span.end + 1).label.value != O) {
            val other = sentence.spansStartingAt(span.end + 1).toSeq.first
            choices += Choice(weightChangeLength, diff => {span.merge(other, true)(diff)})
            choices += Choice(weightChangeLength, diff => {span.merge(other, false)(diff)})
          }

        }
        // prepend
        //      if (!span.isAtStart && sentence.spans(span.predecessor(0).position).toSeq.length == 0)
        //        choices += Choice(weightChangeLength, diff => {span.prepend(1)(diff)})
        //*/
      }
      //debugging
      //    println("Number of choices: " + choices.size)
      //    println(choices.map(_.scoredDiffList(model, obj)).mkString("\n"))

      val proposals = choices.map(_.asProposalWithScoreAcceptance(model, obj, temperature))
      //val proposals = choices.map(_.asProposal(model,obj))
      if (debug) println(proposals.mkString("\n"))

      proposals
    }

    override def postProcessHook(label: Label, difflist: DiffList): Unit = {
      numSamples += 1
      if (numSamples % 100 == 0) print(".")
      if (numSamples % 1000 == 0) println("%-6d".format(numSamples))
      if (debug) {
        val token = label.token
        val sentence = token.seq.asInstanceOf[Sentence]
        println("Spans after: " + sentence.spans.mkString("{", ",", "}"))
        println("Labelled after: " + sentence.map(t => t.word + "/" + t.label.value).mkString(" "))
        val errors = sentence.map(_.label).filter(l => l.index != l.trueIndex)
            .map(l => l.token + " " + l.trueValue + " " + l.value).mkString("\n")
        println("Errors: " + errors)
      }
    }

  }


  object NERModel {
    // Bias term just on labels
    val bias = new TemplateWithDotStatistics1[Label]
    // Transition factors
    val transition = new TemplateWithDotStatistics2[Label, Label] {
      def unroll1(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label) else Nil

      def unroll2(label: Label) = if (label.hasNext) Factor(label, label.token.next.label) else Nil
    }

    val trigram = new TemplateWithDotStatistics3[Label, Label, Label] {
      def unroll1(label: Label) = if (label.hasPrev && label.prev.hasPrev) Factor(label.token.prev.prev.label, label.token.prev.label, label) else Nil

      def unroll2(label: Label) = if (label.hasPrev && label.hasNext) Factor(label.token.prev.label, label, label.token.next.label) else Nil

      def unroll3(label: Label) = if (label.hasNext && label.next.hasNext) Factor(label, label.token.next.label, label.token.next.next.label) else Nil
    }
    // Factor between label and observed token
    val tokPrior = new TemplateWithDotStatistics2[Label, Token] {
      def unroll1(label: Label) = Factor(label, label.token)

      def unroll2(token: Token) = throw new Error("Token values shouldn't change")

    }
    // Skip chain factor
    /*val skipChain = new Template2[Label, Label] with DotStatistics1[Bool] {
      def unroll1 (label:Label) =
        // could cache this search in label.similarSeq for speed
        for (other <- allTokens; if label.token != other && label.token.word == other.word) yield
    Factor(label, other.label)
      def unroll2 (label:Label) = Nil // We handle symmetric case above
      def statistics(label1:Label, label2:Label) = Stat(Bool(label1.token.word==label2.token.word))
    }.init*/
    //  Factor between label, its token and the previous Label
    val unusedTemplate = new TemplateWithDotStatistics3[Label, Label, Token] {
      def unroll1(label: Label) = if (label.hasNext) Factor(label, label.next, label.token.next) else Nil

      def unroll2(label: Label) = if (label.hasPrev) Factor(label.prev, label, label.token) else Nil

      def unroll3(token: Token) = throw new Error("Token values shouldn't change")
    }

    val puncBoolTemplate = new Template3[Label, Label, Token] with DotStatistics1[Bool] {
      def unroll1(label: Label) = if (label.hasNext) Factor(label, label.next, label.token.next) else Nil

      def unroll2(label: Label) = if (label.hasPrev) Factor(label.prev, label, label.token) else Nil

      def unroll3(token: Token) = throw new Error("Token values shouldn't change")

      def statistics(label1: Label, label2: Label, token: Token) = Stat(Bool(label1.value == label2.value && token.values.contains("PUNCTUATION")))
    }.init


    // SPAN BASED FACTORS
    val spanStartsWithToken = new Template2[Span, SpanLabel] with DotStatistics2[Token, SpanLabel] {
      def unroll1(span: Span) = List(Factor(span, span.label))

      def unroll2(spanLabel: SpanLabel) = List(Factor(spanLabel.span.asInstanceOf[Span], spanLabel))

      def statistics(span: Span, spanLabel: SpanLabel) = Stat(span.first, spanLabel)
    }.init

    val spanEndsWithToken = new Template2[Span, SpanLabel] with DotStatistics2[Token, SpanLabel] {
      def unroll1(span: Span) = List(Factor(span, span.label))

      def unroll2(spanLabel: SpanLabel) = List(Factor(spanLabel.span.asInstanceOf[Span], spanLabel))

      def statistics(span: Span, spanLabel: SpanLabel) = Stat(span.last, spanLabel)
    }.init

    def createModel: Model = createModel(false)

    def createModel(spanBased: Boolean): Model = {
      // The model and objective
      val model = new Model
      model += bias
      model += transition
      model += tokPrior
      model += trigram
      //model += skipChain
      //model += unusedTemplate
      if (spanBased) {
        model += spanStartsWithToken
        model += spanEndsWithToken
      }
      model
    }

  }

  def convertIOB2BIO(iob: Seq[LabeledTokenSeq[Token, Label]]) = {
    iob.foreach(s => {
      s.foreach(t => {
        //println("Token "+t.word+"  "+t.label.value+"  "+t.label.value(0)+"  "+(t.label.value(0)=='I'))
        //print("  %-8s %-8s ".format(t.label.trueValue, t.label.value))
        if (t.label.value(0) == 'I' && (!t.hasPrev || t.prev.label.value.substring(1) != t.label.value.substring(1))) {
          val newValue = "B" + t.label.value.substring(1)
          t.label.value = newValue
          t.label.trueValue = newValue
        }
        //println("   x %-8s %-8s %s".format(t.label.trueValue, t.label.value, t.word))
      })
    })
  }

  def convertToSpanableSentences(sentences: Seq[LabeledTokenSeq[Token, Label]]) = {
    sentences.map(sentence => {val result = new Sentence; result ++= sentence; result})
  }

  def loadMalletConll(filename: String, maxSentences: Int): Seq[Sentence] = {
    var wordCount = 0
    val uniqueL = new HashSet[String]
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(new File(filename))
    var sentence = new Sentence
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence //; println("num words " + document.size + " num docs "+documents.size)
        if (sentences.size > maxSentences) {
          println("Loaded " + sentences.length + " sentences with " + wordCount + " words total from file " + filename)
          return sentences
        }
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        var label: String = null
        var word: String = null
        val feats = new ArrayBuffer[String]
        for (i <- 0 until fields.length) {
          if (i == 0) label = fields(i)
          else feats += fields(i)
          if (fields(i).startsWith("WORD"))
            word = fields(i).split("WORD=")(1)
        }
        sentence += new Token(word, feats, label)

        /*if(!uniqueL.contains(label))
          labelDomain.add(label)
        uniqueL.add(label)*/
        wordCount += 1
      }
    }
    println("Loaded " + sentences.length + " sentences with " + wordCount + " words total from file " + filename)
    sentences
  }


  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: SpanNER trainfile testfile")

    val model = new Model
    model += NERModel.bias
    model += NERModel.transition
    model += NERModel.tokPrior
    model += NERModel.trigram
    model += NERModel.spanEndsWithToken

    val objective = new Model
    objective += new TrueLabelTemplate[Label]

    // Read training and testing data.
    val trainSentences = loadMalletConll(args(0), 10000)
    //    LabeledTokenSeq.fromOWPL[Token, Label](Source.fromFile(args(0)),
    //      (word: String, lab: String) => new Token(word, Seq(), lab), "-DOCSTART-")
    val testSentences = loadMalletConll(args(1), 1000)
    //      LabeledTokenSeq.fromOWPL[Token, Label](Source.fromFile(args(1)),
    //      (word: String, lab: String) => new Token(word, Seq(), lab), "-DOCSTART-")

    //sync label domains
    syncLabelDomains

    //    convertIOB2BIO(trainSentences)
    //    convertIOB2BIO(testSentences)

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.map(_.label))
    val testLabels = testSentences.flatMap(_.map(_.label))
    (trainLabels ++ testLabels).foreach(_.set(O)(null))

    // Train for 5 iterations
    val learner = new SpanSampler(model, objective) with SampleRank with GradientAscentUpdates with ParameterAveraging
    learner.temperature = 0.1
    val predictor = new SpanSampler(model, objective)
    predictor.temperature = 0.1

    for (epoch <- 0 until 10) {
      trainSentences.foreach(s=>s.spans.foreach(_.delete(null)))
      learner.process(trainLabels, 5) // Train for 5 iterations through all Labels

      println
      
      // Predict, also by sampling, visiting each variable 3 times.
      testSentences.foreach(s=>s.spans.foreach(_.delete(null)))
      predictor.process(testLabels, 5)

      // Evaluate
      val evaluation = LabeledTokenSeq.segmentEvaluation[Token, Label](trainLabels)
      println
      println("TRAIN " + evaluation.f1)
      println("TEST  " + LabeledTokenSeq.segmentEvaluation[Token, Label](testLabels).f1)
    }
  }

}