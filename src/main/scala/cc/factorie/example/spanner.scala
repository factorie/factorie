package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import cc.factorie.util.Implicits._
import cc.factorie._

object NerModel extends Model {
  class Label(labelStr: String, val span: Document#Span) extends cc.factorie.Label(labelStr) {
    def trueScore = span.trueScore // Label trueScores are managed by the Document#Span, not here
    // This is necessary because we don't know the true label value at the time we create a Span's Label.
  }

  val Capitalized = "^[A-Z].*".r
  val Numeric = "^[0-9]+$".r
  val Punctuation = "[,\\.;:?!()]+".r
  class Token(val word: String) extends BinaryVectorVariable[String] with VarInSeq[Token] with MultiProposer {
    this += "W=" + word
    if (Capitalized.findFirstMatchIn(word) != None) this += "CAPITALIZED"
    if (Numeric.findFirstMatchIn(word) != None) this += "NUMERIC"
    if (Punctuation.findFirstMatchIn(word) != None) this += "PUNCTUATION"
    def document = seq.asInstanceOf[Document]

    def spans = document.spans(this.position) // Not used
    def trueSpan = document.trueSpans.get(this)

    override def toString = word

    def clear = indxs.clear

    def multiPropose(model:Model, objective:Model, difflist: DiffList): Seq[Proposal] = {
      if (word.first.isLowerCase) return Nil // Don't both trying to create spans starting with lowercase words
      //print("."); System.out.flush
      val spans = document.spans(this.position).toSeq
      //			val span = if (spans.length > 0) spans.first else null
      val proposals = new ArrayBuffer[Proposal]
      //proposals += new AutoProposal(diff => diff += new Diff { def variable = span; def redo = {}; def undo = {}}) // Include no change as one of the proposals
      proposals += new AutoProposal(model, objective, diff => {})
      //case class Proposal(modelScore:Double, trueScore:Double, diff:DiffList, span:Document#Span) extends factorie.Proposal
      if (spans.length == 0) {
        // Add a unit-length span, choosing the label that scores best
        for (labelvalue <- Domain[Label]) {
          // TODO Why not just: new AutoProposal(document.addSpan(this.position, 1, label.entry))
          proposals += new AutoProposal(model, objective, diff => document.addSpan(this.position, 1, labelvalue)(diff))
        }
        //println("Token multiPropose span="+proposals.max(_.modelScore).diff.first.variable.asInstanceOf[Document#Span].phrase)
      } else {
        for (span <- spans) {
          // There exists a span
          if (span.canAppend(1)) // Try lengthening
            proposals += new AutoProposal(model, objective, diff => span.append(1)(diff))
          if (span.length > 1) // Try shortening
            proposals += new AutoProposal(model, objective, diff => span.trimEnd(1)(diff))
          proposals += new AutoProposal(model, objective, diff => span.delete(diff)) // Try removing the span
          //println("Delete diff:" + proposals.last.diff)
          for (labelvalue <- Domain[Label]; if (labelvalue!= span.label.value)) // Try changing its label
            proposals += new AutoProposal(model, objective, diff => span.label.set(labelvalue)(diff))
        }
      }
      if (false) {
        val firstindex = Math.max(0, this.position - 5)
        val lastindex = Math.min(seq.length, this.position + 5)
        for (i <- firstindex until lastindex) {
          if (i == position) print("*")
          print(document(i).word + " ")
        }
        println()
      }
      //			println("orig " + spans)
      //			for (proposal <- proposals) {
      //				proposal.diff.redo
      //				val spn = if (proposal.diff.length > 0) proposal.diff.first.variable else null
      //				println("truescore=" + proposal.trueScore + " modelscore=" + proposal.modelScore + " diff=" + proposal.diff + " " + spn)
      //				proposal.diff.undo
      //			}
      //			println()

      if (false) {
        val trueProposal = proposals.max(_ trueScore)
        val modelProposal = proposals.max(_ modelScore)
        trueProposal.diff.redo
        val trueSpan = if (trueProposal.diff.length > 0) trueProposal.diff.first.variable else null
        println("true  truescore=" + trueProposal.trueScore + " modelscore=" + trueProposal.modelScore + " diff.length=" + trueProposal.diff.length + " " + trueSpan)
        trueProposal.diff.undo
        modelProposal.diff.redo
        val modelSpan = if (modelProposal.diff.length > 0) modelProposal.diff.first.variable else null
        println("model truescore=" + modelProposal.trueScore + " modelscore=" + modelProposal.modelScore + " diff.length=" + modelProposal.diff.length + " " + modelSpan)
        modelProposal.diff.undo
        println()
      }

      // Return the list of proposals
      proposals
    }

  }

  class Document extends VariableSeqWithSpans[Token] {
    type SpanType = Span
    val trueSpans = new HashMap[Token, TrueSpan]
    class TrueSpan(initStart: Int, initLength: Int, labelStr: String) extends ImmutableSpanVariable(Document.this, initStart, initLength) {
      val label = new Label(labelStr, null)
      //trueSpans(Document.this(initStart)) = this
      def trueScore = 0.0
      //override def phrase = if (length == 1) this.first.word else this.foldLeft("")(_ + " " + _.word).drop(1) // Span as a string

      def equalsSpan(span: Span) = {
        span != null && initStart == span.start && initLength == span.length && labelStr == span.label.value
      }

      //override def toString =
    }
    def addTrueSpan(token: Token, initStart: Int, initLength: Int, labelStr: String): TrueSpan = {
      val s = new TrueSpan(initStart, initLength, labelStr);
      trueSpans(token) = s;
      s
    }

    def addSpan(initStart: Int, initLength: Int, labelStr: String)(implicit d: DiffList): Span = {
      val s = new Span(initStart, initLength, labelStr)(d);
      s
    }

    override def trueScore = {
      val truePositives = spans.filter(span => trueSpans.values.exists(trueSpan => trueSpan equalsSpan span))

      val trueCount: Double = trueSpans.size
      val guessCount: Double = spans.length
      val recall = if (trueCount > 0) truePositives.length / trueCount else 1.0
      val precision = if (guessCount > 0) truePositives.length / guessCount else 1.0
      val f1 = if (recall > 0.0 || precision > 0.0) 2 * recall * precision / (recall + precision) else 0.0
      //println("F1: " + f1)
      if (precision < 1.0 || recall < 1.0) {
        println("True spans: " + trueSpans.values.mkString(","))
        println("Guess spans: " + spans)
        println("TP: " + truePositives)
      }
      f1
    }

    class Span(initStart: Int, initLength: Int, labelStr: String)(implicit d: DiffList) extends SpanVariableInSeq(initStart, initLength)(d) {
      override def diffIfNotPresent = true

      val label = new Label(Domain[Label].get(0), this)
      //d += new CreationDiff(label)
      label.set(labelStr)(d)
      override def toString = {if (present) "" else "!"} + "Span(" + this.phrase + "=" + this.label.value + ")"

      def trueScore = {
        var ret = 0.0
        //def phrase = if (length == 1) this.first.word else this.foldLeft("")(_ + " " + _.word).drop(1) // Span as a string
        val trueSpan = trueSpans.getOrElse(this.first, null)
        val correctBoundaries = trueSpan != null && trueSpan.start == this.start && trueSpan.length == this.length
        val correctLabel = trueSpan != null && trueSpan.label === this.label
        //				println("truespan: " + trueSpan)
        //				println("correctBoundaries: " + correctBoundaries)
        //				println("correctLabel: " + correctLabel)
        if (this.present) {
          if (trueSpan == null) {
            ret -= 5.0 // false-positive span
          } else {
            if (correctBoundaries) {
              if (correctLabel) ret += 10.0 // correct boundaries and label
              else ret += 5.0 // correct boundaries, incorrect label
            } else {
              if (correctLabel) ret += 4.0 / Math.abs(trueSpan.length - this.length) // correct start and label
              else ret += 1.1 / Math.abs(trueSpan.length - this.length) // correct start, incorrect label
            }
          }
        } else { // hypothesized span not present
          if (trueSpan == null)
            ret += 1.0 // no true span, and no hypothesized span
          else
            ret -= 3.0 // false negative
        }
        //println("Span "+this+" trueSpan "+trueSpan+" trueScore "+ret)
        ret
      }
    }
    
  } // end of Document
  
      // Temporary silliness until I make not all Templates require "vector", and I implement a real scoring template for coref.
    val objective = new Model(new Template1[Document#Span] with ExpTemplate {
      def learningMethod = "unknown"
    	import scalala.tensor.Vector
    	import scala.reflect.Manifest
    	override def score(s:Stat) = s.s1.trueScore
    	type StatType = Stat
  		case class Stat(s1:Document#Span) extends super.Stat with Iterable[Stat] {
    		def vector : Vector = null
    	} 
    	def statistics(v1:Document#Span): Iterable[Stat] = Stat(v1)
    	type S = Stat
    	def init(implicit m1:Manifest[Document#Span]) : this.type = { statClasses += m1.erasure.asInstanceOf[Class[IndexedVariable]]; statClasses.freeze; this }  
    	init
    })




  // Bias term just on labels
  //val labelTemplate = new TemplateWithNeighbors1[Label] with PerceptronLearning { addModelTemplate(this) }


  trait SpannerLearner extends PerceptronLearning {

  }

  abstract class LabelTokenTemplate extends Template2[Document#Span, Label] with ExpStatistics2[Label, Token] with SpannerLearner {

    //gatherAverageWeights = true
    //useAverageWeights = true


    //
    //		override type S = Suff
    //
    //		case class Suff(label:Label,token:Token, present:Boolean) extends super.Suff(label,token) {
    //			override def vector = if (present) vector else {val v = super.vector; v *= -1.0; v}
    //		}

    def printWeights() {
      var labelVar = new Label("ORG", null)
      var tokenVar = new Token(".")
      for (label <- Domain[Label]; token <- statDomains(1)) {
        //for (label <- spanTokensTemplate.sDomains(0); token <- spanTokensTemplate.sDomains(1)) {
        labelVar.set(label)(null)
        tokenVar.clear
        tokenVar += token.toString
        print(labelVar + " " + token + ": ")
        println(weights dot Stat(labelVar, tokenVar).vector)
        //println(label + "," + token + ": " + (spanTokensTemplate.weights dot spanTokensTemplate.Suff(label,token).vector))
      }
    }

  }

  // Label with all words in Span
  val spanTokensTemplate = new LabelTokenTemplate {

    def unroll1(span: Document#Span) = Factor(span, span.label)

    def unroll2(label: Label) = Factor(label.span, label)

    def statistics(span: Document#Span, label: Label) = if (span.present) {for (token <- span) yield Stat(span.label, token)} else Nil
    //def sufficient(span: Document#Span, label: Label) = for (token <- span) yield Suff(span.label, token, span.present)

    NerModel += this

  }.init

  // All words not in a Span
  //  val nonSpanTokensTemplate = new Template1[Token] with Neighbors1[Document#Span] with PerceptronLearning {
  //    val nDomains = Domains[Document#Span]
  //    def sufficient(span:Document#Span) = if (!span.present) { for (token <- span) yield Suff(token) } else Nil
  //    addModelTemplate(this)
  //  }
  //
  // Label with word at start of Span
  val startTokenTemplate = new LabelTokenTemplate {

    def unroll1(span: Document#Span) = Factor(span, span.label)

    def unroll2(label: Label) = Factor(label.span, label)

    def statistics(span: Document#Span, label: Label) = if (span.present) Stat(span.label, span.first) else Nil
    //def sufficient(span: Document#Span, label: Label) = Suff(span.label, span.first, span.present)
    NerModel += this
  }.init

  // Label with word at end of Span
  val endTokenTemplate = new LabelTokenTemplate {

    def unroll1(span: Document#Span) = Factor(span, span.label)

    def unroll2(label: Label) = Factor(label.span, label)

    def statistics(span: Document#Span, label: Label) = if (span.present) Stat(span.label, span.last) else Nil
    //def sufficient(span: Document#Span, label: Label) = Suff(span.label, span.last, span.present)
    NerModel + this
  }.init
  //
  //  // Label with word before the Span
  val prevTokenTemplate = new Template1[Document#Span] with ExpStatistics2[Label, Token] with SpannerLearner {

    def statistics(span: Document#Span) = if (span.present && !span.isAtStart) Stat(span.label, span.predecessor(1)) else Nil
    NerModel += this
  }.init
  //

  // Label with word after the Span
  val nextTokenTemplate = new Template1[Document#Span] with ExpStatistics2[Label, Token] with SpannerLearner {

    def statistics(span: Document#Span) =
      if (span.present && !span.isAtEnd) Stat(span.label, span.successor(1)) else Nil
    NerModel += this
  }.init
  //
  //  val spanIncomingTransitionTemplate = new Template2[Label,Label] with Neighbors1[Document#Span] with PerceptronLearning {
  //    val nDomains = Domains[Document#Span]
  //    def sufficient(span:Document#Span) =
  //      if (span.present && !span.isAtStart) for (span2 <- span.predecessor(1).spans) yield Suff(span2.label, span.label)
  //      else Nil
  //    addModelTemplate(this)
  //  }
  //
  //  val spanOutgoingTransitionTemplate = new Template2[Label,Label] with Neighbors1[Document#Span] with PerceptronLearning {
  //    val nDomains = Domains[Document#Span]
  //    def sufficient(span:Document#Span) =
  //      if (span.present && !span.isAtEnd) for (span2 <- span.successor(1).spans) yield Suff(span.label, span2.label)
  //      else Nil
  //    addModelTemplate(this)
  //  }
  //
  //  // Label with Span length
  //  class LengthLabelTemplate(length:Int) extends Template1[Label] with Neighbors1[Document#Span] with PerceptronLearning {
  //    val nDomains = Domains[Document#Span]
  //    def sufficient(span:Document#Span) = if (span.length == length) Suff(span.label) else Nil
  //    addModelTemplate(this)
  //  }
  //  for (i <- 1 to 5) new LengthLabelTemplate(i)

  // Test for overlapping Spans.  Expensive?
  // addModelTemplate new Template1[Label] with Neighbors1[Document#Span] with PerceptronLearning


  /*
  def predict(labelSeqs:Iterable[Seq[Label]]) = {
    val oldTemplates = modelTemplates.toList
    clearModelTemplates
    addModelTemplate(labelTemplate)
    addModelTemplate(labelTokenTemplate)
    val sampler = new GibbsSampler
    sampler.temperature = 0.001 // Low temperature
    sampler.useQueue = true
    labelSeqs.foreach(seq => sampler.sample(seq, 1))
    addModelTemplates(oldTemplates)
    labelSeqs.foreach(seq => sampler.sample(seq, 1))
  }
*/

  //def parseConll(file:File) : Seq[Seq[Label]] = { }

}


object SpannerDemo {
  val model = NerModel
  import model._

  def load(fileName: String): Seq[Document] = {
    //val dataFile = "/Users/mccallum/research/data/ie/ner2003/eng.train"

    //val fileName = "/Users/riedel/corpora/conll03/eng.train"
    val OTHER = "O" // This is the way that the CoNLL data file signifies the background "OTHER" tag.
    //val otherLabel = new Label(OTHER) // IndexedDomain[Label].index(OTHER) // Make sure that "O" is in Label's IndexDomain
    println("Reading data from " + fileName)
    var documents = new ArrayBuffer[Document]

    // Read data and create Variables
    val source = Source.fromFile(fileName)
    var document = new Document // TODO actually a sentence
    var prevLabelStr = "__NoLabel__"
    var spanStart = 0
    var tokenStart: Token = null
    for (line <- source.getLines) {
      if (line.length < 2) {

        if (prevLabelStr != OTHER && spanStart != -1 && tokenStart != null) {
          document.addTrueSpan(tokenStart, spanStart, document.length - spanStart, prevLabelStr.drop(2)) // drop(2) to remove "I-" prefix
        }
        // newline indicates document break
        documents += document
        //println("num words " + document.size + " num docs "+documents.size)
        document = new Document
        prevLabelStr = "__NoLabel__"
        spanStart = -1
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val labelStr = fields(3).stripLineEnd
        val token = new Token(word)
        document += token
        if (prevLabelStr != labelStr) {
          if (spanStart != -1 && prevLabelStr != OTHER)
            document.addTrueSpan(tokenStart, spanStart, document.length - 1 - spanStart, prevLabelStr.drop(2)) // drop(2) to remove "I-" prefix
          //              document.addTrueSpan(token.prev, spanStart, document.length - 1 - spanStart, prevLabelStr.drop(2)) // drop(2) to remove "I-" prefix
          prevLabelStr = labelStr
          spanStart = document.length - 1
          tokenStart = token
        }
        //println(document.last)
      }
    }
    documents


  }

  def main(args: Array[String]): Unit = {
    val myWorld = new World {
      import model._
      var trainFilename = "/Users/riedel/corpora/conll03/eng.train"
      var testFilename = "/Users/riedel/corpora/conll03/eng.testa" 
      if (args.length == 2) {
        trainFilename = args(0)
        testFilename = args(1)
      }
      val train = load(trainFilename)
      val test = load(testFilename)

      val docs = train.take(500)
      val testDocs = test.take(100)

      // Print all the true spans
      docs.foreach(doc => doc.trueSpans.values.foreach(span => println(span.phrase + " " + span.label.value)))
      //System.exit(-1)

      //var tokens = docs.flatMap(doc => doc) //.take(11) // TODO for now, just look at the first 500 words
      var tokens : Seq[Token] = docs.flatMap(doc => doc) //.take(11) // TODO for now, just look at the first 500 words
      var testTokens : Seq[Token] = test.flatMap(doc => doc)
      var sampler = new MHPerceptronLearner[Token](model, model.objective) {
        var tokenIterator = tokens.elements

        def propose(t:Token, difflist: DiffList): Double = {
          if (!tokenIterator.hasNext)
            tokenIterator = tokens.elements
          val token = tokenIterator.next
          token.propose(model, difflist)
        }
      }

      for (i <- 1 to 20) {
        sampler.process(tokens,1)
        println(i + " Wrong spans")
        //docs.foreach(doc => doc.spans.foreach(span => println(span.label.value.entry + " " + span.phrase)))
        println("train accuracy: " + model.aveScore(docs))
        //				println("span token Weights")
        //				spanTokensTemplate.printWeights
        //				println("start token Weights")
        //				startTokenTemplate.printWeights
        //				println("end token Weights")
        //				endTokenTemplate.printWeights

        sampler.process(testTokens,1)
        println("test accuracy: " + model.aveScore(testDocs))


      }

      //			println("span token Weights")
      //			spanTokensTemplate.printWeights
      //			println("start token Weights")
      //			startTokenTemplate.printWeights
      //			println("end token Weights")
      //			endTokenTemplate.printWeights

      // Set label variables to random values
      //for (document <- docs; label <- document) label.set(label.domain.randomValue)(null)

      // Make a test/train split
      //val (testdocs, traindocs) = docs.shuffle(random).split(0.5)

      // Make some of the training data unlabeled
      //trainVariables.foreach(v => if (random.flip(0.3)) v.trueValue = null)
      //Console.println ("Initial test accuracy = "+ worldAccuracy(testVariables))


      //spanTokensTemplate.sDomains
      //modelTemplates.foreach (f=> )

      //Console.println(ArgumentBias.weights)
      //			for (i <- 0 until IndexedDomain[PredicateArgumentAffinityVector].size) {
      //				println(IndexedDomain[PredicateArgumentAffinityVector].get(i) + "\t" + PredicateArgumentFactor.weights(i))
      //			}


      // Show the parameters
      //Console.println ("Printing parameters of factors "+modelTemplates.size)
      //modelTemplates.foreach (f => Console.println(f.weights.toList))
    }
    0;
  }
}

