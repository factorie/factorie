package cc.factorie.app.nlp.event

import cc.factorie.util.{FileUtils, CmdOptions}
import java.io.{BufferedWriter, FileWriter, File, PrintWriter}
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.segment.{DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter, PlainTokenNormalizer, DeterministicTokenizer}
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import scala.collection.mutable.HashSet
import scala.collection.mutable
import scala.io.Source
import java.net.URL


/**
 * Created by beroth on 3/31/15.
 */


/*
object BBNEventStringMatchingLabelerComponent extends NLPComponent {
  val lists = Seq(
    this.getClass.getResource("/CHARGES"),
    this.getClass.getResource("/JOB_TITLE")
    //    this.getClass.getResource("/MONEY")
  )
  val labels = Seq("CHARGES", "JOB_TITLE")//, "MONEY")
  object TacStringMatchingLabeler extends StringMatchingLabeler(lists, labels)
  def process1(doc: Document): Document = {
    print(s"\tStringMatching[${doc.name}]\n")
    TacStringMatchingLabeler.process(doc)
  }
}
*/

object EventNLPComponents extends CompoundDocumentAnnotator(
  Seq(
    DeterministicNormalizingTokenizer,
    DeterministicSentenceSegmenter,
    new OntonotesForwardPosTagger(new URL("file:///iesl/canvas/sullivan/dev/all-models/src/main/resources/cc/factorie/app/nlp/pos/OntonotesForwardPosTagger.factorie")),
    new BBNEventChainNer(new URL("file:///iesl/canvas/sullivan/dev/factorie/BBNTagger_optimized.factorie")),
    BilouEventStringMatchingLabeler,
    BBNEventPatternBasedEventFinder
  )
)

class ProcessEventCorpusOpts extends CmdOptions{
  val dataDirs = new CmdOption("data-dirs", Nil.asInstanceOf[List[String]], "FILENAME...", "List of directories containing (only) data files in sgml format.")
  val dataFile = new CmdOption("data-files", Nil.asInstanceOf[List[String]], "FILENAME...", "List of files in sgml format.")
  val keyFile = new CmdOption("key-file", "", "FILENAME...", "Key file.")
}

object ProcessEventCorpus {

  val whitespace_chars = "" +
    "\\u0009" + // CHARACTER TABULATION
    "\\u000A" + // LINE FEED (LF)
    "\\u000B" + // LINE TABULATION
    "\\u000C" + // FORM FEED (FF)
    "\\u000D" + // CARRIAGE RETURN (CR)
    "\\u0020" + // SPACE
    "\\u0085" + // NEXT LINE (NEL)
    "\\u00A0" + // NO-BREAK SPACE
    "\\u1680" + // OGHAM SPACE MARK
    "\\u180E" + // MONGOLIAN VOWEL SEPARATOR
    "\\u2000" + // EN QUAD
    "\\u2001" + // EM QUAD
    "\\u2002" + // EN SPACE
    "\\u2003" + // EM SPACE
    "\\u2004" + // THREE-PER-EM SPACE
    "\\u2005" + // FOUR-PER-EM SPACE
    "\\u2006" + // SIX-PER-EM SPACE
    "\\u2007" + // FIGURE SPACE
    "\\u2008" + // PUNCTUATION SPACE
    "\\u2009" + // THIN SPACE
    "\\u200A" + // HAIR SPACE
    "\\u2028" + // LINE SEPARATOR
    "\\u2029" + // PARAGRAPH SEPARATOR
    "\\u202F" + // NARROW NO-BREAK SPACE
    "\\u205F" + // MEDIUM MATHEMATICAL SPACE
    "\\u3000"

  val whitespace_charclass = "["  + whitespace_chars + "]"

  def main(args: Array[String]) {
    val opts = new ProcessEventCorpusOpts
    opts.parse(args)

    /* Load data files */
    if (opts.dataDirs.wasInvoked && opts.dataFile.wasInvoked) {
      println("Please specify either a list of data directories or files but not both.")
      System.exit(1)
    }
    assert(opts.dataDirs.wasInvoked || opts.dataFile.wasInvoked)
    println(opts.dataDirs.value.mkString("\n"))


    val dataFileList = if (opts.dataDirs.wasInvoked) opts.dataDirs.value.flatMap(FileUtils.getFileListFromDir(_)) else opts.dataFile.value
    val docs = dataFileList.flatMap(LoadTac.fromFilename(_))


    val responses = new mutable.HashSet[EventAnswer]()
    var i = 0
    docs.par.map(doc => {
      EventNLPComponents.process(doc)
      i += 1
      doc
    }).seq.foreach{ doc =>
      /*
      println("processing results for doc %s" format doc.name)
      Option(doc.attr[BBNEventNerSpanBuffer]) match {
        case Some(buf) => buf.size match {
          case 0 => println("\tspan buffer is empty")
            val tags = doc.tokens.map(_.attr.exactly[BilouBBNEventNerTag])
            println("\t\t%d tags were non O" format tags.count(_.categoryValue != "O"))
            tags.groupBy(_.baseCategoryValue).mapValues(_.size) foreach {case (t,c) => println("\t\t\t%s appeared %d times".format(t, c))}
          case otw => println("\tspan buffer has %d elements".format(otw))
            println("\t\tspan labels")
            buf.groupBy(_.label.categoryValue).mapValues(_.size ) foreach {case (t,c) => println("\t\t\t%s appeared %d times".format(t, c))}
            val tags = doc.tokens.map(_.attr.exactly[BilouBBNEventNerTag])
            println("\t\t%d tags were non O" format tags.count(_.categoryValue != "O"))
            tags.groupBy(_.baseCategoryValue).mapValues(_.size) foreach {case (t,c) => println("\t\t\t%s appeared %d times".format(t, c))}
        }
        case None => println("\tdid not have EventSpanBuffer")
      }

      val wrt = new BufferedWriter(new FileWriter(doc.name + ".owpl"))
      wrt write "Token\tLemma\tSentenceIdx\tPOS\tNER"
      wrt.newLine()
      doc.tokens foreach { token =>
        wrt write Seq(token.string, token.lemmaString, token.sentence.indexInSection, token.posTag.categoryValue, token.nerTag.categoryValue).mkString("\t")
        wrt.newLine()
      }
      wrt.flush()
      wrt.close()

      */
      doc.attr[MatchedEventPatterns].foreach{p =>
        val ans = EventAnswer(doc.name, p.span.string, p.pattern.eventRole.event, p.pattern.eventRole.role)
        responses.add(ans)
      }
    }

    val key = new mutable.HashSet[EventAnswer]()
    Source.fromFile(opts.keyFile.value).getLines().map(line => new Line(line)).foreach(l =>
      key.add(EventAnswer(l.docid, l.surface, l.event, l.role)))

    val (p,r,f1) = evaluate(key.toSet, responses.toSet)

    println(s"precision=$p recall=$r f1=$f1")
  }

  case class EventAnswer(docid: String, surface: String, event: String, role: String)

  def evaluate(key:Set[EventAnswer], responses:Set[EventAnswer]): (Double, Double, Double) = {
    val correct = responses.count(key.contains).toDouble
    println(s"# responses: ${responses.size} # answers: ${key.size}")
    println(s"# correct responses: $correct")
    val precision = correct/responses.size.toDouble
    val recall = correct/key.size.toDouble
    //    println(s"prec=$precision rec=$recall")
    val f1 = if ((precision+recall) > 0) 2*((precision*recall)/(precision+recall)) else 0
    (precision, recall, f1)
  }

  class Line(line:String) {
    val parts = line.split('\t')
    var docid = ""; var role = ""; var event = ""; var surface = ""; var casOffset = ""
    var startOffset = ""; var endOffset = ""
    var eventCorrect = ""; var roleCorrect = ""; var casCorrect = ""

    parts.length match {
      //line from responses
      case 11 => {
        docid = parts(1)
        role = parts(3)
        event = parts(2)
        surface = parts(4)
      }
      //full line from answer key
      case 18 => {
        docid = parts(1)
        event = parts(2)
        role = parts(3)
        surface = parts(4)
        casOffset = parts(5)
        val casparts = casOffset.split('-')
        startOffset = casparts(0); endOffset = casparts(1)
        eventCorrect = parts(12)
        roleCorrect = parts(13)
        casCorrect = parts(14)

      }
      case _ =>
    }
    override def toString(): String = s"$docid\t$role\t$event\t$surface"
    def toFullString(): String = parts.mkString("\t")
  }



}

