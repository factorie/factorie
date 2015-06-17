package cc.factorie.app.nlp.embedding
import cc.factorie.variable.CategoricalDomain
import scala.io.Source
import java.io._
import java.util.zip.GZIPInputStream
import cc.factorie.app.strings.alphaSegmenter
import org.apache.commons.compress.compressors.CompressorStreamFactory
import scala.xml.pull._
import scala.collection.mutable.ArrayBuffer
import javax.xml.stream._
import javax.xml.stream.events._
import javax.xml.stream.util._
import java.text.NumberFormat
import java.util.Locale
import scala.util.Random

class VocabularyOptions extends cc.factorie.util.CmdOptions {
  val minCount = new CmdOption("min-count", 200, "INT", "Words with count smaller than this will be discarded.  Default is 200.")
  val skipProb = new CmdOption("skip-prob", 0.0, "DOUBLE", "The probabilty that each word string will be skipped in the indexing and counting.  Helps efficiently cull words occurring ~1 times.")
  val maxWikiPages = new CmdOption("max-wiki-pages", Long.MaxValue, "LONG", "Read no more than this number of Wikipedia pages.  Default is unlimited.")
  val input = new CmdOption("input", List("enwiki-latest-pages-articles.xml.bz2"), "TXTFILE", "Text files from which to read training data.  Works with *.txt.gz and Wikipedia enwiki*.xmlgz2.")
}

object Vocabulary {
  var wikipediaArticleCount = 0
  val opts = new VocabularyOptions()

  /** Consume all files and files in directories, tokenize, count, filter by count, and write out results. */
  def main(args:Array[String]): Unit = {
    opts.parse(args)
    // Recursively gather all files listed on command line
    val files = opts.input.value.flatMap(filename => recurseFiles(new File(filename)))
    // Set up a String map to gather counts
    val domain = new CategoricalDomain[String]
    domain.gatherCounts = true
    val printInterval = 100
    val random = new scala.util.Random(0)
    val skipThreshold = 1.0 - opts.skipProb.value
    var printAtCount = printInterval
    var wordCount = 0
    // From all files, segment contents into words, and count them
    files.foreach(file => {
      println("Vocabulary reading "+file.getName())
      for (line <- fileToStringIterator(file)) {
        if (wikipediaArticleCount >= printAtCount) {
          print("\r"+NumberFormat.getNumberInstance(Locale.US).format(wikipediaArticleCount)+" articles, "+NumberFormat.getNumberInstance(Locale.US).format(wordCount)+" tokens, "+NumberFormat.getNumberInstance(Locale.US).format(domain.size)+" vocabulary")
          //print(s"\r$wikipediaArticleCount articles\t$wordCount words") ; 
          printAtCount += printInterval
        }
        for (word <- stringToWords(line)) {
          //println("Vocabulary read word "+word)
          wordCount += 1
          if (skipThreshold == 0.0 || random.nextDouble() < skipThreshold) // Randomly sample to avoid words appearing only ~1 times.
            domain.index(new String(word)) // to avoid memory leak: http://stackoverflow.com/questions/15612157/substring-method-in-string-class-causes-memory-leak
        }
      }
    })
    println(s"Read $wikipediaArticleCount Wikipedia Articles")
    println(s"Read ${domain.countsTotal} tokens, ${domain.size} types.")
    // Get rid of words occurring less than 10 times
    domain.trimBelowCount(opts.minCount.value, preserveCounts = true)
    println(s"Trimed to ${domain.countsTotal} tokens, ${domain.size} types.")
    // Serialize the results
    println("Sorting...")
    val sorted = domain.categories.map(c => (domain.count(c), c)).sortBy(-_._1)
    println("...Sorted.")
    val out = new PrintWriter("vocabulary.txt")
    for ((count, word) <- sorted)
      out.println("%d %s".format(count, word))
    out.close()
    println("Done writing vocabulary.")
  }
  
  def stringToWords(string:String): Iterator[String] = {
    //alphaSegmenter(string).filter(word => !Stopwords.contains(word.toLowerCase))
    alphaSegmenter(string)
  }
  
  def fileToStringIterator(file:File, encoding:String = "UTF8"): Iterator[String] = {
    file.getName match {
      // Plain text file
      case name if name.endsWith(".txt") => Source.fromFile(file, encoding).getLines()
      // Compressed text
      case name if name.endsWith(".txt.gz") => {
        val lineIterator = Source.fromInputStream(new GZIPInputStream(new FileInputStream(file)), encoding).getLines()
        //.foldLeft(new ArrayBuffer[StringBuffer] += new StringBuffer)((a,s) => { if (s.length > 0) a.last.append(s) else a += new StringBuffer; a }).map(_.toString).iterator
        new Iterator[String] {
          def hasNext: Boolean = lineIterator.hasNext
          def next(): String = {
            val sb = new StringBuffer
            var emptyLine = false
            while (lineIterator.hasNext && !emptyLine) {
              val line = lineIterator.next()
              if (line.length > 0) sb.append(line)
              else emptyLine = true
            }
            sb.toString
          }
        }
      }
      case name if name.startsWith("enwiki") && name.endsWith(".xml.bz2") =>
        val docIterator = cc.factorie.app.nlp.load.LoadWikipediaPlainText.fromCompressedFile(file, opts.maxWikiPages.value)
        new Iterator[String] {
          def hasNext: Boolean = docIterator.hasNext
          def next(): String = { wikipediaArticleCount += 1; val doc = docIterator.next(); /*println(doc.name);*/ doc.string }
        }
      // bz2 compress wikipedia XML
      case name if name.startsWith("deprecated enwiki") && name.endsWith(".xml.bz2") => {
        val input = new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(file))
        val xml = new scala.xml.pull.XMLEventReader(Source.fromInputStream(input))
        //val xml = XMLInputFactory.newInstance().createXMLEventReader(new InputStreamReader(input))
        val cleaningRegex = List(
            "\\{\\{[^\\}]*\\}\\}", // Remove everything {{inside}}
            "\\{\\|[^\\}]*\\|\\}", // Remove everything {|inside|}
            "(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
            "\\[\\[(?:File|Image):[^\\]]+\\|", // Remove [[File:WilliamGodwin.jpg|left|thumb|[
            "wikt:|nbsp;|ndash;|br/",
            "Category:"
            ).mkString("|").r
        new Iterator[String] {
          def hasNext: Boolean = {
            val result = xml.hasNext && wikipediaArticleCount < opts.maxWikiPages.value // Artificially stopping early
            if (!result) { /*xml.close();*/ xml.stop(); input.close(); println(getClass.getName+": fileToStringIterator closing.") }
            result
          }
          def next(): String = {
            var done = false
            var insidePage = false
            var insideText = false
            var insideRef = false
            var insideComment = false
            val sb = new StringBuffer
            while (xml.hasNext && !done) {
              xml.next() match {
                //case e => println(e)
                case EvElemStart(_, "page", _, _) => { insidePage = true }
                case EvElemEnd(_, "page") => { insidePage = false; done = true }
                case EvElemStart(_, "text", _, _) => { insideText = true }
                case EvElemEnd(_, "text") => { insideText = false; done = true }
                case EvText(t) if insideText => {
                  if (t.startsWith("!--") && !t.endsWith("--")) insideComment = true
                  else if (t.endsWith("--")) insideComment = false
                  else if (t.startsWith("ref") && !t.endsWith("/")) insideRef = true
                  else if (t == "/ref") insideRef = false
                  else if (!insideRef && !insideComment && !t.startsWith("ref ") && !t.startsWith("#REDIRECT")) { sb append t; sb append ' ' }
                }
                case _ => // ignore all other tags
              }
            }
            var s = cleaningRegex.replaceAllIn(sb.toString, " ")
            //println("Vocabulary.fileToStringIterator "+s)
            s = s.trim; if (s.length > 0) { wikipediaArticleCount += 1; return s } else { /*println(s"Skipping at $wikipediaArticleCount");*/ return next() }
            s
          }
        }
      }
      case _ => throw new Error("Unknown suffix on document name "+file.getName())
    }
  }

  // Recursively descend directory, returning a list of files.
  def recurseFiles(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new scala.collection.mutable.ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= recurseFiles(entry)
    }
    result
  }

}
