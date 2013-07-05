package cc.factorie.app.nlp

import java.io._
import cc.factorie.util.ClasspathURL
import cc.factorie.app.nlp.parse._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

/** A command-line driver for DocumentAnnotators.
    Launch on the command-line, specifying which NLP pipeline steps you want, 
    then it listens on a socket port for new text input, and replies with annoted text, one word per line.
    @author Andrew McCallum */
object NLP {
  val annotators = new scala.collection.mutable.ArrayBuffer[DocumentAnnotator]
  var logStream = System.err
  //val interpreter = new scala.tools.nsc.IMain
  def main(args:Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val socket = new CmdOption("socket", 3228, "SOCKETNUM", "On which socket number NLP server should listen.")
      val encoding = new CmdOption("encoding", "UTF-8", "ENCODING", "Character encoding for reading document text, such as UTF-8")
      val logFile = new CmdOption("log", "-", "FILENAME", "Send logging messages to this filename.")
      // TODO All these options should be replaced by something that will interpret object construction code. -akm
      val token = new CmdOption("token", null, "", "Segment Document into Tokens but not Sentences") { override def invoke = annotators += cc.factorie.app.nlp.segment.ClearTokenizer }
      val token1 = new CmdOption("token1", null, "", "Segment Document into Tokens (but not Sentences) by simple regex") { override def invoke = annotators += cc.factorie.app.nlp.segment.RegexTokenizer}
      val sentence = new CmdOption("sentence", null, "", "Segment Document into Tokens and Sentences") { override def invoke = annotators += cc.factorie.app.nlp.segment.ClearSegmenter }
      val sentence1 = new CmdOption("sentence1", null, "", "Segment pre-tokenized Document into Sentences by simpler regex") { override def invoke = annotators += cc.factorie.app.nlp.segment.SentenceSegmenter }
      val tnorm = new CmdOption("tnorm", null, "", "Normalize token strings") { override def invoke = annotators += cc.factorie.app.nlp.segment.SimplifyPTBTokenNormalizer }
      val pos1 = new CmdOption[String]("pos1", null, "URL", "Annotate POS") { override def invoke = { if (value ne null) System.setProperty(classOf[pos.POS1].getName, value);
        annotators += cc.factorie.app.nlp.pos.POS1 } }
      val wnlemma = new CmdOption("wnlemma", "classpath:cc/factorie/app/nlp/wordnet/WordNet", "URL", "Annotate lemma using WordNet's lemmatizer.") { override def invoke = annotators += cc.factorie.app.nlp.lemma.WordNetLemmatizer }
      val npchunk1 = new CmdOption("mention1", null, "", "Annotate noun mentions") { override def invoke = annotators += cc.factorie.app.nlp.mention.NPChunker1 }
      val mention2 = new CmdOption("mention2", null, "", "Annotate noun mentions") { override def invoke = annotators += cc.factorie.app.nlp.mention.ParseBasedMentionFinding }
      val ner1 = new CmdOption[String]("ner1", null, "URL", "Annotate CoNLL-2003 NER") { override def invoke = { if (value ne null) System.setProperty(classOf[ner.NER1].getName, value); annotators += cc.factorie.app.nlp.ner.NER1 } }
      val ner2 = new CmdOption[String]("ner2", null, "URL", "Annotate Ontonotes NER")  { override def invoke = { if (value ne null) System.setProperty(classOf[ner.NER2].getName, value); annotators += cc.factorie.app.nlp.ner.NER2 } }
      //val parser1 = new CmdOption("parser1", ClasspathURL[DepParser1](".factorie").toString, "URL", "Annotate dependency parse with a simple shift-reduce transition-based model.") { override def invoke = { System.setProperty(classOf[DepParser1].getName, value); annotators += cc.factorie.app.nlp.parse.DepParser1 } }
      val parser1 = new CmdOption[String]("parser1", null, "URL", "Annotate dependency parse with a simple shift-reduce transition-based model.") { override def invoke = { annotators += new cc.factorie.app.nlp.parse.DepParser1(ClasspathURL[DepParser1](".factorie")) } }
      val parser2 = new CmdOption[String]("parser2", null, "URL", "Annotate dependency parse with a state-of-the-art shift-reduce transition-based model.") { override def invoke = { if (value ne null) System.setProperty(classOf[DepParser2].getName, value); annotators += cc.factorie.app.nlp.parse.DepParser2 } }
      val parser3 = new CmdOption[String]("parser3", null, "URL", "Annotate dependency parse with a first-order projective parser.") { override def invoke = { if (value ne null) System.setProperty(classOf[GraphProjectiveParser].getName, value); annotators += cc.factorie.app.nlp.parse.GraphProjectiveParser } }
      val coref1 = new CmdOption[String]("coref1", null, "URL", "Annotate within-document noun mention coreference using a simple left-to-right system") { override def invoke = { if (value ne null) System.setProperty(classOf[coref.WithinDocCoref1].getName, value); annotators += cc.factorie.app.nlp.coref.WithinDocCoref1 } }
      val coref2 = new CmdOption[String]("coref2", null, "URL", "Annotate within-document noun mention coreference using a state-of-the-art system") { override def invoke = { if (value ne null) System.setProperty(classOf[coref.WithinDocCoref2].getName, value); annotators += cc.factorie.app.nlp.coref.WithinDocCoref2 } }

    }
    opts.parse(args)
    if (opts.logFile.value != "-") logStream = new PrintStream(new File(opts.logFile.value))

    try {
      val listener = new ServerSocket(opts.socket.value)
      println("Listening on port "+opts.socket.value)
      while (true)
        new ServerThread(listener.accept(), opts.encoding.value).start()
      listener.close()
    }
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: "+opts.socket.value);
        System.exit(-1)
    }
  }
  
  case class ServerThread(socket: Socket, encoding:String) extends Thread("ServerThread") {
    override def run(): Unit = try {
      val out = new PrintStream(socket.getOutputStream())
      val in = scala.io.Source.fromInputStream(new DataInputStream(socket.getInputStream), encoding)
      assert(in ne null)
      var document = cc.factorie.app.nlp.LoadPlainText.fromString(in.mkString).head
      val time = System.currentTimeMillis
      import Implicits.defaultDocumentAnnotatorMap
      for (processor <- annotators)
        document = processor.process(document)
      //logStream.println("Processed %d tokens in %f seconds.".format(document.length, (System.currentTimeMillis - time) / 1000.0))
      logStream.println("Processed %d tokens.".format(document.tokenCount))
      out.println(document.owplString(annotators.map(p => p.tokenAnnotationString(_))))
      out.close();
      in.close();
      socket.close()
    }
    catch {
      case e: SocketException => () // avoid stack trace when stopping a client with Ctrl-C
      case e: IOException =>  e.printStackTrace();
    }
  }
  
}