package cc.factorie.app.nlp

import java.io._
import cc.factorie.util.ClasspathURL
import cc.factorie.app.nlp.parse._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import cc.factorie.app.nlp.mention.MentionEntityTypeAnnotator1

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
      val token1 = new CmdOption("token1", null, "", "Segment Document into Tokens (but not Sentences) by regex") { override def invoke = annotators += cc.factorie.app.nlp.segment.Tokenizer1 }
      val sentence = new CmdOption("sentence", null, "", "Segment Document into Tokens and Sentences") { override def invoke = annotators += cc.factorie.app.nlp.segment.ClearSegmenter }
      val sentence1 = new CmdOption("sentence1", null, "", "Segment pre-tokenized Document into Sentences by simpler regex") { override def invoke = annotators += cc.factorie.app.nlp.segment.SentenceSegmenter1 }
      val tnorm = new CmdOption("tnorm", null, "", "Normalize token strings") { override def invoke = annotators += cc.factorie.app.nlp.segment.PlainTokenNormalizer }
      val pos1 = new CmdOption[String]("pos1", null, "URL", "Annotate Penn-Treebank-style POS with model trained on WSJ") { override def invoke = { if (value ne null) System.setProperty(classOf[pos.POS1].getName, value); annotators += cc.factorie.app.nlp.pos.POS1WSJ } }
      val pos1o = new CmdOption[String]("pos1o", null, "URL", "Annotate Penn-Treebank-style POS with model trained on Ontonotes") { override def invoke = { if (value ne null) System.setProperty(classOf[pos.POS1].getName, value); annotators += cc.factorie.app.nlp.pos.POS1Ontonotes } }
      val wnlemma = new CmdOption("wnlemma", "classpath:cc/factorie/app/nlp/wordnet/WordNet", "URL", "Annotate lemma using WordNet's lemmatizer.") { override def invoke = annotators += cc.factorie.app.nlp.lemma.WordNetLemmatizer }
      val npchunk1 = new CmdOption("mention1", null, "", "Annotate noun mention boundaries using simple rules on POS tag sequences.  Low quality.") { override def invoke = annotators += cc.factorie.app.nlp.mention.NounChunker1 }
      val mention2 = new CmdOption("mention2", null, "", "Annotate noun mention boundaries using a dependency parser.") { override def invoke = annotators += cc.factorie.app.nlp.mention.ParseBasedMentionFinding }
      val mention3 = new CmdOption("mention3", null, "", "Annotate noun mention boundaries using NER tagger and pronoun patterns.") { override def invoke = annotators += cc.factorie.app.nlp.mention.NerAndPronounMentionFinder }
      val ner1 = new CmdOption[String]("ner1", null, "URL", "Annotate CoNLL-2003 NER") { override def invoke = { if (value ne null) System.setProperty(classOf[ner.NER1].getName, value); annotators += cc.factorie.app.nlp.ner.NER1 } }
      val ner2 = new CmdOption[String]("ner2", null, "URL", "Annotate Ontonotes NER")  { override def invoke = { if (value ne null) System.setProperty(classOf[ner.NER2].getName, value); annotators += cc.factorie.app.nlp.ner.NER2 } }
      //val parser1 = new CmdOption("parser1", ClasspathURL[DepParser1](".factorie").toString, "URL", "Annotate dependency parse with a simple shift-reduce transition-based model.") { override def invoke = { System.setProperty(classOf[DepParser1].getName, value); annotators += cc.factorie.app.nlp.parse.DepParser1 } }
      val parser1 = new CmdOption[String]("parser1", null, "URL", "Annotate dependency parse with a state-of-the-art shift-reduce transition-based model.") { override def invoke = { if (value ne null) System.setProperty(classOf[DepParser1].getName, value); annotators += cc.factorie.app.nlp.parse.DepParser1 } }
      val parser3 = new CmdOption[String]("parser3", null, "URL", "Annotate dependency parse with a first-order projective parser.") { override def invoke = { if (value ne null) System.setProperty(classOf[GraphProjectiveParser].getName, value); annotators += cc.factorie.app.nlp.parse.GraphProjectiveParser } }
      val coref1 = new CmdOption[String]("coref1", null, "URL", "Annotate within-document noun mention coreference using a state-of-the-art system") { override def invoke = { annotators += mention.MentionEntityTypeLabeler ; if (value ne null) System.setProperty(classOf[coref.WithinDocCoref1].getName, value); annotators += cc.factorie.app.nlp.coref.WithinDocCoref1 } }
      val coref1ner = new CmdOption[String]("coref1ner", null, "URL", "Annotate within-document proper- and pro-noun mention coreference using a state-of-the-art system") { override def invoke = { if (value ne null) System.setProperty(classOf[coref.WithinDocCoref1].getName, value); annotators += coref.WithinDocCoref1Ner } }
      val mentiongender = new CmdOption[String]("mention-gender", null, "", "Annotate noun mention with male/female/person/neuter/unknown") { override def invoke = { if (value ne null) System.setProperty(classOf[mention.MentionGenderLabeler].getName, value); annotators += cc.factorie.app.nlp.mention.MentionGenderLabeler } } 
      val mentionnumber = new CmdOption[String]("mention-number", null, "", "Annotate noun mention with singular/plural/unknown") { override def invoke = { if (value ne null) System.setProperty(classOf[mention.MentionNumberLabeler].getName, value); annotators += cc.factorie.app.nlp.mention.MentionNumberLabeler } } 
      val mentionEntitytype = new CmdOption[String]("mention-entity-type", null, "URL", "Annotate noun mention with Ontonotes NER label") { override def invoke = { if (value ne null) System.setProperty(classOf[mention.MentionEntityTypeLabeler].getName, value); annotators += cc.factorie.app.nlp.mention.MentionEntityTypeLabeler } }
    }
    opts.parse(args)
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- annotators) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, annotators.flatMap(_.postAttrs))
    if (opts.logFile.value != "-") logStream = new PrintStream(new File(opts.logFile.value))

    try {
      val listener = new ServerSocket(opts.socket.value)
      println("Listening on port "+opts.socket.value)
      while (true)
        new ServerThread(listener.accept(), opts.encoding.value, pipeline).start()
      listener.close()
    }
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: "+opts.socket.value)
        System.exit(-1)
    }
  }
  
  case class ServerThread(socket: Socket, encoding:String, pipeline: DocumentAnnotator) extends Thread("ServerThread") {
    override def run(): Unit = try {
      val out = new PrintStream(socket.getOutputStream, false, encoding)
      val in = scala.io.Source.fromInputStream(new DataInputStream(socket.getInputStream), encoding)
      assert(in ne null)
      var document = load.LoadPlainText.fromString(in.mkString).head
      val time = System.currentTimeMillis
      document = pipeline.process(document)
      //logStream.println("Processed %d tokens in %f seconds.".format(document.length, (System.currentTimeMillis - time) / 1000.0))
      logStream.println("Processed %d tokens.".format(document.tokenCount))
      out.println(document.owplString(annotators.map(p => p.tokenAnnotationString(_))))
      val mentions = document.attr[mention.MentionList]
      if (mentions ne null) {
        out.println("Mentions:")
        for (mention <- mentions) {
          out.print(mention.phrase)
          for (annotator <- annotators) { val s = annotator.mentionAnnotationString(mention); if (s.length > 0) { out.print('\t'); out.print(s) } }
          out.println()
        }
      }
      for (annotator <- annotators) out.print(annotator.documentAnnotationString(document))
      out.close()
      in.close()
      socket.close()
    }
    catch {
      case e: SocketException => () // avoid stack trace when stopping a client with Ctrl-C
      case e: IOException =>  e.printStackTrace()
    }
  }
  
}
