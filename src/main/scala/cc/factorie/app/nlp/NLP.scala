package cc.factorie.app.nlp

import java.io._
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
      val encoding = new CmdOption("encoding", "UTF-8", "ENCODING", "Character encoding, such as UTF-8")
      val logFile = new CmdOption("log", "-", "FILENAME", "Send logging messages to this filename.")
      // TODO All these options should be replaced by something that will interpret object construction code. -akm
      val tnorm = new CmdOption("tnorm", null, null, "Normalize token strings") { override def invoke = annotators += cc.factorie.app.nlp.segment.SimplifyPTBTokenNormalizer }
      val pos3 = new CmdOption("pos3", "/Users/mccallum/tmp/pos-model", "MODELFILE", "Annotate POS") { override def invoke = annotators += new cc.factorie.app.nlp.pos.POS3(value) }
      val wnlemma = new CmdOption("wnlemma", "/Users/mccallum/tmp/dict", "WordNet-DIR", "Annotate lemma") { override def invoke = annotators += new cc.factorie.app.nlp.lemma.WordNetLemmatizer(value) }
      val mention1 = new CmdOption("mention1", null, null, "Annotate noun mention") { override def invoke = annotators += cc.factorie.app.nlp.mention.NounMention1 }
      val ner3 = new CmdOption("ner3", "/Users/mccallum/tmp/ner-model", "MODELFILE", "Annotate NER") { override def invoke = annotators += new cc.factorie.app.nlp.ner.NER3(value) }
      val parser1 = new CmdOption("parser1", "/Users/mccallum/tmp/parser-model", "MODELFILE", "Annotate dependency parse") { override def invoke = annotators += new cc.factorie.app.nlp.parse.DepParser1(value) }
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
      var document = cc.factorie.app.nlp.LoadPlainText.fromString("<stdin>", in.mkString)
      val time = System.currentTimeMillis
      for (processor <- annotators)
        document = processor.process(document)
      //logStream.println("Processed %d tokens in %f seconds.".format(document.length, (System.currentTimeMillis - time) / 1000.0))
      logStream.println("Processed %d tokens.".format(document.length))
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