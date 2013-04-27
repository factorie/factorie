package cc.factorie.app.nlp

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

object NLP {
  val annotators = new scala.collection.mutable.ArrayBuffer[DocumentAnnotator]
  var logStream = System.err
  //val interpreter = new scala.tools.nsc.IMain
  def main(args:Array[String]): Unit = {
    //processors += cc.factorie.app.nlp.lemma.SimplifyDigitsLemmatizer
    annotators += new cc.factorie.app.nlp.pos.POS3("/Users/mccallum/tmp/pos-model") // TODO Create a command-line approach for setting this.
    annotators += cc.factorie.app.nlp.mention.NounMention1
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val socket = new CmdOption("socket", 3228, "SOCKETNUM", "On which socket number NLP server should listen.")
      val encoding = new CmdOption("encoding", "ENCODING", "UTF-8", "Character encoding, such as UTF-8")
      val logFile = new CmdOption("log", "-", "FILENAME", "Send logging messages to this filename.")
    }
    opts.parse(args)
    if (opts.logFile.value != "-") logStream = new PrintStream(new File(opts.logFile.value))

    try {
      val listener = new ServerSocket(opts.socket.value)
      println("Listening on port "+opts.socket.value)
      while (true)
        new ServerThread(listener.accept()).start()
      listener.close()
    }
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: "+opts.socket.value);
        System.exit(-1)
    }
  }
  
  case class ServerThread(socket: Socket) extends Thread("ServerThread") {
    override def run(): Unit = try {
      val out = new PrintStream(socket.getOutputStream())
      val in = scala.io.Source.fromInputStream(new DataInputStream(socket.getInputStream()))
      val time = System.currentTimeMillis
      var document = cc.factorie.app.nlp.LoadPlainText.fromString("<stdin>", in.mkString)
      for (processor <- annotators)
        document = processor.process(document)
      logStream.println("Processed %d tokens in %f seconds.".format(document.length, (System.currentTimeMillis - time) / 1000.0))
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