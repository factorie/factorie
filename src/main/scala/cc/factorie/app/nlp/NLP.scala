package cc.factorie.app.nlp

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

object NLP {
  val processors = new scala.collection.mutable.ArrayBuffer[DocumentProcessor]
  //val interpreter = new scala.tools.nsc.IMain
  def main(args:Array[String]): Unit = {
    processors += new cc.factorie.app.nlp.pos.POS3("/Users/mccallum/tmp/pos-model") // TODO Create a command-line approach for setting this.
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val socket = new CmdOption("socket", 3228, "SOCKETNUM", "On which socket number NLP server should listen.")
      val encoding = new CmdOption("encoding", "ENCODING", "UTF-8", "Character encoding, such as UTF-8")
      val logFile = new CmdOption("log", "", "FILENAME", "Send logging messages to this filename; not yet implemented.")
    }
    opts.parse(args)

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
      var document = cc.factorie.app.nlp.LoadPlainText.fromString("<stdin>", in.mkString)
      for (processor <- processors)
        document = processor.process(document)
      out.println(document.owplString(processors.map(p => p.tokenAnnotationString(_))))
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