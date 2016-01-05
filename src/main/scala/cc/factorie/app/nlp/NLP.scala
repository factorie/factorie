/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp

import java.io._
import java.net.{ServerSocket, Socket, SocketException}

import cc.factorie.app.nlp.coref.MentionList
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.app.nlp.ner.{ConllStackedChainNer, NoEmbeddingsConllStackedChainNer, OntonotesChainNer, ConllChainNer}
import cc.factorie.app.nlp.parse._
import cc.factorie.util.{ModelProvider, ModelProviderCmdOptions}

import cc.factorie.app.nlp.ner.StaticLexiconFeatures

/** A command-line driver for DocumentAnnotators.
    Launch on the command-line, specifying which NLP pipeline steps you want, 
    then it listens on a socket port for new text input, and replies with annoted text, one word per line.
    @author Andrew McCallum */
object NLP {
  val annotators = new scala.collection.mutable.ArrayBuffer[DocumentAnnotator]
  var logStream = System.err
  //val interpreter = new scala.tools.nsc.IMain
  def main(args:Array[String]): Unit = {
    val t0 = System.currentTimeMillis()
    object opts extends cc.factorie.util.DefaultCmdOptions with ModelProviderCmdOptions {
      val socket = new CmdOption("socket", 3228, "SOCKETNUM", "On which socket number NLP server should listen.")
      val encoding = new CmdOption("encoding", "UTF-8", "ENCODING", "Character encoding for reading document text, such as UTF-8")
      val logFile = new CmdOption("log", "-", "FILENAME", "Send logging messages to this filename.")
      // TODO All these options should be replaced by something that will interpret object construction code. -akm
      val token = new CmdOption("token", null, "", "Segment Document into Tokens (but not Sentences, and do not normalize)") { override def invoke() = annotators += cc.factorie.app.nlp.segment.DeterministicTokenizer }
      val tokenNorm = new CmdOption("token-norm", null, "", "Segment Document into Tokens (but not Sentences) and normalize") { override def invoke() = annotators += cc.factorie.app.nlp.segment.DeterministicNormalizingTokenizer }
      val sentence = new CmdOption("sentence", null, "", "Segment pre-tokenized Document into Sentences by simpler regex") { override def invoke() = annotators += cc.factorie.app.nlp.segment.DeterministicSentenceSegmenter }
//      val tnorm = new CmdOption("tnorm", null, "", "Normalize token strings") { override def invoke() = annotators += cc.factorie.app.nlp.segment.PlainTokenNormalizer }
      val wsjForwardPos = new CmdOption[String]("wsj-forward-pos", null, "URL", "Annotate Penn-Treebank-style POS with model trained on WSJ") { override def invoke() = { if (value ne null) System.setProperty(classOf[pos.WSJForwardPosTagger].getName, value); annotators += cc.factorie.app.nlp.pos.WSJForwardPosTagger } }
      val ontonotesForwardPos = new CmdOption[String]("ontonotes-forward-pos", null, "URL", "Annotate Penn-Treebank-style POS with model trained on Ontonotes") { override def invoke() = { if (value ne null) System.setProperty(classOf[pos.OntonotesForwardPosTagger].getName, value); annotators += cc.factorie.app.nlp.pos.OntonotesForwardPosTagger } }
      val chainPos = new CmdOption[String]("ontonotes-chain-pos", null, "URL", "Annotate Penn-Treebank-style POS with linear chain model") { override def invoke() = { if (value ne null) System.setProperty(classOf[pos.OntonotesChainPosTagger].getName, value); annotators += cc.factorie.app.nlp.pos.OntonotesChainPosTagger } }
      val wnLemma = new CmdOption("wordnet-lemma", "classpath:cc/factorie/app/nlp/wordnet/WordNet", "URL", "Annotate lemma using WordNet's lemmatizer.") { override def invoke() = annotators += cc.factorie.app.nlp.lemma.WordNetLemmatizer }
      //val npchunk1 = new CmdOption("pos-based-mention", null, "", "Annotate noun mention boundaries using simple rules on POS tag sequences.  Low quality.") { override def invoke() = annotators += cc.factorie.app.nlp.mention.NounChunker1 }
      //val mention2 = new CmdOption("parse-based-mention", null, "", "Annotate noun mention boundaries using a dependency parser.") { override def invoke() = annotators += cc.factorie.app.nlp.coref.mention.ParseBasedMentionFinding }
      //val mention3 = new CmdOption("ner-mention", null, "", "Annotate noun mention boundaries using NER tagger and pronoun patterns.") { override def invoke() = annotators += cc.factorie.app.nlp.coref.mention.NerAndPronounMentionFinder }

      // named entity recognition

      val conllchainner = new CmdOption[String]("conll-chain-ner", null, "URL", "Annotate CoNLL-2003 NER") {
        override def invoke() {
          val mp = if(value ne null) ModelProvider.provide[ConllChainNer, File](new File(value)) else ModelProvider.classpath[ConllChainNer]()
          annotators += new ConllChainNer()(mp, StaticLexiconFeatures())
        }
      }
      val basicontonotesner = new CmdOption[String]("ontonotes-chain-ner", null, "URL", "Annotate Ontonotes NER") {
        override def invoke(): Unit = {
          val mp = if(value ne null) ModelProvider.provide[OntonotesChainNer, File](new File(value)) else ModelProvider.classpath[OntonotesChainNer]()
          annotators += new OntonotesChainNer()(mp, StaticLexiconFeatures())
        }
      }

      val noembeddingsconllstackedchainner = new CmdOption[String]("stacked-chain-ner-noembeddings", null, "URL", "Annotate Conll NER using a stacked chain model that doesn't use embeddings")  {
        override def invoke() = {
          val mp = if (value ne null) ModelProvider.provide[ner.NoEmbeddingsConllStackedChainNer, File](new File(value)) else ModelProvider.classpath[ner.NoEmbeddingsConllStackedChainNer]()
          annotators += new ConllStackedChainNer(null, 0, 0.0, false)(mp, StaticLexiconFeatures())
        }
      }

      // parsers
      //val parser1 = new CmdOption("parser1", ClasspathURL[DepParser1](".factorie").toString, "URL", "Annotate dependency parse with a simple shift-reduce transition-based model.") { override def invoke = { System.setProperty(classOf[DepParser1].getName, value); annotators += cc.factorie.app.nlp.parse.DepParser1 } }
      val transitionparser = new CmdOption[String]("transition-based-parser", null, "URL", "Annotate dependency parse with a state-of-the-art shift-reduce transition-based model.") { override def invoke() = { if (value ne null) System.setProperty(classOf[OntonotesTransitionBasedParser].getName, value); annotators += cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser } }
      //val graphparser = new CmdOption[String]("graph-based-parser", null, "URL", "Annotate dependency parse with a first-order projective parser.") { override def invoke() = { if (value ne null) System.setProperty(classOf[OntonotesProjectiveGraphBasedParser].getName, value); annotators += cc.factorie.app.nlp.parse.OntonotesProjectiveGraphBasedParser } }
      
      // coref
      val deterministicNamedCoref = new CmdOption[String]("d-named-coref", null, "", "Simple deterministic coreference on named entities") { override def invoke() = { annotators += coref.DeterministicNamedCoref } }
      val parsestructuredcoref = new CmdOption[String]("parse-lexical-coref", null, "URL", "Annotate within-document noun mention coreference using a state-of-the-art system and parse-based mention finding") { override def invoke() = {  if (value ne null) System.setProperty(classOf[coref.ParseStructuredCoref].getName, value); annotators += cc.factorie.app.nlp.coref.ParseStructuredCoref } }
      val nerstructuredcoref = new CmdOption[String]("ner-lexical-coref", null, "URL", "Annotate within-document proper- and pro-noun mention coreference using a state-of-the-art system") { override def invoke() = { if (value ne null) System.setProperty(classOf[coref.NerStructuredCoref].getName, value); annotators += coref.NerStructuredCoref} }
      val parseforwardcoref = new CmdOption[String]("parse-forward-coref", null, "URL", "Annotate within-document noun mention coreference using a state-of-the-art system and parse-based mention finding") { override def invoke() = {  if (value ne null) System.setProperty(classOf[coref.ParseForwardCoref].getName, value); annotators += cc.factorie.app.nlp.coref.ParseForwardCoref } }
      val nerforwardcoref = new CmdOption[String]("ner-forward-coref", null, "URL", "Annotate within-document proper- and pro-noun mention coreference using a state-of-the-art system") { override def invoke() = { if (value ne null) System.setProperty(classOf[coref.NerForwardCoref].getName, value); annotators += coref.NerForwardCoref} }

    }
    opts.parse(args)
    
    if (!opts.values.exists(_.wasInvoked)) {
       println("Usage: " + opts.usageString)
       println("The tool will perform annotation for whatever tasks you specify, and the prereqs for these tasks. It will only print to stdout the the ones that you request, though.")
       println("Note: if you have downloaded the models via mvn, you will not need to specify urls for the models")
       println("Note: this tool requires nlp models being downloaded. To do this automatically, run \'mvn package -Pnlp-jar-with-dependencies\'")
       sys.exit()
     }
    
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- annotators) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, annotators.flatMap(_.postAttrs))
    if (opts.logFile.value != "-") logStream = new PrintStream(new File(opts.logFile.value))
    println(System.currentTimeMillis()-t0)
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
      for (line <- in.getLines()) {
        var document = load.LoadPlainText.fromString(line).head
        document = pipeline.process(document)
        //logStream.println("Processed %d tokens in %f seconds.".format(document.length, (System.currentTimeMillis - time) / 1000.0))
        logStream.println("Processed %d tokens.".format(document.tokenCount))
        out.println(document.owplString(annotators.map(p => p.tokenAnnotationString(_))))
        val mentions = document.attr[MentionList]
        if (mentions ne null) {
          out.println("Mentions:")
          for (mention <- mentions) {
            out.print(mention.phrase)
            for (annotator <- annotators) { val s = annotator.mentionAnnotationString(mention); if (s.length > 0) { out.print('\t'); out.print(s) } }
            out.println()
          }
        }
        for (annotator <- annotators) out.print(annotator.documentAnnotationString(document))
      }
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
