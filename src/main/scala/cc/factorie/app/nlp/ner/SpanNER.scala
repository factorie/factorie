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
//
//package cc.factorie.app.nlp.ner
//
//import cc.factorie._
//import cc.factorie.optimize._
//import cc.factorie.app.nlp._
//import cc.factorie.util.{BinarySerializer, CubbieConversions, DefaultCmdOptions}
//import java.io.File
//
//
//
//
//object SpanNerFeaturesDomain extends CategoricalVectorDomain[String]
//class SpanNerFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
//  def domain = SpanNerFeaturesDomain
//  override def skipNonCategories = true
//}
//
//abstract class SpanNerTemplate(val model: Parameters) extends DotTemplate2[NerSpan,Conll2003SpanNerLabel] {
//  //override def statisticsDomains = ((SpanNerFeaturesDomain, Conll2003NerDomain))
//  val weights = model.Weights(new la.DenseTensor2(SpanNerFeaturesDomain.dimensionSize, ConllNerDomain.size)) // TODO This ordering seems backwards
//  def unroll1(span:NerSpan) = Factor(span, span.label)
//  def unroll2(label:Conll2003SpanNerLabel) = Factor(label.span, label)
//}
//
//class SpanNerModel extends TemplateModel with Parameters {
//  self =>
//  addTemplates(
//    // Bias term on each individual label 
//    new DotTemplateWithStatistics1[Conll2003SpanNerLabel] {
//      val weights = Weights(new la.DenseTensor1(ConllNerDomain.size))
//    },
//    // Token-Label within Span
//    new SpanNerTemplate(self) {
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = {
//        // TODO Yipes, this is awkward, but more convenient infrastructure could be build.
//        //var vector = new cc.factorie.la.SparseBinaryVector(v._1.head.value.length) with CategoricalVectorValue[String] { val domain = SpanNerFeaturesDomain }
//        val firstToken = v1.head
//        var vector = firstToken.attr[SpanNerFeatures].value.blankCopy
//        for (token <- v1; featureIndex <- token.attr[SpanNerFeatures].value.activeDomain.asSeq)
//          vector += (featureIndex, 1.0) // TODO This is shifing array pieces all over; slow.  Fix it.
//        vector outer v2
//      }
//    },
//    // First Token of Span
//    new SpanNerTemplate(self) {
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = v1.head.attr[SpanNerFeatures].value outer v2
//    },
//    // Last Token of Span
//    new SpanNerTemplate(self) {
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = v1.last.attr[SpanNerFeatures].value outer v2
//    },
//    // Token before Span
//    new SpanNerTemplate(self) {
//      override def unroll1(span:NerSpan) = if (span.head.hasPrev) Factor(span, span.label) else Nil
//      override def unroll2(label:Conll2003SpanNerLabel) = if (label.span.head.hasPrev) Factor(label.span, label) else Nil
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = v1.head.prev.attr[SpanNerFeatures].value outer v2
//    },
//    // Token after Span
//    new SpanNerTemplate(self) {
//      override def unroll1(span:NerSpan) = if (span.last.hasNext) Factor(span, span.label) else Nil
//      override def unroll2(label:Conll2003SpanNerLabel) = if (label.span.last.hasNext) Factor(label.span, label) else Nil
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = v1.last.next.attr[SpanNerFeatures].value outer v2
//    },
//    // Single Token Span
//    new SpanNerTemplate(self) {
//      override def unroll1(span:NerSpan) = if (span.length == 1) Factor(span, span.label) else Nil
//      override def unroll2(label:Conll2003SpanNerLabel) = if (label.span.length == 1) Factor(label.span, label) else Nil
//      override def statistics(v1:NerSpan#Value, v2:Conll2003SpanNerLabel#Value) = v1.head.attr[SpanNerFeatures].value outer v2
//    }
//    //new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext && span.last.next.hasNext) Stat(span.last.next.next, span.label) else Nil },
//    // Span Length with Label
//    //new SpanLabelTemplate with DotStatistics2[SpanLength,Label] { def statistics(span:Span, label:Label) = Stat(span.spanLength, span.label) },
//    // Label of span that preceeds or follows this one
//    //new Template2[Span,Span] with Statistics2[Label,Label] { def unroll1(span:Span) = { val result = Nil; var t = span.head; while (t.hasPrev) { if } } }
//    )
//    def this(file:File) = {
//      this()
//      import CubbieConversions._
//      BinarySerializer.deserialize(SpanNerFeaturesDomain, this, file)
//    }
//}
//
//// The training objective
//class SpanNerObjective extends TemplateModel {
//  addTemplates(
//    new TupleTemplateWithStatistics2[NerSpan,Conll2003SpanNerLabel] {
//      //def statisticsDomains = ((NerSpanDomain, SpanNerLabelDomain))
//      def unroll1(span:NerSpan) = Factor(span, span.label)
//      def unroll2(label:Conll2003SpanNerLabel) = Factor(label.span, label)
//      def score(spanValue:NerSpan#Value, labelValue:Conll2003SpanNerLabel#Value) = {
//        var result = 0.0
//        var trueLabelIncrement = 10.0
//        var allTokensCorrect = true
//        for (token <- spanValue) {
//          //if (token.trueLabelValue != "O") result += 2.0 else result -= 1.0
//          if (token.nerLabel.intValue == labelValue.intValue) {
//            result += trueLabelIncrement
//            trueLabelIncrement += 2.0 // proportionally more benefit for longer sequences to help the longer seq steal tokens from the shorter one.
//          } else if (token.nerLabel.target.categoryValue == "O") {
//            result -= 1.0
//            allTokensCorrect = false
//          } else {
//            result += 1.0
//            allTokensCorrect = false
//          }
//          if (token.spans.length > 1) result -= 100.0 // penalize overlapping spans
//        }
//        if (allTokensCorrect) {
//          if (!spanValue.head.hasPrev || spanValue.head.prev.nerLabel.intValue != labelValue.intValue) result += 5.0 // reward for getting starting boundary correct
//          if (!spanValue.last.hasNext || spanValue.last.next.nerLabel.intValue != labelValue.intValue) result += 5.0 // reward for getting starting boundary correct
//        }
//        result
//      }
//    }
//  )
//}
//
//// The sampler
//class TokenSpanSampler(model:Model, objective:Model)(override implicit val random: scala.util.Random) extends SettingsSampler[Token](model, objective) {
//  // The proposer for changes to Spans touching this Token
//  def settings(token:Token) = new SettingIterator {
//    private val _seq = token.document.asSection
//    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit];
//    val existingSpans = token.spansOfClass[NerSpan](classOf[NerSpan])
//    //println("existing spans = "+existingSpans)
//    for (span <- existingSpans) {
//      // Change label without changing boundaries
//      for (labelValue <- ConllNerDomain; if (labelValue.category != "O"))
//        changes += {(d:DiffList) => span.label.set(labelValue)(d)}
//      // Delete the span
//      changes += {(d:DiffList) => span.delete(d)}
//      if (span.length > 1) {
//        // Trim last word, without changing label
//        changes += {(d:DiffList) => span.trimEnd(1)(d)}
//        // Trim first word, without changing label
//        changes += {(d:DiffList) => span.trimStart(1)(d)}
//        // Split off first and last word, with choices of the label of the split off portion
//        for (labelValue <- ConllNerDomain; if (labelValue.category != "O")) {
//          changes += {(d:DiffList) => { span.trimEnd(1)(d); new NerSpan(_seq, labelValue.category, span.end+1, 1)(d) } }
//          changes += {(d:DiffList) => { span.trimStart(1)(d); new NerSpan(_seq, labelValue.category, span.start-1, 1)(d) } }
//        }
//      }
//      if (span.length == 3) {
//        // Split span, dropping word in middle, preserving label value
//        changes += {(d:DiffList) => span.delete(d); new NerSpan(_seq, span.label.categoryValue, span.start, 1)(d); new NerSpan(_seq, span.label.categoryValue, span.end, 1)(d) }
//      }
//      // Add a new word to beginning, and change label
//      if (span.canPrepend(1)) {
//        for (labelValue <- ConllNerDomain; if (labelValue.category != "O"))
//          changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.prepend(1)(d); span.head.spans.filter(_ != span).foreach(_.trimEnd(1)(d)) } }
//      }
//      // Add a new word to the end, and change label
//      if (span.canAppend(1)) {
//        for (labelValue <- ConllNerDomain; if (labelValue.category != "O"))
//          changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.append(1)(d); span.last.spans.filter(_ != span).foreach(_.trimStart(1)(d)) } }
//      }
//      // Merge two neighboring spans having the same label
//      if (span.hasPredecessor(1)) {
//        val prevSpans = span.predecessor(1).endsSpansOfClass[NerSpan]
//        val prevSpan: NerSpan = if (prevSpans.size > 0) prevSpans.head else null
//        if (prevSpan != null && prevSpan.label.intValue == span.label.intValue) {
//          changes += {(d:DiffList) => { new NerSpan(_seq, span.label.categoryValue, prevSpan.start, prevSpan.length + span.length)(d); span.delete(d); prevSpan.delete(d)}}
//        }
//      }
//      if (span.hasSuccessor(1)) {
//        val nextSpans = span.successor(1).startsSpansOfClass[NerSpan]
//        val nextSpan: NerSpan = if (nextSpans.size > 0) nextSpans.head else null
//        if (nextSpan != null && nextSpan.label.intValue == span.label.intValue) {
//          changes += {(d:DiffList) => { new NerSpan(_seq, span.label.categoryValue, span.start, span.length + nextSpan.length)(d); span.delete(d); nextSpan.delete(d)}}
//        }
//      }
//      //if (span.length > 1) changes += {(d:DiffList) => { span.trimEnd(1)(d); new Span(labelValue.category, seq, position+1, 1)(d) } }
//    }
//    if (existingSpans.isEmpty) {
//      changes += {(d:DiffList) => {}} // The no-op action
//      for (labelValue <- ConllNerDomain; if (labelValue.category != "O")) {
//        // Add new length=1 span, for each label value
//        changes += {(d:DiffList) => new NerSpan(_seq, labelValue.category, token.position, 1)(d)}
//        //if (position != _seq.length-1) changes += {(d:DiffList) => new Span(labelValue.category, _seq, position, 2)(d)}
//      }
//    }
//    //println("Token.settings length="+changes.length)
//    var i = 0
//    def hasNext = i < changes.length
//    def next(d:DiffList) = { val d = new DiffList; changes(i).apply(d); i += 1; d }
//    def reset = i = 0
//  }
//}
//
//// The predictor for test data
//class SpanNerPredictor(model:Model)(implicit random: scala.util.Random) extends TokenSpanSampler(model, null) {
//  def this(file:File)(implicit random: scala.util.Random) = this(new SpanNerModel(file))
//  var verbose = false
//  temperature = 0.0001 
//  override def preProcessHook(t:Token): Token = { 
//    super.preProcessHook(t)
//    if (t.isCapitalized) {
//      if (verbose) t.spansOfClass(classOf[NerSpan]).foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
//      t 
//    } else null.asInstanceOf[Token] 
//  }
//  override def proposalsHook(proposals:Seq[Proposal]): Unit = {
//    if (verbose) println("Test proposal")
//    //proposals.foreach(println(_)); println
//    if (verbose) { proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else ""))); println }
//    super.proposalsHook(proposals)
//  }
//  override def proposalHook(proposal:Proposal): Unit = {
//    super.proposalHook(proposal)
//    // If we changed the possible world last time, try sampling it again right away to see if we can make more changes
//    // TODO Disabled for now, but this should be re-enabled
//    if (false && proposal.diff.size > 0) {
//      val spanDiffs = proposal.diff.filter(d => d.variable match { case s:NerSpan => s.present; case _ => false })
//          spanDiffs.foreach(_.variable match {
//          case span:NerSpan => if (span.present) { if (verbose) println("RECURSIVE PROPOSAL"); this.process1(span.last) }
//          case _ => {}
//        })
//    }
//  }
//}
//
//class SpanNER(implicit val random: scala.util.Random) {
//  var verbose = false
//  
//  class Lexicon(file:File) extends cc.factorie.app.nlp.lexicon.PhraseLexicon(file) {
//    override val name = file.toString.substring(file.toString.lastIndexOf('/')+1).toUpperCase
//  }
//  val lexicons = new scala.collection.mutable.ArrayBuffer[Lexicon]
//  val model = new SpanNerModel
//  val objective = new SpanNerObjective
//  val predictor = new SpanNerPredictor(model)
//
//  def train(trainFiles:Seq[String], testFile:String)(implicit random: scala.util.Random): Unit = {
//    // predictor.verbose = true
//    // Read training and testing data.  The function 'featureExtractor' function is defined below.  Now training on seq == whole doc, not seq == sentece
//    val trainDocuments = trainFiles.flatMap(LoadConll2003.fromFilename(_))
//    val testDocuments = LoadConll2003.fromFilename(testFile)
//    println("Read "+trainDocuments.flatMap(_.sentences).size+" training sentences, and "+testDocuments.flatMap(_.sentences).size+" testing ")
//
//    trainDocuments.foreach(addFeatures(_))
//    testDocuments.foreach(addFeatures(_))
//
//    println("Have "+trainDocuments.map(_.tokenCount).sum+" trainTokens "+testDocuments.map(_.tokenCount).sum+" testTokens")
//    println("FeaturesDomain size="+SpanNerFeaturesDomain.dimensionSize)
//    println("LabelDomain "+ConllNerDomain.toList)
//    
//    if (verbose) trainDocuments.take(10).map(_.tokens).flatten.take(500).foreach(token => { print(token.string+"\t"); printFeatures(token) })
//    
//    // The learner
//    val sampler = new TokenSpanSampler(model, objective) {
//      //logLevel = 1
//      temperature = 0.01
//      override def preProcessHook(t:Token): Token = { 
//        super.preProcessHook(t)
//        if (t.isCapitalized) { // Skip tokens that are not capitalized
//          if (verbose) t.spansOfClass[NerSpan].foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
//          // Skip this token if it has the same spans as the previous token, avoiding duplicate sampling
//          //if (t.hasPrev && t.prev.spans.sameElements(t.spans)) null.asInstanceOf[Token] else
//          t 
//        } else null.asInstanceOf[Token] 
//      }
//      override def proposalsHook(proposals:Seq[Proposal]): Unit = {
//        if (verbose) { proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else "")+(if (p.objectiveScore > 0.0) "OO" else ""))); println }
//        super.proposalsHook(proposals)
//      }
//    }
//    val learner = new SampleRankTrainer(sampler, new AdaGrad)
//    
//    
//    // Train!
//    for (i <- 1 to 11) {
//      println("Iteration "+i) 
//      // Every third iteration remove all the predictions
//      if (i % 3 == 0) { println("Removing all spans"); for (doc <- (trainDocuments ++ testDocuments); section <- doc.sections) section.clearSpans(null) }
//      learner.processContexts(trainDocuments.map(_.tokens).flatten)
//      //learner.learningRate *= 0.9
//      predictor.processAll(testDocuments.map(_.tokens).flatten)
//      println("*** TRAIN OUTPUT *** Iteration "+i); if (verbose) { trainDocuments.foreach(printDocument _); println; println }
//      println("*** TEST OUTPUT *** Iteration "+i); if (verbose) { testDocuments.foreach(printDocument _); println; println }
//      println ("Iteration %2d TRAIN EVAL ".format(i)+evalString(trainDocuments))
//      println ("Iteration %2d TEST  EVAL ".format(i)+evalString(testDocuments))
//    }
//
//  }
//  
//  def run(runXmlDir:String): Unit = {
//    // Read in the data
//    val documents = new scala.collection.mutable.ArrayBuffer[Document]
//    import scala.xml._
//    for (dirname <- List(runXmlDir)) { // TODO make this take multiple directories
//      for (file <- files(new File(dirname))) {
//        //print("Reading ***"+(article\"head"\"title").text+"***")
//        print("Read ***"+file.getCanonicalPath+"***")
//        documents += cc.factorie.app.nlp.segment.ClearTokenizer.process(LoadNYTimesXML.fromFile(file).head)
//        println("  "+documents.last.asSection.length)
//        documents.last.asSection.foreach(t=> print(t.string+" ")); println
//      }
//    }
//    documents.foreach(addFeatures(_))
//    val testTokens = documents.map(_.tokens).flatten
//    println("Have "+testTokens.length+" tokens")
//    println("TokenDomain size="+SpanNerFeaturesDomain.dimensionSize)
//    println("LabelDomain "+ConllNerDomain.toList)
//    predictor.processAll(testTokens, 2)
//    documents.foreach(d => { println("FILE "+d.name); printDocument(d) })
//  }  
//
//  
//  def addFeatures(document:Document): Unit = {
//    for (token <- document.tokens) {
//      val features = token.attr += new SpanNerFeatures(token)
//      val rawWord = token.string
//      val word = cc.factorie.app.strings.simplifyDigits(rawWord)
//      features += "W="+word.toLowerCase
//      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
//      //features += "SUFFIX3="+word.takeRight(3)
//      //features += "PREFIX3="+word.take(3)
//      //features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
//      //if (token.isCapitalized) features += "CAPITALIZED"
//      //if (token.containsDigit) features += "NUMERIC"
//      if (token.isPunctuation) features += "PUNCTUATION"
//      // Add lexicon features.  Do it before making conjunctions so that they will be part of conjunctions
//      if (lexicons.size > 0)
//        for (lexicon <- lexicons) if (lexicon.contains(token)) features += "LEX="+lexicon.name
//    }
//    // Make features of offset conjunctions
//    for (sentence <- document.sentences)
//      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[SpanNerFeatures], List(0), List(0,0), List(-1), List(-1,0), List(1))
//    // The remaining features will not be put into conjunctions
//
//    // For documents that have a "-" within the first three words, the first word is a HEADER feature; apply it to all words in the document
//    //documents.foreach(d => if (d.take(3).map(_.word).contains("-")) { val f = "HEADER="+d(0).word.toLowerCase; d.foreach(t => t += f)})
//
//    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
//    document.sentences.foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t.attr[SpanNerFeatures] += "SENTENCEUPPERCASE"))
//
//    // Add features for character n-grams between sizes 2 and 5
//    document.tokens.foreach(t => if (t.string.matches("[A-Za-z]+")) t.attr[SpanNerFeatures] ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
//
//    // Add features from window of 4 words before and after
//    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplify(t2.word).toLowerCase)))
//    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplify(t2.word).toLowerCase)))
//    
//    // Put features of first mention on later mentions
//    document.tokens.foreach(t => {
//      if (t.isCapitalized && t.string.length > 1 && !t.attr[SpanNerFeatures].activeCategories.exists(f => f.matches(".*FIRSTMENTION.*"))) {
//        //println("Looking for later mentions of "+t.word)
//        var t2 = t
//        while (t2.hasNext) {
//          t2 = t2.next
//          if (t2.string == t.string) { 
//            //println("Adding FIRSTMENTION to "+t2.word); 
//            t2.attr[SpanNerFeatures] ++= t.attr[SpanNerFeatures].activeCategories.filter(_.contains("@")).map(f => "FIRSTMENTION="+f)
//          }
//        }
//      }
//    })
//  }
//
//  def evalString(documents:Seq[Document]): String = {
//    var trueCount = 0
//    var predictedCount = 0
//    var correctCount = 0
//    for (document <- documents) {
//      predictedCount += document.spanCount
//      document.tokens.foreach(_.spansOfClass[NerSpan].foreach(span => if (span.isCorrect) correctCount += 1))
//      for (token <- document.tokens) {
//        val tokenTargetCategory = token.nerLabel.target.categoryValue
//        if (tokenTargetCategory != "O" && (!token.hasPrev || token.prev.nerLabel.target.categoryValue != tokenTargetCategory))
//           trueCount += 1
//      }
//    }
//    def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
//    def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
//    def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
//    "OVERALL f1=%-6f p=%-6f r=%-6f".format(f1, precision, recall)
//  }
//  
//  
//  def printFeatures(token:Token): Unit = {
//    println(token.attr[SpanNerFeatures])
//  }
//
//  
//  def printDocument(document:Document): Unit = {
//    for (token <- document.tokens) {
//      token.startsSpansOfClass[NerSpan].foreach(span => print("<"+span.label.value+">"))
//      print(token.string)
//      token.endsSpansOfClass[NerSpan].foreach(span => print("</"+span.label.value+">"))
//      print(" ")
//    }
//    println
//    for (span <- document.sections.flatMap(_.spansOfClass[NerSpan].sortForward(span => span.start.toDouble))) {
//      println("%s len=%-2d %-8s %-15s %-30s %-15s".format(
//          if (span.isCorrect) " " else "*",
//          span.length,
//          span.label.value, 
//          if (span.hasPredecessor(1)) span.predecessor(1).string else "<START>", 
//          span.phrase, 
//          if (span.hasSuccessor(1)) span.successor(1).string else "<END>"))
//    }
//  }
//  
//  def printToken(token:Token) : Unit = {
//    //print("printToken "+token.word+"  ")
//    val spans = token.spansOfClass[NerSpan]
//    for (span <- spans) {
//      println("%s %-8s %-15s %-30s %-15s".format(
//          if (span.isCorrect) " " else "*",
//          span.label.value, 
//          if (span.hasPredecessor(1)) span.predecessor(1).string else "<START>", 
//          span.phrase, 
//          if (span.hasSuccessor(1)) span.successor(1).string else "<END>"))
//      span.foreach(token => print(token.string+" ")); println
//    }
//  }
//    
//  // Recursively descend directory, returning a list of files.
//  def files(directory:File): Seq[File] = {
//    if (!directory.exists) throw new Error("File "+directory+" does not exist")
//    if (directory.isFile) return List(directory)
//    val result = new scala.collection.mutable.ArrayBuffer[File]
//    for (entry <- directory.listFiles) {
//      if (entry.isFile) result += entry
//      else if (entry.isDirectory) result ++= files(entry)
//    }
//    result
//  }
//
//  
//}
//
//object SpanNER extends SpanNER()(new scala.util.Random(0)) {
//  // The "main", examine the command line and do some work
//  def main(args: Array[String]): Unit = {
//    import CubbieConversions._
//    // Parse command-line
//    object opts extends DefaultCmdOptions {
//      val trainFile = new CmdOption("train", List("eng.train"), "FILE", "CoNLL formatted training file.")
//      val testFile  = new CmdOption("test",  "", "FILE", "CoNLL formatted dev file.")
//      val modelFile =  new CmdOption("model", "spanner.factorie", "FILE", "File for saving or loading model.")
//      val runXmlDir = new CmdOption("run", "xml", "DIR", "Directory for reading NYTimes XML data on which to run saved model.")
//      val lexiconDir =new CmdOption("lexicons", "lexicons", "DIR", "Directory containing lexicon files named cities, companies, companysuffix, countries, days, firstname.high,...") 
//      val verbose =   new CmdOption("verbose", "Turn on verbose output") { override def invoke = SpanNER.this.verbose = false }
//      val noSentences=new CmdOption("nosentences", "Do not use sentence segment boundaries in training.  Improves accuracy when testing on data that does not have sentence boundaries.")
//    }
//    opts.parse(args)
//    
//    if (opts.lexiconDir.wasInvoked) {
//      for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
//        println("Reading lexicon "+filename)
//        lexicons += new Lexicon(new File(opts.lexiconDir.value+"/"+filename))
//      }
//    }
//    
//    if (opts.runXmlDir.wasInvoked) {
//      //println("statClasses "+model.templatesOf[VectorTemplate].toList.map(_.statClasses))
//      BinarySerializer.deserialize(SpanNerFeaturesDomain, model, new File(opts.modelFile.value))
//      run(opts.runXmlDir.value)
//    } else {
//      train(opts.trainFile.value, opts.testFile.value)
//      if (opts.modelFile.wasInvoked)
//        BinarySerializer.serialize(SpanNerFeaturesDomain, model, new File(opts.modelFile.value))
//    }
//  }
//}
//
