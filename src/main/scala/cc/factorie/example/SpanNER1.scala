/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap,ListBuffer}
import cc.factorie._
import cc.factorie.app.tokenseq
import scala.io.Source
import java.io.File
import cc.factorie.util.DefaultCmdOptions

// Sample training command line:
// --train=/Users/mccallum/research/data/ie/ner2003/eng.train,/Users/mccallum/research/data/ie/ner2003/eng.testa --test=/Users/mccallum/research/data/ie/ner2003/eng.testb --model=/Users/mccallum/tmp/spanner1.factorie --lexicons=/Users/mccallum/research/data/resources/lexicons --nosentences --verbose
// Sample running command line:
// --run=/Users/mccallum/research/projects/football/data/1990/12/05  --model=/Users/mccallum/tmp/spanner1.factorie --lexicons=/Users/mccallum/research/data/resources/lexicons --verbose --nosentences
  
/** Span-based named entity recognition.
    Includes the ability to save model to disk, and run saved model on NYTimes-style XML files.
    @author Andrew McCallum */
object SpanNER1 {
  var verbose = false

  // The variable classes
  class Token(word:String, trueLabelString:String) extends tokenseq.Token[Sentence,Token](word) {
    // TODO Consider instead implementing truth with true spans in VariableSeqWithSpans. 
    def trueLabelValue = trueLabelString
    val trueLabelIndex = Domain[Label].index(trueLabelValue)
    def spans:Seq[Span] = seq.spansContaining(position).toList
    def spanStarts: Iterable[Span] = seq.spansStartingAt(position)
    def spanEnds: Iterable[Span] = seq.spansEndingAt(position)
    override def skipNonCategories = true
  }
  class Label(labelName:String, val span: Span) extends LabelVariable(labelName)
  class Span(labelString:String, seq:Sentence, start:Int, len:Int)(implicit d:DiffList) extends SpanVariableInSeq[Token,Span](seq, start, len) {
    val label = new Label(labelString, this)
    def spanLength = new SpanLength(len)
    override def phrase = this.map(_.word).mkString(" ")
    def isCorrect = this.forall(token => token.trueLabelValue == label.value) &&
      (!hasPredecessor(1) || predecessor(1).trueLabelValue != label.value) && 
      (!hasSuccessor(1) || successor(1).trueLabelValue != label.value)
    // Does this span contain the words of argument span in order?
    def contains(span:Span): Boolean = {
      for (i <- 0 until length) {
        if (length - i < span.length) return false
        var result = true
        var i2 = i; var j = 0
        while (j < span.length && i2 < this.length && result) {
          if (span(j).word != this(i2)) result = false
          j += 1; i2 += 1
        }
        if (result == true) return true 
      }
      return false
    }
    override def toString = "Span("+length+","+label.value+":"+this.phrase+")"
  }
  class Sentence extends tokenseq.TokenSeq[Token,Sentence] with VariableSeqWithSpans[Token,Span] {
    var filename:String = null
  }
  class SpanLength(x:Int) extends DiscreteVariable {
    if (x < domain.size) set(x)(null) else set(domain.size-1)(null)
  }
  Domain[SpanLength].size = 6
  class Lexicon(filename:String) extends tokenseq.Lexicon(filename) {
    def name = filename.substring(filename.lastIndexOf('/')+1).toUpperCase
  }
  val lexicons = new ArrayBuffer[Lexicon]
  
  // Not ready for use because does not coordinate with changes to Span boundaries
  val allSpans = new HashMap[String,ListBuffer[Span]] {
    def remove(span:Span): Unit = {
      for (token <-span) {
        val lb: ListBuffer[Span] = this.apply(token.word)
        lb -= span
      }
    }
    def add(span:Span): Unit = {
      for (token <- span) {
        val lb = this.apply(token.word)
        lb += span
      }
    }
    def spansContaining(span:Span): Traversable[Span] = {
      val result = new HashSet[Span]
      for (token <- span) {
        val lb = this.apply(token.word)
        lb.foreach(span2 => if (span2.contains(span)) result += span2)
      }
      return result
    }
  }
  
  // The model
  abstract class SpanLabelTemplate extends Template2[Span,Label] {
    def unroll1(span:Span) = Factor(span, span.label)
    def unroll2(label:Label) = Factor(label.span, label)
  }
  val model = new Model(
    // Bias term on each individual label 
    //new TemplateWithDotStatistics1[Label],
    // Token-Label within Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = for (token <- span) yield Stat(token, label) },
    // First Token of Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = Stat(span.head, span.label) },
    // Last Token of Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = Stat(span.last, span.label) },
    // Token before Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.head.hasPrev) Stat(span.head.prev, span.label) else Nil },
    //new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.head.hasPrev && span.head.prev.hasPrev) Stat(span.head.prev.prev, span.label) else Nil },
    // Token after Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext) Stat(span.last.next, span.label) else Nil },
    // Single Token Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.length == 1) Stat(span.head, span.label) else Nil }
    //new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext && span.last.next.hasNext) Stat(span.last.next.next, span.label) else Nil },
    // Span Length with Label
    //new SpanLabelTemplate with DotStatistics2[SpanLength,Label] { def statistics(span:Span, label:Label) = Stat(span.spanLength, span.label) },
    // Label of span that preceeds or follows this one
    /*new Template2[Span,Span] with Statistics2[Label,Label] {
      def unroll1(span:Span) = { val result = Nil; var t = span.head; while (t.hasPrev) { if } }
    }*/
  )
  
  // The training objective
  val objective = new Model(
    new TemplateWithStatistics2[Span,Label] {
      def unroll1(span:Span) = Factor(span, span.label)
      def unroll2(label:Label) = Factor(label.span, label)
      def score(s:Stat) = {
        val span = s._1
        var result = 0.0
        var trueLabelIncrement = 10.0
        var allTokensCorrect = true
        for (token <- span) {
          //if (token.trueLabelValue != "O") result += 2.0 else result -= 1.0
          if (token.trueLabelValue == span.label.value) {
            result += trueLabelIncrement
            trueLabelIncrement += 2.0 // proportionally more benefit for longer sequences to help the longer seq steal tokens from the shorter one.
          } else if (token.trueLabelValue == "O") {
            result -= 1.0
            allTokensCorrect = false
          } else {
            result += 1.0
            allTokensCorrect = false
          }
          if (token.spans.length > 1) result -= 100.0 // penalize overlapping spans
        }
        if (allTokensCorrect) {
          if (!span.hasPredecessor(1) || span.predecessor(1).trueLabelValue != span.label.value) result += 5.0 // reward for getting starting boundary correct
          if (!span.hasSuccessor(1) || span.successor(1).trueLabelValue != span.label.value) result += 5.0 // reward for getting starting boundary correct
        }
        result
      }
    }
  )
  
  // The sampler
  class TokenSpanSampler(model:Model, objective:Model) extends SettingsSampler[Token](model, objective) {
    // The proposer for changes to Spans touching this Token
    def settings(token:Token) = new SettingIterator {
      val seq = token.seq
      val changes = new ArrayBuffer[(DiffList)=>Unit];
      val existingSpans = token.spans
      //println("existing spans = "+existingSpans)
      for (span <- existingSpans) {
        // Change label without changing boundaries
        for (labelValue <- Domain[Label]; if (labelValue != "O"))
          changes += {(d:DiffList) => span.label.set(labelValue)(d)}
        // Delete the span
        changes += {(d:DiffList) => span.delete(d)}
        if (span.length > 1) {
          // Trim last word, without changing label
          changes += {(d:DiffList) => span.trimEnd(1)(d)}
          // Trim first word, without changing label
          changes += {(d:DiffList) => span.trimStart(1)(d)}
          // Split off first and last word, with choices of the label of the split off portion
          for (labelValue <- Domain[Label]; if (labelValue != "O")) {
            changes += {(d:DiffList) => { span.trimEnd(1)(d); new Span(labelValue, seq, span.end+1, 1)(d) } }
            changes += {(d:DiffList) => { span.trimStart(1)(d); new Span(labelValue, seq, span.start-1, 1)(d) } }
          }
        }
        if (span.length == 3) {
          // Split span, dropping word in middle, preserving label value
          changes += {(d:DiffList) => span.delete(d); new Span(span.label.value, seq, span.start, 1)(d); new Span(span.label.value, seq, span.end, 1)(d) }
        }
        // Add a new word to beginning, and change label
        if (span.canPrepend(1)) {
          for (labelValue <- Domain[Label]; if (labelValue != "O"))
            changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.prepend(1)(d); span.head.spans.filter(_ != span).foreach(_.trimEnd(1)(d)) } }
        }
        // Add a new word to the end, and change label
        if (span.canAppend(1)) {
          for (labelValue <- Domain[Label]; if (labelValue != "O"))
            changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.append(1)(d); span.last.spans.filter(_ != span).foreach(_.trimStart(1)(d)) } }
        }
        //if (span.length > 1) changes += {(d:DiffList) => { span.trimEnd(1)(d); new Span(labelValue, seq, position+1, 1)(d) } }
      }
      if (existingSpans.isEmpty) {
        changes += {(d:DiffList) => {}} // The no-op action
        for (labelValue <- Domain[Label]; if (labelValue != "O")) {
          // Add new length=1 span, for each label value
          changes += {(d:DiffList) => new Span(labelValue, seq, token.position, 1)(d)}
          //if (position != seq.length-1) changes += {(d:DiffList) => new Span(labelValue, seq, position, 2)(d)}
        }
      }
      //println("Token.settings length="+changes.length)
      var i = 0
      def hasNext = i < changes.length
      def next(d:DiffList) = { val d = new DiffList; changes(i).apply(d); i += 1; d }
      def reset = i = 0
    }
  }
  
  // The predictor for test data
  val predictor = new TokenSpanSampler(model, null) { 
    temperature = 0.0001 
    override def preProcessHook(t:Token): Token = { 
      super.preProcessHook(t)
      if (t.isCapitalized) {
        if (verbose) t.spans.foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
        t 
      } else null.asInstanceOf[Token] 
    }
    override def proposalsHook(proposals:Seq[Proposal]): Unit = {
      if (verbose) println("Test proposal")
      //proposals.foreach(println(_)); println
      if (verbose) { proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else ""))); println }
      super.proposalsHook(proposals)
    }
    override def proposalHook(proposal:Proposal): Unit = {
      super.proposalHook(proposal)
      // If we changed the possible world last time, try sampling it again right away to see if we can make more changes
      if (proposal.diff.size > 0) {
        val spanDiffs = proposal.diff.filter(d => d.variable match { case s:Span => s.present; case _ => false })
        spanDiffs.foreach(_.variable match {
          case span:Span => if (span.present) { if (verbose) println("RECURSIVE PROPOSAL"); this.process(span.last) }
          case _ => {}
        })
      }
    }
  }

  
  
  // The "main", examine the command line and do some work
  def main(args: Array[String]): Unit = {
    // Parse command-line
    object opts extends DefaultCmdOptions {
      val trainFile = new CmdOption("train", "FILE", List("eng.train"), "CoNLL formatted training file.")
      val testFile  = new CmdOption("test",  "FILE", "", "CoNLL formatted dev file.")
      val modelDir =  new CmdOption("model", "DIR",  "spanner1.factorie", "Directory for saving or loading model.")
      val runXmlDir = new CmdOption("run",   "DIR",  "xml", "Directory for reading data on which to run saved model.")
      val lexiconDir =new CmdOption("lexicons", "DIR", "lexicons", "Directory containing lexicon files named cities, companies, companysuffix, countries, days, firstname.high,...") 
      val verbose =   new CmdOption("verbose", "Turn on verbose output") { override def invoke = SpanNER1.this.verbose = true }
      val noSentences=new CmdOption("nosentences", "Do not use sentence segment boundaries in training.  Improves accuracy when testing on data that does not have sentence boundaries.")
    }
    opts.parse(args)
    
    if (opts.lexiconDir.wasInvoked) {
      for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
        println("Reading lexicon "+filename)
        lexicons += new Lexicon(opts.lexiconDir.value+"/"+filename)
      }
    }
    
    if (opts.runXmlDir.wasInvoked) {
      //println("statClasses "+model.templatesOf[VectorTemplate].toList.map(_.statClasses))
      model.load(opts.modelDir.value)
      run(opts.runXmlDir.value)
    } else {
      train(opts.trainFile.value, opts.testFile.value, opts.noSentences.wasInvoked)
      if (opts.modelDir.wasInvoked) model.save(opts.modelDir.value)
    }   
  }
  
  
  // Run a pre-trained model on some NYTimes XML data
  def run(runXmlDir:String): Unit = {
    import scala.xml._
    // Read in the data
    val documents = new ArrayBuffer[Sentence]
    for (dirname <- List(runXmlDir)) { // TODO make this take multiple directories
      for (file <- files(new File(dirname))) {
        val article = XML.loadFile(file)
        //println(article \\ "head" \\ "title" text)
        //println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
        val content = article \ "head" \ "docdata" \ "identified-content"
        print("Reading ***"+(article\"head"\"title").text+"***")
        documents += tokenseq.TokenSeq.fromPlainText[Sentence,Token](
          scala.io.Source.fromString((article \ "body" \ "body.content").text), 
          ()=>new Sentence,
          (word,lab)=>new Token(word,lab),
          "O", 
          (strs)=>featureExtractor(strs), 
          "'s|n't|a\\.m\\.|p\\.m\\.|Inc\\.|Corp\\.|St\\.|Mr\\.|Mrs\\.|Dr\\.|([A-Z]\\.)+|vs\\.|[-0-9,\\.]+|$|[-A-Za-z0-9\\+$]+|\\p{Punct}".r)
        documents.last.filename = file.toString
        println("  "+documents.last.size)
        documents.last.foreach(t=> print(t.word+" ")); println
      }
    }
    addFeatures(documents)
    val testTokens = documents.flatMap(x=>x)
    println("Have "+testTokens.length+" tokens")
    println("Domain[Token] size="+Domain[Token].size)
    println("Domain[Label] "+Domain[Label].toList)
    predictor.processAll(testTokens, 2)
    documents.foreach(s => { println("FILE "+s.filename); printSentence(s) })
  }
  
  // Train a new model and evaluate on the dev set
  def train(trainFiles:Seq[String], devFile:String, ignoreSentenceBoundaries:Boolean): Unit = {
    // Read training and testing data.  The function 'featureExtractor' function is defined below.  Now training on seq == whole doc, not seq == sentece
    def newSentenceFromOWPL(filename:String) = if (filename.length == 0) List[Sentence]() else 
      tokenseq.TokenSeq.fromOWPL[Sentence,Token](
        Source.fromFile(new File(filename)), 
        ()=>new Sentence, 
        (word,lab)=>new Token(word,lab), 
        featureExtractor _, 
        (lab:String) => if (lab.length > 2) lab.substring(2) else lab, 
        "-DOCSTART-".r,
        if (ignoreSentenceBoundaries) null else "\\A\\s*\\z".r,
        null)
    val trainSentences = trainFiles.flatMap(newSentenceFromOWPL(_)) 
    val testSentences = newSentenceFromOWPL(devFile) 
    println("Read "+trainSentences.length+" training sentences, and "+testSentences.length+" testing ")

    addFeatures(trainSentences ++ testSentences)
    val trainTokens = trainSentences.flatMap(x=>x) //.take(2000)
    val testTokens = testSentences.flatMap(x=>x)
    println("Have "+trainTokens.length+" trainTokens "+testTokens.length+" testTokens")
    println("Domain[Token] size="+Domain[Token].size)
    println("Domain[Label] "+Domain[Label].toList)
    
    if (verbose) trainTokens.take(500).foreach(printFeatures _)
    
    // The learner
    val learner = new TokenSpanSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {
      temperature = 0.01
      logLevel = 1
      override def preProcessHook(t:Token): Token = { 
        super.preProcessHook(t)
        if (t.isCapitalized) { // Skip tokens that are not capitalized
          if (verbose) t.spans.foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
          // Skip this token if it has the same spans as the previous token, avoiding duplicate sampling
          /*if (t.hasPrev && t.prev.spans.sameElements(t.spans)) null.asInstanceOf[Token] else*/ t 
        } else null.asInstanceOf[Token] 
      }
      override def proposalsHook(proposals:Seq[Proposal]): Unit = {
        if (verbose) { proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else "")+(if (p.objectiveScore > 0.0) "OO" else ""))); println }
        super.proposalsHook(proposals)
      }
    }
    
    // Train!
    for (i <- 1 to 11) {
      println("Iteration "+i) 
      // Every third iteration remove all the predictions
      if (i % 3 == 0) { if (verbose) println("Removing all spans"); (trainSentences ++ testSentences).foreach(_.clearSpans) }
      learner.processAll(trainTokens)
      //learner.learningRate *= 0.9
      predictor.processAll(testTokens)
      println("*** TRAIN OUTPUT *** Iteration "+i); if (verbose) { trainSentences.foreach(printSentence _); println; println }
      println("*** TEST OUTPUT *** Iteration "+i); if (verbose) { testSentences.foreach(printSentence _); println; println }
      println ("Iteration %2d TRAIN EVAL ".format(i)+evalString(trainSentences))
      println ("Iteration %2d TEST  EVAL ".format(i)+evalString(testSentences))
    }

  }

  
  
  
  
  def addFeatures(sentences:Seq[Sentence]): Unit = {
    // Add lexicon features.  Do it before making conjunctions so that they will be part of conjunctions
    if (lexicons.size > 0)
      for (sentence <- sentences; token <- sentence; lexicon <- lexicons) if (lexicon.contains(token)) token += "LEX="+lexicon.name

    // Make features of offset conjunctions
    sentences.foreach(s => s.addNeighboringFeatureConjunctions(List(0), List(0,0), List(-1), List(-1,0), List(1)))

    // Gather tokens into documents
    val documents = new ArrayBuffer[ArrayBuffer[Token]]; documents += new ArrayBuffer[Token]
    sentences.foreach(s => if (s.length == 0) documents += new ArrayBuffer[Token] else documents.last ++= s)
    // For documents that have a "-" within the first three words, the first word is a HEADER feature; apply it to all words in the document
    documents.foreach(d => if (d.take(3).map(_.word).contains("-")) { val f = "HEADER="+d(0).word.toLowerCase; d.foreach(t => t += f)})

    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    sentences.foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t += "SENTENCEUPPERCASE"))

    // Add features for character n-grams between sizes 2 and 5
    sentences.foreach(s => s.foreach(t => if (t.word.matches("[A-Za-z]+")) t ++= t.charNGrams(2,5).map(n => "NGRAM="+n)))

    // Add features from window of 4 words before and after
    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplify(t2.word).toLowerCase)))
    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplify(t2.word).toLowerCase)))
    
    // Put features of first mention on later mentions
    documents.foreach(d => {
      d.foreach(t => {
        if (t.isCapitalized && t.word.length > 1 && !t.values.exists(f => f.matches(".*FIRSTMENTION.*"))) {
          //println("Looking for later mentions of "+t.word)
          var t2 = t
          while (t2.hasNext) {
            t2 = t2.next
            if (t2.word == t.word) { /*println("Adding FIRSTMENTION to "+t2.word); */ t2 ++= t.values.filter(_.contains("@")).map(f => "FIRSTMENTION="+f) }
          }
        }
      })
    })
  }
  
  def evalString(sentences:Seq[Sentence]): String = {
    var trueCount = 0
    var predictedCount = 0
    var correctCount = 0
    for (sentence <- sentences) {
      predictedCount += sentence.spans.length
      sentence.spans.foreach(span => if (span.isCorrect) correctCount += 1)
      for (token <- sentence)
        if (token.trueLabelValue != "O" && (!token.hasPrev || token.prev.trueLabelValue != token.trueLabelValue))
           trueCount += 1
    }
    def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
    def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
    def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
    "OVERALL f1=%-6f p=%-6f r=%-6f".format(f1, precision, recall)
  }
  
  
  def printFeatures(token:Token): Unit = {
    println(token)
  }
  
  def printSentence(sentence:Sentence): Unit = {
    for (token <- sentence) {
      token.spanStarts.foreach(span => print("<"+span.label.value+">"))
      print(token.word)
      token.spanEnds.foreach(span => print("</"+span.label.value+">"))
      print(" ")
    }
    println
    for (span <- sentence.spans.sortForward(span => span.start.toDouble)) {
      println("%s len=%-2d %-8s %-15s %-30s %-15s".format(
          if (span.isCorrect) " " else "*",
          span.length,
          span.label.value, 
          if (span.hasPredecessor(1)) span.predecessor(1).word else "<START>", 
          span.phrase, 
          if (span.hasSuccessor(1)) span.successor(1).word else "<END>"))
    }
  }
  
  def printToken(token:Token) : Unit = {
    //print("printToken "+token.word+"  ")
    val spans = token.spans
    for (span <- spans) {
      println("%s %-8s %-15s %-30s %-15s".format(
          if (span.isCorrect) " " else "*",
          span.label.value, 
          if (span.hasPredecessor(1)) span.predecessor(1).word else "<START>", 
          span.phrase, 
          if (span.hasSuccessor(1)) span.successor(1).word else "<END>"))
      span.foreach(token => print(token.word+" ")); println
    }
  }

  // Feature extraction

  // Simplified form of word for feature generation
  def simplify(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word.toLowerCase
  }

  // Collection of features created using raw features from data file 
  def featureExtractor(initialFeatures:Seq[String]) : Seq[String] = {
    //println("featureExtractor "+initialFeatures)
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    f += "SHAPE="+tokenseq.wordShape(word, 2)
    f += "W="+simplify(word)
    //if (initialFeatures.length > 1) f += "POS="+initialFeatures(1)
    //if (initialFeatures.length > 2) f += "PHRASE="+initialFeatures(2)
    //if (Character.isUpperCase(word(0))) f += "CAPITALIZED" // Already handled by SHAPE features
    f
  }
  
  /** Recursively descend directory, returning a list of files. */
  def files(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }
  

}


