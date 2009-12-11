/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer,HashMap,ListBuffer}
import cc.factorie.application.TokenSeqs
import scala.io.Source
import java.io.File

/** Span-based named entity recognition.
    Includes the ability to save model to disk, and run saved model on NYTimes-style XML files.
    @author Andrew McCallum */
object SpanNER1 {

  // The variable classes
  class Token(word:String, trueLabelString:String) extends TokenSeqs.Token[Sentence,Token](word, trueLabelString) {
    // TODO Consider instead implementing truth with true spans in VariableSeqWithSpans. 
    val trueLabelIndex = Domain[Label].index(trueLabelValue)
    def spans:Seq[Span] = seq.spansContaining(position).toList
    override def skipNonCategories = true
  }
  class Label(labelName:String, val span: Span) extends LabelVariable(labelName)
  class Span(labelString:String, seq:Sentence, start:Int, len:Int)(implicit d:DiffList) extends SpanVariable(seq, start, len) {
    val label = new Label(labelString, this)
    def spanLength = new SpanLength(len)
    override def phrase = this.map(_.word).mkString(" ")
    def isCorrect = this.forall(token => token.trueLabelValue == label.value) &&
    	(!hasPredecessor(1) || predecessor(1).trueLabelValue != label.value) && 
    	(!hasSuccessor(1) || successor(1).trueLabelValue != label.value)
    // Does this span contains the words of argument span in order?
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
  class Sentence extends TokenSeqs.TokenSeq[Token,Sentence] with VariableSeqWithSpans[Token,Span]
  @DomainSize(5) class SpanLength(x:Int) extends DiscreteVariable {
    if (x < domain.size) setByInt(x)(null) else setByInt(domain.size-1)(null)
  }
  
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
    def spansContaining(span:Span): Seq[Span] = {
      val result = new ArrayBuffer[Span]
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
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = for (token <- span) yield Stat(token, label) }.init,
    // First Token of Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = Stat(span.first, span.label) }.init,
    // Last Token of Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = Stat(span.last, span.label) }.init,
    // Token before Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.first.hasPrev) Stat(span.first.prev, span.label) else Nil }.init,
    //new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.first.hasPrev && span.first.prev.hasPrev) Stat(span.first.prev.prev, span.label) else Nil }.init,
    // Token after Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext) Stat(span.last.next, span.label) else Nil }.init,
    // Single Token Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.length == 1) Stat(span.first, span.label) else Nil }.init,
    //new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext && span.last.next.hasNext) Stat(span.last.next.next, span.label) else Nil }.init,
    // Span Length with Label
    //new SpanLabelTemplate with DotStatistics2[SpanLength,Label] { def statistics(span:Span, label:Label) = Stat(span.spanLength, span.label) }.init,
    // Label of span that preceeds or follows this one
    /*new Template2[Span,Span] with Statistics2[Label,Label] {
      def unroll1(span:Span) = { val result = Nil; var t = span.first; while (t.hasPrev) { if } }
    }*/
  )
  
  // The training objective
  val objective = new Model(
    new TemplateWithStatistics2[Span,Label] {
      def unroll1(span:Span) = Factor(span, span.label)
      def unroll2(label:Label) = Factor(label.span, label)
      def score(s:Stat) = {
        val span = s.s1
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
  class TokenSpanSampler(model:Model, objective:Model) extends SamplerOverSettings[Token](model, objective) {
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
            changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.prepend(1)(d); span.first.spans.filter(_ != span).foreach(_.trimEnd(1)(d)) } }
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
  	temperature = 0.001 
  	override def preProcessHook(t:Token): Token = { 
  		super.preProcessHook(t)
  		if (t.isCapitalized) {
  			t.spans.foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
  			t 
  		} else null.asInstanceOf[Token] 
  	}
  	override def proposalsHook(proposals:Seq[Proposal]): Unit = {
  		println("Test proposal")
  		//proposals.foreach(println(_)); println
  		proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else ""))); println
  		super.proposalsHook(proposals)
  	}
  	override def proposalHook(proposal:Proposal): Unit = {
  		super.proposalHook(proposal)
  		// If we changed the possible world last time, try sampling it again right away to see if we can make more changes
  		if (proposal.diff.size > 0) {
  			val spanDiffs = proposal.diff.filter(d => d.variable match { case s:Span => s.present; case _ => false })
  			spanDiffs.foreach(_.variable match {
  			case span:Span => if (span.present) this.process(span.last)
  			})
  		}
  	}
  }

  
  
  
  // The "main", examine the command line and do some work
  def main(args: Array[String]): Unit = {
    // Parse command-line
    if (!(args.length == 2 || args.length == 3)) 
      throw new Error("Usage: ChainNER3 [--model=dirname] trainfile testfile\nor   ChainNER3 --model=dirname --run=xmldir")
    var trainFile: String = null // train
    var devFile: String = null // test while training
    var modelDir: String = null // write or read model from here
    var runXmlDir: String = null // run saved model
    for (arg <- args) {
      if (arg.matches("--model=.*")) { modelDir = arg.split("=").apply(1); println("got "+modelDir) }
      else if (arg.matches("--run=.*")) runXmlDir = arg.split("=").apply(1)
      else if (trainFile == null) trainFile = arg
      else devFile = arg
    }
    
    if (runXmlDir != null) {
    	model.load(modelDir)
      run(runXmlDir)
    } else {
      train(trainFile, devFile)
      if (modelDir != null) model.save(modelDir)
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
        val content = article \\ "head" \\ "docdata" \\ "identified-content"
        print("Reading ***"+(article\\"head"\\"title").text+"***")
        documents += TokenSeqs.TokenSeq.fromPlainText(
        	Source.fromString((article \\ "body" \\ "body.content").text), 
        	(word,lab)=>new Token(word,lab),
        	()=>new Sentence,
        	"O", 
        	(str)=>featureExtractor(List(str)), "'s|n't|[-0-9,\\.]+|$|[-A-Za-z0-9\\+$]+|\\p{Punct}".r)
        println("  "+documents.last.size)
        documents.last.foreach(t=> print(t.word+" ")); println
      }
    }
    addFeatures(documents)
    val testTokens = documents.flatMap(x=>x)
    println("Have "+testTokens.length+" tokens")
    println("Domain[Token] size="+Domain[Token].size)
    println("Domain[Label] "+Domain[Label].toList)
    predictor.process(testTokens, 3)
    documents.foreach(printSentence _)
  }
  
  // Train a new model and evaluate on the dev set
  def train(trainFile:String, devFile:String): Unit = {
    // Read training and testing data.  The function 'featureExtractor' function is defined below
    def newSentenceFromOWPL(filename:String) = 
      TokenSeqs.TokenSeq.fromOWPL[Token,Sentence](
        Source.fromFile(filename), 
        (word,lab)=>new Token(word,lab), 
        ()=>new Sentence, 
        featureExtractor _, 
        (lab:String) => if (lab.length > 2) lab.substring(2) else lab, 
        "-DOCSTART-".r)
    val trainSentences = newSentenceFromOWPL(trainFile) 
    val testSentences = newSentenceFromOWPL(devFile) 
    println("Read "+trainSentences.length+" training sentences, and "+testSentences.length+" testing ")

    addFeatures(trainSentences ++ testSentences)
    val trainTokens = trainSentences.flatMap(x=>x) //.take(2000)
    val testTokens = testSentences.flatMap(x=>x)
    println("Have "+trainTokens.length+" trainTokens "+testTokens.length+" testTokens")
    println("Domain[Token] size="+Domain[Token].size)
    println("Domain[Label] "+Domain[Label].toList)
    
    trainTokens.take(500).foreach(printFeatures _)
    
    // The learner
    val learner = new TokenSpanSampler(model, objective) with SampleRank with ConfidenceWeightedUpdates {
      temperature = 0.01
      logLevel = 1
      override def preProcessHook(t:Token): Token = { 
        super.preProcessHook(t)
        if (t.isCapitalized) { // Skip tokens that are not capitalized
          t.spans.foreach(s => println({if (s.isCorrect) "CORRECT " else "INCORRECT "}+s))
          // Skip this token if it has the same spans as the previous token, avoiding duplicate sampling
          /*if (t.hasPrev && t.prev.spans.sameElements(t.spans)) null.asInstanceOf[Token] else*/ t 
        } else null.asInstanceOf[Token] 
      }
      override def proposalsHook(proposals:Seq[Proposal]): Unit = {
        proposals.foreach(p => println(p+"  "+(if (p.modelScore > 0.0) "MM" else "")+(if (p.objectiveScore > 0.0) "OO" else ""))); println
        super.proposalsHook(proposals)
      }
    }
    
    // Train!
    for (i <- 1 to 11) {
      println("Iteration "+i) 
      // Every third iteration remove all the predictions
      if (i % 3 == 0) { println("Removing all spans"); (trainSentences ++ testSentences).foreach(_.clearSpans) }
      learner.process(trainTokens, 1)
      //learner.learningRate *= 0.9
      predictor.process(testTokens, 1)
      println("*** TRAIN OUTPUT *** Iteration "+i); trainSentences.foreach(printSentence _); println; println
      println("*** TEST OUTPUT *** Iteration "+i); testSentences.foreach(printSentence _); println; println
      println ("Iteration %2d TRAIN EVAL ".format(i)+evalString(trainSentences))
      println ("Iteration %2d TEST  EVAL ".format(i)+evalString(testSentences))
    }

  }

  
  
  
  
  def addFeatures(sentences:Seq[Sentence]): Unit = {
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
            if (t2.word == t.word) { /*println("Adding FIRSTMENTION to "+t2.word);*/ t2 ++= t.values.filter(_.contains("@")).map(f => "FIRSTMENTION="+f) }
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
    for (span <- sentence.spans) {
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
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    f += "SHAPE="+TokenSeqs.wordShape(word, 2)
    f += "W="+simplify(word)
    if (initialFeatures.length > 1) f += "POS="+initialFeatures(1)
    if (initialFeatures.length > 2) f += "PHRASE="+initialFeatures(2)
    if (Character.isUpperCase(word(0))) f += "CAPITALIZED"
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


