/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer,ArrayStack}

object SpanNER1 {

  // The variable classes
  class Token(val word:String, val trueLabelValue:String) extends BinaryVectorVariable[String] with VarInTypedSeq[Token,Sentence] with IterableSettings {
    val trueLabelIndex = Domain[Label].index(trueLabelValue)
    def spans:Seq[Span] = seq.spansContaining(position).toList
    def isCapitalized = java.lang.Character.isUpperCase(word(0))
    // The proposer for changes to Spans touching this Token
    def settings = new SettingIterator {
      val changes = new ArrayBuffer[(DiffList)=>Unit];
      val existingSpans = spans
      //println("existing spans = "+existingSpans)
      for (span <- existingSpans) {
        for (labelValue <- Domain[Label]; if (labelValue != "O"))
          changes += {(d:DiffList) => span.label.set(labelValue)(d)}
        changes += {(d:DiffList) => span.delete(d)}
        if (span.length > 1) {
          changes += {(d:DiffList) => span.trimEnd(1)(d)}
          changes += {(d:DiffList) => span.trimStart(1)(d)}
          for (labelValue <- Domain[Label]; if (labelValue != "O")) {
            changes += {(d:DiffList) => { span.trimEnd(1)(d); new Span(labelValue, seq, span.end+1, 1)(d) } }
            changes += {(d:DiffList) => { span.trimStart(1)(d); new Span(labelValue, seq, span.start-1, 1)(d) } }
          }
        }
        if (span.canPrepend(1)) {
          for (labelValue <- Domain[Label]; if (labelValue != "O"))
          	changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.prepend(1)(d); span.first.spans.filter(_ != span).foreach(_.trimEnd(1)(d)) } }
        }
        if (span.canAppend(1)) {
          for (labelValue <- Domain[Label]; if (labelValue != "O"))
          	changes += {(d:DiffList) => { span.label.set(labelValue)(d); span.append(1)(d); span.last.spans.filter(_ != span).foreach(_.trimStart(1)(d)) } }
        }
        //if (span.length > 1) changes += {(d:DiffList) => { span.trimEnd(1)(d); new Span(labelValue, seq, position+1, 1)(d) } }
      }
      if (existingSpans.isEmpty) {
        changes += {(d:DiffList) => {}} // The no-op action
        for (labelValue <- Domain[Label]; if (labelValue != "O")) {
          changes += {(d:DiffList) => new Span(labelValue, seq, position, 1)(d)}
          //if (position != seq.length-1) changes += {(d:DiffList) => new Span(labelValue, seq, position, 2)(d)}
        }
      }
      println("Token.settings length="+changes.length)
      var i = 0
      def hasNext = i < changes.length
      def next(d:DiffList) = { val d = new DiffList; changes(i).apply(d); i += 1; d }
      def reset = i = 0
    }
  }
  class Label(labelName:String, val span: Span) extends LabelVariable(labelName)
  class Span(labelString:String, seq:Sentence, start:Int, len:Int)(implicit d:DiffList) extends SpanVariable(seq, start, len) {
    val label = new Label(labelString, this)
    def spanLength = new SpanLength(len)
    override def phrase = this.map(_.word).mkString(" ")
    def isCorrect = this.forall(token => token.trueLabelValue == label.value) &&
    	(!hasPredecessor(1) || predecessor(1).trueLabelValue != label.value) && 
    	(!hasSuccessor(1) || successor(1).trueLabelValue != label.value)
    override def toString = "Span("+length+","+label.value+":"+this.phrase+")"
  }
  class Sentence extends VariableSeqWithSpans[Token,Span]
  @DomainSize(5) class SpanLength(x:Int) extends DiscreteVariable {
    if (x < domain.size) setByInt(x)(null) else setByInt(domain.size-1)(null)
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
    // Token after Span
    new SpanLabelTemplate with DotStatistics2[Token,Label] { def statistics(span:Span, label:Label) = if (span.last.hasNext) Stat(span.last.next, span.label) else Nil }.init,
    // Span Length with Label
    //new SpanLabelTemplate with DotStatistics2[SpanLength,Label] { def statistics(span:Span, label:Label) = Stat(span.spanLength, span.label) }.init
  )
  
  // The training objective
  val objective = new Model(
    new TemplateWithStatistics2[Span,Label] {
      def unroll1(span:Span) = Factor(span, span.label)
      def unroll2(label:Label) = Factor(label.span, label)
      def score(s:Stat) = {
        val span = s.s1
        var result = 0.0
        for (token <- span) {
          if (token.trueLabelValue != "O") result += 2.0 else result -= 1.0
          if (token.trueLabelValue == span.label.value) {
            result += 5.0 
            if (!span.hasPredecessor(1) || span.predecessor(1).trueLabelValue != span.label.value) result += 5.0 // reward for getting starting boundary correct
            if (!span.hasSuccessor(1) || span.successor(1).trueLabelValue != span.label.value) result += 5.0 // reward for getting starting boundary correct
          } else result -= 1.0
          if (token.spans.length > 1) result -= 100.0 // penalize overlapping spans
        }
        result
      }
    }
  )
  

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER3 trainfile testfile")

    // Read in the data
    val trainSentences = load(args(0)) //.take(500)
    val testSentences = load(args(1))
    val trainTokens = trainSentences.flatMap(x=>x) //.take(2000)
    val testTokens = testSentences.flatMap(x=>x)
    val allTokens: Seq[Token] = trainTokens ++ testTokens
    // Add features from next and previous tokens 
    allTokens.foreach(t => {
      //if (t.hasPrev) t ++= (for (f1 <- t.values; f2 <- t.prev.values; if (!f1.contains('@') && !f2.contains('@'))) yield f1+"_&_"+f2+"@-1")
      //if (t.hasNext) t ++= (for (f1 <- t.values; f2 <- t.next.values; if (!f1.contains('@') && !f2.contains('@'))) yield f1+"_&_"+f2+"@+1")
      if (t.hasPrev) t ++= t.prev.values.filter(!_.contains('@')).map(_+"@-1")
      if (t.hasNext) t ++= t.next.values.filter(!_.contains('@')).map(_+"@+1")
    })
    println("Have "+trainTokens.length+" trainTokens")
    println("Domain[Label] "+Domain[Label].toList)
    
    trainTokens.take(200).foreach(printFeatures _)
    
    // Sample and Learn!
    val learner = new GibbsSampler1[Token](model, objective) with SampleRank with GradientAscentUpdates {
      temperature = 0.1
      override def preProcessHook(t:Token): Token = { super.preProcessHook(t); if (t.isCapitalized) t else null.asInstanceOf[Token] }
      override def proposalsHook(proposals:Seq[Proposal]): Unit = {
        proposals.foreach(println(_))
        super.proposalsHook(proposals)
      }
    }
    val predictor = new GibbsSampler1[Token](model) { temperature = 0.1 }
    for (i <- 1 to 20) {
      println("Iteration "+i) 
      learner.process(trainTokens, 1)
      predictor.process(testTokens, 1)
      println("*** TRAIN OUTPUT ***"); trainSentences.foreach(printSentence _); println; println
      println("*** TEST OUTPUT ***"); testSentences.foreach(printSentence _); println; println
      //println ("Train accuracy = "+ objective.aveScore(trainLabels))
      //println ("Test  accuracy = "+ objective.aveScore(testLabels))
    }
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
  def wordToFeatures(word:String) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    f += "W="+simplify(word)
    if (word.matches("[A-Za-z0-9]+")) f += "SHAPE="+cc.factorie.application.LabeledTokenSeqs.wordShape(word, 2)
    //if (word.length > 3) f += "PRE="+word.substring(0,3)
    if (Capitalized.findFirstMatchIn(word) != None) f += "CAPITALIZED"
    if (Numeric.findFirstMatchIn(word) != None) f += "NUMERIC"
    if (Punctuation.findFirstMatchIn(word) != None) f += "PUNCTUATION"
    f
  }
  val Capitalized = "^[A-Z].*".r
  val Numeric = "^[0-9]+$".r
  val Punctuation = "[-,\\.;:?!()]+".r

  // Simplified form of word for feature generation
  def simplify(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#")
    else word
  }

  def load(filename:String) : Seq[Sentence] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    var wordCount = 0
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(filename)
    var sentence = new Sentence
    for (line <- source.getLines) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val pos = fields(1)
        val label = if (fields(3).length > 2) fields(3).stripLineEnd.substring(2).intern else fields(3).stripLineEnd.intern
        val token = new Token(word, label)
        token ++= wordToFeatures(word)
        token += "POS="+pos
        sentence += token 
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


