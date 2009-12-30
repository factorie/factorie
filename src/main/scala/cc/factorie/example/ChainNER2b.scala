/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import scala.io.Source
import cc.factorie._ 
import cc.factorie.er._
import cc.factorie.application.LabeledTokenSeqs
import cc.factorie.application.LabeledTokenSeqs.LabeledTokenSeq
import scala.collection.mutable.ArrayBuffer
import java.io.File

/** Demo of CoNLL NER with lots of features.  Aiming for near state-of-the-art accuracy, but not yet finished.
    Apologies for the current mess here as we explore various options. */
object ChainNER2b {

  // Define the variable classes
  class Token(word:String, labelString:String) extends LabeledTokenSeqs.Token[Label,Token](word) {
    type GetterType = TokenGetter; class GetterClass extends TokenGetter
    val label = new Label(labelString, this)
    val wordtype = new Type(simplify(word)); class Type(s:String) extends EnumVariable(s)
    val tags = new Tags; class Tags extends BinaryVectorVariable[String]
    override def +=(feature:String): Unit = {
      //if (feature.matches("(POS|PHRASE)=[-A-Z]+|W=(and|or|of|the|for|de|del)")) tags += feature
      if (feature.matches("POS=[-A-Z]+")) tags += feature
      super.+=(feature)
    }
    override def toString: String = "Token("+word+")@"+position
  }
  
  class Label(labelString:String, token:Token) extends LabeledTokenSeqs.Label[Token,Label](labelString, token) {
    type GetterType = LabelGetter; class GetterClass extends LabelGetter
    override def toString: String = "Label("+value+")@"+token.position
  }

  class TokenGetter extends LabeledTokenSeqs.TokenGetter[Label,Token] {
    override def newTokenGetter = new TokenGetter
    override def newLabelGetter = new LabelGetter
    def tags = getOneWay(_.tags)
    def wordtype = getOneWay(_.wordtype)
  }
  class LabelGetter extends LabeledTokenSeqs.LabelGetter[Token,Label] {
    override def newTokenGetter = new TokenGetter
    override def newLabelGetter = new LabelGetter
  }
  
  // Variable classes Token, Label and LabeledTokenSeq are already defined in cc.factorie.application.LabeledTokenSeqs
  // Use them to define model:
  val model = new Model(
    Foreach[Label] { label => Score(label) } %"Prior",
    Foreach[Label] { label => Score(label, label.token) } %"LabelToken",
    Foreach[Label] { label => Score(label.prev, label) } %"LabelLabel",
    //Foreach[Label] { label => Score(label.prev, label, label.token.tags) },
    //Foreach[Label] { label => Score(label.prev, label, label.next) },
    //Foreach[Label] { label => Score(label.prev, label, label.next, label.token.tags) }
    //For[Label] { label => Score(label, label.token) }                 % "LabelToken",
    //For[Label] { label => Score(label, label.next) }                  % "LabelMarkov",
  )

  def main(args: Array[String]) : Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER1 trainfile testfile")
    // Read training and testing data.  The function 'featureExtractor' function is defined below
    val trainSentences = LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(args(0)), (word,lab)=>new Token(word,lab), featureExtractor _, "-DOCSTART-".r)//.take(200)
    val testSentences =  LabeledTokenSeq.fromOWPL[Token,Label](Source.fromFile(args(1)), (word,lab)=>new Token(word,lab), featureExtractor _, "-DOCSTART-".r)//.take(200)
    println(Domain[Label].toList)
    // Change from CoNLL's IOB notation to to BIO notation
    (trainSentences ++ testSentences).foreach(s => { 
      s.foreach(t => {
        //println("Token "+t.word+"  "+t.label.value+"  "+t.label.value(0)+"  "+(t.label.value(0)=='I'))
        //print("  %-8s %-8s ".format(t.label.trueValue, t.label.value))
        if (t.label.value(0) == 'I' && (!t.hasPrev || t.prev.label.value.substring(1) != t.label.value.substring(1))) {
          val newValue = "B"+t.label.value.substring(1) 
          t.label.value = newValue
          t.label.trueValue = newValue
        }
        //println("   x %-8s %-8s %s".format(t.label.trueValue, t.label.value, t.word))
      })}) 
      
    // Make features of offset conjunctions
    (trainSentences ++ testSentences).foreach(s => s.addNeighboringFeatureConjunctions(List(0), List(0,0), List(-1), List(-1,0), List(0,1), List(1)))
    // Gather tokens into documents
    val documents = new ArrayBuffer[ArrayBuffer[Token]]; documents += new ArrayBuffer[Token]
    (trainSentences ++ testSentences).foreach(s => if (s.length == 0) documents += new ArrayBuffer[Token] else documents.last ++= s)
    // For documents that have a "-" within the first three words, the first word is a HEADER feature; apply it to all words in the document
    documents.foreach(d => if (d.take(3).map(_.word).contains("-")) { val f = "HEADER="+d(0).word.toLowerCase; d.foreach(t => t += f)}) // println(d.take(4).map(_.word).mkString("", " ", "")))
    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    (trainSentences ++ testSentences).foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t += "SENTENCEUPPERCASE"))
    (trainSentences ++ testSentences).foreach(s => s.foreach(t => if (t.word.matches("[A-Za-z]+")) t ++= t.charNGrams(2,5).map(n => "NGRAM="+n)))
    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.prevWindow(5).map(t2 => "PREVWINDOW="+simplify(t2.word).toLowerCase)))
    //(trainSentences ++ testSentences).foreach(s => s.foreach(t => t ++= t.nextWindow(5).map(t2 => "NEXTWINDOW="+simplify(t2.word).toLowerCase)))
    // Put features of first mention o later mentions
    (trainSentences ++ testSentences).foreach(s => {
      s.foreach(t => {
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
    println("Training on "+trainSentences.size+" sentences, "+trainSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Testing  on "+testSentences.size+" sentences, "+testSentences.foldLeft(0)(_+_.size)+" tokens.")
    println("Labels: "+Domain[Label].toList)

    // Get the variables to be inferred; prune the data so that it can finish in just ~2 minutes
    val trainLabels = trainSentences.flatMap(_.labels)//.take(50000) // was take(20000) 
    val testLabels = testSentences.flatMap(_.labels)//.take(2000) // was .take(10000)

    //val targets = trainLabels.take(100)// trainSentences(15).map(_.label)
    //targets.foreach(printLabel(_))

    //trainLabels.take(20).foreach(printLabel(_))
    println("Domain size = "+Domain[Token].size)
    println("Tag Domain size = "+trainLabels.first.token.tags.domain.size)
    
    // Train and test!
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val predictor2 = new GibbsSampler2[Label,Label](model) {
      temperature = 0.01
      def block(label:Label) = label.prev
    }
    val predictor = new GibbsSampler1[Label](model) { temperature = 0.001 }
    val learner = new GibbsSampler1[Label](model) with SampleRank 
    //with PerceptronUpdates with ParameterAveraging
    with ConfidenceWeightedUpdates
    {
      temperature = 0.001
      override def preProcessHook(label:Label) = if (label.valueIsTruth && !label.token.isCapitalized && Global.random.nextDouble > 0.5) null else label
      override def postIterationHook(): Boolean = {
	if(this.isInstanceOf[ParameterAveraging])
	  this.asInstanceOf[ParameterAveraging].setWeightsToAverage
        predictor.process(testLabels, 1)
	if(this.isInstanceOf[ParameterAveraging])
	  this.asInstanceOf[ParameterAveraging].unsetWeightsToAverage

        println("Train errors")
        printErrors(trainLabels, 200)
        println("Test errors")
        printErrors(testLabels, 200)
        println("Iteration "+iterationCount)
        println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](trainLabels))
        println("TEST\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](testLabels))
        true
      }
    }
    //with FactorQueue[Variable with IterableSettings] { def process0(x:AnyRef):DiffList = x match { case l:Label => process(l); case _ => null} }
    // Train for 5 iterations through all Labels
    val startTime = System.currentTimeMillis
    learner.process(trainLabels, 8)
    println("Finished training in "+(System.currentTimeMillis-startTime)/60000.0+" minutes.")
    if(learner.isInstanceOf[ParameterAveraging])
      learner.asInstanceOf[ParameterAveraging].setWeightsToAverage
    
    // Predict, testing BP
    //val targets = new ArrayBuffer[UncoordinatedCategoricalVariable]
    /*val lattice = new BPLattice(model, targets)
    println("Starting BP updates on "+targets.size+" variables")
    lattice.update(100)
    lattice.setVariablesToMax
    targets.foreach(v => println(v.trueValue+"/"+v.value+" "+v.token.word+"  "+
        lattice.marginal(v).toArray.zipWithIndex.map((pair:(Double,Int)) => Tuple2(v.domain.get(pair._2), pair._1)).toString))*/

    
    // Predict, also by sampling, visiting each variable 3 times.
    //new SamplingMaximizer[Label](model).infer(testLabels, 5)
    predictor.process(testLabels, 1)
    predictor.temperature = 0.1; predictor.process(testLabels, 1)
    predictor.temperature = 0.01; predictor.process(testLabels, 1)
    predictor.temperature = 0.001; predictor.process(testLabels, 2)
    printErrors(testLabels, 100)
    println("GibbsSampling inference")
    println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](trainLabels))
    println("TEST\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](testLabels))

    println("BP inference")
    val bppredictor = new BPInferencer[Label](model)
    (trainSentences ++ testSentences).foreach(s => {
      val labels = s.map(_.label)
      println("BP sentence")
      labels.foreach(l => print(l.token.word+"="+l+"  ")); println
      if (labels.size >0) { labels.first.token.seq.foreach(t => print(t.word+"="+t.label+"  ")); println }
      s.foreach(t => print(t.word+"="+t.label+"  ")); println
      if (s.size > 0) {
        var t = s.first
        while (t != null) { print(t.word+"="+t.label); t = t.next }; println
      }
      bppredictor.inferTreewise(labels)
    })
    println("TRAIN\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](trainLabels))
    println("TEST\n"+LabeledTokenSeq.segmentEvaluation[Token,Label](testLabels))
    
    //model.save("/Users/mccallum/tmp/chainner2.factorie")
  }

  def printLabel(label:Label) : Unit = {
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.trueValue, label.value, label.token.toString))
  }
 
  def printDiagnostic(labels:Seq[Label]) : Unit = {
    for (label <- labels; if (label.index != label.domain.index("O"))) {
      if (!label.hasPrev || label.value != label.prev.value) 
        print("%-7s %-7s ".format((if (label.value != label.trueValue) label.trueValue else " "), label.value))
      print(label.token.word+" ")
      if (!label.hasNext || label.value != label.next.value) println()
    }
  }
  
  /** Print diagnostics for error tokens, plus two true tokens before and after. */
  def printErrors(labels:Seq[Label], maxErrors:Int): Unit = {
    val contextSize = 2
    var count = 0
    var i = 0
    while (i < labels.length && count < maxErrors) {
      val label = labels(i)
      if (!label.valueIsTruth && label.hasPrev && label.hasNext && count < maxErrors) {
        var j = Math.max(i-contextSize, 0); var numTruthsAfter = -contextSize
        do {
          val l = labels(j)
          println("%s %-6s %-6s %-18s %s".format((if (l.valueIsTruth) " " else "*"), l.trueValue, l.value, l.token.word, l.token.toString))
          if (l.valueIsTruth) numTruthsAfter += 1 else { numTruthsAfter = 0; count += 1 }
          j += 1
        } while (numTruthsAfter < contextSize && j < labels.length && count < maxErrors) 
        println
        i = j - 1
      }
      i += 1
    }
  }
 
  // Feature extraction
  def simplify(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word.toLowerCase
  }
  def featureExtractor(initialFeatures:Seq[String]) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    f += "SHAPE="+LabeledTokenSeqs.wordShape(word, 2)
    //f ++= wordNGrams(word, 2,5)
    f += "W="+simplify(word)
    f += "POS="+initialFeatures(1)
    f += "PHRASE="+initialFeatures(2)
    if (Character.isUpperCase(word(0))) f += "CAPITALIZED"
    //if (Numeric.findFirstMatchIn(word) != None) f += "NUMERIC"
    //if (Punctuation.findFirstMatchIn(word) != None) f += "PUNCTUATION"
    f
  }
  //val Capitalized = "^[A-Z].*".r
  //val Numeric = "^[0-9]+$".r
  //val Punctuation = "[,\\.;:?!()]+".r

}


