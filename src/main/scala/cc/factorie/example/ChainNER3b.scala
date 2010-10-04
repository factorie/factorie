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
import scala.io.Source
import cc.factorie._
import cc.factorie.er._
import cc.factorie.app.tokenseq.labeled
import scala.collection.mutable.ArrayBuffer
import java.io.File

import scala.collection.mutable.HashMap

/** Demo of CoNLL NER with lots of features.  Aiming for near state-of-the-art accuracy, but not yet finished.
    Apologies for the current mess here as we explore various options. 
    @author Tim Viera */
object ChainNER3b {

  // Misc. Utils
  def printred(s:String): Unit = println("\033[31m" + s + "\033[0m" )
  def printgreen(s:String): Unit = println("\033[32m" + s + "\033[0m")
  class ProgressBar(val N:Double) {
    var completed = 0.0
    def update(amount:Double) = {
      completed += amount
      print("\r%5.1f%% (%s/%s) complete".format(completed*100.0/N, completed, N))
    }
  }


  // Define the variable classes
  class Token(word:String, labelString:String) extends labeled.Token[Sentence,Label,Token](word) {
    type GetterType = TokenGetter; class GetterClass extends TokenGetter
    val label = new Label(labelString, this)
    val wordtype = new Type(simplify(word)); class Type(s:String) extends CategoricalVariable(s)
    val tags = new Tags; class Tags extends BinaryFeatureVectorVariable[String]
    override def +=(feature:String): Unit = {
      //if (feature.matches("(POS|PHRASE)=[-A-Z]+|W=(and|or|of|the|for|de|del)")) tags += feature
      if (feature.matches("POS=[-A-Z]+")) tags += feature
      super.+=(feature)
    }
    override def toString: String = "Token("+word+")@"+position
  }

  class Label(labelString:String, token:Token) extends labeled.Label[Sentence,Token,Label](labelString, token) {
    type GetterType = LabelGetter; class GetterClass extends LabelGetter
    override def toString: String = "Label("+value+")@"+token.position
  }

  class TokenGetter extends labeled.TokenGetter[Sentence,Label,Token] {
    override def newTokenGetter = new TokenGetter
    override def newLabelGetter = new LabelGetter
    def tags = getOneWay(_.tags)
    def wordtype = getOneWay(_.wordtype)
  }

  class LabelGetter extends labeled.LabelGetter[Sentence,Token,Label] {
    override def newTokenGetter = new TokenGetter
    override def newLabelGetter = new LabelGetter
  }

  class Sentence extends labeled.TokenSeq[Token,Label,Sentence]
  type InstanceType = Sentence

  // Variable classes Token, Label and TokenSeq are already defined in cc.factorie.app.tokenseq.labeled
  // Use them to define model:
  val model = new Model(
    //Foreach[Label] { label => Score(label.prev, label, label.token) } %"LabelLabelToken",
    Foreach[Label] { label => Score(label) } %"Prior",
    Foreach[Label] { label => Score(label, label.token) } %"LabelToken",
    Foreach[Label] { label => Score(label.prev, label) } %"LabelLabel",
    Foreach[Label] { label => Score(label.prev, label, label.token.tags) }
//    Foreach[Label] { label => Score(label.prev.prev, label.prev, label) } %"SecondOrder",
//    Foreach[Label] { label => Score(label.prev, label, label.next, label.token.tags) },
//    Foreach[Label] { label => Score(label, label.token.tags) },
    //Foreach[Label] { label => Score(label.prev, label, label.token.tags) },
    //Foreach[Label] { label => Score(label.prev, label, label.next) },
  )

  // Utility function for mapping between different label encodings
  var useBILOU = false

  // Change the encoding of a sequence of Labels from BIO to BILOU
  // WARNING: this operation changes Domain[Label]
  def bio2bilou(labels:Seq[Label]): Unit = {
    if (!useBILOU) return
    labels.foreach(lbl => {
      assert(lbl.trueValue(0) != 'L') // sanity check: make sure you weren't using BILOU already.
      // convert true value
      if (lbl.trueValue(0) == 'B' && (!lbl.hasNext || (lbl.hasNext && lbl.next.trueValue(0) != 'I')))
        lbl.trueValue = "U" + lbl.trueValue.substring(1)
      if (lbl.trueValue(0) == 'I' && (!lbl.hasNext || (lbl.hasNext && lbl.next.trueValue(0) != 'I')))
        lbl.trueValue = "L" + lbl.trueValue.substring(1)
      // convert predicted value
      if (lbl.value(0) == 'B' && (!lbl.hasNext || (lbl.hasNext && lbl.next.value(0) != 'I')))
        lbl.value = "U" + lbl.value.substring(1)
      if (lbl.value(0) == 'I' && (!lbl.hasNext || (lbl.hasNext && lbl.next.value(0) != 'I')))
        lbl.value = "L" + lbl.value.substring(1)
    })
  }

  // Change the encoding of a sequence of Labels from BILOU to BIO
  // NOTE: Domain[Label] does not return BIO, it stays BILOU
  def bilou2bio(labels:Seq[Label]): Unit = {
    if (!useBILOU) return
    var existsatleastoneL = false // sanity check: make sure you were using BILOU.
    labels.foreach(lbl => {
      existsatleastoneL = existsatleastoneL || (lbl.trueValue(0)=='L')
      // convert true value
      if (lbl.trueValue(0) == 'U')
        lbl.trueValue = "B" + lbl.trueValue.substring(1)
      if (lbl.trueValue(0) == 'L')
        lbl.trueValue = "I" + lbl.trueValue.substring(1)
      // convert predicted value
      if (lbl.value(0) == 'U')
        lbl.value = "B" + lbl.value.substring(1)
      if (lbl.value(0) == 'L')
        lbl.value = "I" + lbl.value.substring(1)
    })
    assert(existsatleastoneL)
  }

  // Change from CoNLL's IOB notation to BIO notation
  // NOTE: IOB and BIO labels still have the same string values. So, rest assure the Domain[Label]
  // does not change in this transformation).
  def iob2bio(labels:Seq[Label]): Unit = {
    labels.foreach(lbl => {
        if (lbl.value(0) == 'I' && (!lbl.hasPrev || lbl.prev.value.substring(1) != lbl.value.substring(1))) {
          val newValue = "B" + lbl.value.substring(1)
          lbl.value = newValue
          lbl.trueValue = newValue
        }
      })
  }

  // For capitalized words, aggregate the features of the first occurence of a
  // word to the words that follow it in the document. The assumptions being,
  // the first occurence is typically easier to resolve than others.
  def addFirstMentionFeatures(documents:Seq[Seq[Token]]) {
    documents.foreach(d => {
      for (i <- 0 until d.size) {
        var t = d(i)
        if (t.hasNext && t.isCapitalized && t.word.length > 1 && !t.values.exists(f => f.matches(".*FIRSTMENTION.*"))) {
          // for each token after t
          for (j <- (i+1) until d.size) {
            var t2 = d(j)
            if (t2.word == t.word) {
              // add t's features to t2 prefixed with "FIRSTMENTION="
              t2 ++= t.values.filter(_.contains("@")).map(f => "FIRSTMENTION=" + f)
            }
          }
        }
      }
    })
  }

  def resetLabels(x:Seq[Label]): Unit = x.foreach(_:="O") //x.foreach(_.setRandomly)


  def g(y:String, X:InstanceType): Double = {
    var Y = X(0).label;   Y := y
    local_score(Y)
  }

  def g(y:String, yp:String, X:InstanceType, t:Int): Double = {
    var Y  = X(t).label;  Y  := y
    var Yp = Y.prev;      Yp := yp
    local_score(Y, Yp)
  }

  def g(y:String, yp:String, ypp:String, X:InstanceType, t:Int): Double = {
    var Y  = X(t).label;   Y  := y
    var Yp = Y.prev;       Yp := yp
    var Ypp = Y.prev.prev; Ypp := ypp
    local_score(Y, Yp, Ypp)
  }

  def local_score(Y:Label, local_labels:Label*): Double = {
    // filter out factors touching Y which touch Labels other than Yp
    val factors = model.factors(Y).filter(
      _.variables.forall(x => !x.isInstanceOf[Label] || (x == Y) || local_labels.contains(x))
    )
    // add-up factor scores
    factors.foldLeft(0.0)((acc,f) => acc + f.statistics.score)
  }


  def score_region(Y: Label, local_labels: List[Label]): Double = {
    // filter out factors touching Y which touch Labels other than Yp
    val factors = model.factors(Y).filter(
      _.variables.forall(x => !x.isInstanceOf[Label] || (x == Y) || local_labels.contains(x))
    )
    // add-up factor scores
    factors.foldLeft(0.0)((acc,f) => acc + f.statistics.score)
  }

  // cycle thru all combinations of a List of Variables
  def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.head.hasNext) {vs.head.next; true}
    else if (vs.tail != Nil) {vs.head.reset; vs.head.next; nextValues(vs.tail)}
    else false
  }


  def decode_viterbi(instance: InstanceType): Unit = {
    if (instance.size == 0) return

    val S = Domain[Label]
    val N = instance.size

    // make life less verbose with some typedefs...
    type LabelType = String
    type PathType = List[LabelType]
    type TraceType = Tuple2[Double,PathType]
    val null_PathType = null.asInstanceOf[PathType]

    var V, W = new HashMap[LabelType, TraceType]

    for (y <- S)
      V(y) = (g(y, instance), List(y))

    for (i <- 1 until N) {
      W = new HashMap
      for (y <- S) {
        instance(i).label := y //<< might need to loop over more variables, e.g. order-2 or semi-markov

        var max = Double.NegativeInfinity
        var argmax = null_PathType
        var neighbors = List(instance(i-1).label)
        val neighborSettings = neighbors.map(_.settings); neighborSettings.foreach(setting => {setting.reset; setting.next})
        do {
          var (score, path) = V( neighbors(0).value )
          score += score_region(instance(i).label, neighbors)
          if (score > max || argmax == null) { argmax = path; max = score }
        } while (nextValues(neighborSettings))

        W(y) = (max, argmax ++ List(y))
      }
      // we can forget about V[i-1,:] in the next iteration
      V = W
    }

    // y* = argmax_{y in S} V[N,y]
    var max = Double.NegativeInfinity
    var argmax = null_PathType
    for (y <- S) {
      var (score, path) = V(y)
      if (score > max || argmax == null) {
        argmax = path
        max = score
      }
    }

    // set variables to values in optimal path
    for ((label, value) <- instance.labels.toList.zip(argmax)) label := value

    null
  }



  def main(args: Array[String]): Unit = {

    if (args.length != 2) {
      println("Usage: %s <trainfile> <testfile>\n".format(args(0)))
      return
    }

    printred("Reading training and testing data...")
    // Read training and testing data.  The function 'featureExtractor' function is defined below
    val trainSentences = labeled.TokenSeq.fromOWPL[Sentence,Token](Source.fromFile(new File(args(0))), () => new Sentence, (word,lab) => new Token(word, lab), featureExtractor _) //.take(100)
    val testSentences =  labeled.TokenSeq.fromOWPL[Sentence,Token](Source.fromFile(new File(args(1))), () => new Sentence, (word,lab) => new Token(word, lab), featureExtractor _) //.take(50)

    // Get the variables to be inferred
    val trainLabels = trainSentences.flatMap(_.labels)
    val testLabels = testSentences.flatMap(_.labels)

    // print out performance measures of the current configuration
    def evaluatePerformance() = {
      // NOTE: the evaluation procedures are for the BIO encoding so we need to switch back to it temporarily.
      bilou2bio(testLabels ++ trainLabels)
      println("TRAIN\n"+labeled.segmentEvaluation(trainLabels))
      println("TEST\n"+labeled.segmentEvaluation(testLabels))
      bio2bilou(testLabels ++ trainLabels)
    }


    printred("Adding offset conjunctions...")
    // Make features of offset conjunctions
    (trainSentences ++ testSentences).foreach(s => s.addNeighboringFeatureConjunctions(List(0), List(0,0), List(-1), List(-1,0), List(0,1), List(1)))

    printred("Gathering tokens into documents...")
    // Gather tokens into documents
    val documents = new ArrayBuffer[ArrayBuffer[Token]]; documents += new ArrayBuffer[Token]
    (trainSentences ++ testSentences).foreach(s => if (s.length == 0) documents += new ArrayBuffer[Token] else documents.last ++= s)

    printred("Adding header features...")
    // For documents that have a "-" within the first three words,
    // the first word is a HEADER feature; apply it to all words in the document
    documents.foreach(d => if (d.take(3).map(_.word).contains("-")) { val f = "HEADER="+d(0).word.toLowerCase; d.foreach(t => t += f)}) // println(d.take(4).map(_.word).mkString("", " ", "")))

    printred("Adding uppercase sentence features...")
    // If the sentence contains no lowercase letters,
    // tell all tokens in the sentence they are part of an uppercase sentence
    (trainSentences ++ testSentences).foreach(s => if (!s.exists(_.containsLowerCase)) { s.foreach(t => t += "SENTENCEUPPERCASE"); } )

    printred("Adding character n-gram features...")
    (trainSentences ++ testSentences).foreach(s => s.foreach(t => if (t.word.matches("[A-Za-z]+")) t ++= t.charNGrams(2,5).map(n => "NGRAM="+n)))

    printred("Adding first mention features...")
    addFirstMentionFeatures(documents)


    println("Training on " + trainSentences.size + " sentences, " + trainSentences.foldLeft(0)(_+_.size) + " tokens.")
    println("Testing  on " + testSentences.size + " sentences, " + testSentences.foldLeft(0)(_+_.size) + " tokens.")
    println("Labels: " + Domain[Label].toList)
    println("Domain size = " + Domain[Token].size)
    println("Tag Domain size = " + trainLabels.head.token.tags.domain.size)

    // Set-up learner and predictor
    val predictor = new VariableSettingsSampler[Label](model) { temperature = 0.001 }
    val learner = new VariableSettingsSampler[Label](model) with SampleRank
    //with GradientAscentUpdates with ParameterAveraging
    with ConfidenceWeightedUpdates
    //with MIRAUpdates
    {
      // parameters of learning algorithm
      temperature = 0.001
      learningMargin = 2.0

      override def preProcessHook(label:Label) = {
        if (label.valueIsTruth && !label.token.isCapitalized && cc.factorie.random.nextDouble > 0.5) null else label
      }

      override def postIterationHook(): Boolean = {
        printred("Iteration " + iterationCount)

        // temporarily use the averaged parameters
        if (this.isInstanceOf[ParameterAveraging]) this.asInstanceOf[ParameterAveraging].setWeightsToAverage
        println("processing testing examples...")
        resetLabels(testLabels)
        //predictor.processAll(testLabels)
        runViterbiInference(/*trainSentences ++ */ testSentences)
        evaluatePerformance()
        if (this.isInstanceOf[ParameterAveraging]) this.asInstanceOf[ParameterAveraging].unsetWeightsToAverage

        //resetLabels(trainLabels ++ testLabels)
        //trainLabels.foreach(x => {x.value=x.trueValue})
        //temperature *= temperature
        true
      }
    }
    //with FactorQueue[Variable with IterableSettings] { def process0(x:AnyRef):DiffList = x match { case l:Label => process(l); case _ => null} }

    // Train for serveral iterations
    printred("Training...")
    val startTime = System.currentTimeMillis
    learner.processAll(trainLabels, 10)
    //learner.processAll(trainLabels)
    println("Finished training in " + (System.currentTimeMillis - startTime) / 60000.0 + " minutes.")
    if (learner.isInstanceOf[ParameterAveraging]) learner.asInstanceOf[ParameterAveraging].setWeightsToAverage

    resetLabels(trainLabels ++ testLabels)
    println("==========================================")
    println("GibbsSampling inference")
    println("processing training examples...")
    predictor.processAll(trainLabels)
    println("processing testing examples...")
    predictor.processAll(testLabels, 5)
    evaluatePerformance()

    println("==========================================")
    println("BP inference")
    runBPInference(trainSentences ++ testSentences)
    evaluatePerformance()

    println("==========================================")
    println("Viterbi inference")
    runViterbiInference(trainSentences ++ testSentences)
    evaluatePerformance()
  }

  /* Use Belief Propogation for decoding */
  def runBPInference(data:Seq[InstanceType]) = {

    var progress = new ProgressBar(data.size)
    
    // NOTE the inferencer resets before predicting each sequence 
    // so there is no need to use "resetLabels(data)" out here.
    val bppredictor = new BPInferencer[Label](model)
    data.foreach(s => {
      val labels = s.map(_.label)
      var gibbs_score = model.score(labels)
      resetLabels(labels)
      /*
      println("BP sentence")
      labels.foreach(l => print(l.token.word+"="+l+"  ")); println
      if (labels.size >0) { labels.head.token.seq.foreach(t => print(t.word+"="+t.label+"  ")); println }
      s.foreach(t => print(t.word+"="+t.label+"  ")); println
      if (s.size > 0) {
        var t = s.head
        while (t != null) { print(t.word+"="+t.label); t = t.next }; println
      }
      */
      bppredictor.inferTreewise(labels)

      /*
      var bp_score = model.score(labels)
      if (bp_score < gibbs_score) {
        printred("Warning: BP-config (%.2f) < GS-config (%.2f)".format(bp_score, gibbs_score))
      }
      */

      progress.update(1)
    })
  }


  def runViterbiInference(data:Seq[InstanceType]) = {
    data.foreach(s => {
      val labels = s.map(_.label)
      var bp_score = model.score(labels)
      resetLabels(labels)
      decode_viterbi(s)
      /*
      var viterbi_score = model.score(labels)
      if (bp_score > viterbi_score) {
        println("\033[31mWarning\033[0m: BP(%.3f) > Viterbi(%.3f). viterbi broken?".format(bp_score, viterbi_score))
        assert(false)
      } else if (bp_score < viterbi_score) {
        println("\033[31mWarning\033[0m: BP(%.3f) < Viterbi(%.3f). bp broken?".format(bp_score, viterbi_score))
        //assert(false)
      } else {
        //printgreen("good.")
      }
      */
    })
  }


  def printLabel(label:Label): Unit = {
    println("%-16s TRUE=%-8s PRED=%-8s %s".format(label.token.word, label.trueValue, label.value, label.token.toString))
  }

  def printDiagnostic(labels:Seq[Label]): Unit = {
    for (label <- labels; if (label.intValue != label.domain.index("O"))) {
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
        var j = math.max(i-contextSize, 0); var numTruthsAfter = -contextSize
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
    else if (word.matches("[\\d,]+\\.?\\d*")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word.toLowerCase
  }

  def featureExtractor(initialFeatures:Seq[String]): Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    val word = initialFeatures(0)
    f += "SHAPE="+cc.factorie.app.tokenseq.wordShape(word, 2)
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
