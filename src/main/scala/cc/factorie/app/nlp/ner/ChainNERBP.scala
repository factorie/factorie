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

package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.LabeledDiscreteEvaluation
import cc.factorie.infer.{BP, InferByBPChain}
import cc.factorie.optimize.{Trainer, LikelihoodExample}
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.app.nlp.load.LoadConll2003

class ChainNerBP {

  val model = new ChainNerModel
  val objective = new ChainNerObjective
  class Lexicon(file:File) extends cc.factorie.app.nlp.lexicon.PhraseLexicon(file) {
    override val name = file.toString.substring(file.toString.lastIndexOf('/')+1).toUpperCase
  }
  val lexicons = new scala.collection.mutable.ArrayBuffer[Lexicon]

  def initFeatures(document:Document): Unit = {
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      val features = token.attr += new ChainNerFeatures(token)
      features += "W="+word
      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      //features += "SUFFIX3="+word.takeRight(3)
      //features += "PREFIX3="+word.take(3)
      //features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
      //if (token.containsDigit) features += "NUMERIC"
      if (token.isPunctuation) features += "PUNCTUATION"
      if (lexicons.size > 0)
        for (lexicon <- lexicons) if (lexicon.contains(token)) features += "LEX="+lexicon.name
    }
    for (sentence <- document.sentences)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, (t:Token)=>t.attr[ChainNerFeatures], List(0), List(0,0), List(0,0,-1), List(0,0,1), List(1), List(2), List(-1), List(-2))
    // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
    document.sentences.foreach(s => if (!s.tokens.exists(_.containsLowerCase)) s.tokens.foreach(t => t.attr[ChainNerFeatures] += "SENTENCEUPPERCASE"))
    // Add features for character n-grams between sizes 2 and 5
    document.tokens.foreach(t => if (t.string.matches("[A-Za-z]+")) t.attr[ChainNerFeatures] ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
    // Add features from window of 4 words before and after
    document.tokens.foreach(t => t.attr[ChainNerFeatures] ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.tokens.foreach(t => t.attr[ChainNerFeatures] ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))
    // Put features of first mention on later mentions
    document.tokens.foreach(t => {
      if (t.isCapitalized && t.string.length > 1 && !t.attr[ChainNerFeatures].activeCategories.exists(f => f.matches(".*FIRSTMENTION.*"))) {
        //println("Looking for later mentions of "+t.word)
        var t2 = t
        while (t2.hasNext) {
          t2 = t2.next
          if (t2.string == t.string) { 
            //println("Adding FIRSTMENTION to "+t2.word); 
            t2.attr[ChainNerFeatures] ++= t.attr[ChainNerFeatures].activeCategories.map(f => "FIRSTMENTION="+f)
          }
        }
      }
    })
  }
  
  def hasFeatures(token:Token): Boolean = token.attr.contains(classOf[ChainNerFeatures])
  def hasFeatures(document:Document): Boolean = hasFeatures(document.tokens.head)
  
  def hasLabel(token:Token): Boolean = token.attr.contains(classOf[NerLabel])
  def hasLabels(document:Document): Boolean = hasLabel(document.tokens.head)

  def train(trainFilename:String, testFilename:String)(implicit random: scala.util.Random): Unit = {
    // Read in the data
    val trainDocuments = load.LoadConll2003.fromFilename(trainFilename).take(100)
    val testDocuments = load.LoadConll2003.fromFilename(testFilename).take(20)

    // Add features for NER
    trainDocuments.foreach(initFeatures(_))
    testDocuments.foreach(initFeatures(_))
    println("Example Token features")
    println(trainDocuments(3).tokens.take(10).map(token => token.nerLabel.shortCategoryValue+" "+token.string+" "+token.attr[ChainNerFeatures].toString).mkString("\n"))
    println("Num TokenFeatures = "+ChainNerFeaturesDomain.dimensionDomain.size)
    
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainDocuments.map(_.tokens).flatten.map(_.attr[BioConllNerLabel]) //.take(10000)
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[BioConllNerLabel]) //.take(2000)
 
    // Train for 5 iterations
    val vars = for(td <- trainDocuments; sentence <- td.sentences) yield sentence.tokens.map(_.attr[BioConllNerLabel])
//    val trainingInstances = vars.map(ModelExample(model, _))
//    val newLearner = new cc.factorie.bp.ParallelTrainer(model, trainingInstances) with L2Regularizer { override def sigmaSq = 10.0 }
//    println("Size of families: " + model.familiesOfClass[DotFamily]().size)
//    val optimizer = new LimitedMemoryBFGS(newLearner)
//    optimizer.optimize()
//    optimizer.optimize()
//    trainDocuments.foreach(process(_))
//    testDocuments.foreach(process(_))
//    printEvaluation(trainDocuments, testDocuments, "FINAL")
    val examples = vars.map(v => new LikelihoodExample(v, model, InferByBPChain))
    Trainer.batchTrain(model.parameters, examples)
    //(1 to 100).foreach(i => trainer.processExamples(pieces))

  }
  
  def printEvaluation(trainDocuments:Iterable[Document], testDocuments:Iterable[Document], iteration:String): Unit = {
      println("TRAIN")
      println(evaluationString(trainDocuments))
      println("TEST")
      println(evaluationString(testDocuments))
      println("Iteration "+iteration)
  }
  
  def evaluationString(documents: Iterable[Document]): Unit = {
    //println("Train Token accuracy = "+ NerObjective.aveScore(trainLabels))
    //println(" Test Token accuracy = "+ NerObjective.aveScore(testLabels))
    val buf = new StringBuffer
    // Per-token evaluation
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[BioConllNerLabel]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[BioConllNerLabel](BioConllNerDomain.categories.filter(_.length > 2).map(_.substring(2)))
    for (doc <- documents; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[BioConllNerLabel])
    println("Segment evaluation")
    println(segmentEvaluation)
  }

  // TODO Change this to use Viterbi! -akm
  def process(document:Document): Unit = {
    if (document.tokenCount == 0) return
    if (!hasFeatures(document)) initFeatures(document)
    if (!hasLabels(document)) document.tokens.foreach(token => token.attr += new BioConllNerLabel(token, "O"))
    for(sentence <- document.sentences if sentence.tokens.size > 0) {
	    val vars = sentence.tokens.map(_.attr[BioConllNerLabel]).toSeq
	    BP.inferChainMax(vars, model).setToMaximize(null)
	    //val mfg = new LatticeBP(model, sentence.tokens.map(_.attr[ChainNerLabel]).toSet) with MaxProductLattice
    	//new InferencerBPWorker(mfg).inferTreewise(vars.sampleUniformly, false)
    	//new InferencerBPWorker(mfg).inferTreewise(vars.sampleUniformly, false)
    	//mfg.setToMaxMarginal()
    }
  }
  
  def printSGML(tokens:IndexedSeq[Token]): Unit = {
    var i = 0
    val other = BioConllNerDomain.index("O")
    while (i < tokens.length) {
      if (tokens(i).nerLabel.intValue != other) {
        val start = i
        print("<"+tokens(i).nerLabel.shortCategoryValue+">"+tokens(i).string)
        i += 1
        while (i < tokens.length && tokens(i).nerLabel.categoryValue.startsWith("I-")) {
          print(" "+tokens(i).string)
          i += 1
        }
        var end = i - 1
        print("</"+tokens(i-1).nerLabel.shortCategoryValue+"> ")
      } else {
        print(tokens(i).string+" ")
        i += 1
      }
    }
  }
  
  def printEntities(tokens:IndexedSeq[Token]): Unit = {
    var i = 0
    val other = BioConllNerDomain.index("O")
    while (i < tokens.length) {
      if (tokens(i).nerLabel.intValue != other) {
        val start = i
        print(tokens(i).nerLabel.shortCategoryValue+" "+tokens(i).string+" ")
        i += 1
        while (i < tokens.length && tokens(i).nerLabel.categoryValue.startsWith("I-")) {
          print(tokens(i).string+" ")
          i += 1
        }
        var end = i - 1
        println("%d %d".format(start, end-start+1))
      } else {
        i += 1
      }
    }
  }
  
}

object ChainNerBP extends ChainNerBP {
  import cc.factorie.util.DefaultCmdOptions
  var verbose = false

  def main(args: Array[String]): Unit = {
    // Parse command-line
    object opts extends DefaultCmdOptions {
      val trainFile =     new CmdOption("train", "eng.train", "FILE", "CoNLL formatted training file.")
      val testFile  =     new CmdOption("test",  "eng.testa", "FILE", "CoNLL formatted test file.")
      val modelFile =      new CmdOption("model", "chainner.factorie", "FILE", "File for saving or loading model.")
      val runXmlDir =     new CmdOption("run-xml", "xml", "DIR", "Directory for reading NYTimes XML data on which to run saved model.")
      val runPlainFiles = new CmdOption("run-plain", List("ner.txt"), "FILE...", "List of files for reading plain texgt data on which to run saved model.")
      val lexiconDir =    new CmdOption("lexicons", "lexicons", "DIR", "Directory containing lexicon files named cities, companies, companysuffix, countries, days, firstname.high,...") 
      val verbose =       new CmdOption("verbose", "Turn on verbose output") { override def invoke = ChainNerBP.this.verbose = true }
      //val noSentences=new CmdOption("nosentences", "Do not use sentence segment boundaries in training.  Improves accuracy when testing on data that does not have sentence boundaries.")
    }
    implicit val random = new scala.util.Random(0)
    opts.parse(args)
    
    if (opts.lexiconDir.wasInvoked) {
      for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
        println("Reading lexicon "+filename)
        lexicons += new Lexicon(new File(opts.lexiconDir.value+"/"+filename))
      }
    }
    
    if (opts.runPlainFiles.wasInvoked) {
      BinarySerializer.deserialize(ChainNerFeaturesDomain, model, new File(opts.modelFile.value))
      for (filename <- opts.runPlainFiles.value) {
        val document = load.LoadPlainText.fromFile(new java.io.File(filename)).head
        //println("ChainNer plain document: <START>"+document.string+"<END>")
        //println(document.map(_.string).mkString(" "))
        process(document)
        println()
        println(filename)
        printEntities(document.tokens.toIndexedSeq)
        printSGML(document.tokens.toIndexedSeq)
      }
    } else if (opts.runXmlDir.wasInvoked) {
      //println("statClasses "+model.templatesOf[VectorTemplate].toList.map(_.statClasses))
      BinarySerializer.deserialize(ChainNerFeaturesDomain, model, new File(opts.modelFile.value))
      //run(opts.runXmlDir.value)
    } else {
      train(opts.trainFile.value, opts.testFile.value)
      if (opts.modelFile.wasInvoked)
        BinarySerializer.serialize(ChainNerFeaturesDomain, model, new File(opts.modelFile.value))
    }
    

    //if (args.length != 2) throw new Error("Usage: NER trainfile testfile.")

   

  }

  
}
