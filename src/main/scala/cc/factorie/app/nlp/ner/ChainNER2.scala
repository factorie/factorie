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
import cc.factorie.optimize._
import app.strings._
import cc.factorie.util.BinarySerializer

//import bp._
//import bp.specialized.Viterbi
import optimize._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.LoadConll2003._
import scala.io.Source
import cc.factorie.app.chain._
import scala.io._
import java.io.{FileWriter, BufferedWriter, File}
import scala.math.round
import org.junit.Assert._

object ChainNer2FeaturesDomain extends CategoricalDimensionTensorDomain[String]
class ChainNer2Features(val token:Token) extends BinaryFeatureVectorVariable[String] {
  def domain = ChainNer2FeaturesDomain
  override def skipNonCategories = true
} 

class ChainNer21Model extends TemplateModel with Parameters {
  
  // Bias term on each individual label 
  val biasTemplate = new DotTemplateWithStatistics1[ChainNerLabel] {
    factorName = "bias"
    val weights = Weights(new la.DenseTensor1(Conll2003NerDomain.size))
  }
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[ChainNerLabel,ChainNerFeatures] {
    factorName = "markov"
    val weights = Weights(new la.DenseTensor2(Conll2003NerDomain.size, ChainNerFeaturesDomain.dimensionSize))
    def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[ChainNerFeatures])
    def unroll2(tf: ChainNerFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[ChainNerLabel, ChainNerLabel] {
    factorName = "observation"
    val weights = Weights(new la.DenseTensor2(Conll2003NerDomain.size, Conll2003NerDomain.size))
    def unroll1(label: ChainNerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[ChainNerLabel], label) else Nil
    def unroll2(label: ChainNerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[ChainNerLabel]) else Nil
  } 
  this += biasTemplate
  this += localTemplate
  this += transitionTemplate
}

class ChainNer2Model extends TemplateModel with Parameters {
  // Bias term on each individual label 
  val bias = new DotTemplateWithStatistics1[ChainNerLabel] {
    factorName = "bias"
    val weights = Weights(new la.DenseTensor1(Conll2003NerDomain.size))
  }
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[ChainNerLabel,ChainNer2Features] {
    factorName = "markov"
    val weights = Weights(new la.DenseTensor2(Conll2003NerDomain.size, ChainNer2FeaturesDomain.dimensionSize))
    def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[ChainNer2Features])
    def unroll2(tf: ChainNer2Features) = Factor(tf.token.attr[ChainNerLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[ChainNerLabel, ChainNerLabel] {
    factorName = "observation"
    val weights = Weights(new la.DenseTensor2(Conll2003NerDomain.size, Conll2003NerDomain.size))
    //override def statisticsDomains = ((Conll2003NerDomain, Conll2003NerDomain))
    def unroll1(label: ChainNerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[ChainNerLabel], label) else Nil
    def unroll2(label: ChainNerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[ChainNerLabel]) else Nil
  }
  this += bias
  this += localTemplate
  this += transitionTemplate
}

class TokenSequence(token : Token) extends collection.mutable.ArrayBuffer[Token] {
  this.prepend(token)
  val label : String = token.attr[ChainNerLabel].categoryValue.split("-")(1)
  def key = this.mkString("-")
} 

class ChainNer2 {


  val model = new ChainNer21Model
  val model2 = new ChainNer2Model
  val objective = new ChainNerObjective

  var lexicons : Lexicons = null
  val wordToLex = new scala.collection.mutable.HashMap[String,List[String]]
  var aggregate = false
  var twoStage = false
  val clusters = new scala.collection.mutable.HashMap[String,String]
  var count = 0
  var didagg = false
  var bP = false
  var ss = 10.0

  def prevWindowNum(t:Token, n:Int): IndexedSeq[(Int,Token)] = t.prevWindow(n).map(x => (t.prevWindow(n).indexOf(x),x)).toIndexedSeq
  def nextWindowNum(t:Token, n:Int): IndexedSeq[(Int,Token)] = t.nextWindow(n).map(x => (t.nextWindow(n).indexOf(x),x)).toIndexedSeq

  def prefix( prefixSize : Int, cluster : String ) : String = if(cluster.size > prefixSize) cluster.substring(0, prefixSize) else cluster

  def addContextFeatures[A<:Observation[A]](t : Token, from : Token, vf:Token=>CategoricalDimensionTensorVar[String]) : Unit = {
    vf(t) ++= prevWindowNum(from,2).map(t2 => "CONTEXT="+simplifyDigits(t2._2.string).toLowerCase + "@-" + t2._1)
    vf(t) ++= nextWindowNum(from, 2).map(t2 => "CONTEXT="+simplifyDigits(t2._2.string).toLowerCase + "@" + t2._1)
    for(t2 <- prevWindowNum(from,2)) {
	    if(clusters.contains(t2._2.string)) {
			  vf(t) += ("CONTEXTPATH="+prefix(4, clusters(t2._2.string)) + ("@-" + t2._1.toString))
    	}
    }

    for(t2 <- nextWindowNum(from, 2)) {
	    if(clusters.contains(t2._2.string))
			  vf(t) += ("CONTEXTPATH="+prefix(4, clusters(t2._2.string)) + ("@" + t2._1.toString))
    }
  }
  
  def aggregateContext[A<:Observation[A]](token : Token, vf:Token=>CategoricalDimensionTensorVar[String]) : Unit = {
    var count = 0
    var compareToken : Token = token
    while(count < 200 && compareToken.hasPrev) {
      count += 1
      compareToken = compareToken.prev
      if(token.string.toLowerCase == compareToken.string.toLowerCase)
        addContextFeatures(token, compareToken, vf)
    }
    count = 0
    compareToken = token
    while(count < 200 && compareToken.hasNext) {
      count += 1
      compareToken = compareToken.next
      if(token.string.toLowerCase == compareToken.string.toLowerCase)
        addContextFeatures(token, compareToken, vf)
    }
  }
  

  def initFeatures(document:Document, vf:Token=>CategoricalDimensionTensorVar[String]): Unit = {
    count=count+1
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
	    val features = vf(token)
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
      if (token.isCapitalized) features += "CAPITALIZED"
      else features += "NOTCAPITALIZED"
      if (token.isPunctuation) features += "PUNCTUATION"
      if(lexicons != null) for(lexicon <- lexicons(token)) features += "LEX="+lexicon
      if (clusters.size > 0 && clusters.contains(rawWord)) {
        features += "CLUS="+prefix(4,clusters(rawWord))
        features += "CLUS="+prefix(6,clusters(rawWord))
        features += "CLUS="+prefix(10,clusters(rawWord))
        features += "CLUS="+prefix(20,clusters(rawWord))
      }
    }
    for (sentence <- document.sentences) cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))
    // Add features for character n-grams between sizes 2 and 5
    document.tokens.foreach(t => if (t.string.matches("[A-Za-z]+")) vf(t) ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
    // Add features from window of 4 words before and after
    document.tokens.foreach(t => vf(t) ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
    document.tokens.foreach(t => vf(t) ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))
    if(aggregate) document.tokens.foreach( aggregateContext(_, vf) )
  }
  
  def mode(list : List[String]) : String = {
    val domainCount = new collection.mutable.HashMap[String, Int]
    for(item <- list) {
      if(domainCount.contains(item)) domainCount(item) = domainCount(item) + 1
      else domainCount(item) = 1
    }
    var maxDomain = ""
    var maxCount = 0
    for(domain <- domainCount.keys) {
      if(domainCount(domain) > maxCount) {
        maxCount = domainCount(domain)
        maxDomain = domain
      }
    }
    maxDomain
  }

  def getSequences(document : Document) : List[TokenSequence] = {
    var sequences = List[TokenSequence]()
    var seq : TokenSequence = null
    for(token <- document.tokens) {
      val categoryVal = token.attr[ChainNerLabel].categoryValue
      if(categoryVal.length() > 0) {
        categoryVal.substring(0,1) match {
         case "B" => seq = new TokenSequence(token)
         case "I" => if (seq != null) seq.append(token) else seq = new TokenSequence(token)
         case "U" => seq = new TokenSequence(token)
         case "L" => if (seq != null) seq.append(token) else seq = new TokenSequence(token)
         case _ => null
        }
        if(categoryVal.matches("(L|U)-\\D+")) sequences = seq :: sequences
      }
     }
    sequences
  }

  def  allSubstrings(seq: TokenSequence, length : Int) : List[String] = {
	  if(length == 0) return List[String]()
	  var list = List[String]()
	  for(i <- 0 to seq.length-length) {
		  var sub = ""
		  for(k <- i until i+length) {
			  sub += " " + seq(k).string
		  }
		  list = sub :: list
	  }
	  allSubstrings(seq, length-1) ::: list
  }


  def initSecondaryFeatures(document:Document, extraFeatures : Boolean = false): Unit = {
  
    document.tokens.foreach(t => t.attr[ChainNer2Features] ++= prevWindowNum(t,2).map(t2 => "PREVLABEL" + t2._1 + "="+t2._2.attr[ChainNerLabel].categoryValue))
    document.tokens.foreach(t => t.attr[ChainNer2Features] ++= prevWindowNum(t,1).map(t2 => "PREVLABELCON="+t2._2.attr[ChainNerLabel].categoryValue+"&"+t.string))
    for(t <- document.tokens) {
	    if(t.sentenceHasPrev) {
		    t.attr[ChainNer2Features] ++= prevWindowNum(t,2).map(t2 => "PREVLABELLCON="+t.sentencePrev.attr[ChainNerLabel].categoryValue+"&"+t2._2.string)
    	  t.attr[ChainNer2Features] ++= nextWindowNum(t,2).map(t2 => "PREVLABELLCON="+t.sentencePrev.attr[ChainNerLabel].categoryValue+"&"+t2._2.string)
	    }
    }
   
    val sequences = getSequences(document)
    val tokenToLabelMap = new scala.collection.mutable.HashMap[String, List[String]]
    val extendedPrediction = new scala.collection.mutable.HashMap[String, List[String]]
    val sequenceToLabelMap = new scala.collection.mutable.HashMap[String, List[String]]
    val subsequencesToLabelMap = new scala.collection.mutable.HashMap[String, List[String]]

    if(extraFeatures) {
      for (token <- document.tokens) {
        if(tokenToLabelMap.contains(token.string))
          tokenToLabelMap(token.string) = tokenToLabelMap(token.string) ++ List(token.attr[ChainNerLabel].categoryValue)
        else
          tokenToLabelMap(token.string) = List(token.attr[ChainNerLabel].categoryValue)
      }
	    for (seq <- sequences) {
	      if(sequenceToLabelMap.contains(seq.key))
	        sequenceToLabelMap(seq.key) = sequenceToLabelMap(seq.key) ++ List(seq.label)
	      else
	        sequenceToLabelMap(seq.key) = List(seq.label)
	    }

	    for (seq <- sequences) {
		    for(subseq <- allSubstrings(seq, seq.length)) {
		      if(subsequencesToLabelMap.contains(subseq))
		        subsequencesToLabelMap(subseq) = subsequencesToLabelMap(subseq) ++ List(seq.label)
		      else
			      subsequencesToLabelMap(seq.key) = List(seq.label)
		    }
	    }
  
  	  for (token <- document.tokens) {
        val tokenVote = tokenToLabelMap(token.string)
        token.attr[ChainNer2Features] += "CLASSIFIERLABEL="+mode(tokenVote)
    	}
    
	    for(seq <- sequences) {
		    val seqVote = sequenceToLabelMap(seq.key)
		    val seqLabelMode = mode(seqVote)
		    val subSeqVote = subsequencesToLabelMap(seq.key)
		    val subSeqLabelMode = mode(subSeqVote)
		    for(token <- seq) {
		      token.attr[ChainNer2Features] += "SEQUENCELABEL="+seqLabelMode
		      token.attr[ChainNer2Features] += "SUBSEQUENCELABEL="+subSeqLabelMode
		    }
	    }
	  }
	  for(token <- document.tokens) {
		  if(extendedPrediction.contains(token.string))
        Conll2003NerDomain.categories.map(str => token.attr[ChainNer2Features] += str + "=" + history(extendedPrediction(token.string), str) )
		  if(extendedPrediction.contains(token.string))
        extendedPrediction(token.string) = extendedPrediction(token.string) ++ List(token.attr[ChainNerLabel].categoryValue)
      else
        extendedPrediction(token.string) = List(token.attr[ChainNerLabel].categoryValue)
	  }

    for(token <- document.tokens) {
		  val rawWord = token.string
		  if(token.hasPrev && clusters.size > 0) {
			  if(clusters.contains(rawWord))
		      token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(rawWord)))
			  if(token.hasNext) {
				  var nextRawWord = token.next.string
				  if(clusters.contains(nextRawWord))
					  token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				  if(token.next.hasNext && clusters.contains(token.next.next.string)) {
					  nextRawWord = token.next.next.string
					  token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				  }
			  }
        if(token.hasPrev) {
				  var prevRawWord = token.prev.string
				  if(clusters.contains(prevRawWord))
				    token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(prevRawWord)))
				  if(token.prev.hasPrev && clusters.contains(token.prev.prev.string)) {
				    prevRawWord = token.prev.prev.string
				    token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(prevRawWord)))
				  }
			  }
		  }
	  }
  }

  def history(list : List[String], category : String) : String = {
	  (round( 10.0 * ((list.count(_ == category).toDouble / list.length.toDouble)/3)) / 10.0).toString
  }

  def train(trainFilename:String, testFilename:String): Unit = {
    // Read in the data
    val trainDocuments = LoadConll2003.fromFilename(trainFilename, true)
    val testDocuments = LoadConll2003.fromFilename(testFilename, true)

    // Add features for NER                 \
    println("Initializing training features")
	
  	(trainDocuments ++ testDocuments).foreach(_.tokens.map(token => token.attr += new ChainNerFeatures(token)))

    trainDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))
    println("Initializing testing features")
    testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))
    
    println("Example Token features")
    println(trainDocuments(3).tokens.map(token => token.nerLabel.shortCategoryValue+" "+token.string+" "+token.attr[ChainNerFeatures].toString).mkString("\n"))
    println("Example Test Token features")
    println(testDocuments(1).tokens.map(token => token.nerLabel.shortCategoryValue+" "+token.string+" "+token.attr[ChainNerFeatures].toString).mkString("\n"))
    println("Num TokenFeatures = "+ChainNerFeaturesDomain.dimensionDomain.size)
    
    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(100)
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(20)
 
		if(bP) {
  		val vars = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[ChainNerLabel])
      val examples = vars.map(v => new LikelihoodExample(v.toSeq, model, InferByBPChainSum))
      val trainer = new ParallelBatchTrainer(model.parameters, new LBFGS with L2Regularization)
			trainer.trainFromExamples(examples)
      (trainLabels ++ testLabels).foreach(_.setRandomly())
      trainDocuments.foreach(process(_))
      testDocuments.foreach(process(_))
      printEvaluation(trainDocuments, testDocuments, "FINAL")
	  } else {
      (trainLabels ++ testLabels).foreach(_.setRandomly())
      val learner = new SampleRankTrainer(new GibbsSampler(model, objective), new AdaGrad)
      val predictor = new IteratedConditionalModes[ChainNerLabel](model) // {temperature=0.01}
      println("Example Token features")
      for (iteration <- 1 until 8) {
        learner.processContexts(trainLabels)
        predictor.processAll(trainLabels)
        predictor.processAll(testLabels)
        printEvaluation(trainDocuments, testDocuments, iteration.toString)
        //learner.learningRate *= 0.9
      }
      for (i <- 0 until 3; label <- testLabels) predictor.process(label)
		}
    if(twoStage) {
		 
		  (trainDocuments ++ testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNer2Features(token)))
		
	    for(document <- (trainDocuments ++ testDocuments)) initFeatures(document, (t:Token)=>t.attr[ChainNer2Features])
  		for(document <- (trainDocuments ++ testDocuments)) initSecondaryFeatures(document)
		  println(trainDocuments(3).tokens.map(token => token.nerLabel.target.categoryValue + " "+token.string+" "+token.attr[ChainNer2Features].toString).mkString("\n"))
		  println("Example Test Token features")
		  println(testDocuments(1).tokens.map(token => token.nerLabel.shortCategoryValue+" "+token.string+" "+token.attr[ChainNer2Features].toString).mkString("\n"))
      (trainLabels ++ testLabels).foreach(_.setRandomly())
	    if(bP) {
			  val vars = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[ChainNerLabel])

        val examples = vars.map(v => new LikelihoodExample(v.toSeq, model2, InferByBPChainSum))
        val trainer = new ParallelBatchTrainer(model2.parameters, new LBFGS with L2Regularization)
			  trainer.trainFromExamples(examples)
			  (trainLabels ++ testLabels).foreach(_.setRandomly())
	    
		    trainDocuments.foreach(process(_))
		    testDocuments.foreach(process(_))
			  printEvaluation(trainDocuments, testDocuments, "FINAL")			
	  } else {
      	val learner = new SampleRankTrainer(new GibbsSampler(model2, objective), new AdaGrad())
      	val predictor = new VariableSettingsSampler[ChainNerLabel](model2) {temperature=0.01}

      	for (iteration <- 1 until 8) {
        	learner.processContexts(trainLabels)

        	predictor.processAll(trainLabels)
          predictor.processAll(testLabels)
          printEvaluation(trainDocuments, testDocuments, iteration.toString)
        	//learner.learningRate *= 0.9
      	}
	
      	for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    	  printEvaluation(trainDocuments, testDocuments, "1st")
     }
	  }
    printEvaluation(trainDocuments, testDocuments, "FINAL")

  }

  def test(testFilename:String): Unit = {

    setRandomSeed(75839485)
    // Read in the data
    val testDocuments = LoadConll2003.fromFilename(testFilename, true)
    println("Initializing testing features")
	
	  (testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNerFeatures(token)))
    testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))
    // Add secondary features to domain before it gets frozen
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel])
    (testLabels).foreach(_.setRandomly())

    if(bP) {
      testDocuments.foreach( process(_) )
      if (twoStage) {
        (testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNer2Features(token)))
        testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNer2Features]))
        for(document <- (testDocuments)) initSecondaryFeatures(document)

        testDocuments.foreach( process(_,true) )


      }
    } else {
      val predictor = new VariableSettingsSampler[ChainNerLabel](model) {temperature=0.01}

      for (iteration <- 1 until 3) {
        predictor.processAll(testLabels)
      }

      for (i <- 0 until 3; label <- testLabels) predictor.process(label)
      printEvaluation(testDocuments, testDocuments, "FINAL")

      if(twoStage) {
        (testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNer2Features(token)))
        testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNer2Features]))
        for(document <- (testDocuments)) initSecondaryFeatures(document)

        val predictor = new VariableSettingsSampler[ChainNerLabel](model2) {temperature=0.01}

        for (iteration <- 1 until 3) {
          predictor.processAll(testLabels)
        }

        for (i <- 0 until 3; label <- testLabels) predictor.process(label)
        printEvaluation(testDocuments, testDocuments, "FINAL")
      }
    }
    printEvaluation(testDocuments, testDocuments, "FINAL")
  }

   def printEvaluation(trainDocuments:Iterable[Document], testDocuments:Iterable[Document], iteration:String): Unit = {
      println("TRAIN")
      println(evaluationString(trainDocuments))
      println("TEST")
      println(evaluationString(testDocuments))
      println("Iteration "+iteration)
  }
  
  def evaluationString(documents: Iterable[Document]): Unit = {
    val buf = new StringBuffer
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[ChainNerLabel]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[ChainNerLabel](Conll2003NerDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    for (doc <- documents; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[ChainNerLabel])
    println("Segment evaluation")
    println(segmentEvaluation)
  }

  def process(document:Document, useModel2 : Boolean = false): Unit = {
    if (document.length == 0) return
    for(sentence <- document.sentences if sentence.tokens.size > 0) {
	    val vars = sentence.tokens.map(_.attr[ChainNerLabel]).toSeq
      BP.inferChainMax(vars, if(useModel2) model2 else model)
    }
  }

}

object ChainNer2 extends ChainNer2 {
  import cc.factorie.util.DefaultCmdOptions
  var verbose = false

  def serialize(prefix: String) {
    val modelFile = new File(prefix + "-model")
    val model2File = new File(prefix + "-model2")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    BinarySerializer.serialize(model, modelFile)
    BinarySerializer.serialize(model2, model2File)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinarySerializer.serialize(Conll2003NerDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinarySerializer.serialize(ChainNerFeaturesDomain.dimensionDomain, featuresDomainFile)
    val featuresDomain2File = new File(prefix + "-featuresDomain2")
    BinarySerializer.serialize(ChainNer2FeaturesDomain.dimensionDomain, featuresDomain2File)
  }

  def deSerialize(prefix: String) {
    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-labelDomain'")
    BinarySerializer.deserialize(Conll2003NerDomain, labelDomainFile)
    println("Conll2003NerDomain.length " + Conll2003NerDomain.length)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-featuresDomain'")
    BinarySerializer.deserialize(ChainNerFeaturesDomain.dimensionDomain, featuresDomainFile)
    println("ChainNerFeaturesDomain.dimensionDomain.size: " + ChainNerFeaturesDomain.dimensionDomain.size)
    val featuresDomain2File = new File(prefix + "-featuresDomain2")
    assert(featuresDomain2File.exists(), "Trying to load inexistent label domain file: '" + prefix + "-featuresDomain2'")
    BinarySerializer.deserialize(ChainNer2FeaturesDomain.dimensionDomain, featuresDomain2File)
    println("ChainNerFeaturesDomain.dimensionDomain.size: " + ChainNer2FeaturesDomain.dimensionDomain.size)
    
    val modelFile = new File(prefix + "-model")
    val model2File = new File(prefix + "-model2")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix + "-model'")
    println("model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)
    assertEquals(model.transitionTemplate.weights.value.length, Conll2003NerDomain.length * Conll2003NerDomain.length)
    BinarySerializer.deserialize(model, modelFile)
    println("model.transitionTemplate.weightsSet.length: " + model2.transitionTemplate.weights.value.length)
    println("model.localTemplate.weightsSet.length: " + model2.localTemplate.weights.value.length)


    assert(model2File.exists(), "Trying to load inexisting model file: '" + prefix + "-model2'")
    println("model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)
    assertEquals(model2.transitionTemplate.weights.value.length, Conll2003NerDomain.length * Conll2003NerDomain.length)
    BinarySerializer.deserialize(model2, model2File)
    println("model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)
  
    println("Is the feature domain frozen: " +  ChainNerFeaturesDomain.dimensionDomain.frozen)
    ChainNerFeaturesDomain.dimensionDomain.freeze()
    println("Is the feature domain frozen: " +  ChainNerFeaturesDomain.dimensionDomain.frozen)
    ChainNer2FeaturesDomain.dimensionDomain.freeze()
  }


  def main(args: Array[String]): Unit = {
    // Parse command-line

    object opts extends DefaultCmdOptions {
      val trainFile =     new CmdOption("train", "eng.train", "FILE", "CoNLL formatted training file.")
      val testFile  =     new CmdOption("test",  "eng.testb", "FILE", "CoNLL formatted test file.")
      val sigmaSq  =     new CmdOption("sigmaSq",  "10", "REAL", "Value for regularization constant for BP.")
      val modelDir =      new CmdOption("model", "chainner.factorie", "FILE", "File for saving or loading model.")
      val runXmlDir =     new CmdOption("run-xml", "xml", "DIR", "Directory for reading NYTimes XML data on which to run saved model.")
      val runPlainFiles = new CmdOption("run-plain", List("ner.txt"), "FILE...", "List of files for reading plain texgt data on which to run saved model.")
      val lexiconDir =    new CmdOption("lexicons", "lexicons", "DIR", "Directory containing lexicon files named cities, companies, companysuffix, countries, days, firstname.high,...")
      val brownClusFile = new CmdOption("brown", "", "FILE", "File containing brown clusters.")
      val aggregateTokens = new CmdOption("aggregate", "Turn on context aggregation feature.")
      val extended = new CmdOption("extended", "Turn on 2 stage feature.")
      val verbose =       new CmdOption("verbose", "Turn on verbose output") { override def invoke = ChainNer2.this.verbose = true }
      val yesbp = new CmdOption("bp", "Turn on BP.")
      val justTest = new CmdOption("justTest", "No Training, only Testing.")
      val twoagg = new CmdOption("twoagg", "Turn on second stage aggreggation features.")
    }
    opts.parse(args)

    val lexes = List("WikiArtWork.lst", "WikiArtWorkRedirects.lst", "WikiCompetitionsBattlesEvents.lst", "WikiCompetitionsBattlesEventsRedirects.lst", "WikiFilms.lst", "WikiFilmsRedirects.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiManMadeObjectNames.lst", "WikiManMadeObjectNamesRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst", "WikiPeople.lst", "WikiPeopleRedirects.lst", "WikiSongs.lst", "WikiSongsRedirects.lst", "cardinalNumber.txt", "currencyFinal.txt", "known_corporations.lst", "known_country.lst", "known_jobs.lst", "known_name.lst", "known_names.big.lst", "known_nationalities.lst",  "known_state.lst", "known_title.lst", "measurments.txt", "ordinalNumber.txt", "temporal_words.txt")

    aggregate = opts.aggregateTokens.wasInvoked
    twoStage = opts.extended.wasInvoked
    bP = opts.yesbp.wasInvoked
    ss = opts.sigmaSq.value.toDouble

    if (opts.lexiconDir.wasInvoked) lexicons = Lexicons(opts.lexiconDir.value, lexes)

    if( opts.brownClusFile.wasInvoked) {
          println("Reading brown cluster file " + opts.brownClusFile.value)
          for(line <- Source.fromFile(opts.brownClusFile.value).getLines()){
              val splitLine = line.split("\t")
              clusters(splitLine(1)) = splitLine(0)
          }
    }
    
    if (opts.runPlainFiles.wasInvoked) {
      deSerialize(opts.modelDir.value)
      for (filename <- opts.runPlainFiles.value) {
        val file = new File(filename)
        val document = new Document(io.Source.fromFile(file).mkString).setName(file.getAbsolutePath)
	    cc.factorie.app.nlp.segment.ClearSegmenter.process(document)
        //LoadPlainText.fromFile(new java.io.File(filename))
        //println("ChainNer plain document: <START>"+document.string+"<END>")
        //println(document.map(_.string).mkString(" "))
        document.tokens.map(token => token.attr += new Conll2003ChainNerLabel(token, "O")) 
        document.tokens.map(token => token.attr += new ChainNerFeatures(token))
        initFeatures(document,(t:Token)=>t.attr[ChainNerFeatures])
        // Add secondary features to domain before it gets frozen
        val testLabels = document.tokens.map(_.attr[ChainNerLabel])

        process(document)
        document.tokens.map(token => token.attr += new ChainNer2Features(token))
        initFeatures(document,(t:Token)=>t.attr[ChainNer2Features])
        initSecondaryFeatures(document)

        process(document,true)
 
        println()
        println(filename)

		

        for(s <- document.sentences) {
	   	   for(t <- s.tokens) {
				val label = t.attr[ChainNerLabel].categoryValue
				if(label.matches("(B-|U-).*")) print ("<" + label + ">")
           		print(" " + t.string + " ")
				if(label.matches("(L-|U-).*")) print ("</" + label + ">")
           }
		   println
		}
      }
    } else if(opts.justTest.wasInvoked) {
      	  deSerialize(opts.modelDir.value)
		  test(opts.testFile.value)
	  } else if (opts.runXmlDir.wasInvoked) {
      //println("statClasses "+model.templatesOf[VectorTemplate].toList.map(_.statClasses))
      deSerialize(opts.modelDir.value)
      //run(opts.runXmlDir.value)
    } else {
      train(opts.trainFile.value, opts.testFile.value)
      if (opts.modelDir.wasInvoked) {
        serialize(opts.modelDir.value)
	  }
    }
  }
}
