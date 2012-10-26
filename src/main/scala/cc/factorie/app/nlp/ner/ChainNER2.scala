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

object ChainNer2FeaturesDomain extends CategoricalTensorDomain[String]
class ChainNer2Features(val token:Token) extends BinaryFeatureVectorVariable[String] {
  def domain = ChainNer2FeaturesDomain
  override def skipNonCategories = true
} 

class ChainNer21Model extends CombinedModel {
  
  // Bias term on each individual label 
  val biasTemplate = new DotTemplateWithStatistics1[ChainNerLabel] {
    factorName = "bias"
    lazy val weights = new la.DenseTensor1(Conll2003NerDomain.size)
  }
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[ChainNerLabel,ChainNerFeatures] {
    factorName = "markov"
    lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, ChainNerFeaturesDomain.dimensionSize)
    def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[ChainNerFeatures])
    def unroll2(tf: ChainNerFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[ChainNerLabel, ChainNerLabel] {
    factorName = "observation"
    lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, Conll2003NerDomain.size)
    def unroll1(label: ChainNerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[ChainNerLabel], label) else Nil
    def unroll2(label: ChainNerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[ChainNerLabel]) else Nil
  } 
  this += biasTemplate
  this += localTemplate
  this += transitionTemplate
}

class ChainNer2Model extends CombinedModel {
  // Bias term on each individual label 
  val bias = new DotTemplateWithStatistics1[ChainNerLabel] {
    factorName = "bias"
    lazy val weights = new la.DenseTensor1(Conll2003NerDomain.size)
  }
  // Factor between label and observed token
  val localTemplate = new DotTemplateWithStatistics2[ChainNerLabel,ChainNer2Features] {
    factorName = "markov"
    lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, ChainNer2FeaturesDomain.dimensionSize)
    def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[ChainNer2Features])
    def unroll2(tf: ChainNer2Features) = Factor(tf.token.attr[ChainNerLabel], tf)
  }
  // Transition factors between two successive labels
  val transitionTemplate = new DotTemplateWithStatistics2[ChainNerLabel, ChainNerLabel] {
    factorName = "observation"
    lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, Conll2003NerDomain.size)
    //override def statisticsDomains = ((Conll2003NerDomain, Conll2003NerDomain))
    def unroll1(label: ChainNerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[ChainNerLabel], label) else Nil
    def unroll2(label: ChainNerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[ChainNerLabel]) else Nil
  }
  this += bias
  this += localTemplate
  this += transitionTemplate
}

class ChainNer2WindowModel extends CombinedModel (
  new DotTemplateWithStatistics1[ChainNerLabel] {
    factorName = "bias"
    lazy val weights = new la.DenseTensor1(Conll2003NerDomain.size)
  },
  new DotTemplateWithStatistics2[ChainNerLabel,ChainNerFeatures] {
    factorName = "window"
    lazy val weights = new la.DenseTensor2(Conll2003NerDomain.size, ChainNerFeaturesDomain.dimensionSize)
    def unroll1(label: ChainNerLabel) = Factor(label, label.token.attr[ChainNerFeatures])
    def unroll2(tf: ChainNerFeatures) = Factor(tf.token.attr[ChainNerLabel], tf)
  }
)

class TokenSequence(token : Token) extends collection.mutable.ArrayBuffer[Token] {
  this.prepend(token)
  val label : String = token.attr[ChainNerLabel].categoryValue.split("-")(1)
  def key = this.mkString("-")
} 

class ChainNer2 {


  val model = new ChainNer21Model
  val model2 = new ChainNer2Model
  val windowModel = new ChainNer2WindowModel
  val objective = new ChainNerObjective
  class Lexicon(filename:String) extends cc.factorie.app.chain.Lexicon(filename) {
    def name = filename.substring(filename.lastIndexOf('/')+1).toUpperCase
  }
  val lexicons = new scala.collection.mutable.ArrayBuffer[Lexicon]
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

  def prefix( prefixSize : Int, cluster : String ) : String = {
    if(cluster.size > prefixSize)
         cluster.substring(0, prefixSize)
    else
         cluster
  }

  def subsect(phrase : Array[String], maxOutLength : Int) : List[String] = {
    val middle = (phrase.size/2)
    var keys = List[String]()
    for(i <- (0 to maxOutLength)) {
      var start = middle
      for(j <- (0 to i)) {
        start = middle-j;
        var key : String= ""
        if(start > -1 && (start+i) < phrase.size) {
          for(k <- (0 to i)) {
            key =  key + " " + phrase(start+k)
          }
          keys = key.trim().toLowerCase :: keys
        }
      }
    }
    keys
  }

  def testWordInLex(phrase : Array[String]) : List[String] = {
    if(wordToLex.size > 0) {

      val keys = subsect(phrase, 4)

      var lexes = List[String]()
      for(key <- keys) {
        if(wordToLex.contains(key))
          lexes = wordToLex(key) ::: lexes
      }
      return lexes.distinct
    } else
      return List()

  }

  def locate(token : Token, key : String) : String = {
	val string = token.string
	val split = key.split(" ")
	if(split.length == 1 && key.trim.toLowerCase == string.trim.toLowerCase) return "U-"
	val last = split.length-1
	for(i <- 0 until split.length) {
		if(split(i).toLowerCase == string.toLowerCase) i match {
			case 0 => return "B-"
			case last => return "L-"
		}
	}
        "I-"
  }

  def wordInLex(token : Token) : List[String] = {
    if(wordToLex.size > 0) {
      var checkWords = List()
      val fullLength = 15
      val fullPhrase = new Array[String](fullLength)
      fullPhrase(fullLength/2) = token.string
      var count = 0

      var prevToken = token;
      while(count < (fullLength/2) && prevToken.hasPrev) {
        count = count+1
        prevToken = prevToken.prev
        fullPhrase(fullLength/2-count) = prevToken.string
      }

      count = 0
      var nextToken = token;
      while(count < (fullLength/2) && nextToken.hasNext) {
        count = count+1
        nextToken = nextToken.next
        fullPhrase(fullLength/2+count) = nextToken.string
      }

      val keys = subsect(fullPhrase, 7)

      var lexes = List[String]()
      for(key <- keys) {
        if(wordToLex.contains(key))
          lexes = wordToLex(key).map(locate(token, key) + _) ::: lexes
      }
      return lexes
    } else
      return List()
  }
  
  def addContextFeatures[A<:Observation[A]](t : Token, from : Token, vf:Token=>CategoricalTensorVar[String]) : Unit = {
    didagg = true
    vf(t) ++= prevWindowNum(from,2).map(t2 => "CONTEXT="+simplifyDigits(t2._2.string).toLowerCase + "@-" + t2._1)
    vf(t) ++= nextWindowNum(from, 2).map(t2 => "CONTEXT="+simplifyDigits(t2._2.string).toLowerCase + "@" + t2._1)
  

    for(t2 <- prevWindowNum(from,2)) {
	if(clusters.contains(t2._2.string)) { 
			vf(t) += ("CONTEXTPATH="+prefix(4, clusters(t2._2.string)) + ("@-" + t2._1.toString))
    	}
    }

    for(t2 <- nextWindowNum(from, 2)) {
	if(clusters.contains(t2._2.string)) {
			vf(t) += ("CONTEXTPATH="+prefix(4, clusters(t2._2.string)) + ("@" + t2._1.toString))
		}
    }
  }
  
  def aggregateContext[A<:Observation[A]](token : Token, vf:Token=>CategoricalTensorVar[String]) : Unit = {
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
  

  def initFeatures(document:Document, vf:Token=>CategoricalTensorVar[String]): Unit = {
    //println("Count" + count)
    count=count+1
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
      didagg = false
	  val features = vf(token)
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
      //features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      //features += "POS="+token.posLabel.categoryValue
      if (token.isCapitalized) features += "CAPITALIZED"
      else features += "NOTCAPITALIZED"
      //if (token.isAllCaps) features += "ALLCAPS"
      //if (token.isDigits) features += "DIGITS"
      //if (token.alphaNumeric) features += "ALPHANUMERIC"
      //features += "SUFFIX3="+word.takeRight(3)
      //features += "PREFIX3="+word.take(3)
      //features += "POS="+token.attr[cc.factorie.app.nlp.pos.PosLabel].categoryValue
      //if (token.containsDigit) features += "NUMERIC"
      if (token.isPunctuation) features += "PUNCTUATION"
      /*if (lexicons.size > 0)
        for (lexicon <- lexicons) if (lexicon.contains(token)) features += "LEX="+lexicon.name
      if (wordToLex.size > 0 && wordToLex.contains(token.string.toLowerCase))
        for(lexicon <- wordToLex(token.string.toLowerCase)) features += "LEX="+lexicon */
      for(lexicon <- wordInLex(token)) features += "LEX="+lexicon
      if (clusters.size > 0 && clusters.contains(rawWord)) {
        features += "CLUS="+prefix(4,clusters(rawWord))
        features += "CLUS="+prefix(6,clusters(rawWord))
        features += "CLUS="+prefix(10,clusters(rawWord))
        features += "CLUS="+prefix(20,clusters(rawWord))
      }
		
      //if(didagg) println(token.attr[ChainNerFeatures])
    }
    for (sentence <- document.sentences)
      //cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence, (t:Token)=>t.attr[ChainNerFeatures], "^[^@]*$", List(0), List(0,0), List(0,-1), List(0,1), List(1), List(2), List(-1), List(-2))

      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentence.tokens, vf, "^[^@]*$", List(0), List(1), List(2), List(-1), List(-2))
      // If the sentence contains no lowercase letters, tell all tokens in the sentence they are part of an uppercase sentence
      // document.sentences.foreach(s => if (!s.exists(_.containsLowerCase)) s.foreach(t => t.attr[ChainNerFeatures] += "SENTENCEUPPERCASE"))
      // Add features for character n-grams between sizes 2 and 5
      document.tokens.foreach(t => if (t.string.matches("[A-Za-z]+")) vf(t) ++= t.charNGrams(2,5).map(n => "NGRAM="+n))
      // Add features from window of 4 words before and after
      // document.foreach(t => t.attr[ChainNerFeatures] ++= t.prevWindowNum(4).map(t2 => "PREVWINDOW" + t2._1 + "="+simplifyDigits(t2._2.string).toLowerCase))
      // document.foreach(t => t.attr[ChainNerFeatures] ++= t.nextWindowNum(4).map(t2 => "NEXTWINDOW" + t2._1 + "="+simplifyDigits(t2._2.string).toLowerCase))
      document.tokens.foreach(t => vf(t) ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+simplifyDigits(t2.string).toLowerCase))
      document.tokens.foreach(t => vf(t) ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+simplifyDigits(t2.string).toLowerCase))

    for(token <- document.tokens) {
      if(aggregate) aggregateContext(token, vf)
     }
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
	if(length == 0)
		return List[String]()
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
          //println(seq.key)
          //println(sequenceToLabelMap)
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
		if(extendedPrediction.contains(token.string)) {
			//println("EXTENDED")	
        		//Conll2003NerDomain.categoryValues.map(str => println(str + "=" + history(extendedPrediction(token.string), str)) )
        		Conll2003NerDomain.categories.map(str => token.attr[ChainNer2Features] += str + "=" + history(extendedPrediction(token.string), str) )
		}
		if(extendedPrediction.contains(token.string))
        		extendedPrediction(token.string) = extendedPrediction(token.string) ++ List(token.attr[ChainNerLabel].categoryValue)
        	else
        		extendedPrediction(token.string) = List(token.attr[ChainNerLabel].categoryValue)
	}
	for(token <- document.tokens) {
		val rawWord = token.string
		if (token.hasPrev && clusters.size > 0) {
			if(clusters.contains(rawWord)) {
		        	token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(rawWord)))
			}
			if(token.hasNext) {
				var nextRawWord = token.next.string
				if(clusters.contains(nextRawWord)) {
					token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				}
				if(token.next.hasNext && clusters.contains(token.next.next.string)) {
					nextRawWord = token.next.next.string
					token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				}
			}
      			if(token.hasPrev) {
				var prevRawWord = token.prev.string
				if(clusters.contains(prevRawWord)) {
					token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[ChainNerLabel].categoryValue + "&" + prefix(_,clusters(prevRawWord)))
				}
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
  
  def hasFeatures(token:Token): Boolean = token.attr.contains(classOf[ChainNerFeatures])
  def hasFeatures(document:Document): Boolean = hasFeatures(document.tokens.head)
  
  def hasLabel(token:Token): Boolean = token.attr.contains(classOf[NerLabel])
  def hasLabels(document:Document): Boolean = hasLabel(document.tokens.head)

  def getLabel(string : String) : String = {
	if(string.contains("-"))
		string.split("-")(1)
	else
		return "O"
  }

   def dec2bin(integer : Int ) : String = {
	   var num = integer
           var bin = ""
	   while(num > 0) {
		if(num % 2 == 1) 
			bin = "1" + bin
		else
			bin = "0" + bin
		num = num / 2
	}
	bin
    }


  def train(trainFilename:String, testFilename:String): Unit = {
    // Read in the data
    val trainDocuments = LoadConll2003.fromFilename(trainFilename, true)
    val testDocuments = LoadConll2003.fromFilename(testFilename, true)
	
    println("Taking 100 and 20")

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
			
			var count = 0;

			val vars = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[ChainNerLabel])

//		    val pieces = vars.map(vs => new ForwardBackwardExample(vs.toArray, model.localTemplate, model.transitionTemplate))
//		    val trainer = new ParallelTrainer(pieces, model.familiesOfClass(classOf[DotFamily]))
//		    val optimizer = new LimitedMemoryBFGS(trainer) {
//		      override def postIteration(iter: Int): Unit = {
//			        trainDocuments.foreach(process(_))
//					testDocuments.foreach(process(_))
//			      	printEvaluation(trainDocuments, testDocuments, iter.toString)
//			  }
//		    }
//		      optimizer.optimize()
//		      optimizer.optimize()
      val pieces = vars.map(v => new MaxLikelihoodExample(v.toSeq, InferByBPChainSum))
      val trainer = new BatchTrainer(new L2RegularizedLBFGS(), model)
      (1 to 100).foreach(i => trainer.processAll(pieces))

      (trainLabels ++ testLabels).foreach(_.setRandomly())
	    
      trainDocuments.foreach(process(_))
      testDocuments.foreach(process(_))
      printEvaluation(trainDocuments, testDocuments, "FINAL")
	  } else {
	
      (trainLabels ++ testLabels).foreach(_.setRandomly())
      

      // Train primary (independent classifier) model
      // Train secondary (markov) model
      val learner1 = new SampleRank(new GibbsSampler(model, objective), new StepwiseGradientAscent(1.0))
      //val predictor = new VariableSettingsSampler[ChainNerLabel](model, null)
      val predictor1 = new VariableSettingsSampler[ChainNerLabel](model) {temperature=0.01}


      //(trainDocuments ++ testDocuments).foreach(d => initSecondaryFeatures(d))
      println("Example Token features")

      //val windowPredictor = new VariableSettingsGreedyMaximizer[ChainNerLabel](windowModel)
      /*for(document <- (trainDocuments ++ testDocuments)) {
        windowPredictor.processAll(document.map(_.attr[ChainNerLabel]))
        initSecondaryFeatures(document)
      }*/
      
      for (iteration <- 1 until 8) {
        learner1.processAll(trainLabels)
        predictor1.processAll(testLabels)
        printEvaluation(trainDocuments, testDocuments, iteration.toString)
        //learner.learningRate *= 0.9
      }
      for (i <- 0 until 3; label <- testLabels) predictor1.process(label)
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
		
			var count = 0;

			val vars1 = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[ChainNerLabel])

//		    val pieces1 = vars1.map(vs => new ForwardBackwardExample(vs.toArray, model.localTemplate, model.transitionTemplate))
//		    val trainer1 = new ParallelTrainer(pieces1, model.familiesOfClass(classOf[DotFamily]))
//		    val optimizer1 = new LimitedMemoryBFGS(trainer1) {
//		      override def postIteration(iter: Int): Unit = {
//			        trainDocuments.foreach(process(_))
//					testDocuments.foreach(process(_))
//			      	printEvaluation(trainDocuments, testDocuments, iter.toString)
//			  }
//		    }
//		      optimizer1.optimize()
//		      optimizer1.optimize()
        val pieces = vars1.map(v => new MaxLikelihoodExample(v.toSeq, InferByBPChainSum))
        val trainer = new BatchTrainer(new L2RegularizedLBFGS, model)
        (1 to 100).foreach(i => trainer.processAll(pieces))
		      
			  (trainLabels ++ testLabels).foreach(_.setRandomly())
	    
		      trainDocuments.foreach(process(_))
		      testDocuments.foreach(process(_))
			  printEvaluation(trainDocuments, testDocuments, "FINAL")			
	} else {
      
      // Train secondary (markov) model
      	val learner = new SampleRank(new GibbsSampler(model2, objective), new MIRA)
      //val predictor = new VariableSettingsSampler[ChainNerLabel](model, null)
      	val predictor = new VariableSettingsSampler[ChainNerLabel](model2) {temperature=0.01}

      	for (iteration <- 1 until 8) {
        	learner.processAll(trainLabels)
        	predictor.processAll(testLabels)
        	printEvaluation(trainDocuments, testDocuments, iteration.toString)
        	//learner.learningRate *= 0.9
      	}
	
      	for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    	printEvaluation(trainDocuments, testDocuments, "1st")
    
     }
	}
      printEvaluation(trainDocuments, testDocuments, "FINAL")

      outputCONLLformat(testDocuments, testFilename)

    //}
  }

  def test(testFilename:String): Unit = {
    setRandomSeed(75839485)
    // Read in the data
    val testDocuments = LoadConll2003.fromFilename(testFilename, true)
	
    println("Initializing testing features")
	
	(testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNerFeatures(token)))
    testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))
    // Add secondary features to domain before it gets frozen
   
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[ChainNerLabel]) //.take(20)
    
      (testLabels).foreach(_.setRandomly())
      
      val predictor1 = new VariableSettingsSampler[ChainNerLabel](model) {temperature=1.0}
      for (iteration <- 1 until 3) {
      	predictor1.processAll(testLabels)
      	//printEvaluation(testDocuments, testDocuments, iteration.toString)
      	//learner.learningRate *= 0.9
      	predictor1.temperature *= 0.5
      }
    
      for (i <- 0 until 3; label <- testLabels) predictor1.process(label)

      printEvaluation(testDocuments, testDocuments, "FINAL")

  	  (testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNer2Features(token)))
      testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNer2Features]))

  	  for(document <- (testDocuments)) initSecondaryFeatures(document)	   
      (testLabels).foreach(_.setRandomly())
      
      	val predictor = new VariableSettingsSampler[ChainNerLabel](model2) {temperature=1.0}

	  	for (iteration <- 1 until 7) {
        	predictor.processAll(testLabels)
        	//printEvaluation(testDocuments, testDocuments, iteration.toString)
        	//learner.learningRate *= 0.9
	      	predictor.temperature *= 0.5
      	}
    
      	for (i <- 0 until 3; label <- testLabels) predictor.process(label)
   
      printEvaluation(testDocuments, testDocuments, "FINAL")
      outputCONLLformat(testDocuments, testFilename)
  }

  def convertToIOB(docs : Seq[Document]) : Unit = {
	for(doc <- docs) {
		for (token <- doc.tokens) {
		  val IOBLabel = BILOUtoIOB(token)
		  val targetIOBLabel = BILOUtoIOB(token, true)
          token.attr.remove[ChainNerLabel]
          token.attr += new Conll2003ChainNerLabel(token, targetIOBLabel)
          token.attr[ChainNerLabel].set(token.attr[ChainNerLabel].domain.value(IOBLabel))(null)
		}
	}
  }
  
  def BILOUtoIOB(token : Token, target : Boolean = false) : String = {
	  var prevLabel : String = null
	  var nerlabel : String = null
	  if(target) {
		nerlabel = token.nerLabel.target.categoryValue
     	if(token.sentenceHasPrev) 
			prevLabel = token.sentencePrev.nerLabel.target.categoryValue
 	  } else {
		nerlabel = token.nerLabel.categoryValue
		if(token.sentenceHasPrev) 
			prevLabel = token.sentencePrev.nerLabel.categoryValue
	  } 
	
	  var BILOUSplit : Array[String] = null
      var prevSplit : Array[String] = null
      if(token.sentenceHasPrev) {
        if(prevLabel.contains("-"))
          prevSplit = prevLabel.split("-")
        else
          prevSplit = Array("O","")
      }

      if(nerlabel.contains("-"))
        BILOUSplit = nerlabel.split("-")
      else
        BILOUSplit = Array("O","")
      var IOBLabel : String = null
      if(BILOUSplit(0) == "O")
        IOBLabel = "O"
      else if(BILOUSplit(0) == "I")
        IOBLabel = "I-" + BILOUSplit(1)
      else if(BILOUSplit(0) == "B" || BILOUSplit(0) == "U") {
        if( token.sentenceHasPrev && (prevSplit(0)=="L"||prevSplit(0)=="U") && prevSplit(1)==BILOUSplit(1))
          IOBLabel = "B-" + BILOUSplit(1)
        else
          IOBLabel = "I-" + BILOUSplit(1)
      } else if(BILOUSplit(0) == "L")
        IOBLabel = "I-" + BILOUSplit(1)
	IOBLabel
  }

  def outputCONLLformat(documents : Seq[Document], conllFile : String)  : Unit = {
    val corpus = Source.fromFile(conllFile).getLines().buffered

    var outfile = conllFile
    if(bP) outfile += "Bp" else outfile += "Sample"
    if(bP) outfile += ss
    outfile += ".out"
    val outputFile = new File(outfile)
    val writer = new BufferedWriter(new FileWriter(outputFile))

    for(document <- documents) {
	  var docStarted = false
	  if(corpus.head.contains("DOCSTART"))
      	corpus.next()
      //writer.write("\n")
      for(sentence <- document.sentences) {
        for(token <- sentence.tokens) {
          var BILOUSplit : Array[String] = null
          var prevSplit : Array[String] = null
          if(token.sentenceHasPrev) {
            if(token.sentencePrev.nerLabel.categoryValue.contains("-"))
              prevSplit = token.sentencePrev.nerLabel.categoryValue.split("-")
            else
              prevSplit = Array("O","")
          }

          if(token.nerLabel.categoryValue.contains("-"))
            BILOUSplit = token.nerLabel.categoryValue.split("-")
          else
            BILOUSplit = Array("O","")
          var IOBLabel : String = null
          if(BILOUSplit(0) == "O")
            IOBLabel = "O"
          else if(BILOUSplit(0) == "I")
            IOBLabel = "I-" + BILOUSplit(1)
          else if(BILOUSplit(0) == "B" || BILOUSplit(0) == "U") {
            if( token.sentenceHasPrev && (prevSplit(0)=="L"||prevSplit(0)=="U") && prevSplit(1)==BILOUSplit(1))
              IOBLabel = "B-" + BILOUSplit(1)
            else
              IOBLabel = "I-" + BILOUSplit(1)
          } else if(BILOUSplit(0) == "L")
            IOBLabel = "I-" + BILOUSplit(1)
		  if(corpus.head.trim().length == 0)
			writer.write(corpus.next().trim() + "\n")
          //writer.write(token.string + " |> " + corpus.next().trim() + " <| " + IOBLabel + "\n")
          val ns = corpus.next().trim().split(" ")
		  val output = ns(0) + " " + ns(1) + " " + ns(3)
          writer.write(output + " " + IOBLabel + "\n")
		  /*if(ns(3) != IOBLabel) {
			println(ns(0) + "-" + ns(3) + " - " + IOBLabel + " - " + token.nerLabel.categoryValue)
			for(t <- token.sentence) {
				if(t.string==ns(0))
					print("=>")
				print(t.string + "\t")
			}
			println("")
			for(t <- token.sentence) {
				print(t.attr[ChainNerLabel].target.categoryValue + "\t")
			}
			println("")
			for(t <- token.sentence) {
				print(t.attr[ChainNerLabel].categoryValue + "\t")
			}
		  }*/
        }
		if(corpus.hasNext && corpus.head.trim().length == 0)
        	writer.write(corpus.next().trim() + "\n")
		writer.flush()
      }
    }
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
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[ChainNerLabel]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[ChainNerLabel](Conll2003NerDomain.categories.filter(_.length > 2).map(_.substring(2)))
    for (doc <- documents; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[ChainNerLabel])
    println("Segment evaluation")
    println(segmentEvaluation)
  }

  def process(document:Document): Unit = {
    if (document.length == 0) return
    if (!hasFeatures(document)) initFeatures(document, (t:Token)=>t.attr[ChainNerFeatures])  // TODO check to see if secondary features were already added
    if (true) {
    	for(sentence <- document.sentences if sentence.tokens.size > 0) {
	  		val vars = sentence.tokens.map(_.attr[ChainNerLabel]).toSeq
	  		//Viterbi.searchAndSetToMax(vars, model.localTemplate, model.transitionTemplate)
	  		BP.inferChainMax(vars, model)
    	}
    } else {
      for (token <- document.tokens) if (token.attr[ChainNerLabel] == null) token.attr += new Conll2003ChainNerLabel(token, Conll2003NerDomain.category(0)) // init value doens't matter
      val localModel = new CombinedModel(model.subModels(0), model.subModels(1))
      val localPredictor = new VariableSettingsGreedyMaximizer[ChainNerLabel](localModel, null)
      for (label <- document.tokens.map(_.attr[ChainNerLabel])) localPredictor.process(label)
      val predictor = new VariableSettingsSampler[ChainNerLabel](model, null)
      for (i <- 0 until 3; label <- document.tokens.map(_.attr[ChainNerLabel])) predictor.process(label)
    }	
  }
  
  def printSGML(tokens:IndexedSeq[Token]): Unit = {
    var i = 0
    val other = Conll2003NerDomain.index("O")
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
    val other = Conll2003NerDomain.index("O")
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

object ChainNer2 extends ChainNer2 {
  import cc.factorie.util.DefaultCmdOptions
  var verbose = false

  def main(args: Array[String]): Unit = {
    // Parse command-line

    object opts extends DefaultCmdOptions {
      val trainFile =     new CmdOption("train", "eng.train", "FILE", "CoNLL formatted training file.")
      val testFile  =     new CmdOption("test",  "eng.testb", "FILE", "CoNLL formatted test file.")
      val sigmaSq  =     new CmdOption("sigmaSq",  "10", "REAL", "Value for regularization constant for BP.")
      val modelDir =      new CmdOption("model", "chainner.factorie", "DIR", "Directory for saving or loading model.")
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
	//val noSentences=new CmdOption("nosentences", "Do not use sentence segment boundaries in training.  Improves accuracy when testing on data that does not have sentence boundaries.")
    }
    opts.parse(args)

    val lexes = List("WikiArtWork.lst", "WikiArtWorkRedirects.lst", "WikiCompetitionsBattlesEvents.lst", "WikiCompetitionsBattlesEventsRedirects.lst", "WikiFilms.lst", "WikiFilmsRedirects.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiManMadeObjectNames.lst", "WikiManMadeObjectNamesRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst", "WikiPeople.lst", "WikiPeopleRedirects.lst", "WikiSongs.lst", "WikiSongsRedirects.lst", "cardinalNumber.txt", "currencyFinal.txt", "known_corporations.lst", "known_country.lst", "known_jobs.lst", "known_name.lst", "known_names.big.lst", "known_nationalities.lst",  "known_state.lst", "known_title.lst", "measurments.txt", "ordinalNumber.txt", "temporal_words.txt")

    aggregate = opts.aggregateTokens.wasInvoked
    twoStage = opts.extended.wasInvoked
    bP = opts.yesbp.wasInvoked
    ss = opts.sigmaSq.value.toDouble

    if (opts.lexiconDir.wasInvoked) {
      //for (filename <- List("cities", "companies", "companysuffix", "countries", "days", "firstname.high", "firstname.highest", "firstname.med", "jobtitle", "lastname.high", "lastname.highest", "lastname.med", "months", "states")) {
      for(filename <- lexes) {
        println("Reading lexicon "+filename)
        lexicons += new Lexicon(opts.lexiconDir.value+"/"+filename)
        val source = Source.fromFile(opts.lexiconDir.value+"/"+filename)
        for(line <- source.getLines()) {
          if(wordToLex.contains(line.toLowerCase)) {
            wordToLex(line.toLowerCase) = wordToLex(line.toLowerCase) ++ List(filename);
          } else {
            wordToLex(line.toLowerCase) = List(filename);
          }
        }
      }
    }

    if( opts.brownClusFile.wasInvoked) {
          println("Reading brown cluster file " + opts.brownClusFile.value)
          for(line <- Source.fromFile(opts.brownClusFile.value).getLines()){
              val splitLine = line.split("\t")
              clusters(splitLine(1)) = splitLine(0)
          }
    }
    
    if (opts.runPlainFiles.wasInvoked) {
      model.load(opts.modelDir.value)
      for (filename <- opts.runPlainFiles.value) {
        val document = LoadPlainText.fromFile(new java.io.File(filename), false)
        //println("ChainNer plain document: <START>"+document.string+"<END>")
        //println(document.map(_.string).mkString(" "))
        process(document)
        println()
        println(filename)
        printEntities(document.tokens)
        printSGML(document.tokens)
      }
    } else if(opts.justTest.wasInvoked) { 
		model.load(opts.modelDir.value)
        model2.load(opts.modelDir.value + "2")
		test(opts.testFile.value)
	} else if (opts.runXmlDir.wasInvoked) {
      //println("statClasses "+model.templatesOf[VectorTemplate].toList.map(_.statClasses))
      model.load(opts.modelDir.value)
      //run(opts.runXmlDir.value)
    } else {
      train(opts.trainFile.value, opts.testFile.value)
      if (opts.modelDir.wasInvoked) {
		model.save(opts.modelDir.value)
		model2.save(opts.modelDir.value + "2")
	  }
    }
    //if (args.length != 2) throw new Error("Usage: NER trainfile testfile.")
  }
}
