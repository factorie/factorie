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
import app.strings._
import cc.factorie.util.{ClasspathURL, CmdOptions, HyperparameterMain, BinarySerializer}
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import scala.io._
import java.io._
import scala.math.round
import scala.collection.mutable.ListBuffer
import cc.factorie.app.nlp.embeddings._
import cc.factorie.model.DotFamilyWithStatistics2
import cc.factorie.optimize.{ParameterAveraging, AdaGrad}
import cc.factorie.variable._
import cc.factorie.optimize.Trainer
import cc.factorie.la.WeightsMapAccumulator


class TokenSequence[T<:NerTag](token: Token)(implicit m: Manifest[T]) extends collection.mutable.ArrayBuffer[Token] {
  this.prepend(token)
  val label : String = token.attr[T].categoryValue.split("-")(1)
  def key = this.mkString("-")
}

class StackedChainNer[L<:NerTag](labelDomain: CategoricalDomain[String],
                        newLabel: (Token, String) => L,
                        labelToToken: L => Token,
                        embeddingMap: SkipGramEmbedding,
                        embeddingDim: Int,
                        scale: Double,
                        useOffsetEmbedding: Boolean,
                        url: java.net.URL=null)(implicit m: Manifest[L]) extends DocumentAnnotator {
  object NERModelOpts {
    val argsList = new scala.collection.mutable.HashMap[String, String]()
    argsList += ("scale" -> scale.toString)
    argsList += ("embeddingDim" -> embeddingDim.toString)
  }

  object Demonyms extends lexicon.PhraseLexicon("iesl/demonyms") {
    for (line <- io.Source.fromInputStream(lexicon.ClasspathResourceLexicons.getClass.getResourceAsStream("iesl/demonyms.txt")).getLines()) {
      val fields = line.trim.split(" ?\t ?") // TODO The currently checked in version has extra spaces in it; when this is fixed, use simply: ('\t')
      for (phrase <- fields.drop(1)) this += phrase
    }
  }

  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    if (!document.tokens.head.attr.contains(m.runtimeClass))
      document.tokens.map(token => token.attr += newLabel(token, "O"))
    if (!document.tokens.head.attr.contains(classOf[ChainNerFeatures])) {
      document.tokens.map(token => {token.attr += new ChainNerFeatures(token)})
      initFeatures(document,(t:Token)=>t.attr[ChainNerFeatures])
    }
    process(document, useModel2 = false)
    if (!document.tokens.head.attr.contains(classOf[ChainNer2Features])) {
      document.tokens.map(token => token.attr += new ChainNer2Features(token))
      initFeatures(document,(t:Token)=>t.attr[ChainNer2Features])
      initSecondaryFeatures(document)
    }
    process(document,useModel2 = true)
    document
  }
  def prereqAttrs = Seq(classOf[Sentence], classOf[PennPosTag])
  def postAttrs = Seq(m.runtimeClass).asInstanceOf[Seq[Class[_]]]
  def tokenAnnotationString(token:Token): String = token.attr[L].categoryValue

  object ChainNer2FeaturesDomain extends CategoricalVectorDomain[String]
  class ChainNer2Features(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = ChainNer2FeaturesDomain
    override def skipNonCategories = true
  }
  object ChainNerFeaturesDomain extends CategoricalVectorDomain[String]
  class ChainNerFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = ChainNerFeaturesDomain
    override def skipNonCategories = true
  }

  class StackedChainNereModel[Features <: CategoricalVectorVar[String]](featuresDomain1:CategoricalVectorDomain[String],
                                                             labelToFeatures1:L=>Features,
                                                             labelToToken1:L=>Token,
                                                             tokenToLabel1:Token=>L)(implicit mf: Manifest[Features])
    extends ChainModel(labelDomain, featuresDomain1, labelToFeatures1, labelToToken1, tokenToLabel1) with Parameters {

    // Factor for embedding of observed token
    val embedding = new DotFamilyWithStatistics2[L, EmbeddingVariable] {
      val weights = Weights(new la.DenseTensor2(labelDomain.size, embeddingDim))
    }

    val embeddingPrev = new DotFamilyWithStatistics2[L, EmbeddingVariable] {
      val weights = Weights(new la.DenseTensor2(labelDomain.size, embeddingDim))
    }

    val embeddingNext = new DotFamilyWithStatistics2[L, EmbeddingVariable] {
      val weights = Weights(new la.DenseTensor2(labelDomain.size, embeddingDim))
    }

    override def factors(variables:Iterable[Var]): Iterable[Factor] = {
      val result = new ListBuffer[Factor]
      variables match {
        case labels: Iterable[L] if variables.forall(v => m.runtimeClass.isAssignableFrom(v.getClass)) =>
          var prevLabel: L = null.asInstanceOf[L]
          for (label <- labels) {
            result += bias.Factor(label)
            result += obs.Factor(labelToFeatures(label), label)
            if (prevLabel ne null) {
              result += markov.Factor(prevLabel, label)
              if (useObsMarkov) result += obsmarkov.Factor(prevLabel, label, labelToFeatures(label))
            }
            val scale = NERModelOpts.argsList("scale").toDouble
            if (embeddingMap != null ) {
              if (embeddingMap.contains(labelToToken(label).string)) result += embedding.Factor(label, new EmbeddingVariable(embeddingMap(labelToToken(label).string) * scale))
              if (useOffsetEmbedding && labelToToken(label).sentenceHasPrev && embeddingMap.contains(labelToToken(label).prev.string)) result += embeddingPrev.Factor(label, new EmbeddingVariable(embeddingMap(labelToToken(label).prev.string) * scale))
              if (useOffsetEmbedding && labelToToken(label).sentenceHasNext && embeddingMap.contains(labelToToken(label).next.string)) result += embeddingNext.Factor(label, new EmbeddingVariable(embeddingMap(labelToToken(label).next.string) * scale))
            }
            prevLabel = label
          }
      }
      result
    }

    override def getLocalScores(varying: Seq[L]): Array[DenseTensor1] = {
      val biasScores = bias.weights.value
      val obsWeights = obs.weights.value
      val a = Array.fill[DenseTensor1](varying.size)(null)
      var i = 0
      while (i < varying.length) {
        val scores = obsWeights.leftMultiply(labelToFeatures(varying(i)).value.asInstanceOf[Tensor1]).asInstanceOf[DenseTensor1]
        scores += biasScores
        if (embeddingMap != null) {
          if (embeddingMap.contains(labelToToken(varying(i)).string)) scores += embedding.weights.value * (embeddingMap(labelToToken(varying(i)).string))
          if (i >= 1 && embeddingMap.contains(labelToToken(varying(i-1)).string)) scores += embeddingPrev.weights.value * (embeddingMap(labelToToken(varying(i-1)).string))
          if (i < varying.length-1 && embeddingMap.contains(labelToToken(varying(i+1)).string)) scores += embeddingNext.weights.value * (embeddingMap(labelToToken(varying(i+1)).string))
        }
        a(i) = scores
        i += 1
      }
      a
    }

    override def accumulateExtraObsGradients(gradient: WeightsMapAccumulator, obs: Tensor1, position: Int, labels: Seq[L]): Unit = {
      if (embeddingMap ne null) {
        if (embeddingMap.contains(labelToToken(labels(position)).string)) gradient.accumulate(embedding.weights, obs outer embeddingMap(labelToToken(labels(position)).string))
        if (position >= 1 && embeddingMap.contains(labelToToken(labels(position-1)).string)) gradient.accumulate(embeddingPrev.weights, obs outer embeddingMap(labelToToken(labels(position-1)).string))
        if (position < labels.length-1 && embeddingMap.contains(labelToToken(labels(position+1)).string)) gradient.accumulate(embeddingNext.weights, obs outer embeddingMap(labelToToken(labels(position+1)).string))
      }
    }
  }
  val model = new StackedChainNereModel[ChainNerFeatures](ChainNerFeaturesDomain, l => labelToToken(l).attr[ChainNerFeatures], labelToToken, t => t.attr[L])
  val model2 = new StackedChainNereModel[ChainNer2Features](ChainNer2FeaturesDomain, l => labelToToken(l).attr[ChainNer2Features], labelToToken, t => t.attr[L])

  val objective = cc.factorie.variable.HammingObjective //new HammingTemplate[LabeledMutableDiscreteVar]()

  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    // freeze!
    ChainNerFeaturesDomain.freeze()
    ChainNer2FeaturesDomain.freeze()
    println("Found model")
  }
  else {
    println("model not found")
  }
  println("Model info: scale= "+ NERModelOpts.argsList("scale").toDouble)

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(ChainNerFeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(ChainNer2FeaturesDomain.dimensionDomain, is)
    BinarySerializer.serialize(NERModelOpts.argsList, is)
    BinarySerializer.serialize(model, is)
    BinarySerializer.serialize(model2, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(ChainNerFeaturesDomain.dimensionDomain, is)
    BinarySerializer.deserialize(ChainNer2FeaturesDomain.dimensionDomain, is)
    BinarySerializer.deserialize(NERModelOpts.argsList, is)
    BinarySerializer.deserialize(model, is)
    BinarySerializer.deserialize(model2, is)
    is.close()
  }

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

  def addContextFeatures[A<:Observation[A]](t : Token, from : Token, vf:Token=>CategoricalVectorVar[String]) : Unit = {
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
  
  def aggregateContext[A<:Observation[A]](token : Token, vf:Token=>CategoricalVectorVar[String]) : Unit = {
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

  def initFeatures(document:Document, vf:Token=>CategoricalVectorVar[String]): Unit = {
    count=count+1
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
	    val features = vf(token)
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
      //if (token.isCapitalized) features += "CAPITALIZED"
      //else features += "NOTCAPITALIZED"
      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      if (word.length > 5) { features += "P="+cc.factorie.app.strings.prefix(word, 4); features += "S="+cc.factorie.app.strings.suffix(word, 4) }
      if (token.isPunctuation) features += "PUNCTUATION"

      if (lexicon.iesl.Month.containsLemmatizedWord(word)) features += "MONTH"
      if (lexicon.iesl.Day.containsLemmatizedWord(word)) features += "DAY"

      if (lexicon.iesl.PersonFirst.containsLemmatizedWord(word)) features += "PERSON-FIRST"
      if (lexicon.iesl.PersonFirstHigh.containsLemmatizedWord(word)) features += "PERSON-FIRST-HIGH"
      if (lexicon.iesl.PersonFirstHighest.containsLemmatizedWord(word)) features += "PERSON-FIRST-HIGHEST"
      if (lexicon.iesl.PersonFirstMedium.containsLemmatizedWord(word)) features += "PERSON-FIRST-MEDIUM"

      if (lexicon.iesl.PersonLast.containsLemmatizedWord(word)) features += "PERSON-LAST"
      if (lexicon.iesl.PersonLastHigh.containsLemmatizedWord(word)) features += "PERSON-LAST-HIGH"
      if (lexicon.iesl.PersonLastHighest.containsLemmatizedWord(word)) features += "PERSON-LAST-HIGHEST"
      if (lexicon.iesl.PersonLastMedium.containsLemmatizedWord(word)) features += "PERSON-LAST-MEDIUM"

      if (lexicon.iesl.PersonHonorific.containsLemmatizedWord(word)) features += "PERSON-HONORIFIC"

      if (lexicon.iesl.Company.contains(token)) features += "COMPANY"
      if (lexicon.iesl.JobTitle.contains(token)) features += "JOB-TITLE"
      if (lexicon.iesl.OrgSuffix.contains(token)) features += "ORG-SUFFIX"

      if (lexicon.iesl.Country.contains(token)) features += "COUNTRY"
      if (lexicon.iesl.City.contains(token)) features += "CITY"
      if (lexicon.iesl.PlaceSuffix.contains(token)) features += "PLACE-SUFFIX"
      if (lexicon.iesl.USState.contains(token)) features += "USSTATE"
      if (lexicon.iesl.Continents.contains(token)) features += "CONTINENT"

      if (lexicon.wikipedia.Person.contains(token)) features += "WIKI-PERSON"
      if (lexicon.wikipedia.Event.contains(token)) features += "WIKI-EVENT"
      if (lexicon.wikipedia.Location.contains(token)) features += "WIKI-LOCATION"
      if (lexicon.wikipedia.Organization.contains(token)) features += "WIKI-ORG"
      if (lexicon.wikipedia.ManMadeThing.contains(token)) features += "MANMADE"
      if (Demonyms.contains(token)) features += "DEMONYM"

      if (lexicon.wikipedia.Book.contains(token)) features += "WIKI-BOOK"
      if (lexicon.wikipedia.Business.contains(token)) features += "WIKI-BUSINESS"
      if (lexicon.wikipedia.Film.contains(token)) features += "WIKI-FILM"

      if (lexicon.wikipedia.LocationAndRedirect.contains(token)) features += "WIKI-LOCATION-REDIRECT"
      if (lexicon.wikipedia.PersonAndRedirect.contains(token)) features += "WIKI-PERSON-REDIRECT"
      if (lexicon.wikipedia.OrganizationAndRedirect.contains(token)) features += "WIKI-ORG-REDIRECT"

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

  def getSequences(document : Document) : List[TokenSequence[L]] = {
    var sequences = List[TokenSequence[L]]()
    var seq : TokenSequence[L] = null
    for(token <- document.tokens) {
      val categoryVal = token.attr[L].categoryValue
      if(categoryVal.length() > 0) {
        categoryVal.substring(0,1) match {
         case "B" => seq = new TokenSequence[L](token)
         case "I" => if (seq != null) seq.append(token) else seq = new TokenSequence[L](token)
         case "U" => seq = new TokenSequence[L](token)
         case "L" => if (seq != null) seq.append(token) else seq = new TokenSequence[L](token)
         case _ => null
        }
        if(categoryVal.matches("(L|U)-\\D+")) sequences = seq :: sequences
      }
     }
    sequences
  }

  def  allSubstrings(seq: TokenSequence[L], length : Int) : List[String] = {
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
    document.tokens.foreach(t => t.attr[ChainNer2Features] ++= prevWindowNum(t,2).map(t2 => "PREVLABEL" + t2._1 + "="+t2._2.attr[L].categoryValue))
    document.tokens.foreach(t => t.attr[ChainNer2Features] ++= prevWindowNum(t,1).map(t2 => "PREVLABELCON="+t2._2.attr[L].categoryValue+"&"+t.string))
    for(t <- document.tokens) {
	    if(t.sentenceHasPrev) {
		    t.attr[ChainNer2Features] ++= prevWindowNum(t,2).map(t2 => "PREVLABELLCON="+t.sentencePrev.attr[L].categoryValue+"&"+t2._2.string)
    	  t.attr[ChainNer2Features] ++= nextWindowNum(t,2).map(t2 => "PREVLABELLCON="+t.sentencePrev.attr[L].categoryValue+"&"+t2._2.string)
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
          tokenToLabelMap(token.string) = tokenToLabelMap(token.string) ++ List(token.attr[L].categoryValue)
        else
          tokenToLabelMap(token.string) = List(token.attr[L].categoryValue)
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
        labelDomain.categories.map(str => token.attr[ChainNer2Features] += str + "=" + history(extendedPrediction(token.string), str) )
		  if(extendedPrediction.contains(token.string))
        extendedPrediction(token.string) = extendedPrediction(token.string) ++ List(token.attr[L].categoryValue)
      else
        extendedPrediction(token.string) = List(token.attr[L].categoryValue)
	  }

    for(token <- document.tokens) {
		  val rawWord = token.string
		  if(token.hasPrev && clusters.size > 0) {
			  if(clusters.contains(rawWord))
		      token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[L].categoryValue + "&" + prefix(_,clusters(rawWord)))
			  if(token.hasNext) {
				  var nextRawWord = token.next.string
				  if(clusters.contains(nextRawWord))
					  token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[L].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				  if(token.next.hasNext && clusters.contains(token.next.next.string)) {
					  nextRawWord = token.next.next.string
					  token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[L].categoryValue + "&" + prefix(_,clusters(nextRawWord)))
				  }
			  }
        if(token.hasPrev) {
				  var prevRawWord = token.prev.string
				  if(clusters.contains(prevRawWord))
				    token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[L].categoryValue + "&" + prefix(_,clusters(prevRawWord)))
				  if(token.prev.hasPrev && clusters.contains(token.prev.prev.string)) {
				    prevRawWord = token.prev.prev.string
				    token.attr[ChainNer2Features] ++= List(4,6,10,20).map("BROWNCON="+token.prev.attr[L].categoryValue + "&" + prefix(_,clusters(prevRawWord)))
				  }
			  }
		  }
	  }
  }

  object EmbeddingDomain extends DiscreteDomain(NERModelOpts.argsList("embeddingDim").toInt)
  class EmbeddingVariable(t:la.Tensor1) extends VectorVariable(t) { def domain = EmbeddingDomain }
  object EmbeddingDomain2 extends DiscreteDomain(EmbeddingDomain.size * EmbeddingDomain.size)
  class EmbeddingVariable2(t:la.Tensor1) extends VectorVariable(t) { def domain = EmbeddingDomain2 }

  def history(list : List[String], category : String) : String = {
	  (round( 10.0 * ((list.count(_ == category).toDouble / list.length.toDouble)/3)) / 10.0).toString
  }

  def train(trainDocuments: Seq[Document],testDocuments: Seq[Document], rate: Double, delta: Double): Double = {
    implicit val random = new scala.util.Random(0)
    // Read in the data

    // Add features for NER                 \
    println("Initializing training features")
	
  	(trainDocuments ++ testDocuments).foreach(_.tokens.map(token => token.attr += new ChainNerFeatures(token)))

    trainDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))
    ChainNerFeaturesDomain.freeze()
    println("Initializing testing features")
    testDocuments.foreach(initFeatures(_,(t:Token)=>t.attr[ChainNerFeatures]))

    if (embeddingMap != null) println("StackedChainNer #tokens with no embedding %d/%d".format(trainDocuments.map(_.tokens.filter(t => !embeddingMap.contains(t.string))).flatten.size, trainDocuments.map(_.tokens.size).sum))
    println("StackedChainNer #tokens with no brown clusters assigned %d/%d".format(trainDocuments.map(_.tokens.filter(t => !clusters.contains(t.string))).flatten.size, trainDocuments.map(_.tokens.size).sum))

    val trainLabels = trainDocuments.map(_.tokens).flatten.map(_.attr[L with LabeledMutableDiscreteVar]) //.take(100)
    val testLabels = testDocuments.map(_.tokens).flatten.map(_.attr[L with LabeledMutableDiscreteVar]) //.take(20)
 
    val vars = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[L with LabeledMutableDiscreteVar])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    println("Training with " + examples.length + " examples")
    Trainer.onlineTrain(model.parameters, examples, optimizer=new AdaGrad(rate=rate, delta=delta) with ParameterAveraging, useParallelTrainer=false)
    trainDocuments.foreach(process(_, useModel2=false))
    testDocuments.foreach(process(_, useModel2=false))
    printEvaluation(trainDocuments, testDocuments, "FINAL 1")

    (trainDocuments ++ testDocuments).foreach( _.tokens.map(token => token.attr += new ChainNer2Features(token)))

    for(document <- trainDocuments ++ testDocuments) initFeatures(document, (t:Token)=>t.attr[ChainNer2Features])
    for(document <- trainDocuments ++ testDocuments) initSecondaryFeatures(document)
    ChainNer2FeaturesDomain.freeze()
    //println(trainDocuments(3).tokens.map(token => token.nerTag.target.categoryValue + " "+token.string+" "+token.attr[ChainNer2Features].toString).mkString("\n"))
    //println("Example Test Token features")
    //println(testDocuments(1).tokens.map(token => token.nerTag.shortCategoryValue+" "+token.string+" "+token.attr[ChainNer2Features].toString).mkString("\n"))
    (trainLabels ++ testLabels).foreach(_.setRandomly)
    val vars2 = for(td <- trainDocuments; sentence <- td.sentences if sentence.length > 1) yield sentence.tokens.map(_.attr[L with LabeledMutableDiscreteVar])

    val examples2 = vars2.map(v => new model2.ChainLikelihoodExample(v.toSeq))
    Trainer.onlineTrain(model2.parameters, examples2, optimizer=new AdaGrad(rate=rate, delta=delta) with ParameterAveraging, useParallelTrainer=false)

    trainDocuments.foreach(process)
    testDocuments.foreach(process)
    printEvaluation(trainDocuments, testDocuments, "FINAL")
  }

  def test(testDocs: Seq[Document]): (Double, Double, Double) = {
    var tokenTotal = 0.0
    var sentenceTotal = 0.0
    val t0 = System.currentTimeMillis()
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](labelDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    testDocs.foreach(doc => {
      process(doc)
      for(sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
      sentenceTotal += doc.sentenceCount
      tokenTotal += doc.tokenCount
    })
    val totalTime = System.currentTimeMillis() - t0
    var sentencesPerSecond = (sentenceTotal / totalTime) * 1000.0
    var tokensPerSecond = (tokenTotal / totalTime) * 1000.0
    (sentencesPerSecond, tokensPerSecond, segmentEvaluation.f1)
  }
  
   def printEvaluation(testDocuments:Iterable[Document]): Double = {
     val test = evaluationString(testDocuments)
     println(test)
     test
   }

   def printEvaluation(trainDocuments:Iterable[Document], testDocuments:Iterable[Document], iteration:String): Double = {
     println("TRAIN")
     println(evaluationString(trainDocuments))
     println("TEST")
     val test = evaluationString(testDocuments)
     println(test)
     test
   }
  
  def evaluationString(documents: Iterable[Document]): Double = {
    val buf = new StringBuffer
    buf.append(new LabeledDiscreteEvaluation(documents.flatMap(_.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))))
    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](labelDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    for (doc <- documents; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
    println("Segment evaluation")
    println(segmentEvaluation)
    segmentEvaluation.f1
  }
  def process(document:Document, useModel2 : Boolean): Unit = {
    if (document.tokenCount == 0) return
    for(sentence <- document.sentences if sentence.tokens.size > 0) {
      val vars = sentence.tokens.map(_.attr[L]).toSeq
      (if (useModel2) model2 else model).maximize(vars)(null)
    }
  }
}

class ConllStackedChainNer(embeddingMap: SkipGramEmbedding,
                embeddingDim: Int,
                scale: Double,
                useOffsetEmbedding: Boolean,
                url: java.net.URL=null) extends StackedChainNer[BilouConllNerTag](BilouConllNerDomain, (t, s) => new BilouConllNerTag(t, s), l => l.token, embeddingMap, embeddingDim, scale, useOffsetEmbedding, url) {
  override def process(document:Document): Document = {
    if (document.tokenCount > 0) {
      val doc = super.process(document)
      // Add and populated NerSpanList attr to the document 
      doc.attr.+=(new ner.ConllNerSpanList ++= document.sections.flatMap(section => BilouConllNerDomain.spanList(section)))
      doc
    } else document
  }
}
//object ConllStackedChainNer extends ConllStackedChainNer(SkipGramEmbedding, 100, 1.0, true, ClasspathURL[ConllStackedChainNer](".factorie"))

class NoEmbeddingsConllStackedChainNer(url:java.net.URL) extends ConllStackedChainNer(null, 0, 0.0, false, url)
object NoEmbeddingsConllStackedChainNer extends NoEmbeddingsConllStackedChainNer(ClasspathURL[NoEmbeddingsConllStackedChainNer](".factorie"))

class StackedChainNerOpts extends CmdOptions with SharedNLPCmdOptions{
  val trainFile =     new CmdOption("train", "eng.train", "FILE", "CoNLL formatted training file.")
  val testFile  =     new CmdOption("test",  "eng.testb", "FILE", "CoNLL formatted test file.")
  val modelDir =      new CmdOption("model", "StackedNER.factorie", "FILE", "File for saving or loading model.")
  val runXmlDir =     new CmdOption("run-xml", "xml", "DIR", "Directory for reading NYTimes XML data on which to run saved model.")
  val brownClusFile = new CmdOption("brown", "", "FILE", "File containing brown clusters.")
  val aggregateTokens = new CmdOption("aggregate", true, "BOOLEAN", "Turn on context aggregation feature.")
  val rate =  new CmdOption("rate", 0.18, "DOUBLE", "Learning rate")
  val delta =  new CmdOption("delta", 0.066, "DOUBLE", "Learning delta")
  val saveModel = new CmdOption("save-model", false, "BOOLEAN", "Whether to save the model")
  val runOnlyHere = new CmdOption("runOnlyHere", false, "BOOLEAN", "Run Experiments only on this machine")

  val dataDir = new CmdOption("data", "/home/vineet/canvas/embeddings/data/conll2003/", "STRING", "CONLL data path")
  val embeddingDim = new CmdOption("embeddingDim", 100, "INT", "embedding dimension")
  val embeddingScale = new CmdOption("embeddingScale", 10.0, "FLOAT", "The scale of the embeddings")
  val useOffsetEmbedding = new CmdOption("useOffsetEmbeddings", true, "BOOLEAN", "Whether to use offset embeddings")
}

object ConllStackedChainNerTrainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    // Parse command-line
    val opts = new StackedChainNerOpts
    opts.parse(args)
    val ner = new ConllStackedChainNer(null: SkipGramEmbedding, opts.embeddingDim.value, opts.embeddingScale.value, opts.useOffsetEmbedding.value)

    ner.aggregate = opts.aggregateTokens.wasInvoked

    if (opts.brownClusFile.wasInvoked) {
      println("Reading brown cluster file " + opts.brownClusFile.value)
      for(line <- Source.fromFile(opts.brownClusFile.value).getLines()){
        val splitLine = line.split("\t")
        ner.clusters(splitLine(1)) = splitLine(0)
      }
    }

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainDocsFull = load.LoadConll2003(BILOU=true).fromFilename(opts.trainFile.value)
    val testDocsFull =  load.LoadConll2003(BILOU=true).fromFilename(opts.testFile.value)

    val trainDocs = trainDocsFull.take((trainDocsFull.length*trainPortionToTake).floor.toInt)
    val testDocs = testDocsFull.take((testDocsFull.length*testPortionToTake).floor.toInt)


    val result = ner.train(trainDocs,testDocs, opts.rate.value, opts.delta.value)
    if (opts.saveModel.value) {
      ner.serialize(new FileOutputStream(opts.modelDir.value))
	  }

    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(result,opts.targetAccuracy.value.toDouble)

    result
  }
}

object ConllStackedChainNerOptimizer {
  def main(args: Array[String]) {
    val opts = new StackedChainNerOpts
    opts.parse(args)
    opts.saveModel.setValue(false)

    if (opts.runOnlyHere.value) {
      opts.saveModel.setValue(true)
      val result = ConllStackedChainNerTrainer.evaluateParameters(args)
      println("result: "+ result)
    }
    else {
      val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
      val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-4, 1e4))
      /*
      val ssh = new cc.factorie.util.SSHActorExecutor("apassos",
        Seq("avon1", "avon2"),
        "/home/apassos/canvas/factorie-test",
        "try-log/",
        "cc.factorie.app.nlp.parse.DepParser2",
        10, 5)
        */
      val qs = new cc.factorie.util.QSubExecutor(60, "cc.factorie.app.nlp.ner.ConllStackedChainNerTrainer")
      val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(rate, delta), qs.execute, 200, 180, 60)
      val result = optimizer.optimize()
      println("Got results: " + result.mkString(" "))
      opts.saveModel.setValue(true)
      println("Running best configuration...")
      import scala.concurrent.duration._
      import scala.concurrent.Await
      Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 5.hours)
      println("Done")
    }
  }
}
