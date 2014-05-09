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
package cc.factorie.app.nlp.segment

import scala.collection.mutable.{Map, ArrayBuffer}
import scala.util.Random
import java.io._
import cc.factorie._
import cc.factorie.util.{BinarySerializer, HyperparameterMain}
import cc.factorie.util.CubbieConversions._
import cc.factorie.variable._
import cc.factorie.app.nlp._
import cc.factorie.optimize.{LikelihoodExample, BatchTrainer, OnlineTrainer}
import cc.factorie.app.chain.ChainModel

/** A linear-chain CRF model for Chinese word segmentation with four companion 
    objects, each pre-trained on a different corpus that corresponds to a 
    different variety of written Mandarin. 
    @author Henry Oskar Singer  */

class ChainChineseWordSegmenter(
  labelDomain: SegmentationLabelDomain = BIOSegmentationDomain
) extends DocumentAnnotator {

  val singleCharWordTable = new CategoricalDomain[String]
  val bigramTable = new CategoricalDomain[String]
  val prefixTable = new CategoricalDomain[String]
  val suffixTable = new CategoricalDomain[String]
  val rareWordThreshold = 100

  def this(filePath: String) {
    this()
    deserialize(filePath)
  }
  def this(dataStream: InputStream) {
    this()
    deserialize(dataStream)
  }
  def this(url: java.net.URL) = this(url.openConnection().getInputStream)

  def process(document: Document): Document = {

    //Since tokens are position-based, and the character tagger removes whitespace,
    //its necessary to consider whitespace when creating tokens from tagged characters
    val whiteSpaceOffsets = labelDomain.getWhiteSpaceOffsets(document.string)
    val segmentedText = segment(document)

    var tokenStart = 0

    ( 0 to segmentedText.size ).foreach{ i =>

      if( i == 0 || labelDomain.isEndOfSentence(segmentedText(i - 1).character.string(0)) )
        new Sentence(document)
    
      if( i > 0 && (i == segmentedText.size || labelDomain.indicatesSegmentStart(segmentedText(i).categoryValue))){
        new Token(document, whiteSpaceOffsets(tokenStart) + tokenStart, whiteSpaceOffsets(i - 1) + i)

        tokenStart = i
      }
    } 

    document
  }

  //Word segmentation is the first thing that happens to Chinese text in the 
  //pipeline, so there shouldn't be any attrs before word segmentation
  def prereqAttrs = Seq()

  def postAttrs = Seq(classOf[Token], classOf[Sentence])

  def tokenAnnotationString(token: Token): String = {
    token.string + "\t"
  } 

  def serialize(filePath: String): Unit = serialize(new FileOutputStream(new File(filePath)))
  def serialize(stream: OutputStream): Unit = {
    
    val dataStream = new DataOutputStream(new BufferedOutputStream(stream)) 

    BinarySerializer.serialize(singleCharWordTable.dimensionDomain, dataStream)
    BinarySerializer.serialize(bigramTable.dimensionDomain, dataStream)
    BinarySerializer.serialize(prefixTable.dimensionDomain, dataStream)
    BinarySerializer.serialize(suffixTable.dimensionDomain, dataStream)
    BinarySerializer.serialize(SegmentationFeaturesDomain.dimensionDomain, dataStream)
    BinarySerializer.serialize(model, dataStream)
    dataStream.close
  }

  def deserialize(filePath: String): Unit = deserialize(new FileInputStream(new File(filePath)))
  def deserialize(stream: InputStream): Unit = {
    
    val dataStream = new DataInputStream(new BufferedInputStream(stream))

    BinarySerializer.deserialize(singleCharWordTable.dimensionDomain, dataStream)
    BinarySerializer.deserialize(bigramTable.dimensionDomain, dataStream)
    BinarySerializer.deserialize(prefixTable.dimensionDomain, dataStream)
    BinarySerializer.deserialize(suffixTable.dimensionDomain, dataStream)
    BinarySerializer.deserialize(SegmentationFeaturesDomain.dimensionDomain, dataStream)
    BinarySerializer.deserialize(model, dataStream)
    dataStream.close
  }

  def train(filePaths: List[String]): Unit = {

    println("Training In Progress")
    println("\tFeature Extraction In Progress")

    val labeledCorpora = filePaths.map(
                            filePath => labelDomain.getLabeledCharacters(new File(filePath))
                          ).flatten.toIndexedSeq

    populateFeatureTables(labeledCorpora.flatten)

    val trainingSegmentables = getSegmentables(labeledCorpora)

    SegmentationFeaturesDomain.freeze
    
    println("\tFeature Extraction Completed")

    val examples = 
      trainingSegmentables.map( segmentable =>
        new model.ChainLikelihoodExample(segmentable.links.map( _.label ))
      ).toList

    Random.setSeed(0)

    val shuffledExamples = Random.shuffle(examples)
    val trainer = new OnlineTrainer(model.parameters)

    trainer.trainFromExamples(shuffledExamples)

    println("Training Complete\n")
  }

  def populateFeatureTables(labeledCorpus: IndexedSeq[(String, String)]): Unit = {

    populateSingleCharWordTable(labeledCorpus)
    populateBigramTable(labeledCorpus)
    populateAffixTables(labeledCorpus)
  }

  def populateAffixTables(labeledCorpus: IndexedSeq[(String, String)]): Unit = {
    
    val (prefixes, suffixes) = getAffixes(labeledCorpus)

    prefixTable.clear
    prefixes.foreach( prefix => prefixTable.index(prefix) )
    prefixTable.freeze

    suffixTable.clear
    suffixes.foreach( suffix => suffixTable.index(suffix) )
    suffixTable.freeze
  }

  def getAffixes(labeledCorpus: IndexedSeq[(String, String)]): (List[String], List[String]) = {

    val words = getWords(labeledCorpus).filter( word => word.length > 1 )
    val tempDomain = new CategoricalDomain[String]
    
    tempDomain.gatherCounts = true 
    words.foreach( word => tempDomain.index(word) )
    tempDomain.trimAboveCount(rareWordThreshold)

    val rareWords = tempDomain.categories.toList
    val prefixes = rareWords.map( 
      word => word.slice(0,1) 
    ).distinct
    val suffixes = rareWords.map( 
      word => word.slice(word.size-1, word.size) 
    ).distinct

    (prefixes, suffixes)
  }

  def getWords(labeledCorpus: IndexedSeq[(String, String)]): List[String] = {

    val delimiter = '|'

    labeledCorpus.map(
      pair => {
               if ( labelDomain.indicatesSegmentStart(pair._2) ) 
                 delimiter+pair._1 
               else 
                 pair._1
              }
    ).mkString.split(delimiter).toList
  }

  def populateBigramTable(labeledCorpus: IndexedSeq[(String, String)]): Unit = {

    val bigrams = getBigrams(labeledCorpus)

    bigramTable.clear
    bigrams.foreach( bigram => bigramTable.index(bigram) )
    bigramTable.freeze
  }

  def getBigrams(labeledCorpus: IndexedSeq[(String, String)]): List[String] = {
    
    val charsOnly = labeledCorpus.map( pair => pair._1 )
    val bigramZip = ("0" +: charsOnly).zip(charsOnly :+ "0").slice(1, charsOnly.size)

    bigramZip.map( pair => pair._1 + pair._2 ).toList.distinct
  }

  def populateSingleCharWordTable(labeledCorpus: IndexedSeq[(String, String)]): Unit = {
    
    val onlySingleCharWords = getOnlySingleCharWords(labeledCorpus)

    singleCharWordTable.clear
    onlySingleCharWords.foreach( char => singleCharWordTable.index(char) )
    singleCharWordTable.freeze
  }

  def getOnlySingleCharWords(labeledCorpus: IndexedSeq[(String, String)]): List[String] = {

    val (singleInstances, nonSingleInstances) = labeledCorpus.partition( 
      pair => labelDomain.isSolitary(pair._2) 
    )
    val singleChars = singleInstances.map( pair => pair._1 ).toSet
    val nonSingleChars = nonSingleInstances.map( pair => pair._1 ).toSet
                             
    (singleChars -- (singleChars & nonSingleChars)).toList
  }

  object SegmentationFeaturesDomain extends CategoricalVectorDomain[String]
  class SegmentationFeatures(val features: Seq[String])
    extends BinaryFeatureVectorVariable[String] {

    override def skipNonCategories = true
    def domain = SegmentationFeaturesDomain

    this ++= features
  } 

  class Character(character: String, labelString: String, featureSeq: Seq[String]) 
    extends app.chain.Observation[Character] 
    with ChainLink[Character, Segmentable] {
    
    val features = new SegmentationFeatures(featureSeq)
    val label = new SegmentationLabel(labelString, this)

    def string = character
  }

  class SegmentationLabel(labelName: String, val character: Character) 
    extends LabeledCategoricalVariable(labelName) {

    def domain = labelDomain
  }

  class Segmentable extends variable.Chain[Segmentable, Character]

  val model = new ChainModel[SegmentationLabel, SegmentationFeatures, Character](
    labelDomain,
    SegmentationFeaturesDomain,
    label => label.character.features,
    label => label.character,
    character => character.label
  )

  def getF1Score(trainPath: String, logPath: String): Double = {

    val labelSeq = segment(trainPath)
    val myWords = new ArrayBuffer[ArrayBuffer[SegmentationLabel]]
    val numTrueWords: Double = labelSeq.count(
      label => labelDomain.indicatesSegmentStart(label.target.categoryValue)
    )
    
    labelSeq.foreach{ label => 
      if ( !labelDomain.indicatesSegmentStart(label.categoryValue) && myWords.size > 0 )
        myWords(myWords.size - 1) += label
      else
        myWords += (new ArrayBuffer[SegmentationLabel] :+ label)
    }

    val pad = new ArrayBuffer[SegmentationLabel]
    val wordZip = (pad +: myWords).zip(myWords :+ pad).slice(1, myWords.size)
    val numCorrect: Double = wordZip.count{ 
      wordPair => wordPair._1.forall( label => label.valueIsTarget ) && 
                  labelDomain.indicatesSegmentStart(wordPair._2(0).target.categoryValue)
    }
    val printer = new PrintWriter(new BufferedWriter(new FileWriter(logPath)))
    val printString: String = myWords.filter( word => 
      word.exists( label => !label.valueIsTarget ) 
    ).map( 
      word => word.map( label => label.character.string +
                                 "/" + label.categoryValue +
                                 "/" + label.target.categoryValue +
                                 "\t"
              ).reduceLeft(_+_)
    ).reduceLeft( 
      (x,y) => x + "\n" + y 
    )

    printer.write(printString)
    printer.close
    
    val precision = numCorrect / myWords.size
    val recall = numCorrect / numTrueWords

    println("Precision: " + precision + "\tRecall: " + recall)
    2 * (precision * recall)/(precision + recall)
  }

  def segment(filePath: String): IndexedSeq[SegmentationLabel] = segment(getSegmentables(new File(filePath)))
  def segment(document: Document): IndexedSeq[SegmentationLabel] = segment(getSegmentables(document))
  def segment(segmentables: IndexedSeq[Segmentable]): IndexedSeq[SegmentationLabel] = {

    val labelSeqs = segmentables.map( _.links.map( _.label ) )

    labelSeqs.foreach( labelSeq => model.maximize(labelSeq)(null) )

    labelSeqs.flatten
  }

  def getSegmentables(corpus: File): IndexedSeq[Segmentable] = {

    val labeledExamples: IndexedSeq[IndexedSeq[(String, String)]] = labelDomain.getLabeledCharacters(corpus)

    getSegmentables(labeledExamples)
  }
  def getSegmentables(document: Document): IndexedSeq[Segmentable] = {

    val labeledExamples: IndexedSeq[IndexedSeq[(String, String)]] = labelDomain.getLabeledCharacters(document)

    getSegmentables(labeledExamples)
  }
  def getSegmentables(labeledExamples: IndexedSeq[IndexedSeq[(String, String)]]): IndexedSeq[Segmentable] = {

    val segmentables = labeledExamples.map( 
      example => new Segmentable ++= (0 until example.size).map( 
        i => new Character(example(i)._1, example(i)._2, characterToFeatures(i, example))
      )
    )

    println("Segmentables Retrieved")

    segmentables
  }

  //Returns the list of features for a character in an unsegmented data set
  //Labeling scheme: PP (prev prev) P (prev) N (next) NN (next next) *L (* label)
  def characterToFeatures(i: Int, labeledCharacters: IndexedSeq[(String, String)]): Seq[String] = {

    val defaultFeature = "INVALID"
    val numChars   = labeledCharacters.size
    val cneg2label = "C-2"
    val cneg1label = "C-1"
    val c0label    = "C0"
    val cpos1label = "C+1"
    val cpos2label = "C+2"

    val cneg2 = 
      if( i - 2 >= 0 ) labeledCharacters(i-2)._1
      else defaultFeature
    val cneg1 = 
      if( i - 1 >= 0 ) labeledCharacters(i-1)._1
      else defaultFeature
    val c0 = labeledCharacters(i)._1
    val cpos1 = 
      if( i + 1 < numChars ) labeledCharacters(i+1)._1
      else defaultFeature
    val cpos2 = 
      if( i + 2 < numChars ) labeledCharacters(i+2)._1
      else defaultFeature
    val features = new ArrayBuffer[String]

    //Add unigram character identity features
    features ++= Seq(
      cneg2 + cneg2label,
      cneg1 + cneg1label,
      c0    + c0label,
      cpos1 + cpos1label
    )

    //Add bigram character identity features
    features ++= Seq(
      cneg2 + cneg1 + cneg2label + cneg1label,
      cneg1 + c0    + cneg1label + c0label,
      cneg1 + cpos1 + cneg1label + cpos1label,
      c0    + cpos1 + c0label    + cpos1label,
      c0    + cpos2 + c0label    + cpos2label
    )

    //Add feature functions including reduplication, known bigram, 
    //solitary character, prefix and affix
    features ++= List(
      (cneg1 equals c0,                           "RE" + cneg1label + c0label),
      (cneg1 equals cpos1,                        "RE" + cneg1label + cpos1label),
      (tableContains(bigramTable, cneg1+c0),      "BI" + cneg1      + c0),
      (tableContains(singleCharWordTable, cneg1), "UN" + cneg1),
      (tableContains(singleCharWordTable, c0),    "UN" + c0),
      (tableContains(singleCharWordTable, cpos1), "UN" + cpos1),
      (tableContains(prefixTable, cneg1),         "PR" + cneg1),
      (tableContains(suffixTable, c0),            "SU" + c0)
    ).filter( pair => pair._1 ).map( pair => pair._2 ).toList

    features.toList.filter( feature => !feature.contains(defaultFeature) ).toSeq
  }

  def tableContains(domain: CategoricalDomain[String], element: String): Boolean = {

    try {
      domain.getIndex(element)
      return true
    } catch {
      case _ : Throwable => return false
    }
  }
}
