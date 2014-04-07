package cc.factorie.app.nlp.segment

import scala.collection.mutable.{Map, ArrayBuffer}
import java.io._
import cc.factorie._
import cc.factorie.util.BinarySerializer
import cc.factorie.util.CubbieConversions._
import cc.factorie.variable._
import cc.factorie.app.nlp._
import cc.factorie.optimize.{LikelihoodExample, BatchTrainer, OnlineTrainer}
import cc.factorie.app.chain.ChainModel

/** A linear-chain CRF model for Chinese word segmentation with four companion 
    objects, each pre-trained on a different corpus that corresponds to a 
    different variety of written Mandarin. 
    @author Henry Oskar Singer  */

//Microsoft-Research-Asia-corpus-trained model (Beijing)
class MSRChainChineseWordSegmenter(url: java.net.URL)
  extends ChainChineseWordSegmenter(url)
object MSRChainChineseWordSegmenter
  extends MSRChainChineseWordSegmenter(cc.factorie.util.ClasspathURL[MSRChainChineseWordSegmenter](".factorie"))

//City-University-of-Hong-Kong-corpus-trained model (Hong Kong)
class CUHChainChineseWordSegmenter(url: java.net.URL)
  extends ChainChineseWordSegmenter(url)
object CUHChainChineseWordSegmenter
  extends CUHChainChineseWordSegmenter(cc.factorie.util.ClasspathURL[CUHChainChineseWordSegmenter](".factorie"))

class ChainChineseWordSegmenter(
  labelDomain: SegmentationLabelDomain = BIOSegmentationDomain
) extends DocumentAnnotator {

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

      if( i == 0 || labelDomain.isEndOfSentence(segmentedText(i - 1).character.string) )
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

    BinarySerializer.serialize(SegmentationFeaturesDomain.dimensionDomain, dataStream)
    BinarySerializer.serialize(model, dataStream)
    dataStream.close
  }

  def deserialize(filePath: String): Unit = deserialize(new FileInputStream(new File(filePath)))
  def deserialize(stream: InputStream): Unit = {
    
    val dataStream = new DataInputStream(new BufferedInputStream(stream))

    BinarySerializer.deserialize(SegmentationFeaturesDomain.dimensionDomain, dataStream)
    BinarySerializer.deserialize(model, dataStream)
    dataStream.close
  }

  def train(filePath: String): Unit = {

    println("Training In Progress")
    println("\tFeature Extraction In Progress")

    val trainingSegmentables = getSegmentables(new File(filePath))
    SegmentationFeaturesDomain.freeze
    
    println("\tFeature Extraction Completed")

    val examples = 
      trainingSegmentables.map( segmentable =>
        new model.ChainLikelihoodExample(segmentable.links.map( _.label ))
      ).toSeq
    val trainer = new OnlineTrainer(model.parameters)

    trainer.trainFromExamples(examples)

    println("Training Complete\n")
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

  def getF1Score(filePath: String): Double = {

    val labelSeq = segment(filePath)
    val myWords = new ArrayBuffer[ArrayBuffer[SegmentationLabel]]()
    val numTrueWords: Double = labelSeq.count( label => labelDomain.indicatesSegmentStart(label.target.categoryValue) )
    
    labelSeq.foreach{ label => 
      if(!labelDomain.indicatesSegmentStart(label.categoryValue) && myWords.size > 0) 
        myWords(myWords.size - 1) += label
      else
        myWords += (new ArrayBuffer[SegmentationLabel] :+ label)
    }
    
    val numCorrect: Double = myWords.count( word => word.forall( label => label.valueIsTarget ) )
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

    val labeledCharacters: IndexedSeq[(String, String)] = labelDomain.getLabeledCharacters(corpus)

    getSegmentables(labeledCharacters)
  }
  def getSegmentables(document: Document): IndexedSeq[Segmentable] = {

    val labeledCharacters: IndexedSeq[(String, String)] = labelDomain.getLabeledCharacters(document)

    getSegmentables(labeledCharacters)
  }
  def getSegmentables(labeledCharacters: IndexedSeq[(String, String)]): IndexedSeq[Segmentable] = {

    val numChars = labeledCharacters.size
    val printNum = numChars/100
    val segmentables = new ArrayBuffer[Segmentable]
    var characterBuffer = new ArrayBuffer[(String, String)]
    var currentSegmentable = new Segmentable

    ( 0 until numChars ).foreach{ i =>
      val currentChar = labeledCharacters(i)

      characterBuffer += currentChar

      if(currentChar._2 equals "P"){
        currentSegmentable ++= ( 0 until characterBuffer.size ).map( j =>
            new Character(characterBuffer(j)._1, 
                          characterBuffer(j)._2, 
                          characterToFeatures(j, characterBuffer)
                         )
        )
        segmentables += currentSegmentable

        currentSegmentable = new Segmentable
        characterBuffer = new ArrayBuffer[(String, String)]
      }
    }
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
                   cpos1 + cpos1label, 
                   cpos2 + cpos2label
                 )

    //Add bigram character identity features
    features ++= Seq(
                   cneg2 + cneg1 + cneg2label + cneg1label,
                   cneg2 + c0    + cneg2label + c0label,
                   cneg2 + cpos1 + cneg2label + cpos1label,
                   cneg2 + cpos2 + cneg2label + cpos2label,
                   cneg1 + c0    + cneg1label + c0label,
                   cneg1 + cpos1 + cneg1label + cpos1label,
                   cneg1 + cpos2 + cneg1label + cpos2label,
                   c0    + cpos1 + c0label    + cpos1label,
                   c0    + cpos2 + c0label    + cpos2label,
                   cpos1 + cpos2 + cpos1label + cpos2label
                 )

    //Add reduplication features
    features ++= Seq(
                   if(cneg2 equals cneg1) "R" + cneg2label + cneg1label else defaultFeature,
                   if(cneg2 equals c0)    "R" + cneg2label + c0label    else defaultFeature,
                   if(cneg1 equals c0)    "R" + cneg1label + c0label    else defaultFeature,
                   if(cneg1 equals cpos1) "R" + cneg1label + cpos1label else defaultFeature,
                   if(c0 equals cpos1)    "R" + c0label    + cpos1label else defaultFeature,
                   if(c0 equals cpos2)    "R" + c0label    + cpos2label else defaultFeature,
                   if(cpos1 equals cpos2) "R" + cpos1label + cpos2label else defaultFeature
                 )

    features.toList.filter( feature => !feature.contains(defaultFeature) ).toSeq
  }
}
