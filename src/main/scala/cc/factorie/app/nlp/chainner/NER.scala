package cc.factorie.app.nlp.chainner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File

object NerDomain extends CategoricalDomain[String]
class NerLabel(val token:Token, initialValue:String) extends LabelVariable(initialValue) { def domain = NerDomain } 

object TokenFeatureDomain extends CategoricalVectorDomain[String]
class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = TokenFeatureDomain }  

object NerModel extends TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[NerLabel], 
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[NerLabel, NerLabel] {
      def unroll1(label: NerLabel) = if (label.token.sentenceHasPrev) Factor(label.token.sentencePrev.attr[NerLabel], label) else Nil
      def unroll2(label: NerLabel) = if (label.token.sentenceHasNext) Factor(label, label.token.sentenceNext.attr[NerLabel]) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[NerLabel,TokenFeatures] {
      def unroll1(label: NerLabel) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf: TokenFeatures) = Factor(tf.token.attr[NerLabel], tf)
    }
  )

object NerObjective extends TemplateModel(new ZeroOneLossTemplate[NerLabel])


object NER {
  
  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile.")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainSentences.flatten.map(_.attr[NerLabel]).take(10000)
    val testLabels = testSentences.flatten.map(_.attr[NerLabel]).take(2000)
    
    def printAccuracy: Unit = {
      println ("Train accuracy = "+ NerObjective.aveScore(trainLabels))
      println ("Test  accuracy = "+ NerObjective.aveScore(testLabels))
    }

    // Train for 5 iterations
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    printAccuracy
    val learner = new VariableSettingsSampler[NerLabel](NerModel, NerObjective) with SampleRank with GradientAscentUpdates
    val predictor = new VariableSettingsSampler[NerLabel](NerModel)
    for (i <- 1 until 5) {
      learner.processAll(trainLabels)
      predictor.processAll(testLabels)
      printAccuracy
    }

    // Predict, also by sampling, visiting each variable 3 times.
    //predictor.processAll(testLabels, 3)
    for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    
    // Evaluate
    println("FINAL")
    printAccuracy
  }

  
  def load(filename:String) : Seq[Document] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    val Capitalized = "^[A-Z].*".r
    val Numeric = "^[0-9]+$".r
    val Punctuation = "[-,\\.;:?!()]+".r

    val documents = new ArrayBuffer[Document]
    var document = new Document("CoNLL2003-"+documents.length, "")
    documents += document
    val source = Source.fromFile(new File(filename))
    var sentence = new Sentence(document)(null)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        //sentence.stringLength = document.stringLength - sentence.stringStart
        //document += sentence
        document.appendString("\n")
        sentence = new Sentence(document)(null)
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
        document = new Document("CoNLL2003-"+documents.length, "")
        documents += document
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = fields(1)
        val labelString = fields(3).stripLineEnd
        document.appendString(" ")
        val token = new Token(sentence, word)
        if (false && document.stringLength < 100) {
          println("word=%s documentlen=>%s<".format(word, document.string))
          println("token start,end %d,%d".format(token.stringStart, token.stringLength))
          println("token=%s".format(token.string))
          println()
        }
        token.attr += new NerLabel(token, labelString)
        token.attr += new TokenFeatures(token)
        //val label = new NerLabel[Token](labelString, word)
        // Add features to Token
        token.attr[TokenFeatures] += "W="+word
        token.attr[TokenFeatures] += "SUFFIX3="+word.takeRight(3)
        token.attr[TokenFeatures] += "PREFIX3="+word.take(3)
        token.attr[TokenFeatures] += "POS="+partOfSpeech
        if (Capitalized.findFirstMatchIn(word) != None) token.attr[TokenFeatures] += "CAPITALIZED"
        if (Numeric.findFirstMatchIn(word) != None) token.attr[TokenFeatures] += "NUMERIC"
        if (Punctuation.findFirstMatchIn(word) != None) token.attr[TokenFeatures] += "PUNCTUATION"
      }
    }
    //sentence.stringLength = document.stringLength - sentence.stringStart
    println("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.length).sum+" tokens total from file "+filename)
    documents
  }
}