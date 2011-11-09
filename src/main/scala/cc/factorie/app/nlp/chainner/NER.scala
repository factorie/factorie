package cc.factorie.app.nlp.chainner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File

object NerDomain extends CategoricalDomain[String]
class NerLabel[T<:Token](val token:T, initialValue:String) extends LabelVariable(initialValue) { def domain = NerDomain } 
trait NerToken extends Token {
  val nerLabel = new NerLabel[ThisType](this, "O")
}

object PosDomain extends CategoricalDomain[String]
class PosLabel[T<:Token](val token:T, initialValue:String) extends LabelVariable(initialValue) { def domain = PosDomain } 
trait PosToken extends Token {
  val posLabel = new PosLabel[ThisType](this, PosDomain.getCategory(0))
}


object NER {
  // The variable classes
  object TokenFeatureDomain extends CategoricalVectorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = TokenFeatureDomain }  
  class Token(sentence:Sentence, s:Int, l:Int) extends cc.factorie.app.nlp.Token(s, l) with NerToken with InChain[Token,Sentence] {
    sentence += this
    //val nerFeatures = new TokenFeatures(this)
  }
  class Sentence(doc:Document, start:Int, len:Int) extends cc.factorie.app.nlp.Sentence(start, len) with ChainInChain[Sentence,Token,Document] {
    doc += this
  }
  class Document(name:String) extends cc.factorie.app.nlp.Document(name) with Chain[Document,Sentence]
  
  // The model
  val model = new TemplateModel(
    // Bias term on each individual label 
    new TemplateWithDotStatistics1[NerLabel[Token]], 
    // Transition factors between two successive labels
    new TemplateWithDotStatistics2[NerLabel[Token], NerLabel[Token]] {
      def unroll1(label: NerLabel[Token]) = if (label.token.hasPrev) Factor(label.token.prev.nerLabel, label) else Nil
      def unroll2(label: NerLabel[Token]) = if (label.token.hasNext) Factor(label, label.token.next.nerLabel) else Nil
    },
    // Factor between label and observed token
    new TemplateWithDotStatistics2[NerLabel[Token],TokenFeatures] {
      def unroll1(label: NerLabel[Token]) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf: TokenFeatures) = throw new Error("Token values shouldn't change")
    }
  )
  
  // The training objective
  val objective = new TemplateModel(new ZeroOneLossTemplate[NerLabel[Token]])
  

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: ChainNER2 trainfile testfile.")

    // Read in the data
    val trainSentences = load(args(0))
    val testSentences = load(args(1))

    // Get the variables to be inferred (for now, just operate on a subset)
    val trainLabels = trainSentences.flatten.map((t:Token) => t.attr[NerLabel[Token]]).take(10000)
    val testLabels = testSentences.flatten.map(_.attr[NerLabel[Token]]).take(2000)
    
    def printAccuracy: Unit = {
      println ("Train accuracy = "+ objective.aveScore(trainLabels))
      println ("Test  accuracy = "+ objective.aveScore(testLabels))
    }

    println("Human Labeling")
    printAccuracy

    {
      val v = trainLabels.head
      println(v.categoryValue)
      println(v.target.categoryValue)
      println(model.factors(Seq(v)))
      println("Score="+objective.score(Seq(v)))
      println("Avescore="+objective.aveScore(Seq(v)))
      println("Score="+objective.score(trainLabels.take(20)))
      println("Avescore="+objective.aveScore(trainLabels.take(20)))
      for (x <- trainLabels.take(10))
        println("word=%s current=%s target=%s score=%f correct=%s".format(x.token.string, x.categoryValue, x.target.categoryValue, objective.score(Seq(x)), if (x.valueIsTarget) "true" else "false"))
      val n = 15
      println("Factors("+n+")="+objective.factors(trainLabels.take(n)).size)
      println(trainLabels.take(n).map(_.token.string).mkString("\n"))
      println()
      println("Factors("+n+")=\n"+objective.factors(trainLabels.take(n)).sortBy(f => f.variables.head.asInstanceOf[NerLabel[Token]].token.stringStart).map(f => f.variables.head.asInstanceOf[NerLabel[Token]].token.string).mkString("\n"))
      println()
      val l1 = trainLabels.take(n).last
      val ln = trainLabels(0)
      println(l1+" "+System.identityHashCode(l1))
      println(ln+" "+System.identityHashCode(ln))
      val f1 = objective.factors(Seq(l1)).head
      val fn = objective.factors(Seq(ln)).head
      println(f1)
      println(fn)
      println(l1 == ln)
      println(f1 == fn)
    }
    //System.exit(0)
    
    // Train for 5 iterations
    (trainLabels ++ testLabels).foreach(_.setRandomly())
    printAccuracy
    val learner = new VariableSettingsSampler[NerLabel[Token]](model, objective) with SampleRank with GradientAscentUpdates
    val predictor = new VariableSettingsSampler[NerLabel[Token]](model)
    for (i <- 1 until 5) {
      learner.processAll(trainLabels)
      predictor.processAll(testLabels)
      printAccuracy
    }

    // Predict, also by sampling, visiting each variable 3 times.
    //predictor.processAll(testLabels, 3)
    for (i <- 0 until 3; label <- testLabels) predictor.process(label)
    
    // Evaluate
    printAccuracy
  }

  
  def load(filename:String) : Seq[Sentence] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    val Capitalized = "^[A-Z].*".r
    val Numeric = "^[0-9]+$".r
    val Punctuation = "[-,\\.;:?!()]+".r

    var wordCount = 0
    var document = new Document("CoNLL2003")
    val source = Source.fromFile(new File(filename))
    var sentence = new Sentence(document, 0, 0)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        sentence.stringLength = document.stringLength - sentence.stringStart
        //document += sentence
        document.appendString("\n")
        sentence = new Sentence(document, document.stringLength, document.stringLength) 
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = fields(1)
        val labelString = fields(3).stripLineEnd
        document.appendString(" ")
        val token = new Token(sentence, document.stringLength, word.length)
        document.appendString(word)
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
        wordCount += 1
      }
    }
    sentence.stringLength = document.stringLength - sentence.stringStart
    println("Loaded "+document.length+" sentences with "+wordCount+" words total from file "+filename)
    document
  }
}