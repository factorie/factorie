package cc.factorie.example

object SimpleChainNER {

  // The data
	class Token(val word:String, features:Seq[String], labelString:String) extends BinaryVectorVariable[String] with VarInSeq[Token] {
		val label: Label = new Label(labelString, this)
  	this ++= features
	}
	class Label(labelname: String, val token: Token) extends LabelVariable(labelname) {
	  def hasNext = token.hasNext && token.next.label != null
	  def hasPrev = token.hasPrev && token.prev.label != null
	  def next = token.next.label
	  def prev = token.prev.label
	}
  class Sentence extends VariableSeq[Token]
  
  // The model and objective
  val model = new Model
  // Bias term just on labels 
  model += new TemplateWithDotStatistics1[Label] 
  // Transition factors
  model += new TemplateWithDotStatistics2[Label, Label] {
		def unroll1(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label) else Nil
		def unroll2(label: Label) = if (label.hasNext) Factor(label, label.token.next.label) else Nil
	}
  // Factor between label and observed token
	model += new TemplateWithDotStatistics2[Label, Token] {
		def unroll1(label: Label) = Factor(label, label.token)
		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
	}
  // 	Factor between label, its token and the previous Label
  val unusedTemplate = new TemplateWithDotStatistics3[Label, Label, Token] {
		def unroll1(label:Label) = if (label.hasNext) Factor(label, label.next, label.token.next) else Nil
		def unroll2(label:Label) = if (label.hasPrev) Factor(label.prev, label, label.token) else Nil
		def unroll3(token:Token) = throw new Error("Token values shouldn't change")
	}
  val objective = new Model
  objective += new TrueLabelTemplate[Label]
  
  // Feature extraction
  def wordToFeatures(word:String, initialFeatures:String*) : Seq[String] = {
    import scala.collection.mutable.ArrayBuffer
    val f = new ArrayBuffer[String]
    f += "W="+word
    f ++= initialFeatures
  	if (word.length > 3) f += "PRE="+word.substring(0,3)
  	if (Capitalized.findFirstMatchIn(word) != None) f += "CAPITALIZED"
  	if (Numeric.findFirstMatchIn(word) != None) f += "NUMERIC"
  	if (Punctuation.findFirstMatchIn(word) != None) f += "PUNCTUATION"
  	f
  }
  val Capitalized = "^[A-Z].*".r
	val Numeric = "^[0-9]+$".r
	val Punctuation = "[,\\.;:?!()]+".r

	// Read data, train and test
	def main(args: Array[String]) : Unit = {
  	// Read in the data
  	var dataDir = "/Users/mwick/data/conll/"
  	//var dataDir = "/Users/mccallum/research/data/ie/ner2003/"
  	var trainFilename = dataDir+"eng.train"
  	var testFilename = dataDir+"eng.testa" 
  	if (args.length == 2) {
  		trainFilename = args(0)
  		testFilename = args(1)
  	}
  	val trainSentences = load(trainFilename)
  	val testSentences = load(testFilename)
  	val allTokens : Seq[Token] = trainSentences.flatMap(x=>x) ++ testSentences.flatMap(x=>x)
  	// Add features from next and previous tokens 
  	allTokens.foreach(t => {
  		if (t.hasPrev) t ++= t.prev.values.filter(!_.contains('@')).map(_+"@-1")
  		if (t.hasNext) t ++= t.next.values.filter(!_.contains('@')).map(_+"@+1")
  	})
  	// Get the variables to be inferred; prune the data so that it can finish in just ~2 minutes
  	val trainLabels : Seq[Label] = trainSentences.flatMap(_.map(_.label)).take(20000)
  	val testLabels : Seq[Label] = testSentences.flatMap(_.map(_.label)).take(10000)
  	// Sample and Learn!
  	(trainLabels ++ testLabels).foreach(_.setRandomly)
  	val learner = new GibbsSampler(model, objective)
	  with SampleRank
	  with MIRAUpdates //with ConfidenceWeightedUpdates //with MIRAUpdates
	  with ParameterAveraging //comment this to disable param averaging
	  {temperature=0.01}
  	//with FactorQueue[Variable with IterableSettings] { def process0(x:AnyRef):DiffList = x match { case l:Label => process(l); case _ => null} }
  	val predictor = new GibbsSampler(model){temperature=0.01}
  	for (i <- 0 until 6) {
  	  println("Iteration "+(i+1)+"...") 
  		trainLabels.take(50).foreach(printLabel _); println; println
  		printDiagnostic(trainLabels.take(400))
  		println ("Train accuracy = "+ objective.aveScore(trainLabels))
  		println ("Test  accuracy = "+ objective.aveScore(testLabels))
  		learner.process(trainLabels, 1)
  		learner.learningRate *= 0.9
  		predictor.temperature *= 0.9
//	  System.out.println("NUM UPS: " + learner.numUpdates)
		if(learner.isInstanceOf[ParameterAveraging]) //not quite right, we need to set weights back to learn again...
		  learner.asInstanceOf[ParameterAveraging].setWeightsToAverage
  		predictor.process(testLabels, 1)
  	}
  	0
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
 
  def load(filename:String) : Seq[Sentence] = {
  	import scala.io.Source
  	import scala.collection.mutable.ArrayBuffer
  	var wordCount = 0
    var sentences = new ArrayBuffer[Sentence]
    val source = Source.fromFile(filename)
    var sentence = new Sentence
    for (line <- source.getLines) {
      if (line.length < 2) { // Sentence boundary
        sentences += sentence //; println("num words " + document.size + " num docs "+documents.size)
        sentence = new Sentence
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val pos = fields(1)
        val label = fields(3).stripLineEnd
        sentence += new Token(word, wordToFeatures(word,pos), label)
        wordCount += 1
      }
    }
    println("Loaded "+sentences.length+" sentences with "+wordCount+" words total from file "+filename)
    sentences
  }

}


