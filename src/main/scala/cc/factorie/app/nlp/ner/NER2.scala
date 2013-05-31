package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.util.{BinarySerializer, CubbieConversions}

/** A simple named entity recognizer, trained on Ontonotes data.
    It does not have sufficient features to be state-of-the-art. */
class NER2 extends DocumentAnnotator {
  def this(filename: String) = { this(); deserialize(filename) }

  object FeaturesDomain extends CategoricalTensorDomain[String]
  class FeaturesVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = true
  } 
  
  // The model
  val model = new TemplateModel with Parameters {
    // Bias term on each individual label
    this += new DotTemplateWithStatistics1[BilouOntonotesNerLabel] {
      override def neighborDomain1 = BilouOntonotesNerDomain
      val weights = Weights(new la.DenseTensor1(BilouOntonotesNerDomain.size))
    }
    // Transition factors between two successive labels
    this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel, BilouOntonotesNerLabel] {
      override def neighborDomain1 = BilouOntonotesNerDomain
      override def neighborDomain2 = BilouOntonotesNerDomain
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, BilouOntonotesNerDomain.size))
      def unroll1(label:BilouOntonotesNerLabel) = if (label.token.hasPrev) { assert(label.token.prev.attr[BilouOntonotesNerLabel] ne null); Factor(label.token.prev.attr[BilouOntonotesNerLabel], label) } else Nil
      def unroll2(label:BilouOntonotesNerLabel) = if (label.token.hasNext) { assert(label.token.next.attr[BilouOntonotesNerLabel] ne null); Factor(label, label.token.next.attr[BilouOntonotesNerLabel]) } else Nil
    }
    // Factor between label and observed token
    this += new DotTemplateWithStatistics2[BilouOntonotesNerLabel, FeaturesVariable] {
      override def neighborDomain1 = BilouOntonotesNerDomain
      override def neighborDomain2 = FeaturesDomain
      val weights = Weights(new la.DenseTensor2(BilouOntonotesNerDomain.size, FeaturesDomain.dimensionSize))
      def unroll1(label:BilouOntonotesNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
      def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't change")
    }
  }
  // The training objective
  val objective = new HammingTemplate[BilouOntonotesNerLabel]

  // Methods of DocumentAnnotator
  override def tokenAnnotationString(token:Token): String = token.attr[BilouOntonotesNerLabel].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouOntonotesNerLabel])
  def process1(document:Document): Document = {
    if (document.tokenCount > 0) {
      val alreadyHadFeatures = document.hasAnnotation(classOf[FeaturesVariable])
      if (!alreadyHadFeatures) addFeatures(document)
      for (token <- document.tokens) if (token.attr[BilouOntonotesNerLabel] eq null) token.attr += new BilouOntonotesNerLabel(token, "O")
      for (sentence <- document.sentences if sentence.tokens.size > 0)
        BP.inferChainMax(sentence.tokens.map(_.attr[BilouOntonotesNerLabel]).toSeq, model)
      if (!alreadyHadFeatures) { document.annotators.remove(classOf[FeaturesVariable]); for (token <- document.tokens) token.attr.remove[FeaturesVariable] }
    }
    document
  }
  
  // Feature creation
  def addFeatures(document:Document): Unit = {
    document.annotators(classOf[FeaturesVariable]) = this
    import cc.factorie.app.strings.simplifyDigits
    for (token <- document.tokens) {
      val features = new FeaturesVariable(token)
      token.attr += features
      val rawWord = token.string
      val word = simplifyDigits(rawWord).toLowerCase
      features += "W="+word
      features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
      if (token.isPunctuation) features += "PUNCTUATION"
    }
    for (section <- document.sections)
      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(section.tokens, (t:Token)=>t.attr[FeaturesVariable], Seq(0), Seq(-1), Seq(-2), Seq(1), Seq(2), Seq(0,0))
  }
  
  // Parameter estimation
  def train(trainDocs:Iterable[Document], testDocs:Iterable[Document]): Unit = {
    def labels(docs:Iterable[Document]): Iterable[BilouOntonotesNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouOntonotesNerLabel]))
    val learner = new optimize.SampleRankTrainer(new GibbsSampler(model, objective), new optimize.AdaGrad)
    for (iteration <- 1 until 3) { 
      learner.processContexts(labels(trainDocs))
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(labels(testDocs)))
    }
  }
  def train(trainFilename:String, testFilename:String): Unit = {
    val trainDocs = LoadOntonotes5.fromFilename(trainFilename, nerBilou=true)
    val testDocs = LoadOntonotes5.fromFilename(testFilename, nerBilou=true)
    for (token <- trainDocs.head.tokens.drop(100).take(100))
      println("%20s  %10s %10s".format(token.string, token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue))
    (trainDocs ++ testDocs).foreach(addFeatures(_))
    train(trainDocs, testDocs)
    FeaturesDomain.freeze()
    for (token <- trainDocs.head.tokens.drop(100).take(100))
      println("%20s  %10s %10s".format(token.string, token.attr[BilouOntonotesNerLabel].target.categoryValue, token.attr[BilouOntonotesNerLabel].categoryValue))
    //println(trainDocs.last.owplString(List(_.attr[BilouOntonotesNerLabel].target.categoryValue, _.attr[BilouOntonotesNerLabel].categoryValue)))
  }
  
  // Serialization
  def serialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename); if (file.getParentFile eq null) file.getParentFile.mkdirs()
    BinarySerializer.serialize(FeaturesDomain.dimensionDomain, model, file)
  }
  def deserialize(filename: String) {
    import CubbieConversions._
    val file = new File(filename)
    assert(file.exists(), "Trying to load non-existent file: '" +file)
    BinarySerializer.deserialize(FeaturesDomain.dimensionDomain, model, file)
  }
}

object NER2 {
  def main(args:Array[String]): Unit = {
    val ner = new NER2
    ner.train(args(0), args(1))
    ner.serialize(args(2))
  }
}
