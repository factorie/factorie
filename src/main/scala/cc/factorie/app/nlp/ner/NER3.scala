package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._
import java.io.File
import cc.factorie.util.{BinarySerializer, CubbieConversions}

class NER3 extends DocumentAnnotator {
  def this(filename: String) = { this(); deserialize(filename) }

  object FeaturesDomain extends CategoricalTensorDomain[String]
  class FeaturesVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeaturesDomain
    override def skipNonCategories = true
  } 
  
  // The model
  val model = new TemplateModel with Parameters {
    addTemplates(
      // Bias term on each individual label
      new DotTemplateWithStatistics1[BilouConllNerLabel] {
        override def neighborDomain1 = BilouConllNerDomain
        val weights = Weights(new la.DenseTensor1(BilouConllNerDomain.size))
      },
      // Transition factors between two successive labels
      new DotTemplateWithStatistics2[BilouConllNerLabel, BilouConllNerLabel] {
        override def neighborDomain1 = BilouConllNerDomain
        override def neighborDomain2 = BilouConllNerDomain
        val weights = Weights(new la.DenseTensor2(BilouConllNerDomain.size, BilouConllNerDomain.size))
        def unroll1(label:BilouConllNerLabel) = if (label.token.hasPrev) { assert(label.token.prev.attr[BilouConllNerLabel] ne null); Factor(label.token.prev.attr[BilouConllNerLabel], label) } else Nil
        def unroll2(label:BilouConllNerLabel) = if (label.token.hasNext) { assert(label.token.next.attr[BilouConllNerLabel] ne null); Factor(label, label.token.next.attr[BilouConllNerLabel]) } else Nil
      },
      // Factor between label and observed token
      new DotTemplateWithStatistics2[BilouConllNerLabel, FeaturesVariable] {
        override def neighborDomain1 = BilouConllNerDomain
        override def neighborDomain2 = FeaturesDomain
        val weights = Weights(new la.DenseTensor2(BilouConllNerDomain.size, FeaturesDomain.dimensionSize))
        def unroll1(label:BilouConllNerLabel) = Factor(label, label.token.attr[FeaturesVariable])
        def unroll2(token:FeaturesVariable) = throw new Error("FeaturesVariable values shouldn't change")
      }
    )
  }
  // The training objective
  val objective = new HammingTemplate[BilouConllNerLabel]

  // Methods of DocumentAnnotator
  override def tokenAnnotationString(token:Token): String = token.attr[BilouConllNerLabel].categoryValue
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouConllNerLabel])
  def process1(document:Document): Document = {
    if (document.tokenCount > 0) {
      val alreadyHadFeatures = document.hasAnnotation(classOf[FeaturesVariable])
      if (!alreadyHadFeatures) addFeatures(document)
      for (token <- document.tokens) if (token.attr[BilouConllNerLabel] eq null) token.attr += new BilouConllNerLabel(token, "O")
      for (sentence <- document.sentences if sentence.tokens.size > 0)
        BP.inferChainMax(sentence.tokens.map(_.attr[BilouConllNerLabel]).toSeq, model)
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
    def labels(docs:Iterable[Document]): Iterable[BilouConllNerLabel] = docs.flatMap(doc => doc.tokens.map(_.attr[BilouConllNerLabel]))
    val learner = new optimize.SampleRankTrainer(new GibbsSampler(model, objective), new optimize.AdaGrad)
    for (iteration <- 1 until 3) { 
      learner.processContexts(labels(trainDocs))
      trainDocs.foreach(process(_)); println("Train accuracy "+objective.accuracy(labels(trainDocs)))
      testDocs.foreach(process(_));  println("Test  accuracy "+objective.accuracy(labels(testDocs)))
    }
  }
  def train(trainFilename:String, testFilename:String): Unit = {
    // TODO Make fixLabels no longer necessary by having LoadConll2003 directly create the right label type.
    def fixLabels(docs:Iterable[Document]): Iterable[Document] = { for (doc <- docs; token <- doc.tokens) token.attr += new BilouConllNerLabel(token, token.attr[NerLabel].categoryValue); docs }
    val trainDocs = fixLabels(LoadConll2003.fromFilename(trainFilename, BILOU=true))
    val testDocs = fixLabels(LoadConll2003.fromFilename(testFilename, BILOU=true))
    (trainDocs ++ testDocs).foreach(addFeatures(_))
    train(trainDocs, testDocs)
    FeaturesDomain.freeze()
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

object NER3 {
  def main(args:Array[String]): Unit = {
    val ner = new NER3
    ner.train(args(0), args(1))
    ner.serialize(args(2))
  }
}
