package cc.factorie.app.nlp.mention
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.coref.EntityType
import cc.factorie.app.nlp.ner.OntonotesNerDomain
import cc.factorie.util.BinarySerializer
import cc.factorie.optimize._
import java.io._

// TODO EntityType should be moved here, and renamed MentionType (since it has a "val mention" among its arguments). -akm

/** Cheap number predictor based on rules and lexicons.  Really this should use a real morphological analyzer. */
class MentionTypeLabeler extends DocumentAnnotator {
  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = this(url.openConnection.getInputStream)
  
  object FeatureDomain extends CategoricalTensorDomain[String]
  class FeatureVariable extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories: Boolean = domain.dimensionDomain.frozen
  }
  lazy val model = new LinearMultiClassClassifier(OntonotesNerDomain.size, FeatureDomain.dimensionSize)
  
  def features(mention:Mention): FeatureVariable = {
    val features = new FeatureVariable
    val words = mention.span.tokens.map(token => cc.factorie.app.strings.collapseDigits(token.string))
    features ++= words
    features += "HEAD="+mention.headToken.string
    features += "LAST="+words.last
    features += "FIRST="+words.last
    mention.span.tokens.head.prevWindow(3).foreach(token => features += "PREV="+token.string)
    mention.span.tokens.head.nextWindow(3).foreach(token => features += "NEXT="+token.string)
    for (lexicon <- Seq(lexicon.iesl.PersonFirst, lexicon.iesl.PersonLast, lexicon.iesl.AllPlaces, lexicon.iesl.Company)) {
      if (lexicon.contains(mention.span)) features += "LEX="+lexicon.name
      if (lexicon.containsWord(mention.headToken.string)) features += "HEADLEX="+lexicon.name
    }
    // TODO Add more features
    features
  }
  
  def process1(document:Document): Document = {
    import MentionNumberDomain._
    for (mention <- document.attr[MentionList])
      mention.attr.getOrElseUpdate(new EntityType(mention, "O")) := model.classification(features(mention).value).bestLabelIndex
    document
  }
  override def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[MentionList].filter(_.span.contains(token)); mentions.map(_.attr[EntityType].categoryValue).mkString(",") }
  override def mentionAnnotationString(mention:Mention): String = { val t = mention.attr[EntityType]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[EntityType])
 
  def filterMentions(mentions:Seq[Mention]): Iterable[Mention] = mentions.groupBy(m => m.attr[Entity]).filter(x => x._2.length > 1).map(x => x._2).flatten

  def train(trainDocs:Iterable[Document], testDocs:Iterable[Document]): Unit = {
    implicit val random = new scala.util.Random(0)
    val trainMentions = trainDocs.flatMap(_.attr[MentionList])
    FeatureDomain.dimensionDomain.gatherCounts = true
    trainMentions.foreach(features(_))
    FeatureDomain.dimensionDomain.trimBelowCount(3)
    //val entityMentions = allMentions.groupBy(m => m.attr[Entity]).filter(x => x._2.length > 1).map(x => x._2).flatten
    val examples = for (doc <- trainDocs; mention <- filterMentions(doc.attr[MentionList])) yield
      new LinearMultiClassExample(model.weights, features(mention).value, mention.attr[EntityType].intValue, LinearObjectives.hingeMultiClass)
    val testMentions = testDocs.flatMap(doc => filterMentions(doc.attr[MentionList]))
    println("Training ")
    def evaluate(): Unit = {
      println("TRAIN\n"+(new cc.factorie.app.classify.Trial[EntityType,la.Tensor1](model, OntonotesNerDomain, (t:EntityType) => features(t.mention).value) ++= trainMentions.map(_.attr[EntityType])).toString)
      println("\nTEST\n"+(new cc.factorie.app.classify.Trial[EntityType,la.Tensor1](model, OntonotesNerDomain, (t:EntityType) => features(t.mention).value) ++= testMentions.map(_.attr[EntityType])).toString)
    }
    Trainer.onlineTrain(model.parameters, examples.toSeq, maxIterations=3, evaluate = evaluate)
  }

  // Serialization
  def serialize(filename: String): Unit = {
    val file = new File(filename); if (file.getParentFile ne null) file.getParentFile.mkdirs()
    serialize(new java.io.FileOutputStream(file))
  }
  def deserialize(file: File): Unit = {
    require(file.exists(), "Trying to load non-existent file: '" +file)
    deserialize(new java.io.FileInputStream(file))
  }
  def serialize(stream: java.io.OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val sparseEvidenceWeights = new la.DenseLayeredTensor2(model.weights.value.dim1, model.weights.value.dim2, new la.SparseIndexedTensor1(_))
    model.weights.value.foreachElement((i, v) => if (v != 0.0) sparseEvidenceWeights += (i, v))
    model.weights.set(sparseEvidenceWeights)
    val dstream = new java.io.DataOutputStream(stream)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val dstream = new java.io.DataInputStream(stream)
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, dstream)
    model.weights.set(new la.DenseLayeredTensor2(PTBPosDomain.size, FeatureDomain.dimensionDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }

}

object MentionTypeLabeler extends MentionTypeLabeler(cc.factorie.util.ClasspathURL[MentionTypeLabeler](".factorie"))

object MentionTypeLabelerTrainer {
  def main(args:Array[String]): Unit = {
    println("usage: trainfile [modelfile]")
    var trainDocs = coref.ConllCorefLoader.loadWithParse(args(0), loadSingletons=false, disperseEntityTypes=true)
    val testDocs = trainDocs.takeRight(20)
    val labeler = new MentionTypeLabeler
    for (mention <- labeler.filterMentions(testDocs.flatMap(_.attr[MentionList])))
      println("%20s  %s".format(mention.attr[EntityType].target.categoryValue, mention.span.phrase))
    trainDocs = trainDocs.dropRight(20)
    labeler.train(trainDocs, testDocs)
    for (mention <- labeler.filterMentions(testDocs.flatMap(_.attr[MentionList])))
      println("%20s %-20s  %s".format(mention.attr[EntityType].target.categoryValue, mention.attr[EntityType].categoryValue, mention.span.phrase))

    if (args.length > 1) labeler.serialize(args(1))
    
  }
}
