package cc.factorie.app.nlp.mention
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.ner.OntonotesNerDomain
import cc.factorie.util.BinarySerializer
import java.io._
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.optimize.{PredictorExample, Trainer, OptimizableObjectives}
import cc.factorie.app.classify.backend.LinearMulticlassClassifier

//'Entity Type' is a misnomer that is used elsewhere in the literature, use it too. Really, this is a type associated with a mention, not an entity


object MentionEntityTypeDomain extends CategoricalDomain[String]{
  this ++= OntonotesNerDomain.categories
  this += "MISC"
}
class MentionEntityType(val mention:Mention, targetValue:String) extends LabeledCategoricalVariable(targetValue) {
    def domain =  MentionEntityTypeDomain
}

class MentionEntityTypeLabeler extends DocumentAnnotator {
  def this(stream:InputStream) = { this(); deserialize(stream) }
  def this(file: File) = this(new FileInputStream(file))
  def this(url:java.net.URL) = this(url.openConnection.getInputStream)
  
  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories: Boolean = domain.dimensionDomain.frozen
  }
  lazy val model = new LinearMulticlassClassifier(MentionEntityTypeDomain.size, FeatureDomain.dimensionSize)
  
  def features(mention:Mention): FeatureVariable = {
    val features = new FeatureVariable
    var tokens = mention.tokens.toSeq
    if (tokens.head.string == "the") tokens = tokens.drop(1)
    if (tokens.length > 0 && tokens.last.string == "'s") tokens = tokens.dropRight(1)
    if (tokens.length == 0) return features // TODO Complain further here? 
    val words = tokens.map(token => cc.factorie.app.strings.collapseDigits(token.string))
    features ++= words
    features += "HEAD="+mention.headToken.string
    features += "LAST="+words.last
    features += "FIRST="+words.head
    mention.head.prevWindow(3).foreach(token => features += "PREV="+token.string)
    mention.last.nextWindow(3).foreach(token => features += "NEXT="+token.string)
    for (lexicon <- lexicons) {
      if (lexicon.contains(tokens)) features += "LEX="+lexicon.name
      if (lexicon.containsWord(mention.headToken.string)) features += "HEADLEX="+lexicon.name
    }
    // TODO Add more features
    features
  }
  val lexicons = Seq(
      lexicon.iesl.PersonFirst,
      lexicon.iesl.PersonLast,
      lexicon.iesl.Month,
      lexicon.iesl.PersonHonorific,
      lexicon.iesl.Company,
      lexicon.iesl.Country,
      lexicon.iesl.City,
      lexicon.iesl.AllPlaces,
      lexicon.iesl.USState,
      lexicon.wikipedia.Person,
      lexicon.wikipedia.Event,
      lexicon.wikipedia.Location,
      lexicon.wikipedia.Organization,
      lexicon.wikipedia.ManMadeThing,
      lexicon.wikipedia.Event)
  
  val PersonLexicon = new lexicon.UnionLexicon("MentionEntityTypePerson", lexicon.PersonPronoun, lexicon.PosessiveDeterminer)
  def isWordNetPerson(token:Token): Boolean = wordnet.WordNet.isHypernymOf("person", wordnet.WordNet.lemma(token.string, "NN"))
  def entityTypeIndex(mention:Mention): Int = {
    if (PersonLexicon.contains(mention) || isWordNetPerson(mention.headToken)) MentionEntityTypeDomain.index("PERSON")
    else model.classification(features(mention).value).bestLabelIndex
  }
  def processMention(mention: Mention): Unit = {
    val label = mention.attr.getOrElseUpdate(new MentionEntityType(mention, "O"))
    label.set(entityTypeIndex(mention))(null)
  }
  def process(document:Document): Document = {
    for (mention <- document.attr[MentionList]) processMention(mention)
    document
  }

  override def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[MentionList].filter(_.contains(token)); mentions.map(_.attr[MentionEntityType].categoryValue).mkString(",") }
  override def mentionAnnotationString(mention:Mention): String = { val t = mention.attr[MentionEntityType]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[MentionEntityType])
 
  def filterTrainingMentions(mentions:Seq[Mention]): Iterable[Mention] = 
    mentions.groupBy(m => m.attr[Entity]).filter(x => x._2.length > 1).map(x => x._2).flatten.filter(mention => !PersonLexicon.contains(mention))

  def train(trainDocs:Iterable[Document], testDocs:Iterable[Document]): Unit = {
    implicit val random = new scala.util.Random(0)
    val trainMentions = trainDocs.flatMap(_.attr[MentionList])
    FeatureDomain.dimensionDomain.gatherCounts = true
    trainMentions.foreach(features(_))
    FeatureDomain.dimensionDomain.trimBelowCount(3)
    val examples = for (doc <- trainDocs; mention <- filterTrainingMentions(doc.attr[MentionList])) yield
      new PredictorExample(model, features(mention).value, mention.attr[MentionEntityType].intValue, OptimizableObjectives.hingeMulticlass)
    val testMentions = testDocs.flatMap(doc => filterTrainingMentions(doc.attr[MentionList]))
    println("Training ")
    def evaluate(): Unit = {
      println("TRAIN\n"+(new cc.factorie.app.classify.Trial[MentionEntityType,la.Tensor1](model, MentionEntityTypeDomain, (t:MentionEntityType) => features(t.mention).value) ++= trainMentions.map(_.attr[MentionEntityType])).toString)
      println("\nTEST\n"+(new cc.factorie.app.classify.Trial[MentionEntityType,la.Tensor1](model, MentionEntityTypeDomain, (t:MentionEntityType) => features(t.mention).value) ++= testMentions.map(_.attr[MentionEntityType])).toString)
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
    val dstream = new java.io.DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }
  def deserialize(stream: java.io.InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val dstream = new java.io.DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, dstream)
    model.weights.set(new la.DenseLayeredTensor2(FeatureDomain.dimensionDomain.size, MentionEntityTypeDomain.size, new la.SparseIndexedTensor1(_)))
    BinarySerializer.deserialize(model, dstream)
    dstream.close()  // TODO Are we really supposed to close here, or is that the responsibility of the caller
  }

}

object MentionEntityTypeLabeler extends MentionEntityTypeLabeler(cc.factorie.util.ClasspathURL[MentionEntityTypeLabeler](".factorie"))

object MentionEntityTypeLabelerTrainer {
  def main(args:Array[String]): Unit = {
    if (args.length == 0) println("usage: trainfile [modelfile]")
    var trainDocs = coref.ConllCorefLoader.loadWithParse(args(0), loadSingletons=false, disperseEntityTypes=true)
    val testDocs = trainDocs.takeRight(20)
    trainDocs = trainDocs.dropRight(20)
    val labeler = new MentionEntityTypeLabeler
    for (mention <- labeler.filterTrainingMentions(testDocs.flatMap(_.attr[MentionList])))
      println("%20s  %s".format(mention.attr[MentionEntityType].target.categoryValue, mention.phrase))

    labeler.train(trainDocs, testDocs)
    (trainDocs ++ testDocs).foreach(labeler.process(_))
    for (mention <- labeler.filterTrainingMentions(testDocs.flatMap(_.attr[MentionList])))
      println("%20s %-20s %-20s  %s".format(mention.attr[MentionEntityType].target.categoryValue, mention.attr[MentionEntityType].categoryValue, labeler.isWordNetPerson(mention.headToken).toString, mention.phrase))

    if (args.length > 1) labeler.serialize(args(1))
    
  }
}


//this gives each Mention and MentionEntityType. This is a very simple rule-based annotator that can not even produce predictions
//for many of the categories in  OntonotesMentionEntityTypeDomain, only PERSON, ORG, GPE, and EVENT.
//todo: this is a candidate for deletion
object MentionEntityTypeAnnotator1 extends DocumentAnnotator {
  import MentionEntityTypeAnnotator1Util._
  def process(document:Document): Document = {
    document.attr[MentionList].foreach(predictMentionEntityType(_))
    document
  }
  def predictMentionEntityType(m: Mention): Unit = {
    val prediction = classifyUsingRules(m.tokens.map(_.lemmaString))
    m.attr += new MentionEntityType(m,prediction)
  }
  override def tokenAnnotationString(token:Token): String = {
    token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionEntityType].categoryValue + ":" + m.indexOf(token)).mkString(","); case _ => "_" }
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[MentionEntityType])

}

//the reason some functions for above are pulled into a separate object as this simplifies the API with other projects greatly
object MentionEntityTypeAnnotator1Util {
  final val articles = Seq("a","A","the","The").toSet

  //this expects cased strings as input
  def classifyUsingRules(strings: Seq[String]): String = {

    val uStr = strings.filter(!articles.contains(_))
    val str = uStr.map(_.toLowerCase)
    val str1 = strings.mkString(" ")
    val uStr1 = uStr.mkString(" ")

    val isPerson = detectIfPerson(str,uStr)
    val isPlace = detectIfPlace(str1,uStr1)
    val isEvent = detectIfEvent(str1,uStr1)
    val isOrganization = detectIfOrg(str1,uStr1)
    val onlyOne =  Seq(isPerson, isPlace, isEvent,  isOrganization).count(y => y) == 1

    if(onlyOne){
      if(isPerson) "PERSON"
      else if(isPlace) "GPE"
      else if(isEvent) "EVENT"
      else if(isOrganization) "ORG"
      else
        "O"
    }else{
      if(isPlace && isOrganization) //the place lexicon is mostly contained in the organization lexicon, so you need to treat it carefully.
        "GPE"
      else if(isPlace && isPerson)
        "GPE"
      else
        "O"
    }
  }

  def detectIfPerson(strs: Seq[String], uStrs: Seq[String]): Boolean = {
    val isCased = strs.zip(uStrs).exists(ab => ab._1 != ab._2)
    val str = strs.mkString(" ")
    val uStr = uStrs.mkString(" ")
    val fullStringPerson = lexicon.wikipedia.Person.contains(str) && isCased
    val fields = strs
    val uFields = uStrs

    val firstIsCased = fields.nonEmpty && fields(0) != uFields(0)
    val secondIsCased = if(fields.length == 2) fields(1) != uFields(1) else false
    val firstContained = fields.nonEmpty && (lexicon.uscensus.PersonFirstFemale.contains(fields(0)) || lexicon.uscensus.PersonFirstMale.contains(fields(0)))
    val secondContained =   if(fields.length == 2) lexicon.uscensus.PersonLast.contains(fields(1))  else false

    val firstName = fields.length == 2  && firstContained  &&  firstIsCased  && secondIsCased
    val firstName2 = fields.length == 1 && lexicon.iesl.PersonFirst.contains(fields(0))  &&  firstIsCased
    val lastName = fields.length == 2   && firstContained && secondContained   &&  secondIsCased && firstIsCased
    val lastName2 = fields.length == 1   && lexicon.uscensus.PersonLast.contains(fields(0))   &&  firstIsCased
    val bothNames = fields.length == 2 && firstContained && secondContained
    val isI = fields.length == 1 && uStr == "I"

    val isPerson = lastName || lastName2 || firstName || firstName2|| fullStringPerson  || bothNames
    isPerson  && ! isI
  }
  //the follow three methods just check for exact string matches
  def detectIfPlace(s: String, us: String): Boolean = {
    lexicon.iesl.AllPlaces.contains(s)
  }
  def detectIfOrg(s: String, us: String): Boolean = {
    lexicon.iesl.OrgSuffix.contains(s)
  }
  def detectIfEvent(s: String, us: String): Boolean = {
    lexicon.wikipedia.Event.contains(s)
  }

  def classifyUsingRules(rawString: String): String = {
    classifyUsingRules(rawString.split(" "))
  }

}
