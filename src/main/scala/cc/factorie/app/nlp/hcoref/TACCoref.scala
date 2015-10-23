package cc.factorie.app.nlp.hcoref

import java.io._
import java.util.zip.GZIPInputStream

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.ParseForwardCoref
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.segment.{DeterministicNormalizingTokenizer, DeterministicSentenceSegmenter}
import cc.factorie.util.{NonValidatingXML, VectorUtils}
import cc.factorie.variable.{BagOfWordsVariable, CategoricalDomain, DenseDoubleBagVariable}

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import scala.util.Random

/**
 * @author John Sullivan
 */
object TACCorefWithFactorie {
  def main(args:Array[String]) {
    val tacRoot = args(0)
    val evalPath = args(1)

    val map = new Tac2009FlatDocumentMap(tacRoot)

    val refMentions = ProcessQueries.loadQueries(evalPath + ".xml", evalPath + ".tab")

    println("loaded %d mentions/queries in %d entities.".format(refMentions.size, refMentions.map(_.entId).toSet.size))

    val pipelineElements = Seq(
      DeterministicNormalizingTokenizer,
      DeterministicSentenceSegmenter,
      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      ParseForwardCoref
    )

    val pipeline = DocumentAnnotatorPipeline(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap.toMap, Nil, pipelineElements.flatMap(_.postAttrs))

    println("Processing ref mentions and documents: ")
    refMentions.par.foreach{ rMention =>
      val doc = new Document(map.getDoc(rMention.docId).toIterator.mkString("\n")).setName(rMention.docId)
      rMention.doc = Some(doc)
      rMention.getTokenSpan.map(ts => doc.getCoref.addMention(new Phrase(ts))) // we add our gold mentions before coref and processing
      pipeline.process(doc)
      print(".")
    }

    val converter = new RefMentionConverter(pipeline)

    val mentions = refMentions.flatMap(converter.toDocEntNode).toSeq
    println("Found %d mentions in documents out of %d total mention (%.4f \\%)".format(mentions.size, refMentions.size, mentions.size.toDouble/refMentions.size))

    val splitPoint = (mentions.size * 0.75).toInt
    val (train, test) = mentions.splitAt(splitPoint)

    println("Split into %d training and %d testing".format(train.size, test.size))
    implicit val rand = new Random()

    val tacCoref = new DocEntityCoref {implicit val random: Random = rand

      def estimateIterations(mentionCount: Int) = mentionCount * 100

      val model = new DocEntityCorefModel(4.0, 0.25, 1.0, 2.0, 0.25, 1.0, 0.25, 3.0, 0.25, 1.0, 0.25)

      val autoStopThreshold = 10000
    }

    val sampler = tacCoref.getSampler(test)
    sampler.infer
  }
}

object TACCoref {

  //val tagger = new OntonotesForwardPosTagger()

  def main(args:Array[String]) {
    val tacRoot = args(0)
    val evalPath = args(1)
    val embeddingFile = args(2)

    val embeddings = EmbeddingSpace.fromFile(embeddingFile)

    val map = new Tac2009FlatDocumentMap(tacRoot)

    val refMentions = ProcessQueries.loadQueries(evalPath + ".xml", evalPath + ".tab")

    val mentions = refMentions.flatMap{ rMention =>
      val doc = new Document(map.getDoc(rMention.docId).toIterator.mkString("\n")).setName(rMention.docId)
      DeterministicNormalizingTokenizer.process(doc)
      DeterministicSentenceSegmenter.process(doc)
      rMention.doc = Some(doc)

      val tokenSpanOpt = doc.getSectionByOffsets(rMention.getOffsets._1, rMention.getOffsets._2).getOrElse(doc.asSection).offsetSnapToTokens(rMention.getOffsets._1, rMention.getOffsets._2)
      if(tokenSpanOpt.isEmpty) {
        println("for doc %s didn't find token span from name %s and offsets: %s".format(rMention.docId, rMention.name, rMention.getOffsets))
      }
      tokenSpanOpt.map{ tokenSpan =>

        val nameBag = new BagOfWordsVariable()
        val contextBag = new BagOfWordsVariable()
        val nerBag = new BagOfWordsVariable()
        val mentionBag = new BagOfWordsVariable()
        val numberBag = new BagOfWordsVariable()
        val truth = new BagOfWordsVariable()
        val contextVec = new DenseDoubleBagVariable(50)


        nameBag ++= tokenSpan.tokens.map(_.string)
        contextBag ++= tokenSpan.contextWindow(10).groupBy(_.string).mapValues(_.size.toDouble)
        contextVec.set(embeddings.embedPhrase(contextBag.value.asHashMap.keySet.toSeq))(null)
        nerBag += rMention.entType
        truth += rMention.entId

        new Mention[DenseDocEntityVars](new DenseDocEntityVars(nameBag, contextBag, nerBag, contextVec, numberBag, truth), rMention.id)(null)
      }
    }
    println("done finding token spans and building mentions")

    val splitPoint = (mentions.size * 0.75).toInt
    val (train, test) = mentions.splitAt(splitPoint)

    println("Split into %d training and %d testing".format(train.size, test.size))
    implicit val rand = new Random()

    class DocEntityModel(namesWeights:Double, namesShift:Double, nameEntropy:Double, contextsWeight:Double, contextsShift:Double, matchScore:Double, matchPenalty:Double, denseContextWeight:Double, denseContextShift:Double) extends CorefModel[DenseDocEntityVars] {
      this += new ChildParentCosineDistance(namesWeights, namesShift, {v:DenseDocEntityVars => v.names})
      this += new ChildParentCosineDistance(contextsWeight, contextsShift, {v:DenseDocEntityVars => v.context})
      this += new MatchConstraint(matchScore, matchPenalty, {v:DenseDocEntityVars => v.nerType})
      this += new DenseCosineDistance(denseContextWeight, denseContextShift, {v:DenseDocEntityVars => v.contextVec})
      this += new BagOfWordsEntropy(nameEntropy, {v:DenseDocEntityVars => v.names})
    }


    val model = new DocEntityModel(1.0, -0.25, 0.5, 1.0, -0.25, 1.0, -10.0, 1.0, -0.25)

    val trainer = new CorefSampler[DenseDocEntityVars](model, train, train.size * 100)
      with AutoStoppingSampler[DenseDocEntityVars]
      with CanopyPairGenerator[DenseDocEntityVars]
      with NoSplitMoveGenerator[DenseDocEntityVars]
      with DebugCoref[DenseDocEntityVars]
      with TrainingObjective[DenseDocEntityVars]
      with PrintlnLogger {
      def newInstance(implicit d: DiffList) = new Node[DenseDocEntityVars](new DenseDocEntityVars())

      val autoStopThreshold = 10000
    }
    trainer.train(100000)

    println(trainer.model.parameters.tensors)

    val sampler = new CorefSampler[DenseDocEntityVars](model, test, test.size * 100)
      with AutoStoppingSampler[DenseDocEntityVars]
      with CanopyPairGenerator[DenseDocEntityVars]
      with NoSplitMoveGenerator[DenseDocEntityVars]
      with DebugCoref[DenseDocEntityVars]
      with TrainingObjective[DenseDocEntityVars]
      with PrintlnLogger {
      def newInstance(implicit d: DiffList) = new Node[DenseDocEntityVars](new DenseDocEntityVars())

      val autoStopThreshold = 10000
    }

    sampler.infer

    //println(EvaluatableClustering.evaluationString(test.predictedClustering, test.trueClustering))
    val goldMap = test.map { mention =>
      mention.variables.truth.value.asHashMap.keySet.head -> mention.uniqueId
    }.groupBy(_._1).mapValues(_.map(_._2).toSet)

    val predMap = test.map{m:Node[DenseDocEntityVars] => m.root}.toSet.map { entities:Node[DenseDocEntityVars] =>
      entities.variables.truth.value.topWord -> entities.mentions.map(_.uniqueId).toSet
    }.toMap
    //println(LinkingScorer.scoreString(predMap, goldMap))
  }
}

/**
 * Takes a docId and returns the raw text of the corresponding document
 */
trait DocumentMap {
  def getDoc(docId:String):BufferedReader
}

class Tac2009FlatDocumentMap(tacRoot:String) extends DocumentMap {
  def getDoc(docId:String):BufferedReader = {
    val filePath = s"$tacRoot/$docId.sgm"
    new BufferedReader(new FileReader(filePath))
  }
}

object ProcessQueries {


  def loadQueries(queryXMLFile:String, queryTabFile:String):Iterable[ReferenceMention] = {
    val entMap =  Source.fromFile(queryTabFile).getLines().map { line =>
      val Array(mentId, entId, entType) = line.split("\\s+")
      mentId -> (entId, entType)
    }.toMap

    NonValidatingXML.loadFile(queryXMLFile).\\("kbpentlink").\\("query").map { qXML =>
      val id = (qXML \ "@id").text.trim
      val name = (qXML \ "name").text.trim
      val docName = (qXML \ "docid").text.trim
      val beg = qXML \ "beg"
      val end = qXML \ "end"
      assert(beg.isEmpty == end.isEmpty)
      val offsets:Option[(Int, Int)] = if (beg.isEmpty || end.isEmpty) None else Some(beg.text.toInt, end.text.toInt)
      ReferenceMention(id, name, docName, offsets, entMap(id)._1, entMap(id)._2)
    }
  }
}

case class ReferenceMention(id:String, name:String, docId:String, offsets:Option[(Int, Int)], entId:String, entType:String) {
  var doc:Option[Document] = None
  def getOffsets:(Int, Int) = offsets.getOrElse {
    val start = doc.get.string.replaceAll("""-\n""","-").replaceAll("""\n"""," ").indexOfSlice(name)
    val end = start + name.length - 1
    start -> end
  }
  def getTokenSpan = doc.get.getSectionByOffsets(this.getOffsets._1, this.getOffsets._2).getOrElse(doc.get.asSection).offsetSnapToTokens(this.getOffsets._1, this.getOffsets._2)
}

object RefMentionConverterNoPipeline {
  def toDocEntNode(ref:ReferenceMention):Option[Mention[DocEntityVars]] = {
    val doc = ref.doc.get
    DeterministicNormalizingTokenizer.process(doc)
    DeterministicSentenceSegmenter.process(doc)

    val offsetOpt = ref.offsets match {
      case None =>
        ref.name.r.findFirstMatchIn(doc.string).map(m => m.start -> m.end)
      case otw => otw
    }
    offsetOpt.flatMap{ case (s, e) =>
      doc.getSectionByOffsets(s, e).flatMap(_.offsetSnapToTokens(s, e)) match {
        case Some(refSpan) =>
          implicit val d:DiffList = null
          val xMent = new Mention[DocEntityVars](new DocEntityVars())
          xMent.variables.names ++= refSpan.map{t:Token => t.lemmaString}.toCountBag
          xMent.variables.context ++= refSpan.contextWindow(10).map(_.lemmaString).toCountBag

          Option(doc.coref).flatMap{_.findOverlapping(refSpan)} match {
            case Some(ment) =>
              xMent.variables.++=(DocEntityVars.fromWithinDocEntity(ment.entity))(null)
              xMent.withinDocEntityId = ment.entity.uniqueId
            case None => println("Could not find coref or align mention: " + ref)
          }
          Some(xMent)
        case None =>
          println("WARNING: Failed to find tokens for reference mention: " + ref)
          None
      }
    }
  }
}

class RefMentionConverter(val pipeline:DocumentAnnotationPipeline) {

  def toDocEntNode(ref:ReferenceMention):Option[Mention[DocEntityVars]] = {
    val doc = pipeline.process(ref.doc.get)

    val offsetOpt = ref.offsets match {
      case None =>
        ref.name.r.findFirstMatchIn(doc.string).map(m => m.start -> m.end)
      case otw => otw
    }
    offsetOpt.flatMap{ case (s, e) =>
      doc.getSectionByOffsets(s, e).flatMap(_.offsetSnapToTokens(s, e)) match {
        case Some(refSpan) =>
          implicit val d:DiffList = null
          val xMent = new Mention[DocEntityVars](new DocEntityVars(), ref.id)
          xMent.variables.names ++= refSpan.map{t:Token => t.lemmaString}.toCountBag
          xMent.variables.context ++= refSpan.contextWindow(10).map(_.lemmaString).toCountBag
          xMent.variables.truth += ref.entId

          Option(doc.coref).flatMap{_.findOverlapping(refSpan)} match {
            case Some(ment) =>
              xMent.variables.++=(DocEntityVars.fromWithinDocEntity(ment.entity))(null)
              xMent.withinDocEntityId = ment.entity.uniqueId
            case None => println("Could not find coref or align mention: " + ref)
          }
          Some(xMent)
        case None =>
          println("WARNING: Failed to find tokens for reference mention: " + ref)
          None
      }
    }
  }
}

object GenerateEmbeddings {
  def main(args:Array[String]) {
    val tacRoot = args(0)
    val evalPath = args(1)
    val embeddingFilename = args(2)

    val map = new Tac2009FlatDocumentMap(tacRoot)

    val refMentions = ProcessQueries.loadQueries(evalPath + ".xml", evalPath + ".tab")

    val tokens = refMentions.map{ rMention =>
      val doc = new Document(map.getDoc(rMention.docId).toIterator.mkString("\n")).setName(rMention.docId)
      DeterministicNormalizingTokenizer.process(doc)
      DeterministicSentenceSegmenter.process(doc)
      doc.tokens.map(_.lemmaString)
    }

    println("loaded and tokenized, starting embeddings")

    val dimensions = 50
    val iterations = 10
    val regularizer = 10
    val learningRate = 0.1


    val random = new scala.util.Random(0)
    val domain = new CategoricalDomain[String]()
    val space = new EmbeddingSpace(domain,dimensions,random)
    println("embeddings initialized")
    space.learnEmbeddingsFromText(tokens,iterations,regularizer,learningRate)

    println("writing embeddings")
    Embeddings.writeEmbedding(new File(embeddingFilename), space)
    //testEmbeddings(space,test)
  }
}

object EmbeddingSpace{
  import VectorUtils._
  def fromFile(fileName:String):EmbeddingSpace ={
    val reader = if(fileName.endsWith(".gz") || fileName.endsWith("tgz")) new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(new File(fileName)))))
    else new BufferedReader(new InputStreamReader(new FileInputStream(new File(fileName))))
    var result:EmbeddingSpace=null
    val map = new HashMap[String,Array[Double]]
    var line: String = ""
    //val tmpResult = new ArrayBuffer[Pair[String,Array[Double]]]
    while({line = reader.readLine(); line != null}){
      val pair = line.split("[\t]")
      assert(pair.length == 2, "{%s} is %d in length" format(line, pair.length))
      val weights = pair(1).split(" ").map(e => e.toDouble)
      if (result==null)result = new EmbeddingSpace(new CategoricalDomain[String],weights.length,new scala.util.Random(0))
      result.setEmbedding(pair(0),weights)
    }
    result
  }
  def stopWordStats(space:EmbeddingSpace,stop:Seq[String],control:Seq[String]){
    val mean = zero(space.dimensionality)
    var meanNorm = 0.0
    //val variance = zero(space.dimensionality)
    space.wordTypes.foreach(mean += _)
    space.wordTypes.foreach(meanNorm += _.twoNorm)
    mean /= space.wordTypes.size.toDouble
    meanNorm /= space.wordTypes.size.toDouble
    //space.wordTypes.foreach(x => variance += x.twoDistance(mean))
    println("Mean: "+mean.mkString(","))
    println("||Mean||: "+mean.twoNorm)
    println("Average ||Mean||: "+meanNorm)
    val wordsAndLabels = stop.map(_ -> "stop") ++ control.map(_ -> "ctrl")
    val numStops = wordsAndLabels.filter(_._2=="stop").size
    val numControl = wordsAndLabels.size-numStops
    var stopFromMean=0.0
    var controlFromMean=0.0
    println("Words: ")
    for((word,label) <- wordsAndLabels){
      val x = space.getOrElseZero(word)
      val norm = x.twoNorm
      val toMean = (x-mean).twoNorm
      val h = x.normalizedEntropyForLogValues
      if (label=="stop")stopFromMean+=toMean else controlFromMean+=toMean
      //if (label=="stop")stopFromMean+=h else controlFromMean+=h
      println("  "+label+" "+h+" "+toMean+" "+word+" "+norm)
    }
    stopFromMean /= numStops
    controlFromMean /= numControl
    val boundary = (stopFromMean + controlFromMean)/2
    println("Stop from mean: "+stopFromMean)
    println("Control from mean: "+controlFromMean)
    var numCorrect=0
    var total=0
    for((word,label) <- wordsAndLabels){
      val x = space.getOrElseZero(word)
      val toMean = (x-mean).twoNorm
      val predictStop = toMean < boundary
      val isStop = label=="stop"
      if((predictStop && isStop) || (!predictStop && !isStop))numCorrect += 1
      total+=1
    }
    println("Accuracy: "+numCorrect.toDouble/total.toDouble)
  }
}
class EmbeddingSpace(val domain:CategoricalDomain[String],val dimensionality:Int,val random:scala.util.Random){
  import VectorUtils._
  val wordTypes = new ArrayBuffer[Array[Double]]
  def mean = {val r = zero(dimensionality);var i=0;while(i<wordTypes.size){r+=wordTypes(i);i+=1};r/=wordTypes.size.toDouble;r}
  def setEmbedding(s:String,v:Array[Double]) ={
    val idx = domain.index(s)
    if (idx==wordTypes.size)wordTypes += v
    else if(idx<wordTypes.size)wordTypes(idx)=v
    else throw new Exception("Error: domain and word type embeddings buffer are out of sync.")
  }
  def apply(s:String):Array[Double] = {
    val idx = domain.index(s)
    var result:Array[Double] = null
    if (idx<wordTypes.size)result = wordTypes(idx)
    else if(idx==wordTypes.size){
      result = newEmbedding(s)
      wordTypes += result
    }else throw new Exception("Error: domain and word type embeddings buffer are out of sync.")
    assert(result!=null)
    result
  }
  def getOrElseZero(s:String):Array[Double] ={
    val idx = domain.index(s)
    var result:Array[Double] = null
    if (idx<wordTypes.size)result = wordTypes(idx)
    else if(idx==wordTypes.size){
      result = zero(dimensionality)
      wordTypes += result
    }else throw new Exception("Error: domain and word type embeddings buffer are out of sync.")
    assert(result!=null)
    result
  }
  def embedPhrase(words:Seq[String]) = {
    val result = zero(dimensionality)
    for(v <- words.map(getOrElseZero(_)))result += v
    result
  }
  def learnEmbeddingsFromText(examples:Iterable[Iterable[String]],iterations:Int,regularizer:Double,learningRate:Double){
    learnEmbeddings(examples.map(ws=>new EmbeddingExample(ws.toIndexedSeq,this)).toIndexedSeq,iterations,regularizer,learningRate)
  }
  def learnEmbeddings(examples:IndexedSeq[EmbeddingExample],iterations:Int,regularizer:Double,learningRate:Double){
    assert(examples.forall(_.space eq this))
    assert(examples.forall(_.words.length>1))
    println("Learning embeddings.")
    for (i <- 1 to iterations){
      println("Iteration "+i)
      var j=0
      for (example <- random.shuffle(examples)){
        gradientStep(example,examples(random.nextInt(examples.size)),regularizer,learningRate*2.0/(math.sqrt(1.0+i.toDouble)))
        j+=1
      }
      monitorDoc(examples.head)
      println("Num updates: "+numUpdates+" out of "+numSteps+" opportunities.")
    }
  }
  def monitorDoc(example:EmbeddingExample){
    println("  Monitoring example")
    for(w <- example.words){
      val v = getOrElseZero(w)
      println("    -w: "+w+" v: "+v.twoNorm())
    }
  }
  var numUpdates=0
  var numSteps=0
  def gradientStep(example:EmbeddingExample,counterExample:EmbeddingExample,regularizer:Double,learningRate:Double){
    val margin = regularizer/10.0
    var i=0
    val totalSum = example.computeSum()
    assert(!totalSum.hasNaN)
    while(i<example.wordVectors.length){
      val word = example.words(i)
      val wordv = example.wordVectors(i)
      val contextSum = totalSum - wordv
      assert(!contextSum.hasNaN)
      contextSum /= (example.words.length-1.0)
      assert(!contextSum.hasNaN)
      val negativeExample = counterExample.computeSum()
      negativeExample/=counterExample.words.length.toDouble
      //val negativeExample = counterExample.wordVectors(random.nextInt(counterExample.wordVectors.length))
      //val negativeExample = this.apply(domain(random.nextInt(domain.size)).category)
      //val negativeExample = this.apply(domain(random.nextInt(domain.size)).category).makeCorruptObservation(_.corrupt(sigma,random))
      if((wordv dot contextSum) < (wordv dot negativeExample) + margin){
        wordv += (contextSum, learningRate)
        //assert(!wordv.hasNaN)
        wordv += (negativeExample, -learningRate)
        //assert(!wordv.hasNaN)
        val norm = wordv.twoNorm
        if(norm>regularizer)wordv/=(norm/regularizer)
        numUpdates += 1
      }
      numSteps += 1
      i+=1
    }
  }
  def newEmbedding(s:String) = randomArray(dimensionality,random)/dimensionality
}
class EmbeddingExample(val words:IndexedSeq[String],val space:EmbeddingSpace){
  import VectorUtils._
  val wordVectors = words.map(space(_))
  def computeSum():Array[Double]={val contextSum=zero(space.dimensionality);wordVectors.foreach(contextSum += _);contextSum}
}

object Embeddings{
  //val test = Seq("vldb","emnlp","icml","nips","icvpr","acl","relation extraction","database","knowledge base","entity","coreference","graphical model","approach","face","physics","machine learning","cryptography","graphics","networks","learning","amccallum","elearnedmiller","amoore","speytonjones","ablum","tmitchell","dkarger")

  def writeEmbedding(file:File,space:EmbeddingSpace){
    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    for(word <- space.domain.categories){
      val vec = space.getOrElseZero(word)
      out.write(word+"\t"+vec.mkString(" ")+"\n")
      out.flush
    }
    out.flush
    out.close
  }
}
