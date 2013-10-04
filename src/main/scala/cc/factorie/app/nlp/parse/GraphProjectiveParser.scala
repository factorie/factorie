package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp._
import cc.factorie._
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.la.{Tensor, WeightsMapAccumulator}
import util.{HyperparameterMain, ClasspathURL, DoubleAccumulator}
import scala.collection.mutable.ArrayBuffer
import java.io._
import cc.factorie.variable.{TensorVar, HashFeatureVectorVariable, DiscreteDomain}
import cc.factorie.model.Parameters

/**
 * User: apassos
 * Date: 5/19/13
 * Time: 9:44 AM
 */
class GraphProjectiveParser extends DocumentAnnotator {
  parser =>

  object MyFeaturesDomain extends DiscreteDomain(1e7.toInt) // 10 million features
  class FeatureVector extends HashFeatureVectorVariable {
    def domain = MyFeaturesDomain
  }

  def getTokenFeatureVector(t: Token): TensorVar = {
    val f = new FeatureVector
    val tWord = t.string
    val tPos = t.attr[PennPosLabel].categoryValue
    f += "TOKENPOS="+tPos
    f += "TOKENWORD="+tWord
    f += "TOKENID="+tPos+"&"+tWord
    f
  }

  def getParentFeatureVector(p: Token): TensorVar = {
    val f = new FeatureVector
    val pWord = if (p ne null) p.string else "ROOT"
    val pPos = if (p ne null) p.attr[PennPosLabel].categoryValue else "ROOTPOS"
    f += "PARENTPOS="+pPos
    f += "PARENTWORD="+pWord
    f += "PARENTID="+pPos+"&"+pWord
    assert(f ne null)
    f
  }

  override def tokenAnnotationString(token:Token): String = {
    val sentence = token.sentence
    val pt = if (sentence ne null) sentence.attr[ParseTree] else null
    if (pt eq null) "_\t_"
    else (pt.parentIndex(token.positionInSentence)+1).toString+"\t"
  }

  def getPairwiseFeatureVector(t: Token, p: Token): TensorVar = {
    val f = new FeatureVector
    val tWord = t.string
    val tPos = t.attr[PennPosLabel].categoryValue
    val pWord = if (p ne null) p.string else "ROOT"
    val pPos = if (p ne null) p.attr[PennPosLabel].categoryValue else "ROOTPOS"
    f += "WORDPAIR="+tWord+"&"+pWord
    f += "POSPAIR="+tPos+"&"+pPos
    f += "PARENTPAIRCHILDPOS="+pPos+"&"+pWord+"&"+tPos
    f += "PARENTPAIRCHILDWORD="+pPos+"&"+pWord+"&"+tWord
    f += "CHILDPAIRPARENTPOS="+tPos+"&"+tWord+"&"+pPos
    f += "CHILDPAIRPARENTWORD="+tPos+"&"+tWord+"&"+pWord
    f += "JOINTALL="+tPos+"&"+tWord+"&"+pWord+"&"+pPos
    if ((p ne null) && (t ne null)) {
      val first = if (p.positionInSentence < t.positionInSentence) p else t
      val second = if (p.positionInSentence < t.positionInSentence) t else p
      var x = first
      while (x.sentenceNext ne second) {
        x = x.sentenceNext
        f += "BETWEENPOS="+pPos+"&"+x.attr[PennPosLabel].categoryValue+"&"+tPos
      }
      val prevHeadPos = if (p.sentenceHasPrev) p.sentencePrev.attr[PennPosLabel].categoryValue else "NOPREV"
      val prevTokPos = if (t.sentenceHasPrev) t.sentencePrev.attr[PennPosLabel].categoryValue else "NOPREV"
      val nextHeadPos = if (p.sentenceHasNext) p.sentenceNext.attr[PennPosLabel].categoryValue else "NONEXT"
      val nextTokPos = if (t.sentenceHasNext) t.sentenceNext.attr[PennPosLabel].categoryValue else "NONEXT"
      f += "HNhPcC="+pPos+"&"+nextHeadPos+"&"+prevTokPos+"&"+tPos
      f += "PhHPcC="+prevHeadPos+"&"+pPos+"&"+prevTokPos+"&"+tPos
      f += "HNhCNc="+pPos+"&"+nextHeadPos+"&"+tPos+"&"+nextTokPos
      f += "PhHCNc="+prevHeadPos+"&"+pPos+"&"+tPos+"&"+nextTokPos
      val distance = math.abs(t.positionInSentence - p.positionInSentence)
      for (i <- 0 to distance) {
        f += "EdgeLength>="+i
      }
      val normDistance = distance*10/ t.sentence.length
      for (i <- 0 to normDistance) {
        f += "NormDistance>="+i
      }
    }
    f
  }

  def groundTruthEdges(s: Sentence): Seq[TensorVar] = {
    s.attr[ParseTree]._targetParents.zipWithIndex.flatMap(i => {
      val parentToken = if (i._1 < 0) null else s.tokens(i._1)
      val childToken = s.tokens(i._2)
      Seq(getTokenFeatureVector(childToken), getParentFeatureVector(parentToken), getPairwiseFeatureVector(childToken, parentToken))
    })
  }
  object DependencyModel extends Parameters {
    val weights = Weights(new cc.factorie.la.DenseTensor1(MyFeaturesDomain.dimensionSize))
  }

  class ProjectiveParser(val weights: Tensor, val sent: Sentence, val negativeExamples: Seq[(Int,Int)]) {
    val knownParents = ArrayBuffer[Int]()
    val sentLength = sent.length
    (0 until sentLength).foreach(e => knownParents.append(-3))
    negativeExamples.foreach(e => knownParents(e._2) = e._1)
    val tokenScores = Array.fill(sent.length+1)(0.0)
    val parentScores = Array.fill(sent.length+1)(0.0)
    parentScores(0) = getParentFeatureVector(null).value.dot(weights)
    for (i <- 0 until sentLength) {
      tokenScores(i+1) = getTokenFeatureVector(sent.tokens(i)).value.dot(weights)
      parentScores(i+1) = getParentFeatureVector(sent.tokens(i)).value.dot(weights)
    }
    val edgeScores = Array.fill(sent.length+1,sent.length+1)(Double.NaN)
    def getEdgeScore(parent: Int, child: Int) : Double = {
      assert(parent != child, "can't add an edge from a token to itself")
      if (edgeScores(parent)(child).isNaN) {
        val loss = if ((child > 0) && (knownParents(child-1) == parent -1)) -1.0 else 0.0
        edgeScores(parent)(child) = if (child > 0)
          loss + getPairwiseFeatureVector(sent.tokens(child - 1), if (parent > 0) sent.tokens(parent - 1) else null).value.dot(weights) + tokenScores(child) + parentScores(parent)
        else 0
      }
      edgeScores(parent)(child)
    }


    class SpanData(val split: Int, val score: Double)
    case class LeftComplete(override val split: Int, override val score: Double) extends SpanData(split, score)
    case class LeftIncomplete(override val split: Int, override val score: Double) extends SpanData(split, score)
    case class RightComplete(override val split: Int, override val score: Double) extends SpanData(split, score)
    case class RightIncomplete(override val split: Int, override val score: Double) extends SpanData(split, score)

    def backwardSearch(left: Int, right: Int, d: SpanData, edges: ArrayBuffer[(Int, Int)]) {
      if (left >= right) return
      d match {
        case d: LeftComplete =>
          val node = lcTable(left)(right)
          // println("searching lc " + left + " " + right)
          backwardSearch(left, node.split, LeftIncomplete(0, 0), edges)
          backwardSearch(node.split, right, LeftComplete(0, 0), edges)
        case d: RightComplete =>
          val node = rcTable(left)(right)
          // println("searching rc " + left + " " + right)
          backwardSearch(left, node.split, RightComplete(0, 0), edges)
          backwardSearch(node.split, right, RightIncomplete(0, 0), edges)
        case d: LeftIncomplete =>
          val node = liTable(left)(right)
          // println("searching li " + left + " " + right)
          edges.append((left, right))
          backwardSearch(left, node.split, LeftComplete(0, 0), edges)
          backwardSearch(node.split+1, right, RightComplete(0, 0), edges)
        case d: RightIncomplete =>
          // println("searching ri " + left + " " + right)
          val node = riTable(left)(right)
          edges.append((right, left))
          backwardSearch(left, node.split, LeftComplete(0, 0), edges)
          backwardSearch(node.split+1, right, RightComplete(0, 0), edges)
      }
    }

    val lcTable = Array.fill[LeftComplete](sentLength+1, sentLength+1)(null)
    val liTable = Array.fill[LeftIncomplete](sentLength+1, sentLength+1)(null)
    val rcTable = Array.fill[RightComplete](sentLength+1, sentLength+1)(null)
    val riTable = Array.fill[RightIncomplete](sentLength+1, sentLength+1)(null)

    def parse() = {
      for (i <- 0 until sentLength+1) {
        lcTable(i)(i) = LeftComplete(i, 0)
        liTable(i)(i) = LeftIncomplete(i, 0)
        rcTable(i)(i) = RightComplete(i, 0)
        riTable(i)(i) = RightIncomplete(i, 0)
      }

      for (height <- 1 until sentLength+1) {
        for (left <- 0 until sentLength+1 - height) {
          // here we'll fill the table cell left,left+height
          val right = left+height
          var bestLI = null.asInstanceOf[LeftIncomplete]
          var bestRI = null.asInstanceOf[RightIncomplete]
          for (split <- left until left+height) {
            val li = LeftIncomplete(split, lcTable(left)(split).score + rcTable(split+1)(right).score)
            bestLI = if ((bestLI eq null) || (li.score > bestLI.score)) li else bestLI
            val ri = RightIncomplete(split, lcTable(left)(split).score + rcTable(split+1)(right).score)
            bestRI = if ((bestRI eq null) || (ri.score > bestRI.score)) ri else bestRI
          }
          liTable(left)(right) = LeftIncomplete(bestLI.split, bestLI.score + getEdgeScore(left, right))
          riTable(left)(right) = RightIncomplete(bestRI.split, bestRI.score + getEdgeScore(right, left))

          var bestLC = null.asInstanceOf[LeftComplete]
          for (split <- left+1 to left+height) {
            // println(left + " " + split + " " + (left+height))
            val lc = LeftComplete(split, liTable(left)(split).score + lcTable(split)(right).score)
            bestLC = if ((bestLC eq null) || (lc.score > bestLC.score)) lc else bestLC
          }
          lcTable(left)(right) = bestLC

          var bestRC = null.asInstanceOf[RightComplete]
          for (split <- left until left+height) {
            val rc = RightComplete(split, rcTable(left)(split).score + riTable(split)(right).score)
            bestRC = if ((bestRC eq null) || (rc.score > bestRC.score)) rc else bestRC
          }
          rcTable(left)(right) = bestRC
        }
      }
      val finalParse = lcTable(0)(sentLength)
      val score = finalParse.score
      val edges = ArrayBuffer[(Int, Int)]()
      backwardSearch(0, sentLength, finalParse, edges)
      val parents = Array.ofDim[Int](sentLength)
      var sum = 0.0
      edges.foreach(e => {
        sum += getEdgeScore(e._1, e._2)
        assert(parents(e._2-1) == 0)
        assert(e._2 != 0)
        parents(e._2-1) = e._1-1
      })
      assert(math.abs(score - sum) < 0.01*math.abs(score) + 0.0001, "edge scores should match: " + score + " " + sum)
      (parents, score)
    }
  }

  class StructuredPerceptronParsingExample(val sent: Sentence) extends cc.factorie.optimize.Example {
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) {
      if (gradient ne null) {
        val gt = groundTruthEdges(sent)
        gt.foreach(f => gradient.accumulate(DependencyModel.weights, f.value))

        val projectiveParser = new ProjectiveParser(DependencyModel.weights.value,
          sent,
          sent.attr[ParseTree].targetParents.zipWithIndex)
        val (parents, score) = projectiveParser.parse()
        for (i <- 0 until sent.tokens.length) {
          gradient.accumulate(DependencyModel.weights,
            getPairwiseFeatureVector(sent.tokens(i), if (parents(i) == -1) null else sent.tokens(parents(i))).value,
            -1.0)
          gradient.accumulate(DependencyModel.weights,
            getTokenFeatureVector(sent.tokens(i)).value,
            -1.0)
          gradient.accumulate(DependencyModel.weights,
            getParentFeatureVector(if (parents(i) == -1) null else sent.tokens(parents(i))).value,
            -1.0)
          if (value ne null) {
            val trueParent = sent.attr[ParseTree].parents(i)
            val returned = parents(i)
            value.accumulate(math.min(0, projectiveParser.getEdgeScore(trueParent+1,i+1) - projectiveParser.getEdgeScore(returned+1, i+1)))
          }
        }
      }
    }
  }

  def serialize(filename: String) = {
    val file = new File(filename); if (file.getParentFile != null && !file.getParentFile.exists) file.getParentFile.mkdirs()
    cc.factorie.util.BinarySerializer.serialize(DependencyModel, file)
  }
  def deserialize(file: String) = cc.factorie.util.BinarySerializer.deserialize(DependencyModel, new File(file))
  def deserialize(url: java.net.URL) = cc.factorie.util.BinarySerializer.deserialize(DependencyModel, new DataInputStream(url.openConnection().getInputStream))

  def train(trainSentences: Seq[Sentence], testSentences: Seq[Sentence], file: String, nThreads: Int, nIters: Int = 10) {
    val examples = trainSentences.map(new StructuredPerceptronParsingExample(_))
    val rng = new scala.util.Random(0)
    val opt = new cc.factorie.optimize.AdaGradRDA(1.0, 0.0, 0.0001, 0.0001)
    val trainer = new optimize.SynchronizedOptimizerOnlineTrainer(DependencyModel.parameters, opt, maxIterations = 10, nThreads = nThreads)
    for (iteration <- 0 until nIters) {
      trainer.processExamples(rng.shuffle(examples))
      val t0 = System.currentTimeMillis()
      println("Predicting train set..."); trainSentences.par.foreach { s => parse(s) } // Was par
      println("Predicting test set...");  testSentences.par.foreach { s => parse(s) } // Was par
      println("Processed in " + (trainSentences.length+testSentences.length)*1000.0/(System.currentTimeMillis()-t0) + " sentences per second")
      println("Training UAS = "+ ParserEval.calcUas(trainSentences.map(_.attr[ParseTree])))
      println(" Testing UAS = "+ ParserEval.calcUas(testSentences.map(_.attr[ParseTree])))
      println()
      println("Saving model...")
      if (file != "") serialize(file + "-iter-"+iteration)
    }
    println("Finished training.")
    opt.finalizeWeights(DependencyModel.parameters)
  }

  def parse(sent: Sentence) {
    val tree = sent.attr.getOrElseUpdate(new ParseTree(sent))
    val parser = new ProjectiveParser(DependencyModel.weights.value, sent, Seq())
    val (parents, _) = parser.parse()
    for (i <- 0 until sent.length) tree.setParent(i, parents(i))
  }

  def process(document: Document) = {
    document.sentences.foreach(parse)
    document
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[pos.PennPosLabel]) // TODO Also TokenLemma?  But we don't have a lemmatizer that matches the training data
  def postAttrs: Iterable[Class[_]] = List(classOf[ParseTree])
}

object GraphProjectiveParser extends GraphProjectiveParser {
  deserialize(ClasspathURL[GraphProjectiveParser](".factorie"))
}

object GraphProjectiveParserOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions{
  val trainFile = new CmdOption("train", "", "FILES", "CoNLL-2008 train file.")
  //val devFile =   new CmdOption("dev", "", "FILES", "CoNLL-2008 dev file")
  val testFile =  new CmdOption("test", "", "FILES", "CoNLL-2008 test file.")
  val model      = new CmdOption("model", "parser-model", "FILE", "File in which to save the trained model.")
  val nThreads   = new CmdOption("nThreads", 10, "INT", "Number of threads to use.")
}

object GraphProjectiveParserTrainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    val opts = GraphProjectiveParserOpts
    opts.parse(args)

    val parser = new GraphProjectiveParser


    val trainDoc = load.LoadOntonotes5.fromFilename(opts.trainFile.value).head
    val testDoc = load.LoadOntonotes5.fromFilename(opts.testFile.value).head

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainSentencesFull = trainDoc.sentences.toSeq
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    val testSentencesFull = testDoc.sentences.toSeq
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)



    // Train
    parser.train(trainSentences, testSentences, opts.model.value, math.min(opts.nThreads.value, Runtime.getRuntime.availableProcessors()))
    // Test

    // Print accuracy diagnostics
    println("Predicting train set..."); parser.process(trainDoc)
    println("Predicting test set...");  parser.process(testDoc)
    println("Training UAS = "+ ParserEval.calcUas(trainDoc.sentences.toSeq.map(_.attr[ParseTree])))
    val testUAS = ParserEval.calcUas(testDoc.sentences.toSeq.map(_.attr[ParseTree]))
    println("Testing UAS = "+ testUAS)
    println()
    println("Done.")
    if(opts.targetAccuracy.wasInvoked) assert(testUAS > opts.targetAccuracy.value.toDouble, "Did not reach accuracy requirement")
    testUAS
  }

}