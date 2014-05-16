/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp._
import cc.factorie._
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.la.{Tensor, WeightsMapAccumulator}
import cc.factorie.util.{Threading, HyperparameterMain, ClasspathURL, DoubleAccumulator, FileUtils}
import scala.collection.mutable.ArrayBuffer
import java.io._
import cc.factorie.variable.{TensorVar, HashFeatureVectorVariable, DiscreteDomain}
import cc.factorie.model.Parameters
import cc.factorie.optimize._

/** A graph-based projective dependency parser.
    @author Alexandre Passos */
class ProjectiveGraphBasedParser extends DocumentAnnotator {
  parser =>

  def this(url:java.net.URL) = { this(); deserialize(url) }
  
  object MyFeaturesDomain extends DiscreteDomain(1e7.toInt) // 10 million features
  class FeatureVector extends HashFeatureVectorVariable {
    def domain = MyFeaturesDomain
  }

  def getTokenFeatureVector(t: Token): TensorVar = {
    val f = new FeatureVector
    val tWord = t.string
    val tPos = t.attr[PennPosTag].categoryValue
    f += "TOKENPOS="+tPos
    f += "TOKENWORD="+tWord
    f += "TOKENID="+tPos+"&"+tWord
    f
  }

  def getParentFeatureVector(p: Token): TensorVar = {
    val f = new FeatureVector
    val pWord = if (p ne null) p.string else "ROOT"
    val pPos = if (p ne null) p.attr[PennPosTag].categoryValue else "ROOTPOS"
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
    val tPos = t.attr[PennPosTag].categoryValue
    val pWord = if (p ne null) p.string else "ROOT"
    val pPos = if (p ne null) p.attr[PennPosTag].categoryValue else "ROOTPOS"
    val pPosition = if (p ne null) p.positionInSentence else -1
    val tPosition = t.positionInSentence
    val dir = if (pPosition < tPosition) "R" else "L"
    f += dir+"WORDPAIR="+tWord+"&"+pWord
    f += dir+"POSPAIR="+tPos+"&"+pPos
    f += dir+"PARENTPAIRCHILDPOS="+pPos+"&"+pWord+"&"+tPos
    f += dir+"PARENTPAIRCHILDWORD="+pPos+"&"+pWord+"&"+tWord
    f += dir+"CHILDPAIRPARENTPOS="+tPos+"&"+tWord+"&"+pPos
    f += dir+"CHILDPAIRPARENTWORD="+tPos+"&"+tWord+"&"+pWord
    f += dir+"JOINTALL="+tPos+"&"+tWord+"&"+pWord+"&"+pPos
    if ((p ne null) && (t ne null)) {
      val first = if (p.positionInSentence < t.positionInSentence) p else t
      val second = if (p.positionInSentence < t.positionInSentence) t else p
      var x = first
      while (x.sentenceNext ne second) {
        x = x.sentenceNext
        f += dir+"BETWEENPOS="+pPos+"&"+x.attr[PennPosTag].categoryValue+"&"+tPos
      }
      val prevHeadPos = if (p.sentenceHasPrev) p.sentencePrev.attr[PennPosTag].categoryValue else "NOPREV"
      val prevTokPos = if (t.sentenceHasPrev) t.sentencePrev.attr[PennPosTag].categoryValue else "NOPREV"
      val nextHeadPos = if (p.sentenceHasNext) p.sentenceNext.attr[PennPosTag].categoryValue else "NONEXT"
      val nextTokPos = if (t.sentenceHasNext) t.sentenceNext.attr[PennPosTag].categoryValue else "NONEXT"
      def addFourGramFeature(f: FeatureVector, name: String, a: String, b: String, c: String, d: String): Unit = {
        f += name+a+b+c+d
        f += 1+name+a+b+c
        f += 2+name+a+b+d
        f += 3+name+a+c+d
        f += 4+name+b+c+d
      }
      addFourGramFeature(f, dir+"HNhPcC=",pPos,nextHeadPos,prevTokPos,tPos)
      addFourGramFeature(f, dir+"PhHPcC=", prevHeadPos, pPos, prevTokPos, tPos)
      addFourGramFeature(f, dir+"HNhCNc=", pPos, nextHeadPos, tPos, nextTokPos)
      addFourGramFeature(f, dir+"PhHCNc=", prevHeadPos, pPos, tPos, nextTokPos)
      val distance = math.abs(t.positionInSentence - p.positionInSentence)
      for (i <- 0 to distance) {
        f += dir+"EdgeLength>="+i
      }
      val normDistance = distance*10/ t.sentence.length
      for (i <- 0 to normDistance) {
        f += dir+"NormDistance>="+i
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
    parentScores(0) = weights.dot(getParentFeatureVector(null).value)
    for (i <- 0 until sentLength) {
      tokenScores(i+1) = weights.dot(getTokenFeatureVector(sent.tokens(i)).value)
      parentScores(i+1) = weights.dot(getParentFeatureVector(sent.tokens(i)).value)
    }
    val edgeScores = Array.fill(sent.length+1,sent.length+1)(Double.NaN)
    def getEdgeScore(parent: Int, child: Int) : Double = {
      assert(parent != child, "can't add an edge from a token to itself")
      if (edgeScores(parent)(child).isNaN) {
        val loss = if ((child > 0) && (knownParents(child-1) == parent -1)) -1.0 else 0.0
        edgeScores(parent)(child) = if (child > 0)
          loss + weights.dot(getPairwiseFeatureVector(sent.tokens(child - 1), if (parent > 0) sent.tokens(parent - 1) else null).value) + tokenScores(child) + parentScores(parent)
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
    val opt = new AdaMira(0.1) with ParameterAveraging // new cc.factorie.optimize.AdaGradRDA(1.0, 0.0, 0.0001, 0.0001)
    val trainer = new optimize.SynchronizedOptimizerOnlineTrainer(DependencyModel.parameters, opt, maxIterations = 10, nThreads = 1)
    for (iteration <- 0 until nIters) {
      trainer.processExamples(rng.shuffle(examples))
      opt match { case op: ParameterAveraging => op.setWeightsToAverage(DependencyModel.parameters) }
      val t0 = System.currentTimeMillis()
      println("Predicting train set..."); Threading.parForeach(trainSentences, nThreads) { s => parse(s) } // Was par
      println("Predicting test set...");  Threading.parForeach(testSentences, nThreads) { s => parse(s) } // Was par
      println("Processed in " + (trainSentences.map(_.tokens.length).sum+testSentences.map(_.tokens.length).sum)*1000.0/(System.currentTimeMillis()-t0) + " tokens per second")
      println("Training UAS = "+ ParserEval.calcUas(trainSentences.map(_.attr[ParseTree])))
      println(" Testing UAS = "+ ParserEval.calcUas(testSentences.map(_.attr[ParseTree])))
      opt match { case op: ParameterAveraging => op.unSetWeightsToAverage(DependencyModel.parameters) }
      println()
      //println("Saving model...")
      //if (file != "") serialize(file + "-iter-"+iteration)
    }
    println("Finished training.")
    opt.finalizeWeights(DependencyModel.parameters)
  }
  
  def test(testSentences:Iterable[Sentence]): (Double, Double, Double, Double) = {
    val t0 = System.currentTimeMillis()
    testSentences.foreach(parse)
    val totalTime = System.currentTimeMillis() - t0
    val totalTokens = testSentences.map(_.tokens.length).sum
    val totalSentences = testSentences.size
    val pred = testSentences.map(_.attr[ParseTree])
    (ParserEval.calcLas(pred), ParserEval.calcUas(pred), totalTokens*1000.0/totalTime, totalSentences*1000.0/totalTime)
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
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[pos.PennPosTag]) // TODO Also TokenLemma?  But we don't have a lemmatizer that matches the training data
  def postAttrs: Iterable[Class[_]] = List(classOf[ParseTree])
}

class WSJProjectiveGraphBasedParser(url:java.net.URL) extends ProjectiveGraphBasedParser(url)
object WSJProjectiveGraphBasedParser extends ProjectiveGraphBasedParser(ClasspathURL[WSJProjectiveGraphBasedParser](".factorie"))

class OntonotesProjectiveGraphBasedParser(url:java.net.URL) extends ProjectiveGraphBasedParser(url)
object OntonotesProjectiveGraphBasedParser extends ProjectiveGraphBasedParser(ClasspathURL[OntonotesProjectiveGraphBasedParser](".factorie"))

object ProjectiveGraphBasedParserOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions{
  val trainFile = new CmdOption("train", "", "FILENAME", "Training file.")
  val testFile =  new CmdOption("test", "", "FILENAME", "Testing file.")
  //val devFile =   new CmdOption("dev", Nil.asInstanceOf[String], "FILENAME", "CoNLL-2008 dev file")
  val trainDir = new CmdOption("trainDir", "", "FILENAME", "Directory containing training files.")
  val testDir = new CmdOption("testDir", "", "FILENAME", "Directory containing testing files.")
  val ontonotes = new CmdOption("onto", true, "BOOLEAN", "Whether training and testing files are in Ontonotes or CoNLL-2008 format")
  val model      = new CmdOption("model", "parser-model", "FILE", "File in which to save the trained model.")
  val nThreads   = new CmdOption("nThreads", Runtime.getRuntime.availableProcessors(), "INT", "Number of threads to use.")
}

object ProjectiveGraphBasedParserTrainer extends HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double = {
    val opts = ProjectiveGraphBasedParserOpts
    opts.parse(args)

    val parser = new ProjectiveGraphBasedParser

    assert(opts.trainFile.wasInvoked || opts.trainDir.wasInvoked)
    
    // Load the sentences
    def loadSentences(fileOpt: opts.CmdOption[String], dirOpt: opts.CmdOption[String]): Seq[Sentence] = {
      var fileExt = if (opts.ontonotes.value) ".dep.pmd" else ""
      var fileList = Seq.empty[String]
      if (fileOpt.wasInvoked) fileList = Seq(fileOpt.value)
      if (dirOpt.wasInvoked) fileList ++= FileUtils.getFileListFromDir(dirOpt.value, fileExt)
      fileList.flatMap(fname => (if (opts.ontonotes.value) load.LoadOntonotes5.fromFilename(fname) else load.LoadConll2008.fromFilename(fname)).head.sentences.toSeq)
    }

    val trainSentencesFull = loadSentences(opts.trainFile, opts.trainDir)
    //val devSentencesFull = loadSentences(opts.devFile, opts.devDir)
    val testSentencesFull = loadSentences(opts.testFile, opts.testDir)

    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)
    //val devSentences = devSentencesFull.take((testPortionToTake*devSentencesFull.length).floor.toInt)

    println("Total train sentences: " + trainSentences.size)
    println("Total test sentences: " + testSentences.size)
//    val trainDoc = load.LoadOntonotes5.fromFilename(opts.trainFile.value).head
//    val testDoc = load.LoadOntonotes5.fromFilename(opts.testFile.value).head
//
//    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 1.0
//    val testPortionToTake =  if(opts.testPortion.wasInvoked) opts.testPortion.value.toDouble  else 1.0
//    val trainSentencesFull = trainDoc.sentences.toSeq
//    val trainSentences = trainSentencesFull.take((trainPortionToTake*trainSentencesFull.length).floor.toInt)
//    val testSentencesFull = testDoc.sentences.toSeq
//    val testSentences = testSentencesFull.take((testPortionToTake*testSentencesFull.length).floor.toInt)


    // Train
    parser.train(trainSentences, testSentences, opts.model.value, math.min(opts.nThreads.value, Runtime.getRuntime.availableProcessors()))
    
    // Test
    println("Predicting train set...");
    val(trainLAS, trainUAS, trainTokSpeed, trainSentSpeed) = parser.test(trainSentences)
    println(s"Training UAS=${trainUAS}, LAS=${trainLAS}, ${trainTokSpeed} tokens/sec, ${trainSentSpeed} sentences/sec")
    println()
    
    println("Predicting test set...");
    val(testLAS, testUAS, testTokSpeed, testSentSpeed) = parser.test(testSentences)
    println(s"Training UAS=${testUAS}, LAS=${testLAS}, ${testTokSpeed} tokens/sec, ${testSentSpeed} sentences/sec")
    
    // Print accuracy diagnostics
    //println("Predicting train set..."); parser.process(trainDoc)
    //println("Predicting test set...");  parser.process(testDoc)
//    println("Training UAS = "+ ParserEval.calcUas(trainDoc.sentences.toSeq.map(_.attr[ParseTree])))
//    val testUAS = ParserEval.calcUas(testDoc.sentences.toSeq.map(_.attr[ParseTree]))
//    println("Testing UAS = "+ testUAS)
    println()
    println("Done.")
    if(opts.targetAccuracy.wasInvoked) cc.factorie.assertMinimalAccuracy(testUAS,opts.targetAccuracy.value.toDouble)

    testUAS
  }

}