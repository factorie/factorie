///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie.app.nlp.pos
//
//import cc.factorie._
//import cc.factorie.util._
//import cc.factorie.la._
//import cc.factorie.optimize._
//import cc.factorie.app.nlp._
//import cc.factorie.app.chain.ChainModel
//
///** Part-of-speech tagging with training by BP in the linear-chains. */
//class POS2 extends Infer with util.FastLogging {
//  
//  object PosFeaturesDomain extends CategoricalTensorDomain[String]
//  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain }
//
//  class PosModel extends ChainModel(
//      PosDomain,
//      PosFeaturesDomain,
//      labelToFeatures = (p: PosLabel) => p.token.attr[PosFeatures], // TODO: this is a little rough -brian
//      labelToToken = (p: PosLabel) => p.token,
//      tokenToLabel = (t: Token) => t.posLabel)
//  object model extends PosModel
//      
//  var useSentenceBoundaries = false
//  
//  def initPosAttr(sentenceTokens:Seq[Token]): Unit = {
//    if (sentenceTokens.length < 1 || sentenceTokens.head.attr[PosFeatures] != null) return
//    var addedPosFeatures = false
//    for (token <- sentenceTokens) {
//      if (token.attr[PosLabel] eq null) token.attr += new PosLabel(token, PosDomain.category(0)) // initial value doesn't matter
//      if (token.attr[PosFeatures] eq null) {
//        addedPosFeatures = true
//        val rawWord = token.string
//        val word = cc.factorie.app.strings.simplifyDigits(rawWord)
//        val features = token.attr += new PosFeatures(token)
//        features += "W="+word.toLowerCase
//        features += "SHAPE="+cc.factorie.app.strings.stringShape(rawWord, 2)
//        features += "SUFFIX3="+word.takeRight(3)
//        features += "PREFIX3="+word.take(3)
//        if (token.isPunctuation) features += "PUNCTUATION"
//      }
//    }
//    if (addedPosFeatures)
//      cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions(sentenceTokens, (t:Token)=> t.attr[PosFeatures], List(1), List(-1))
//  }
//  def initPosAttr(document:Document): Unit = initPosAttr(document.tokens) // TODO Note that this always ignores Sentence boundaries, because Observation.addNeighborFeatureConjunctions is broken in this regard.
//
//  def train(
//      trainDocuments:Iterable[Document],
//      testDocuments:Iterable[Document],
//      modelPrefix: String = "pos-model",
//      numIterations: Int = 100): Unit = {
//    
//    val examples = {
//      for (document <- trainDocuments; sentence <- document.sentences.filter(_.tokens.size > 1)) yield 
//        ChainModel.createChainExample(sentence.tokens.map(_.attr[PosLabel]))
//    }
//    val trainer = new BatchTrainer(model, new ConstantLearningRate)
//    
//    var iteration = 0
//    while (!trainer.isConverged && iteration < numIterations) {
////      for ((exampleBatch, batchNumber) <- examples.grouped(1000).zipWithIndex) {
////        println("[Training] batch number: " + batchNumber)
////	    trainer.processExamples(exampleBatch)
////      }
//      trainer.processExamples(examples)
//      printAccuracy("Train", trainDocuments)
//      printAccuracy("Test ", testDocuments)
//      println("---------------------------")
//      model.serialize("pos-wsj-model-iter=" + iteration)
//      iteration += 1
//    }
//    logger.info("FINAL")
//    printAccuracy("Train", trainDocuments)
//    printAccuracy("Test ", testDocuments)
//  }
//  
//  def printAccuracy(msg:String, documents:Iterable[Document]): Unit = {
//    for ((sentenceBatch, i) <- documents.flatMap(_.sentences).grouped(1000).zipWithIndex) {
//      println("[Infer] batch number: " + i)
//      for (sentence <- sentenceBatch) {
//	    val summary = model.inferByMaxProduct(sentence.tokens.map(_.posLabel))
//	    summary.setToMaximize(null)
//      }
//    }
//    logger.info(msg+" token accuracy = "+HammingObjective.accuracy(documents.flatMap(_.tokens.map(_.attr[PosLabel]))))
//  }
//
//  /** Predict the part-of-speech tag of the words in the document, and store it in attr[PosLabel] */
//  def apply(document:Document): Unit = {
//    initPosAttr(document)
//    if (useSentenceBoundaries) 
//      for (sentence <- document.sentences) { 
//        val summary = model.inferByMaxProduct(sentence.map(_.attr[PosLabel]))
//        summary.setToMaximize(null) 
//      }
//    else {
//      val summary = model.inferByMaxProduct(document.tokens.map(_.attr[PosLabel]))
//      summary.setToMaximize(null)
//    }
//  }
//  
//  def apply(labels:Seq[PosLabel]): BPSummary = {
//    initPosAttr(labels.map(_.token))
//    BP.inferChainMax(labels, model)
//  }
//  
//  override def infer(variables:Iterable[Var], model:Model, summary:Summary[Marginal] = null): Option[Summary[Marginal]] = variables match {
//    case labels:Seq[PosLabel] if (labels.forall(_.isInstanceOf[PosLabel])) => Some(apply(labels))
//    case _ => None
//  }
//
//  // Add run as server
//}
//
//
//// For example:
//// POS1 --train /Users/mccallum/research/data/ie/ner2003/eng.train --test /Users/mccallum/research/data/ie/ner2003/eng.testa --model pos.fac
//// POS1 --model pos.fac --run ~/research/data/text/nipstxt/nips11/0620.txt
//object POS2 extends POS2 {
//  def main(args: Array[String]): Unit = {
//    object opts extends cc.factorie.util.DefaultCmdOptions {
//      val trainFile =    new CmdOption("train", "eng.train", "FILE", "CoNLL 2003 format file from which to get training data.")
//      val testFile =     new CmdOption("test", "eng.testa", "FILE", "CoNLL 2003 format file from which to get testing data.")
//      val modelPrefix =  new CmdOption("model", "pos", "DIR", "Directory in which to save the trained model.")
//      val runFiles =     new CmdOption("run", List("input.txt"), "FILE...", "Plain text files from which to get data on which to run.")
//      val owpl =         new CmdOption("owpl", false, "", "")
//      val onto =         new CmdOption("onto", false, "", "")
//      val numIterations =new CmdOption("iterations", 100, "", "")
//    }
//    opts.parse(args)
//    if (opts.trainFile.wasInvoked) train()
//    else if (opts.runFiles.wasInvoked) run()
//    else throw new Error("Must use either --train or --run.")
//        
//    def train(): Unit = {
//      // Read in the data
//      val loader: (String) => Seq[Document] = {
//        if (opts.owpl.wasInvoked) LoadOWPL.fromFilename(_, (t: Token, l: String) => new PosLabel(t, l)) 
//        else if (opts.onto.wasInvoked) LoadOntonotes5.fromFilename(_) 
//        else LoadConll2003.fromFilename(_)
//      }
//        
//      val trainDocuments = loader(opts.trainFile.value) // .take(10)
//      val testDocuments =  loader(opts.testFile.value)  //.take(10)
//
//      // Add features for POS
//      trainDocuments.foreach(initPosAttr(_))
//      testDocuments.foreach(initPosAttr(_))
//      //println("Example Token features")
//      //println(trainDocuments(3).tokens.take(10).map(_.attr[PosFeatures].toString).mkString("\n"))
//      println("Num TokenFeatures = "+PosFeaturesDomain.dimensionSize)
//      
//      this.train(trainDocuments, testDocuments, modelPrefix = opts.modelPrefix.value, numIterations = opts.numIterations.value)
//    
//      model.serialize(opts.modelPrefix.value)
//    }
//    
//    def run(): Unit = {
//      model.deSerialize(opts.modelPrefix.value)
//      for (filename <- opts.runFiles.value) {
//        val document = new Document(io.Source.fromFile(filename).getLines.mkString("\n"))
//        segment.Tokenizer.process(document)
//        segment.SentenceSegmenter.process(document)
//        initPosAttr(document)
//        apply(document)
//        for (token <- document.tokens)
//          println("%s %s".format(token.string, token.attr[PosLabel].categoryValue))
//      }
//    }
//  }
//
//  
//}