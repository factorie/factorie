// package cc.factorie.app.nlp.ner

// import cc.factorie.util.{FileUtils, HyperparameterMain}
// import cc.factorie.app.nlp.{UnknownDocumentAnnotator, Token, Document}
// import scala.io.Source
// import java.io.{FileOutputStream, File}

// /**
//  * @author John Sullivan
//  */
// object TrainEventChainNer extends HyperparameterMain {

//   def fromFile(file: java.io.File, idx: Int): Document = {
//     val doc = new Document("").setName("BBN-"+idx)
//     doc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
//     //    val lines = Source.fromFile(file).getLines.filter(_.length > 0)
//     //    val lines2 = lines.filter(line => {!line.startsWith("-DOCSTART-")})
//     for (line <- Source.fromFile(file).getLines) {
//       val fields = line.split("\t")
//       if (fields.length == 2) {
//         val word = fields(0)
//         if (!word.startsWith("-DOCSTART-")) {
//           val tag = fields(1)
//           assert(tag.length > 0)
//           val token = new Token(doc, word)
//           token.attr += new LabeledBilouBBNEventNerTag(token, tag)
//         }
//       } else println("wrong # of fields: "+line)
//     }

//     val sentenceSegmenter = cc.factorie.app.nlp.segment.DeterministicSentenceSegmenter
//     val segDoc = sentenceSegmenter.process(doc)
//     for (token <- segDoc.tokens) {
//       if (token.nerTag == null){
//         //        println(s"${token.string} IS NULL")
//         //sentenceSegmenter removes nerTags from sentence delimiters apparently
//         token.attr += new LabeledBilouBBNEventNerTag(token, "O")
//       }
//     }
//     segDoc.asSection.chainFreeze
//     segDoc
//     //    doc.asSection.chainFreeze
//     //    doc
//   }

//   def fromDir(dirname:String):Seq[Document] = FileUtils.getFileListFromDir(dirname).zipWithIndex.map{ case (filename, idx) => fromFile(new File(filename), idx)}

//   def evaluateParameters(args: Array[String]) = {
//     val opts = new ChainNerOpts
//     opts parse args

//     val ner = new BBNEventChainNer(null)
//     val trainDocs = fromDir(opts.trainDir.value)
//     val testDocs = fromDir(opts.testDir.value)

//     val score = ner.train(trainDocs, testDocs, opts.rate.value, opts.delta.value)(new scala.util.Random(0))

//     if(opts.serialize.value) {
//       ner.serialize(new FileOutputStream(opts.saveModel.value))
//     }
//     score
//   }
// }

// object BBNEventTaggerOptimizer {
//   def main(args: Array[String]) {
//     val opts = new ChainNerOpts
//     opts.trainDir.setValue("/iesl/canvas/ksilvers/data/bbn-event-fixed/bbn-event-train")
//     opts.testDir.setValue("/iesl/canvas/ksilvers/data/bbn-event-fixed/bbn-event-test")
//     opts.saveModel.setValue("/iesl/canvas/sullivan/dev/factorie/BBNTagger_optimized.factorie")
//     opts.parse(args)

//     opts.serialize.setValue(false)

//     println("BBNEventTaggerOptimizer: using opts: ")
//     println(s"train-dir=${opts.trainDir.value} test-dir=${opts.testDir.value} save-model=${opts.saveModel.value}")

//     val rate = cc.factorie.util.HyperParameter(opts.rate, new cc.factorie.util.LogUniformDoubleSampler(1e-3, 1))
//     val delta = cc.factorie.util.HyperParameter(opts.delta, new cc.factorie.util.LogUniformDoubleSampler(1e-6, 1))
//     val qs = new cc.factorie.util.QSubExecutor(8, "cc.factorie.app.nlp.ner.TrainEventChainNer")
//     val optimizer = new cc.factorie.util.HyperParameterSearcher(opts, Seq(rate, delta), qs.execute, 20, 18, 60)
//     val result = optimizer.optimize()

//     println("Got results: "+result.mkString(" "))
//     opts.serialize.setValue(true)
//     println("Running best configuration...")
//     import scala.concurrent.duration._
//     import scala.concurrent.Await
//     Await.result(qs.execute(opts.values.flatMap(_.unParse).toArray), 5.hours)
//     println("Done")
//   }
// }


