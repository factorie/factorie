package cc.factorie.app.bib.hcoref

import cc.factorie.util.{EvaluatableClustering, DefaultCmdOptions}
import cc.factorie.app.nlp.hcoref._
import com.mongodb.{MongoClient, DB}
import cc.factorie.variable.{DiffList, Var}
import scala.io.Source
import scala.util.Random

/**
 * @author John Sullivan
 */
trait MongoOpts extends DefaultCmdOptions {
  val host = new CmdOption("host", "localhost", "HOSTNAME", "The hostname of the mongo db.")
  val port = new CmdOption("port", 27017, "PORT", "The port of the mongo db.")
  val database = new CmdOption("db-name", "", "STRING", "The name of the database to connect to", true)
}



object AuthorCoref {

  val opts = new AuthorModelOptions with MongoOpts {
    val embeddingFile = new CmdOption("embedding-file", "", "FILE", "The name of the embedding file to load.", true)
    val embeddingDim = new CmdOption("embedding-dim", 200, "INT", "The dimension of the embeddings")
  }

  def loadEmbeddings(filename:String, dim:Int):Map[String, Array[Double]] = {
    val src = Source.fromFile(filename)
    val embMap = src.getLines().map{ line =>
      val word :: embeddings = line.split("""\s+""").toList
      assert(embeddings.length == opts.embeddingDim.value, "Expected embedding of length %d, found %d for word %s".format(opts.embeddingDim.value, embeddings.length, word))
      word -> embeddings.map(_.toDouble).toArray
    }.toMap
    src.close()
    embMap
  }

  def main(args:Array[String]) {
    opts parse args
    val embeddingMap = loadEmbeddings(opts.embeddingFile.value, opts.embeddingDim.value)
    println("Loaded embedding map")
    val db = new MongoClient(opts.host.value, opts.port.value).getDB(opts.database.value)
    val coll = new MongoAuthorCollection(db, embeddingMap)
    println("setup db")
    val authors = coll.loadMentions
    println("loaded %d authors".format(authors.size))

    implicit val r = new Random()
    val model = AuthorCorefModel.fromCmdOptions(opts)

    val trainer = new CorefSampler[AuthorVars](model, authors.labeled, authors.size * 150)
      with AutoStoppingSampler[AuthorVars]
      with CanopyPairGenerator[AuthorVars]
      with NoSplitMoveGenerator[AuthorVars]
      with DebugCoref[AuthorVars]
      with TrainingObjective[AuthorVars] {
      def newInstance(implicit d: DiffList) = new Node[AuthorVars](new AuthorVars())

      val autoStopThreshold = 10000
    }
    trainer.train(100000)

    authors.toSingletons()
    println(trainer.model.parameters.tensors)


    val numSamples = 50 * authors.size
    println("beginning %d samples".format(numSamples))
    val sampler = new CorefSampler[AuthorVars](model, authors, numSamples)
      with AutoStoppingSampler[AuthorVars]
      with CanopyPairGenerator[AuthorVars]
      with NoSplitMoveGenerator[AuthorVars]
      //with DebugDiffListMoveGenerator[AuthorVars]
      with DebugCoref[AuthorVars] {
      val autoStopThreshold = 10000
      val logger = Logger.default

      def outerGetBagSize(n: Node[AuthorVars]) = n.variables.firstNames.size

      def newInstance(implicit d:DiffList) = new Node[AuthorVars](new AuthorVars(opts.embeddingDim.value))
    }

    sampler.infer

    println(EvaluatableClustering.evaluationString(authors.filter(_.variables.truth.size > 0).predictedClustering, authors.filter(_.variables.truth.size > 0).trueClustering))
  }
}
