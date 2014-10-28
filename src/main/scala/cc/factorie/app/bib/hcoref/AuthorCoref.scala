package cc.factorie.app.bib.hcoref

import cc.factorie.util.DefaultCmdOptions
import cc.factorie.app.nlp.hcoref._
import com.mongodb.{MongoClient, DB}
import cc.factorie.variable.{DiffList, Var}

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
    val fieldList = new CmdOption("field-list", List("",""), "LIST[FIELD]", "The list of fieldnames to deserialize from mongo.")
  }

  def main(args:Array[String]) {
    opts parse args
    val db = new MongoClient(opts.host.value, opts.port.value).getDB(opts.database.value)
    val coll = new MongoAuthorCollection(opts.fieldList.value, db)
    val authors = coll.loadAll
    println("loaded %d authors".format(authors.size))

    val model = AuthorCorefModel.fromCmdOptions(opts)
    val numSamples = 300 * authors.size
    println("beginning %d samples".format(numSamples))
    val sampler = new CorefSampler[AuthorVars](model, mentions, numSamples)
      with AutoStoppingSampler[AuthorVars]
      with CanopyPairGenerator[AuthorVars]
      with NoSplitMoveGenerator[AuthorVars]
      with DebugCoref[AuthorVars] {
      val autoStopThreshold = 10000
      val logger = Logger.default

      def newInstance(implicit d:DiffList) = new Node[AuthorVars](new AuthorVars())
    }

    sampler.infer
  }
}
