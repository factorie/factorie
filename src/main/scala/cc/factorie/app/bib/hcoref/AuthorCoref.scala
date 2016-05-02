/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.bib.hcoref

import cc.factorie.app.nlp.hcoref._
import cc.factorie.util.{DefaultCmdOptions, EvaluatableClustering}
import cc.factorie.variable.DiffList
import com.mongodb.MongoClient

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

  val opts = new AuthorModelOptions with MongoOpts with InMemoryHashMapKeystoreOpts

  def main(args:Array[String]) {
    opts parse args
    val embeddingMap = InMemoryHashMapKeystore.fromOpts(opts)
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
      with TrainingObjective[AuthorVars]
      with PrintlnLogger {
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
      with DebugCoref[AuthorVars]
      with PrintlnLogger{
      val autoStopThreshold = 10000

      def outerGetBagSize(n: Node[AuthorVars]) = n.variables.firstNames.size

      def newInstance(implicit d:DiffList) = new Node[AuthorVars](new AuthorVars(opts.keystoreDim.value))
    }

    sampler.infer

    println(EvaluatableClustering.evaluationString(authors.filter(_.variables.truth.size > 0).predictedClustering, authors.filter(_.variables.truth.size > 0).trueClustering))

    println("Inserting rows")
    coll.updateCollection(authors.filter(a => a.exists && !AuthorNodeCubbie.deletions.contains(a.uniqueId)))
  }


  def runOnCanopy(ncs:Iterable[AuthorNodeCubbie], opts:AuthorModelOptions):Iterable[AuthorNodeCubbie] = {
    val mentions = ncs.map{ nc =>
      if(nc.isMention.value) {
        new Mention[AuthorVars](AuthorVars.fromNodeCubbie(nc), nc.id.toString)(null)
      } else {
        new Node[AuthorVars](AuthorVars.fromNodeCubbie(nc), nc.id.toString)(null)
      }
    }

    implicit val rand = new Random()
    val model = AuthorCorefModel.fromCmdOptions(opts)
    val sampler = new CorefSampler[AuthorVars](model, mentions, mentions.size * 50)
      with AutoStoppingSampler[AuthorVars]
      with CanopyPairGenerator[AuthorVars]
      with NoSplitMoveGenerator[AuthorVars]
      with DebugCoref[AuthorVars]
      with TrainingObjective[AuthorVars]
      with PrintlnLogger {
      def newInstance(implicit d: DiffList) = new Node[AuthorVars](new AuthorVars())

      val autoStopThreshold = 10000
    }

    sampler.infer

    sampler.entities.filter(_.exists).map(AuthorNodeCubbie.fromNode)
  }
}
