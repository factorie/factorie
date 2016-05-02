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
package cc.factorie.app.uschema.tac

import com.mongodb.{DB, MongoClient}
import cc.factorie.app.uschema._

/**
 * Created by beroth on 2/11/15.
 */

class LoadTacDataOptions extends cc.factorie.util.DefaultCmdOptions {
  val tacData = new CmdOption("tac-data", "", "FILE", "tab separated file with TAC training data")
  val mongoHost = new CmdOption("mongo-host","localhost","STRING","host with running mongo db")
  val mongoPort = new CmdOption("mongo-port", 27017, "INT", "port mongo db is running on")
  val dbname = new CmdOption("db-name", "tac", "STRING", "name of mongo db to write data into")
}

object LoadTacDataIntoMongo {

  val opts = new LoadTacDataOptions

  def main(args: Array[String]) : Unit = {
    opts.parse(args)

    val tReadStart = System.currentTimeMillis
    val kb = EntityRelationKBMatrix.fromTsv(opts.tacData.value)
    val tRead = (System.currentTimeMillis - tReadStart)/1000.0
    println(f"Reading from file took $tRead%.2f s")

    println("Stats:")
    println("Num Rows:" + kb.numRows())
    println("Num Cols:" + kb.numCols())
    println("Num cells:" + kb.nnz())

    val tWriteStart = System.currentTimeMillis
    val mongoClient = new MongoClient( opts.mongoHost.value , opts.mongoPort.value )
    val db:DB = mongoClient.getDB( opts.dbname.value )
    //kb.writeToMongoCellBased(db)
    kb.writeToMongo(db)
    val tWrite = (System.currentTimeMillis - tWriteStart)/1000.0
    println(f"Writing to mongo took $tWrite%.2f s")

    val tReadMongoStart = System.currentTimeMillis
    //val kb2 = KBMatrix.fromMongoCellBased(db)
    val kb2 = new EntityRelationKBMatrix
    kb2.populateFromMongo(db)
    val tReadMongo = (System.currentTimeMillis - tReadMongoStart)/1000.0
    println(f"Reading from mongo took $tReadMongo%.2f s")

    val tCompareStart = System.currentTimeMillis
    val haveSameContent = kb.hasSameContent(kb2)
    val tCompare = (System.currentTimeMillis - tCompareStart)/1000.0
    println(f"Comparison of kbs took $tCompare%.2f s")

    if (haveSameContent) {
      println("OK: matrix in mongo has same content as matrix on disk")
    } else {
      println("FAILURE: matrix in mongo has different content as matrix on disk")
      println("Stats for mongo matrix:")
      println("Num Rows:" + kb2.numRows())
      println("Num Cols:" + kb2.numCols())
      println("Num cells:" + kb2.nnz())
    }

    val tPrune0Start = System.currentTimeMillis
    val prunedMatrix0 = kb.prune(0,0)
    val tPrune0 = (System.currentTimeMillis - tPrune0Start)/1000.0
    println(f"pruning with threshold 0,0 took $tPrune0%.2f s")
    println("Stats of pruned matrix:")
    println("Num Rows:" + prunedMatrix0.numRows())
    println("Num Cols:" + prunedMatrix0.numCols())
    println("Num cells:" + prunedMatrix0.nnz())

    val tPrune1Start = System.currentTimeMillis
    val prunedMatrix1 = kb.prune(1,1)
    val tPrune1 = (System.currentTimeMillis - tPrune1Start)/1000.0
    println(f"pruning with threshold 1,1 took $tPrune1%.2f s")
    println("Stats of pruned matrix:")
    println("Num Rows:" + prunedMatrix1.numRows())
    println("Num Cols:" + prunedMatrix1.numCols())
    println("Num cells:" + prunedMatrix1.nnz())

    val tPrune2Start = System.currentTimeMillis
    val prunedMatrix2 = kb.prune(2,2)
    val tPrune2 = (System.currentTimeMillis - tPrune2Start)/1000.0
    println(f"pruning with threshold 2,2 took $tPrune2%.2f s")
    println("Stats of pruned matrix:")
    println("Num Rows:" + prunedMatrix2.numRows())
    println("Num Cols:" + prunedMatrix2.numCols())
    println("Num cells:" + prunedMatrix2.nnz())

    val tPrune3Start = System.currentTimeMillis
    val prunedMatrix3 = kb.prune(2,1)
    val tPrune3 = (System.currentTimeMillis - tPrune3Start)/1000.0
    println(f"pruning with threshold 2,1 took $tPrune3%.2f s")
    println("Stats of pruned matrix:")
    println("Num Rows:" + prunedMatrix3.numRows())
    println("Num Cols:" + prunedMatrix3.numCols())
    println("Num cells:" + prunedMatrix3.nnz())

  }
}
