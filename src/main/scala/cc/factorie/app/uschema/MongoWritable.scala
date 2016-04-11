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
package cc.factorie.app.uschema

import com.mongodb.DB

/**
 * Created by beroth on 3/9/15.
 */
trait MongoWritable {
  def writeToMongo(db: DB)
  def populateFromMongo(db: DB)
  // Usage: new mongoW; mongoW.populateFromMongo(db)
}

object MongoWritable {
  // Some constants that should be used for specifying the collection names MongoWritables are written to.
  // Prefixes are passed via the collectionPrefix field.
  // The writeToMongo method may choose to write to more collections, concatenating the prefix with some of the suffixes.
  // TOD: ?? A suffix can be either passed as an argument to writeToMongo, or set programatically.

  // Prefixes must not contain the underscore "_" symbol.
  val ENTITY_ROW_MAP_PREFIX = "entityRowMap"
  val ENTITY_COL_MAP_PREFIX = "entityColMap"
  val COOC_MATRIX_PREFIX = "coocMatrix"

}