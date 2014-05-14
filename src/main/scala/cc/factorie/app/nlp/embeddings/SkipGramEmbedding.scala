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
package cc.factorie.app.nlp.embeddings

import cc.factorie.util.ClasspathURL
import cc.factorie.la
import java.util.zip.GZIPInputStream

object SkipGramEmbedding extends SkipGramEmbedding(s => ClasspathURL.fromDirectory[SkipGramEmbedding](s).openConnection().getInputStream, 100)

class SkipGramEmbedding(val inputStreamFactory: String=> java.io.InputStream, dimensionSize: Int) extends scala.collection.mutable.LinkedHashMap[String,la.DenseTensor1] {
  def sourceFactory(string:String): io.Source = io.Source.fromInputStream(new GZIPInputStream(inputStreamFactory(string)))

  println("Embedding reading size: %d".format(dimensionSize))

  initialize()
  def initialize() {
    val source = sourceFactory("skip-gram-d100.W.gz")
    var count = 0
    for (line <- source.getLines()) {
      val fields = line.split("\\s+")
      val tensor = new la.DenseTensor1(fields.drop(1).map(_.toDouble))
      assert(tensor.dim1 == dimensionSize)
      this(fields(0)) = tensor
      count += 1
      if (count % 100000 == 0) println("word vector count: %d".format(count))
    }
    source.close()
  }

  def close(string:String): Seq[String] = {
    val t = this(string)
    if (t eq null) return Nil
    val top = new cc.factorie.util.TopN[String](10)
    for ((s,t2) <- this) top.+=(0, t.dot(t2), s)
    top.map(_.category)
  }
}