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