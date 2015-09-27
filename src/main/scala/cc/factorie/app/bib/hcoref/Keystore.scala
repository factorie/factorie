package cc.factorie.app.bib.hcoref

import java.io.File

import cc.factorie.util.{DefaultCmdOptions, VectorUtils}

import scala.collection.JavaConverters._
import scala.io.Source

/**
 * @author John Sullivan
 */
trait Keystore {
  def dimensionality:Int
  def retrieve(key:String):Option[Array[Double]]

  val missingKeys = new java.util.concurrent.ConcurrentHashMap[String, Int]().asScala.withDefaultValue(0)

  import VectorUtils._
  def generateVector(keys:Iterable[String]):Array[Double] = keys.flatMap{ key =>
    val res = retrieve(key)
    if(res.isEmpty) {
      missingKeys += key -> (missingKeys(key) + 1)
    }
    res
  }.foldLeft(new Array[Double](dimensionality)){case (tot, arr) => tot += arr; tot}
}

trait InMemoryHashMapKeystoreOpts extends DefaultCmdOptions {
  val keystorePath = new CmdOption("embedding-file", "", "FILE", "The file which contains the embeddings", true)
  val keystoreDim = new CmdOption("embedding-dim", 200, "INT", "The number of dimensions in the embedding")
  val keystoreDelim = new CmdOption("embedding-delim", " ", "DELIMITER", "The delimiter between fields in the embedding file")
}

object InMemoryHashMapKeystore {
  def fromOpts(opts:InMemoryHashMapKeystoreOpts):InMemoryHashMapKeystore = new InMemoryHashMapKeystore(new File(opts.keystorePath.value), opts.keystoreDim.value, opts.keystoreDelim.value)
}

class InMemoryHashMapKeystore(embeddingFile:File, val dimensionality:Int, fileDelimiter:String = " ") extends Keystore {

  val src = Source.fromFile(embeddingFile)
  private val store = src.getLines().map { line =>
    val key :: vec = line.split(fileDelimiter).toList
    assert(vec.length == dimensionality)
    key -> vec.map(_.toDouble).toArray
  }.toMap
  src.close()

  def retrieve(key: String) = store.get(key)
}
