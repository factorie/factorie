package cc.factorie.app.nlp.embeddings
import cc.factorie.la._
import scala.util.Random
import java.io.{FileReader, RandomAccessFile}

class WordReader(file: String) extends Iterator[String] {
  private var in = new FileReader(file)
  private var sb: StringBuilder = null
  moveToNext()

  private def moveToNext(): Unit = {
    sb = new StringBuilder()
    var c = in.read // read char
    // go inside only if c is bad char
    while (c != -1 && (c == '\n' || c == '\t' || c == ' ' || c == '\r')) c = in.read

    // go inside only if c is good char
    while (c != -1 && c != '\n' && c != '\t' && c != ' ' && c != '\r') {
      sb.+=(c.toChar) // add the good char
      c = in.read() //  read next char
    }
  }

  def hasNext(): Boolean = sb.length() > 0
  def next(): String = { moveToNext; sb.toString }

}
class LineReader(in: RandomAccessFile) extends Iterator[String] {
  //private var in = new FileReader(file)
  private var sb: StringBuilder = null
  moveToNext()

  private def moveToNext(): Unit = {
    sb = new StringBuilder()
    var c = in.read // read char
    // go inside only if c is bad char
    while (c != -1 && c == '\n') c = in.read

    // go inside only if c is good char
    while (c != -1 && c != '\n') {
      sb.+=(c.toChar) // add the good char
      c = in.read() //  read next char
    }
  }

  def hasNext(): Boolean = sb.length() > 0
  def next(): String = { moveToNext; sb.toString }

}
class FastLineReader(file: String, skipBytes: Long) extends Iterator[String] {
  //private var in = new FileReader(file)
  private var sb: StringBuilder = null
  private val in = new FileReader(file)
  in.skip(skipBytes)
  moveToNext()

  private def moveToNext(): Unit = {
    sb = new StringBuilder()
    var c = in.read // read char
    // go inside only if c is bad char
    while (c != -1 && c == '\n') c = in.read

    // go inside only if c is good char
    while (c != -1 && c != '\n') {
      sb.+=(c.toChar) // add the good char
      c = in.read() //  read next char
    }
  }

  def hasNext(): Boolean = sb.length() > 0
  def next(): String = { moveToNext; sb.toString }

}

object TensorUtils {
  val rng = new Random
  def cosineDistance(x: Tensor1, y: Tensor1): Double = {
    val xnorm = x.twoNorm
    val ynorm = y.twoNorm
    x.dot(y) / (xnorm * ynorm)

  }
  // random initialization is done in the same way as google's word2vec.
  def setToRandom1(t: DenseTensor1): DenseTensor1 = {
    for (i <- 0 until t.length)
      t(i) = (rng.nextInt(Int.MaxValue) / (Int.MaxValue - 1).toDouble) / t.length
    t
  }

}
