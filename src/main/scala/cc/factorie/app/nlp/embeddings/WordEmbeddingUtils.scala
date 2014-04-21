package cc.factorie.app.nlp.embeddings
import cc.factorie.la._
import scala.util.Random
import java.io.{FileReader, BufferedReader, FileInputStream, InputStreamReader}
import java.nio.charset.Charset
import java.util.zip.GZIPInputStream

class FastWordReader(file: String, encoding: String = "ISO-8859-15") extends Iterator[String] {
  private var in = file.endsWith(".gz") match {
    case false => new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
    case true => new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file)), encoding))
  }
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

class FastLineReader(file: String, skipBytes: Long = 0, encoding: String = "ISO-8859-15") extends Iterator[String] {
  //private var in = new FileReader(file)
  private var sb: StringBuilder = null
  private var line: String = null
  private var in = file.endsWith(".gz") match {
    case false => new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
    case true => new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file)), encoding))
  }
  //println("Default charset of this JVM : " + in.getEncoding() + ", so using " + in.getEncoding() +  " to read the input file")
  in.skip(skipBytes)
  moveToNext()

  private def moveToNext(): Unit = {
    sb = new StringBuilder()
    var c = in.read() // read char . Internally, read 1 byte or 2 byte or 3 bytes depending encoding.
    // go inside only if c is bad char
    while (c != -1 && c == '\n') c = in.read

    // go inside only if c is good char
    while (c != -1 && c != '\n') {
      sb.+=(c.toChar) // add the good char
      c = in.read() //  read next char
    } 
   // line = in.readLine()
  }

  def hasNext(): Boolean = {
    return sb.length() > 0
   // val ans = line !=  null && line.size > 0
   // println("line: " + line + " ans : " + ans)
   //if (ans == false) in.close()
   //  return ans
  }
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
  def setToRandom2(t : DenseTensor2): DenseTensor2 = {
      val V = t.dim1
      val D = t.dim2
      for (v <- 0 until V)
        for (d <- 0 until D) 
          t(v,d) = (rng.nextInt(Int.MaxValue) / (Int.MaxValue - 1).toDouble) / D
      t
  }

}

