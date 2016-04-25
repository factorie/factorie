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
package cc.factorie.app.nlp.embeddings
import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.util.zip.GZIPInputStream

import cc.factorie.la._

import scala.util.Random

class FastWordReader(file: String, encoding: String = "UTF8") extends Iterator[String] {
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

class FastLineReader(file: String, skipBytes: Long = 0, encoding: String = "UTF8") extends Iterator[String] {
  //private var in = new FileReader(file)
  private var sb: StringBuilder = null
  private var line: String = null
  private var in = file.endsWith(".gz") match {
    case false => new BufferedReader(new InputStreamReader(new FileInputStream(file), encoding))
    case true => new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file)), encoding))
  }
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
  }

  def hasNext(): Boolean = {
    return sb.length() > 0
  }
  def next(): String = { moveToNext; sb.toString }

}

object TensorUtils {
  val rng = new Random(5) // fix the seed
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

