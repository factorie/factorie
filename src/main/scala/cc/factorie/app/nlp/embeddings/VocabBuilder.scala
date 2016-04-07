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
import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream, OutputStreamWriter}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import cc.factorie.app._

import scala.util.Random


class VocabBuilder(vocab_hash_size: Int = 20e6.toInt, sampling_table_size: Int = 1e8.toInt, load_factor: Double = 0.7) {

  // Highly Optimized hash table using linear probing 
  private var vocab_max_size = 1000
  private var vocab_size = 0
  private var min_reduce = 1
  private val power: Double = 0.75

  // assign internal params 
  private val rng = new Random(5) // fix the seed
  private var train_words = 0
  private var prev_vocab_size = 0

  // allocate data structures
  private var vocab_hash = new Array[Int](vocab_hash_size) // vocab_hash stores pointer to the vocab array 
  private var vocab = new Array[vocab_word](vocab_max_size) //  vocab array holds the (word, cnt) pairs. In future, huffman encoding will be added. 
  private var sampling_table = new Array[Int](sampling_table_size)
  private var sub_sampling_table = new Array[Double](vocab_size)

  (0 until vocab_hash_size).foreach(i => vocab_hash(i) = -1)
  (0 until vocab_max_size).foreach(i => vocab(i) = null)

  def size(): Int = vocab_size
  def trainWords(): Long = {
    // compute only if the vocab has changed .
    if (prev_vocab_size != vocab_size) {
      train_words = 0
      vocab.foreach(word => train_words += word.cn)
      prev_vocab_size = vocab_size
    }
    train_words
  }

  def addWordToVocab(key: String): Unit = {
    val id = getId(key)
    if (id == -1) {
      vocab(vocab_size) = new vocab_word(1, key)
      vocab_size += 1
      // resize vocab array 
      if (vocab_size + 2 >= vocab_max_size) {
        vocab_max_size += 1000
        var vocab1 = new Array[vocab_word](vocab_max_size)
        (0 until vocab_size).foreach(i => vocab1(i) = vocab(i))
        (vocab_size until vocab_max_size).foreach(i => vocab1(i) = null)
        vocab = vocab1
      }
      var hash = get_word_hash(key)
      while (vocab_hash(hash) != -1) {
        hash = (hash + 1) % vocab_hash_size
      }
      vocab_hash(hash) = vocab_size - 1
    } else vocab(id).cn += 1
    // resize vocab_hash array  
    if (vocab_size > vocab_hash_size * load_factor) {
      reduce_vocab(min_reduce)
      min_reduce += 1
    }

  }

  def getCount(word: String): Int = {
    var hash = get_word_hash(word)
    var a = -1
    while (true) {
      a = vocab_hash(hash)
      if (a == -1) return -1
      if (vocab(a).wrd.equals(word)) return vocab(a).cn
      hash = (hash + 1) % vocab_hash_size
    }
    return -1
  }

  def getCount(id: Int): Int = {
    vocab(id).cn
  }

  def getId(word: String): Int = {
    var hash = get_word_hash(word)
    var a = -1
    while (true) {
      a = vocab_hash(hash)
      if (a == -1) return -1
      if (vocab(a).wrd.equals(word)) return a
      hash = (hash + 1) % vocab_hash_size
    }
    return -1
  }

  def getWord(id: Int): String = vocab(id).wrd

  // step-1 :first removes the word whose count < min_count , then sorts it
  // step-2 : removes the stop-words - stops words from factorie and word length should be greater than 1. Will ignore punctutations
  // step-3 : if vocab_size > max_vocab_size, then only take max_vocab_size 
  def sortVocab(min_count: Int = 5, ignoreStopWords: Int = 0, max_vocab_size: Int = 2e6.toInt): Unit = {
    for (a <- vocab_size until vocab.size) vocab(a) = null
    vocab = vocab.filter(ele => (ele != null && ele.cn >= min_count)).sortWith((x, y) => y.cn < x.cn)
    if (ignoreStopWords == 1) {
      vocab = vocab.filter(w => !nlp.lexicon.StopWords.containsWord(w.wrd)  && w.wrd.size > 1 )
    }   
    if (vocab.size > max_vocab_size)
      vocab = vocab.take(max_vocab_size)
    vocab_size = vocab.size
    rehash()

  }
  // Vocab IO . Default option of storing is plain txt 
  def saveVocab(filename: String, binary: Int = 0, encoding: String = "UTF8"): Unit = {
    var out = binary match {
      case 0 => new java.io.PrintWriter(filename, encoding)
      case 1 => new OutputStreamWriter(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(filename))), encoding)
    }
    for (i <- 0 until vocab_size) {
      out.write(vocab(i).wrd + " " + vocab(i).cn + "\n") // format : <word><space><count><newline>. TODO : JSON/XML in future ?
      assert(i == getId(vocab(i).wrd))
      out.flush()
    }
    out.close()
  }

  // load the vocab file 
  // format should be <word><space><count><newline>
  // will also figure out if filename is binary or not based on filetype
  def loadVocab(filename: String, encoding: String = "UTF8"): Unit = {
    var in = filename.endsWith(".gz") match {
      case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(filename)), encoding).getLines
      case false => io.Source.fromFile(filename, encoding).getLines
    }
    for (line <- in) {
      val wordCntdetail = line.stripLineEnd.split(' ')
      assert(wordCntdetail.size == 2)
      val word = wordCntdetail(0)
      val cnt = wordCntdetail(1).toInt
      addWordToVocab(word)
      vocab(getId(word)).cn = cnt // Assumption : vocab file does have duplicate words
    }
  }

  // Sampling Functions : same as Google's word2vec
  def buildSamplingTable(): Unit = {
    var i = 0
    var train_words_pow: Long = 0
    var running_word_proportion: Double = 0
    for (a <- 0 until vocab_size) {
      train_words_pow += math.pow(vocab(a).cn, power).toLong
      if (train_words_pow < 0) {
        println("train_words_pow went negative")
        println("ERROR: buildSamplingTable failed")
        return 
      }
    }
    i = 0
    running_word_proportion = math.pow(vocab(i).cn, power) / train_words_pow.toDouble
    for (a <- 0 until sampling_table_size) {
      sampling_table(a) = i
      if (a / sampling_table_size.toDouble > running_word_proportion) {
        i += 1
        running_word_proportion += math.pow(vocab(i).cn, power) / train_words_pow.toDouble
      }
      if (i >= vocab_size) i = vocab_size - 1
    }
  }
  // Sub Sampling : same as Google's word2vec
  def buildSubSamplingTable(sample: Double): Unit = {
    trainWords()
    if (sample > 0) {
      sub_sampling_table = new Array[Double](vocab_size)
      for (a <- 0 until vocab_size) {
        val cnt = vocab(a).cn
        val ran = (math.sqrt(cnt / (sample * train_words)) + 1) * (sample * train_words) / cnt
        sub_sampling_table(a) = ran
      }
    }
  }
  
  def getRandWordId(): Int = sampling_table(rng.nextInt(sampling_table_size))
  def getRandWord(): String = vocab(getRandWordId).wrd
  def getSubSampleProb(id: Int): Double = sub_sampling_table(id)

  // TODO huffman encoding for Hierarchical SoftMax: def BuildBinaryTree = { } 

  // helper functions 
  // reduce_vocab is hacky function which makes sure vocab can handle any kind of streamming words
  private def reduce_vocab(min_reduce: Int): Unit = {
    var a = 0
    var b = 0
    // why not use vocab.filter() ? ans : filter would shrink the vocab array. We dont want that .
    // keep only those words whose cnt > min_reduce
    for (a <- 0 until vocab_size) if (vocab(a).cn > min_reduce) {
      vocab(b) = vocab(a)
      b += 1
    } else vocab(a) = null
    vocab_size = b
    // now, rehash
    rehash()

  }
  // hash function for the string
  private def get_word_hash(word: String): Int = {
    var hash: Long = 0 // made Long to avoid overflow
    for (ch <- word)
      hash = (hash * 257 + ch) % vocab_hash_size
    hash = hash % vocab_hash_size
    return hash.toInt
  }

  private def rehash(): Unit = {
    (0 until vocab_hash_size).foreach(i => vocab_hash(i) = -1)
    for (a <- 0 until vocab_size) {
      var hash = get_word_hash(vocab(a).wrd)
      while (vocab_hash(hash) != -1) hash = (hash + 1) % vocab_hash_size
      vocab_hash(hash) = a
    }
  }

}

class vocab_word(cnt: Int = 0, w: String) {
  var cn = cnt
  var wrd = w
  override def toString() = "( " + cn + ", " + wrd + " ) "
}
