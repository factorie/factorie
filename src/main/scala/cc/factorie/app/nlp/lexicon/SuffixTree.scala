package cc.factorie.app.nlp.lexicon

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.io.Source
import java.io.File
import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer}
import cc.factorie.app.chain.Observation

/**
 * Created by kate on 5/20/14.
 */

//FIXME doesnt seem to deal well with weird chars e.g. "André Gide"

/** not actually a SuffixTree, based on Jinho Choi/ClearNLP's "AffixTree.java". goal here is given a query P and a
set of words S, want O(P) time lookups.
 * params:
 * prefix - passing "prefix" in as true means we build/lookup based on phrase "prefixes", not "suffixes"
  * but that is probably not relevant to our lives
  * TODO get rid of "prefix" arg -- probably don't need it here
  * */
class SuffixTree(prefix: Boolean) {
  var root = new SuffixNode()
  var size = 1

  def add(strings: Seq[String]): Unit = {
    var beginIndex, direction, len = strings.length
    if (prefix){ beginIndex = 0; direction = 1 }
    else {beginIndex = len - 1; direction = -1; len = 0}
    var curr = root
    for (i <- beginIndex to len by direction) {
      if (!curr.contains(strings(i))) {
        curr.put(strings(i), new SuffixNode())
        size += 1
      }
      curr = curr.get(strings(i))
    }
    curr.setEndState(true)
  }

  /* TODO get rid of "min" argument -- probably don't need it.
  For now, setting "min" to true means that we stop searching as soon as the first element
  of "strings" is found in the tree */
  def getSuffixIndex(strings: Seq[String], min: Boolean): Int = {
    var beginIndex, direction, index = -1
    var len = strings.length
    var curr = root
    if (prefix){ beginIndex = 0; direction = 1 }
    else {beginIndex = len - 1; direction = -1; len = 0}

    for (i <- beginIndex to len by direction) {
      if (!curr.contains(strings(i))) return index
      curr = curr.get(strings(i))
      if (curr.isEndState) {
        index = i
        if (min) return index
      }
    }
    index
  }

  def contains(strings: Seq[String]): Boolean = {
    getSuffixIndex(strings, false) == 0
  }

  override def toString(): String = {
    s"<SuffixTree $size> ${root.toString()}"
  }
}

class SuffixNode {
  var endState: Boolean = false
  val contents = new HashMap[String, SuffixNode]
  def get(s: String): SuffixNode = { contents.getOrElse(s, null) }
  def put(s: String, n: SuffixNode): Unit = { contents.put(s, n) }
  def setEndState(b: Boolean): Unit = {endState = b}
  def isEndState: Boolean = endState
  def contains(s: String): Boolean = contents.contains(s)
  override def toString(): String = {
    var st = ""
    contents.keys.foreach(k => {
      st += s"[ $k ] --> ${contents(k).toString()} \n"
    })
    st
  }
}

//class HashyLexicon(val name: String, val tokenizer: StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer: Lemmatizer = LowercaseLemmatizer) extends MutableLexicon {
//  def this(file: File) = { this(file.toString, cc.factorie.app.strings.nonWhitespaceSegmenter, LowercaseLemmatizer); this.++=(Source.fromFile(file)(scala.io.Codec.UTF8))}
//
//  val allwords = new ListBuffer[String]
//  val wordTree = new SuffixTree(false)
//
//  def +=(phrase:String): Unit = {
//    allwords += phrase
//    val words: Seq[String] = tokenizer(phrase).toSeq
//    wordTree.add(words.map(lemmatizer.lemmatize(_)))
//  }
//
//  /* blah */
//  def containsLemmatizedWord(word: String): Boolean = {
//    containsLemmatizedWords(List(word).toSeq) //excuse my scala
//  }
//
//  /* checks whether the lexicon contains this already lemmatized/tokenized phrase (ie multi-word expr or single word) */
//  def containsLemmatizedWords(words: Seq[String]): Boolean = {
//    wordTree.contains(words)
//  }
//
//  def contains[T<:Observation[T]](query: Seq[T]): Boolean = {
//    val lem = query.map(_.string)
//    containsLemmatizedWords(lem.map(lemmatizer.lemmatize(_)).toSeq)
//  }
//
//  /* tokenize/lemmatize T.string first, then check if it's in the lexicon */
//  def contains[T<:Observation[T]](query: T): Boolean = {
//    val tokenized = tokenizer(query.string).toSeq
//    val lemmatized = tokenized.map(lemmatizer.lemmatize(_))
//    containsLemmatizedWords(lemmatized)
//  }
//
//  override def toString(): String = { "<ExplicitLex with "+allwords.length+" words>" }
//
//  /* print all the words contained in the lexicon */
//  def printWords(): Unit = { allwords.foreach(println) }
//
//  /* print the lexicon's suffix tree */
//  def printTree(): Unit = { print(wordTree.toString().slice(0,200)) }
//}
//
///* a union of many HashyLexicons, in the style of lexicon.UnionLexicon */
//class HashyUnionLexicon(val name: String, val members: HashyLexicon*) extends MutableLexicon {
//  def tokenizer: StringSegmenter = members.head.tokenizer
//  def lemmatizer: Lemmatizer = members.head.lemmatizer
//  def containsLemmatizedWord(word: String): Boolean = members.exists(_.containsLemmatizedWord(word))
//  def containsLemmatizedWords(word: Seq[String]): Boolean = members.exists(_.containsLemmatizedWords(word))
//  def contains[T<:Observation[T]](query: T): Boolean = members.exists(_.contains(query))
//  def contains[T<:Observation[T]](query: Seq[T]): Boolean = members.exists(_.contains(query))
//  def +=(s:String): Unit = {throw new Error("method not implemented for HashyUnionLexicon")}
//  override def toString(): String = {
//    var st = "UNION { "
//    members.foreach(st += _.toString()+" , ")
//    st += " } "
//    st
//  }
//}
