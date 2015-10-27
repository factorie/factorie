/*
 * This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
 * http://factorie.cs.umass.edu, http://github.com/factorie
 * Licensed to the University of Massachusetts Amherst (UMass) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * UMass licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Copyright C 2014, 2015, Oracle and/or its affiliates. All rights reserved.
 */

package cc.factorie.app.nlp.lexicon

import java.io.Serializable

import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.lemma.Lemmatizer
import cc.factorie.util.{JavaHashSet, Logger}
import cc.factorie.variable.CategoricalVectorVar

import scala.collection.mutable.Set

/**
 * An implementation of the Aho-Corasick (1975) string matching automaton.
 */
class AhoCorasick(val sep : String) extends Serializable {
  val root : TrieNode = new TrieNode(sep)
  @transient private var constructed : Boolean = false
  
  /** Construct an instance from a Seq of phrases. */
  def this(sep : String, lexicon : Seq[Seq[String]]) = { this(sep); this ++= lexicon }

  /** Accessor method for constructed. */
  def isConstructed() : Boolean = { constructed }
  
  /**
   * Checks if the input phrase appears exactly in the lexicon.
   * This just treats the lexicon as a Trie, without using the fail transitions.
   */
  def findExactMention(input : Seq[String]) : Boolean = {
    if (!constructed) {
      setTransitions()
    }
    var i = 0
    var curNode = root
    var found = true
    //Iterate through the Trie testing to see if the next token exists
    while ((i < input.length) && (found)) {
      val head = input(i)
      val next = curNode.lookupToken(head)
      if (next != None) {
        curNode = next.get
        i = i + 1
      } else {
        //failed to find the next transition
        found = false
      } 
    }
    //if we reached the end of the input stream
    if (found) {
      //check if the current node should emit and if the output matches the input
      if (!curNode.getExactEmit) {
        found = false
      }
    }
    found
  }
  
  /** Finds all mentions of the trie phrases in a tokenized input text. */
  def findMentions(input : Seq[String]) : Set[LexiconMention] = {
    if (!constructed) {
      setTransitions()
    }
    val mentions : Set[LexiconMention] = JavaHashSet[LexiconMention]()
    var i = 0
    var curNode = root
    while (i < input.length) {
      val head : String = input(i)
      val next = curNode.lookupToken(head)
      //logger.log(Logger.INFO)("Head = " + head + ", idx = " + i + ", label = " + curNode.label + " next = " + next.toString())
      if (next != None) {
        curNode = next.get
        i = i + 1
      } else if (curNode != root) {
        curNode = curNode.failNode
      } else {
        curNode = curNode.failNode
        i = i + 1
      }
      if (curNode.getEmit()) {
        //logger.log(Logger.INFO)("Emitting from node " + curNode.label)
        for (e <- curNode.getOutputSet()) {
          //logger.log(Logger.INFO)("Emitting at depth " + e)
          val strBuffer = new StringBuffer()
          var j = i - e
          while (j < i-1) {
            strBuffer.append(input(j))
            strBuffer.append(sep)
            j = j + 1
          }
          strBuffer.append(input(j))
          mentions.add(new LexiconMention(strBuffer.toString,i-e,i))
        }
      } 
    }
    mentions
  }

  /** Tags a Token's features with a specific tag, if it's context forms a phrase in the trie. */
  def tagMentions(input : Seq[Token], featureFunc : (Token => CategoricalVectorVar[String]), tag : String, tokenFunc : (Token => String)) : Unit = {
    innerTagMentions(input,featureFunc,tag,tokenFunc)
  }

  /** Tags a Token's features with a specific tag, if it's context forms a phrase in the trie. */
  def tagMentions(input : Seq[Token], featureFunc : (Token => CategoricalVectorVar[String]), tag : String) : Unit = {
    innerTagMentions(input,featureFunc,tag,((t : Token) => t.string))
  }

  /**
   * Tags a Token's features with a specific tag, if it's context forms a phrase in the trie, after lemmatizing each token.
   */
  def lemmatizeAndTagMentions(input : Seq[Token], featureFunc : (Token => CategoricalVectorVar[String]), tag : String, lemmatizer : Lemmatizer) : Unit = {
    innerTagMentions(input,featureFunc,tag,(t:Token) => lemmatizer.lemmatize(t.string))
  }

  /** Inner function which tags tokens, after applying the supplied lemmatizeFunc. */
  private def innerTagMentions(input : Seq[Token], featureFunc : (Token => CategoricalVectorVar[String]), tag : String, lemmatizeFunc : (Token => String)) : Unit = {
    if (!constructed) {
      setTransitions()
    }
    var i = 0
    var curNode = root
    while (i < input.length) {
      val tokenString : String = lemmatizeFunc(input(i))
      //logger.log(Level.INFO)("Head = " + head + ", idx = " + i + ", label = " + curNode.label)
      val next = curNode.lookupToken(tokenString)
      if (next != None) {
        curNode = next.get
        i = i + 1
      } else if (curNode != root) {
        curNode = curNode.failNode
      } else {
        curNode = curNode.failNode
        i = i + 1
      }
      if (curNode.getEmit) {
        //annotate tokens
        var j = i - 1
        while (j >= (i - curNode.getEmitDepth)) {
          featureFunc(input(j)) += tag
          //logger.log(Logger.INFO)("Tagging text " + input(j))
          j = j - 1
        }
      }
    }
  }

  /** Adds a Seq of phrases into the current Trie, and fixes the failure transitions. */
  def ++=(input : Seq[Seq[String]]) : Unit = synchronized {
    AhoCorasick.logger.log(Logger.INFO)("Appending to automaton")
    for (e <- input) { root.add(e,0) }
    setTransitions()
  }
  
  /**
   * Adds a single phrase to the Trie. The failure transitions will be recalculated on the next lookup.
   */
  def +=(input : Seq[String]) : Unit = synchronized {
    root.add(input,0)
    constructed = false
  }

  /** Calculate the failure transitions. */
  def setTransitions() : Unit = synchronized {
    if (!constructed) {
      TrieNode.setFailureTransitions(root)
      constructed = true
    }
  }
  
  def size() : Int = { root.getNumPhrases() }
  
  /** Logs the tree structure to stderr. */
  def logTrie() : Unit = { TrieNode.logTrie(root) }
  
  override def toString() : String = { "Aho-Corasick automaton containing " + size() + " phrases." }
  
  /** Serialization methods. Reconstructing the Trie from a source is usually faster. */
  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(in: java.io.ObjectInputStream): Unit = { in.defaultReadObject(); constructed = false; setTransitions() }
}

object AhoCorasick {
  private val logger = Logger.getLogger("cc.factorie.app.nlp.lexicon.AhoCorasick")
}

/**
 * An Aho-Corasick mention, containing the mention string, and the start & end
 * character indices in the original text.
 */
class LexiconMention(val mention : String, val startIdx : Int, val endIdx : Int) extends Serializable {
  override def toString() : String = { "Mention: " + mention + ", startIdx = " + startIdx + ", endIdx = " + endIdx }
  
  override def hashCode() : Int = { mention.hashCode() ^ startIdx ^ endIdx }
  
  override def equals(obj : Any) : Boolean = {
    if (obj == null) {
      return false
    }
    if (getClass() != obj.getClass()) {
      return false
    }
    val other = obj.asInstanceOf[LexiconMention]
    if (!this.mention.equals(other.mention)) {
      return false
    }
    if (this.startIdx != other.startIdx) {
      return false
    }
    if (this.endIdx != other.endIdx) {
      return false
    }
    return true
  }
}
