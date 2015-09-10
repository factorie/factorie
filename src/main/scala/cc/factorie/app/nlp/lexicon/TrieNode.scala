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

import cc.factorie.util.Logger
import cc.factorie.util.JavaHashMap
import cc.factorie.util.JavaHashSet
import java.io.Serializable
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.Map

/**
 * A Trie implementation. Used as an internal data structure by Aho-Corasick.
 * Use the methods in AhoCorasick to interact with this Trie.
 * 
 * It's similar to the SuffixTree implementation, but with an extra link used if 
 * the lookup at a specific node fails.
 */
class TrieNode(val label : String, var root : TrieNode, val sep : String, val depth : Int) extends Serializable {
  private var transitionMap : Map[String, TrieNode] = null
  private var key : String = null // key value pair to save transitionMap overhead
  private var value : TrieNode = null // key value pair to save transitionMap overhead
  private var outputSet : Set[Int] = null
  private var output : Int = -1 // save outputSet overhead
  @transient var failNode : TrieNode = root
  private var phrases : Int = 0
  private var emit : Boolean = false
  private var exactEmit : Boolean = false
  private var maxEmitDepth : Int = 0
  
  def this(sep : String) = {
    this("",null,sep,0)
    root = this
    failNode = this
  }
  
  /** Returns the number of phrases below this node. */
  def getNumPhrases() : Int = { phrases }
  
  /** Returns true if this node emits a value. */
  def getEmit() : Boolean = { emit }
  
  /** Returns true if this node emits a value it was constructed with. */
  def getExactEmit() : Boolean = { exactEmit }
  
  /** Returns the maximum depth in the outputSet. */
  def getEmitDepth() : Int = { maxEmitDepth }
  
  /** Write the node out to the logger. */
  def logNode() : Unit = {
    val buffer = new StringBuilder()
    buffer.append("Label = ")
    buffer.append(label)
    buffer.append("\n")
    buffer.append("Fail state = ")
    if (root == failNode) {
      buffer.append("<root>\n")
    } else {
      buffer.append(failNode.label + " at depth " + failNode.depth)
      buffer.append("\n")
    }
    buffer.append("Depth = ")
    buffer.append(depth)
    buffer.append("\n")
    if (emit) {
      buffer.append("Emit = true\n")
    } else {
      buffer.append("Emit = false\n")
    }
    buffer.append("Transition table:\n")
    if (transitionMap != null) {
      for (e <- transitionMap) {
        buffer.append("\t\t")
        buffer.append(e._1)
        buffer.append("\n")
      }
    } else if (key != null) {
      buffer.append("\t\t")
      buffer.append(key)
      buffer.append("\n")
    }
    TrieNode.logger.log(Logger.INFO)(buffer.toString())
  }
  
  def lookupToken(tok : String) : Option[TrieNode] = {
    if (transitionMap == null) {
      if (tok.equals(key)) { Some(value) } else { None }
    } else {
      transitionMap.get(tok)
    }
  }

  def getOutputSet() : Set[Int] = {
    if (outputSet == null) {
      val set = JavaHashSet[Int](1)
      if (output != -1) { set.add(output) }
      set
    } else { outputSet }
  }

  /** Appends a phrase to the current node. */
  def add(phrase : Seq[String], index : Int) : Unit = {
    if (phrase.length <= index) {
      emit = true
      exactEmit = true
      if ((output == -1) && (outputSet == null)) {
        output = depth
      } else {
        if (outputSet == null) { constructSet }
        outputSet.add(depth)
      }
      maxEmitDepth = depth
    } else {
      val curWord : String = phrase(index)
      if (curWord.equals(key)) {
        value.add(phrase,index+1)
      } else {
        var transitionNode : TrieNode = null
        if ((transitionMap == null) && (key == null)) {
          transitionNode = new TrieNode(curWord, root, sep, depth + 1)
          key = curWord
          value = transitionNode
        } else {
          if (transitionMap == null) {
            constructMap()
          }
          transitionNode = transitionMap.getOrElse(curWord,null)
          if (transitionNode == null) {
            transitionNode = new TrieNode(curWord, root, sep, depth + 1)
            transitionMap += (curWord -> transitionNode)
          }
        }
        transitionNode.add(phrase,index+1)
      }
      phrases = phrases + 1
    }
  }

  /** Helper methods */
  private def constructMap() : Unit = {
    transitionMap = JavaHashMap[String,TrieNode](3)
    transitionMap += (key -> value)
    key = null
    value = null
  }

  private def constructSet() : Unit = {
    outputSet = JavaHashSet[Int](3)
    if (output != -1) { outputSet.add(output) }
    output = -1
  }
  
  
  private def appendOutputSet(other : Set[Int]) : Unit = {
    if ((output == -1) && (other.size == 1)) {
      output = other.last
    } else {
      constructSet()
      outputSet ++= other
    }
  }

  /** Updates the maxEmitDepth. */
  private def updateEmitDepth() : Unit = {
    if (outputSet == null) {
      if (output == -1) {
        //shouldn't happen
        maxEmitDepth = 0
      } else { maxEmitDepth = output }
    } else {
      for (e <- outputSet) {
        if (e > maxEmitDepth) { maxEmitDepth = e }
      }
    }
  }
  
  /**
   * Serialization method. It's usually faster to reconstruct the Trie from the source, but this ensures compatibility.
   */
  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(in : java.io.ObjectInputStream) : Unit = { in.defaultReadObject(); failNode = root }
}

object TrieNode {
  private val logger : Logger = Logger.getLogger("cc.factorie.app.nlp.lexicon.TrieNode")
  
  def setFailureTransitions(root : TrieNode) : Unit = {
    //logger.log(Level.INFO,"Setting failure transitions.")
    val queue = new Queue[TrieNode]()
    if (root.transitionMap != null) {
        queue ++= root.transitionMap.values
    } else if (root.key != null) {
        queue += root.value
    }

    while (!queue.isEmpty) {
      val curNode = queue.dequeue()
      if (curNode.transitionMap != null) {
        for (e <- curNode.transitionMap) {
          val childNode = e._2
          queue += childNode
          var curFailNode = curNode.failNode
          while ((curFailNode != root) && (curFailNode.lookupToken(childNode.label) == None)) {
            curFailNode = curFailNode.failNode
          }
          
          val tmp = curFailNode.lookupToken(childNode.label)
          if ((tmp != None) && (tmp.get != childNode)) {
            val node = tmp.get
            childNode.failNode = node
            if (node.emit) {
              childNode.emit = true
              childNode.appendOutputSet(node.getOutputSet)
              childNode.updateEmitDepth()
            }
          }
        }
      } else if (curNode.key != null) {
        //a transition in (key,value)
        val childNode = curNode.value
        queue += childNode
        var curFailNode = curNode.failNode
        while ((curFailNode != root) && (curFailNode.lookupToken(childNode.label) == None)) {
          curFailNode = curFailNode.failNode
        }
        
        val tmp = curFailNode.lookupToken(childNode.label)
        if ((tmp != None) && (tmp.get != childNode)) {
          val node = tmp.get
          childNode.failNode = node
          if (node.emit) {
            childNode.emit = true
            childNode.appendOutputSet(node.getOutputSet)
            childNode.updateEmitDepth()
          }
        }
      }
    }
  }

  def logTrie(root : TrieNode) : Unit = {
    logger.log(Logger.INFO)("Logging trie")
    val queue = new Queue[TrieNode]()
    if (root.transitionMap != null) {
      queue ++= root.transitionMap.values
    } else if (root.key != null) {
      queue += root.value
    }

    root.logNode()
    
    while (!queue.isEmpty) {
      val curNode = queue.dequeue()
      curNode.logNode()
      if (curNode.transitionMap != null) {
        queue ++= curNode.transitionMap.values
      } else if (curNode.key != null) {
        queue += curNode.value
      }
    }
  }
}
