/* Copyright 2014.
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
   limitations under the License.
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
class TrieNode(val label : String, val output : String, var root : TrieNode, val sep : String, val depth : Int) extends Serializable {
    val transitionMap : Map[String, TrieNode] = JavaHashMap[String,TrieNode]()
    val outputSet : Set[(String,Int)] = JavaHashSet[(String,Int)]()
    @transient var failNode : TrieNode = root
    private var phrases : Int = 0
    private var emit : Boolean = false
    private var exactEmit : Boolean = false
    private var maxEmitDepth : Int = 0

    def this(sep : String) = {
        this("","",null,sep,0)
        root = this
        failNode = this
    }

    /**
     * Returns the number of phrases below this node.
     */
    def getNumPhrases() : Int = { phrases }

    /**
     * Returns true if this node emits a value.
     */
    def getEmit() : Boolean = { emit }
    
    /**
     * Returns true if this node emits a value it was constructed with.
     */
    def getExactEmit() : Boolean = { exactEmit }

    /**
     * Returns the maximum depth in the outputSet.
     */
    def getEmitDepth() : Int = { maxEmitDepth }

    /**
     * Write the node out to the logger.
     */
    def logNode() : Unit = {
        val buffer = new StringBuilder()
        buffer.append("Label = ")
        buffer.append(label)
        buffer.append("\n")
        buffer.append("Fail state = ")
        if (root == failNode) {
            buffer.append("<root>\n")
        } else {
            buffer.append(failNode.label)
            buffer.append("\n")
        }
        buffer.append("Output = ")
        buffer.append(output)
        buffer.append("\n")
        if (emit) {
            buffer.append("Emit = true\n")
        } else {
            buffer.append("Emit = false\n")
        }
        buffer.append("Transition table:\n")
        for (e <- transitionMap) {
            buffer.append("\t\t")
            buffer.append(e._1)
            buffer.append("\n")
        }
        TrieNode.logger.log(Logger.INFO)(buffer.toString())
    }
    
    /**
     * Appends a phrase to the current node.
     */
    def +=(phrase : Queue[String]) : Unit = {
        if (phrase.isEmpty) {
            emit = true
            exactEmit = true
            outputSet.add((output,depth))
            maxEmitDepth = depth
        } else {
            val curWord : String = phrase.dequeue
            var transitionNode = transitionMap.getOrElse(curWord,null)
            if (transitionNode == null) {
                val newOutput = if (this == root) {curWord} else {output + sep + curWord}
                transitionNode = new TrieNode(curWord, newOutput, root, sep, depth + 1)
                transitionMap += (curWord -> transitionNode)
            }
            transitionNode += phrase
            phrases = phrases + 1
        }
    }

    /**
     * Updates the maxEmitDepth.
     */
    private def updateEmitDepth() : Unit = {
        for (e <- outputSet) {
            if (e._2 > maxEmitDepth) {
                maxEmitDepth = e._2
            }
        }
    }
    
    /**
     * Serialization method. It's usually faster to reconstruct the Trie from the source, but this ensures compatibility.
     */
    def readObject(in : java.io.ObjectInputStream) : Unit = { in.defaultReadObject(); failNode = root }
}

object TrieNode {
    private val logger : Logger = Logger.getLogger("cc.factorie.app.nlp.lexicon.TrieNode")

    def setFailureTransitions(root : TrieNode) : Unit = {
        //logger.log(Level.INFO,"Setting failure transitions.")
        val queue = new Queue[TrieNode]()
        queue ++= root.transitionMap.values

        while (!queue.isEmpty) {
            val curNode = queue.dequeue()
            for (e <- curNode.transitionMap) {
                val childNode = e._2
                queue += childNode
                var curFailNode = curNode.failNode
                while ((curFailNode != root) && (!curFailNode.transitionMap.contains(childNode.label))) {
                    curFailNode = curFailNode.failNode
                }
                
                val tmp = curFailNode.transitionMap.getOrElse(childNode.label,null)
                if ((tmp != null) && (tmp != childNode)) {
                    childNode.failNode = tmp
                    //Should this emit if the childNode doesn't?
                    if (tmp.emit) {
                        childNode.emit = true
                        childNode.outputSet ++= tmp.outputSet
                        childNode.updateEmitDepth()
                    }
                }
            }
        }
    }

    def logTrie(root : TrieNode) : Unit = {
        logger.log(Logger.INFO)("Logging trie")
        val queue = new Queue[TrieNode]()
        queue ++= root.transitionMap.values

        root.logNode()
        
        while (!queue.isEmpty) {
            val curNode = queue.dequeue()
            curNode.logNode()
            queue ++= curNode.transitionMap.values
        }
    }
}
