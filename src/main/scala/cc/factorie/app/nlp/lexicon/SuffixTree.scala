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
package cc.factorie.app.nlp.lexicon

import scala.collection.mutable.HashMap

/**
 * Created by kate on 5/20/14.
 */

//FIXME doesnt seem to deal well with weird chars e.g. "André Gide"

/** Based on Jinho Choi/ClearNLP's "AffixTree.java".
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