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

package cc.factorie.app.nlp.parse
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load.LoadOntonotes5
import cc.factorie.util.Cubbie
import cc.factorie.variable.{EnumDomain, LabeledCategoricalVariable}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// Representation for a dependency parse

// TODO I think this should instead be "ParseEdgeLabelDomain". -akm
object ParseTreeLabelDomain extends EnumDomain {
  val acomp, advcl, advmod, agent, amod, appos, attr, aux, auxpass, cc, ccomp, complm, conj, csubj, csubjpass, 
  dep, det, dobj, expl, hmod, hyph, infmod, intj, iobj, mark, meta, neg, nmod, nn, npadvmod, nsubj, nsubjpass, 
  num, number, oprd, parataxis, partmod, pcomp, pobj, poss, possessive, preconj, predet, prep, prt, punct, 
  quantmod, rcmod, root, xcomp = Value
  index("") // necessary for empty categories
  freeze()
  def defaultCategory = "nn"
  def defaultIndex = index(defaultCategory)
}
// TODO I think this should instead be "ParseEdgeLabels extends LabeledCategoricalSeqVariable". -akm
class ParseTreeLabel(val tree:ParseTree, targetValue:String = ParseTreeLabelDomain.defaultCategory) extends LabeledCategoricalVariable(targetValue) { def domain = ParseTreeLabelDomain }

object ParseTree {
  val rootIndex = -1
  val noIndex = -2
}

// TODO This initialization is really inefficient.  Fix it. -akm
class ParseTree(val sentence:Sentence, theTargetParents:Array[Int], theTargetLabels:Array[Int]) {
  def this(sentence:Sentence) = this(sentence, Array.fill[Int](sentence.length)(ParseTree.noIndex), Array.fill(sentence.length)(ParseTreeLabelDomain.defaultIndex)) // Note: this puts in dummy target data which may be confusing
  def this(sentence:Sentence, theTargetParents:Seq[Int], theTargetLabels:Seq[String]) = this(sentence, theTargetParents.toArray, theTargetLabels.map(c => ParseTreeLabelDomain.index(c)).toArray)
  def check(parents:Array[Int]): Unit = {
    val l = sentence.length; var i = 0; while (i < parents.length) {
      require(parents(i) < l)
      i += 1
    }
  }
  check(theTargetParents)
  val _labels = theTargetLabels.map(s => new ParseTreeLabel(this, ParseTreeLabelDomain.category(s))).toArray
  val _parents = { val p = new Array[Int](theTargetParents.length); System.arraycopy(theTargetParents, 0, p, 0, p.length); p }
  val _targetParents = theTargetParents
  def labels: Array[ParseTreeLabel] = _labels
  def parents: Array[Int] = _parents
  def targetParents: Array[Int] = _targetParents
  def setParentsToTarget(): Unit = System.arraycopy(_targetParents, 0, _parents, 0, _parents.length)
  def numParentsCorrect: Int = { var result = 0; for (i <- 0 until _parents.length) if (_parents(i) == _targetParents(i)) result += 1; result }
  def parentsAccuracy: Double = numParentsCorrect.toDouble / _parents.length
  def numLabelsCorrect: Int = {var result = 0; for (i <- 0 until _labels.length) if (_labels(i).valueIsTarget) result += 1; result }
  def labelsAccuracy: Double = numLabelsCorrect.toDouble / _labels.length
  /** Returns the position in the sentence of the root token. */ 
  def rootChildIndex: Int = firstChild(-1)
  /** Return the token at the root of the parse tree.  The parent of this token is null.  The parentIndex of this position is -1. */
  def rootChild: Token = sentence.tokens(rootChildIndex)
  /** Make the argument the root of the tree.  This method does not prevent their being two roots. */
  def setRootChild(token:Token): Unit = setParent(token.position - sentence.start, -1)
  /** Returns the sentence position of the parent of the token at position childIndex */
  def parentIndex(childIndex:Int): Int = if (childIndex == ParseTree.rootIndex) ParseTree.noIndex else _parents(childIndex)
  def targetParentIndex(childIndex:Int): Int = if (childIndex == ParseTree.rootIndex) ParseTree.noIndex else _targetParents(childIndex)
  /** Returns the parent token of the token at position childIndex (or null if the token at childIndex is the root) */
  def parent(childIndex:Int): Token = {
    val idx = _parents(childIndex)
    if (idx == -1) null // -1 is rootIndex
    else sentence.tokens(idx)
  }
  /** Returns the parent token of the given token */
  def parent(token:Token): Token = { require(token.sentence eq sentence); parent(token.position - sentence.start) }
  /** Set the parent of the token at position 'child' to be at position 'parentIndex'.  A parentIndex of -1 indicates the root.  */
  def setParent(childIndex:Int, parentIndex:Int): Unit = _parents(childIndex) = parentIndex
  def setTargetParent(childIndex:Int, parentIndex:Int): Unit = _targetParents(childIndex) = parentIndex
  /** Set the parent of the token 'child' to be 'parent'. */
  def setParent(child:Token, parent:Token): Unit = {
    require(child.sentence eq sentence)
    if (parent eq null) {
      _parents(child.position - sentence.start) = -1
    } else {
      require(parent.sentence eq sentence)
      _parents(child.position - sentence.start) = parent.position - sentence.start
    }
  }

  //TODO: all of the  following methods are inefficient if the parse tree is fixed, and various things
  //can be precomputed.

  /** Return the sentence index of the first token whose parent is 'parentIndex' */
  private def firstChild(parentIndex:Int): Int = {
    var i = 0
    while ( i < _parents.length) {
      if (_parents(i) == parentIndex) return i
      i += 1
    }
    -1
  }


  /** Return a list of tokens who are the children of the token at sentence position 'parentIndex' */
  def children(parentIndex:Int): Seq[Token] = {
    getChildrenIndices(parentIndex).map(i => sentence.tokens(i))
  }

  def getChildrenIndices(parentIndex:Int, filter : Int => Boolean = {x => false}): Seq[Int] = {
    val result = new ArrayBuffer[Int]
    var i = 0
    while (i < _parents.length) {
      if (_parents(i) == parentIndex) result += i
      i += 1
    }
    result.sorted.takeWhile( i => !filter(i))
  }

  def subtree(parentIndex:Int): Seq[Token] = {
    getSubtreeInds(parentIndex).map(sentence.tokens(_))
  }

  def getSubtreeInds(parentIndex: Int, filter : Int => Boolean = {x => false}): Seq[Int] = {
    val result = new ArrayBuffer[Int]()
    result += parentIndex
    result ++= getChildrenIndices(parentIndex, filter).flatMap(getSubtreeInds(_)).distinct
    result
  }

  def leftChildren(parentIndex:Int): Seq[Token] = {
    val result = new scala.collection.mutable.ArrayBuffer[Token]
    var i = 0
    while (i < parentIndex) {
      if (_parents(i) == parentIndex) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  def rightChildren(parentIndex:Int): Seq[Token] = {
    val result = new scala.collection.mutable.ArrayBuffer[Token]
    var i = parentIndex+1
    while (i < _parents.length) {
      if (_parents(i) == parentIndex) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  /** Return a list of tokens who are the children of parentToken */
  //def children(parentToken:Token): Seq[Token] = children(parentToken.position - sentence.start)
  /** Return a list of tokens who are the children of the token at sentence position 'parentIndex' and who also have the indicated label value. */
  def childrenLabeled(index:Int, labelIntValue:Int): Seq[Token] = {
    val result = new scala.collection.mutable.ArrayBuffer[Token]
    var i = 0
    while (i < _parents.length) {
      if (_parents(i) == index && _labels(i).intValue == labelIntValue) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  def leftChildrenLabeled(parentIndex:Int, labelIntValue:Int): Seq[Token] = {
    val result = new scala.collection.mutable.ArrayBuffer[Token]
    var i = 0
    while (i < parentIndex) {
      if (_parents(i) == parentIndex && _labels(i).intValue == labelIntValue) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  def rightChildrenLabeled(parentIndex:Int, labelIntValue:Int): Seq[Token] = {
    val result = new scala.collection.mutable.ArrayBuffer[Token]
    var i = parentIndex+1
    while (i < _parents.length) {
      if (_parents(i) == parentIndex && _labels(i).intValue == labelIntValue) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  //def childrenOfLabel(token:Token, labelIntValue:Int): Seq[Token] = childrenOfLabel(token.position - sentence.start, labelIntValue)
  //def childrenLabeled(index:Int, labelValue:DiscreteValue): Seq[Token] = childrenLabeled(index, labelValue.intValue) 
  //def childrenOfLabel(token:Token, labelValue:DiscreteValue): Seq[Token] = childrenOfLabel(token.position - sentence.start, labelValue.intValue)
  /** Return the label on the edge from the child at sentence position 'index' to its parent. */
  def label(index:Int): ParseTreeLabel = _labels(index)
  def copy: ParseTree = {
    val newTree = new ParseTree(sentence, targetParents, labels.map(_.target.categoryValue))
    for (i <- 0 until sentence.length) {
      newTree._parents(i) = this._parents(i)
      newTree._labels(i).set(this._labels(i).intValue)(null)
    }
    newTree
  }
  /** Return the label on the edge from 'childToken' to its parent. */
  //def label(childToken:Token): ParseTreeLabel = { require(childToken.sentence eq sentence); label(childToken.position - sentence.start) }
  override def toString: String = {
    val tokenStrings = {
      if (sentence.tokens.forall(_.posTag ne null))
        sentence.tokens.map(t => t.string + "/" + t.posTag.categoryValue)
      else
        sentence.tokens.map(_.string)
    }
    val labelStrings = _labels.map(_.value.toString())
    val buff = new StringBuffer()
    for (i <- 0 until sentence.length)
      buff.append(i + " " + _parents(i) + " " + tokenStrings(i) + " " + labelStrings(i) + "\n")
    buff.toString
  }

  def toStringTex:String = {
    def texEdges(idx:Int, builder:StringBuilder):StringBuilder = this.children(idx) match {
      case empty if empty.isEmpty => builder
      case children => children.foreach { token =>
        val childIdx = token.positionInSentence
        val parentIdx = token.parseParentIndex
        val label = token.parseLabel.categoryValue
        builder.append("  \\depedge{%s}{%s}{%s}".format(parentIdx + 1, childIdx + 1, label)).append("\n") // latex uses 1-indexing
        texEdges(childIdx, builder)
      }
        builder
    }
    val sentenceString = this.sentence.tokens.map(_.string).mkString(""" \& """) + """\\"""

    val rootId = this.rootChildIndex
    val rootLabel = this.label(rootId).categoryValue // should always be 'root'
    val rootString = "  \\deproot{%s}{%s}".format(rootId, rootLabel)

    val sb = new StringBuilder
    sb.append("""\begin{dependency}""").append("\n")
    sb.append("""  \begin{deptext}""").append("\n")
    sb.append(sentenceString).append("\n")
    sb.append("""  \end{deptext}""").append("\n")
    sb.append(rootString).append("\n")
    texEdges(rootId, sb)
    sb.append("""\end{dependency}""").append("\n")

    sb.toString()
  }

  /** Get the sequence of tokens making up the path that goes through the root
    * between start and end in the parse tree; returns an empty Seq if the
    * tree contains cycles. */
  def getPath(start: Int, end: Int): Seq[Int] = {
    val path = ArrayBuffer[Int]()

    // get path up to root, end token, or end of a cycle
    var currentTok = start
    path += currentTok
    while (parents(currentTok) != -1 && currentTok != end && !path.contains(parents(currentTok))) {
      currentTok = parents(currentTok)
      path += currentTok
    }

    // only bother continuing if we didn't find a cycle
    if (!path.contains(parents(currentTok))) {
      // if haven't yet reached end, continue down from root
      val pathDown = ArrayBuffer[Int]()
      val visited = mutable.Set[Int]()
      visited += currentTok
      findDescendantPath(currentTok, end, pathDown, visited)
      if(pathDown.nonEmpty)
        return path ++= pathDown.dropRight(1).reverse
    }
    Seq()
  }

  /** Get the sequence of token indices making up the shortest path between start
    * and end in the parse tree, as well as their deepest common token in the tree */
  def getShortestPath(start: Int, end: Int): (Seq[Int], Int) = {
    val path = ArrayBuffer[Int]()

    // get path up to root or end token
    var currentTok = start
    path += currentTok
    while(parents(currentTok) != -1 && currentTok != end){
      currentTok = parents(currentTok)
      path += currentTok
    }

    // if haven't yet reached end, continue down from root
    val pathDown = ArrayBuffer[Int]()
    val visited = mutable.Set[Int]()
    visited += currentTok
    findDescendantPath(currentTok, end, pathDown, visited)

    var lastCommon = currentTok
    while(pathDown.nonEmpty && path.nonEmpty && pathDown.last == path.last){
      lastCommon = pathDown.last
      pathDown -= pathDown.last
      path -= path.last
    }
    path += lastCommon
    (path ++ pathDown.reverse, lastCommon)
  }

  /** Populate path with path from current to end in the parse tree; path will be empty
    * if the tree contains cycles */
  def findDescendantPath(current: Int, end: Int, path: mutable.Buffer[Int], visited: mutable.Set[Int]): Boolean = {
    if(current == -1) false
    else if(current == end) true
    else {
      children(current).foreach{child =>
        val childIdx = child.positionInSentence
        if(!visited.contains(childIdx)) { // avoid cycles
          visited += childIdx
          if (findDescendantPath(childIdx, end, path, visited)) {
            path += current
            return true
          }
        }
      }
      false
    }
  }

  /** Get a nicely formatted String representation of the shortest dependency path
      between the tokens at indices start and end */
  def getStringShortestPath(start: Int, end: Int): String = {
    val (path, lastCommonTok) = getShortestPath(start, end)
    getPrettyPathString(path :+ end, _ == lastCommonTok)
  }

  /** Turns a list of token indices and a function determining when the direction of the arrows
    * should reverse into a pretty-printed dependency path. See getStringRootPath for
    * a usage example. */
  def getPrettyPathString(path: Seq[Int], reverseArrows: Int => Boolean): String = {
    var reversedArrows = false
    val sb = new StringBuilder()
    path.foreach{tokenIdx =>
      val token = sentence(tokenIdx)
      if(reverseArrows(tokenIdx)){
        reversedArrows = true
        sb.append(s"${token.string}")
      }
      else if(reversedArrows) {
        sb.append(s" <-${token.parseLabel.categoryValue}- ${token.string}")
      }
      else{
        sb.append(s"${token.string} -${token.parseLabel.categoryValue}-> ")
      }
    }
    sb.toString
  }

  /** Get a nicely formatted String representation of the dependency path that goes
      through the root between the tokens at indices start and end */
  def getStringRootPath(start: Int, end: Int): String = {
    val path = getPath(start, end)
    if(path.nonEmpty) getPrettyPathString(path :+ end,  t => t == rootChildIndex)
    else ""
  }

}

// Example usages:
// token.sentence.attr[ParseTree].parent(token)
// sentence.attr[ParseTree].children(token)
// sentence.attr[ParseTree].setParent(token, parentToken)
// sentence.attr[ParseTree].label(token)
// sentence.attr[ParseTree].label(token).set("SUBJ")

// Methods also created in Token supporting:
// token.parseParents
// token.setParseParent(parentToken)
// token.parseChildren
// token.parseLabel
// token.leftChildren

class ParseTreeCubbie extends Cubbie {
  val parents = IntListSlot("parents")
  val labels = StringListSlot("labels")
  def newParseTree(s:Sentence): ParseTree = new ParseTree(s) // This will be abstract when ParseTree domain is unfixed
  def storeParseTree(pt:ParseTree): this.type = {
    parents := pt.parents
    labels := pt.labels.map(_.categoryValue)
    this
  }
  def fetchParseTree(s:Sentence): ParseTree = {
    val pt = newParseTree(s)
    for (i <- 0 until s.length) {
      pt.setParent(i, parents.value(i))
      pt.label(i).setCategory(labels.value(i))(null)
    }
    pt
  }
}