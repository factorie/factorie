/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
import java.lang.StringBuffer
import collection.mutable.ArrayBuffer
import cc.factorie.variable._
import cc.factorie.app.nlp.phrase.{PhraseList, Phrase}
import scala.collection.mutable

// Representation for a dependency parse

// TODO I think this should instead be "ParseEdgeLabelDomain". -akm
object CollapsedParseTreeLabelDomain extends EnumDomain {
  ParseTreeLabelDomain.foreach(v => index(v.category))

  //for collapsed parse trees
  Seq("about", "above", "across", "after", "against", "around", "at", "as", "before", "behind", "below", "beneath", "beside", "besides",
    "between", "beyond", "by", "down", "during", "except", "for", "from", "in", "inside", "into", "like", "near", "of", "off", "on", "out",
    "outside", "over", "since", "through", "throughout", "till", "to", "than", "toward", "under", "until", "up", "upon", "via",   "with", "without").foreach(index)

  index("") // necessary for empty categories
  def defaultCategory = "nn"
}

object ParseTree2 {
  def mutableFromParseTree(pt:ParseTree) = new MutableParseTree(pt.sentence, pt.targetParents, pt.labels.map(_.categoryValue))
  def immutableFromParseTree(pt:ParseTree) = new ImmutableParseTree(pt.sentence, pt.targetParents, pt.labels.map(_.categoryValue))
  def collapsedFromParseTree(pt:ParseTree) = immutableFromParseTree(pt).toCollapsedParseTree
}

class ParseTreeParent(val tree:ParseTree2, targetIdx:Int = ParseTree.noIndex) extends LabeledIntegerVariable(targetIdx) {
  def vertex = if(value >= 0) tree.vertices(value) else null
}
class ParseTreeLabel2(val tree:ParseTree2, targetValue:String = CollapsedParseTreeLabelDomain.defaultCategory) extends LabeledCategoricalVariable(targetValue) { def domain = CollapsedParseTreeLabelDomain }

//We need different kinds of vertices, s.t. for example simple parse trees and collapsed parse trees can be treated similarly
trait ParseTreeVertex {
  def tree:ParseTree2
  def tokens:Seq[Token]
  def headToken:Token = tokens.head
  def token = headToken
}
class TokenParseTreeVertex(val tree:ParseTree2, override val headToken:Token) extends ParseTreeVertex {
  def tokens: Seq[Token] = Seq(headToken)
}
class PhraseParseTreeVertex(val tree:ParseTree2, val phrase:Phrase) extends ParseTreeVertex {
  def tokens: Seq[Token] = phrase.tokens
  override def headToken: Token = phrase.headToken
}

trait ParseTree2 {
  val sentence:Sentence

  protected val _vertices:Array[ParseTreeVertex]
  protected val _labels:Array[ParseTreeLabel2]
  protected val _parents:Array[ParseTreeParent]

  private lazy val vertexOfTokenMap = _vertices.flatMap(v => v.tokens.map(_ -> v)).toMap

  //require(_parents.length == _vertices.length)
  //require(_labels.length == _vertices.length)

  implicit def parentToInt(p:ParseTreeParent) = p.value
  implicit def tokenToVertex(token:Token) = vertexOfTokenMap.getOrElse(token,null)

  //println("ParseTree parents "+theTargetParents.mkString(" "))
  //println(" ParseTree labels "+theTargetLabels.mkString(" "))
  //println(" ParseTree labels "+_labels.map(_.categoryValue).mkString(" "))
  def labels = _labels
  def vertices = _vertices
  def vertex(idx:Int) = if(idx < 0) null else _vertices(idx)
  def parents= _parents
  def setParentsToTarget(): Unit = _parents.foreach(p => p.set(p.target.value)(null))
  def numParentsCorrect: Int = _parents.count(_.valueIsTarget)
  def parentsAccuracy: Double = numParentsCorrect.toDouble / _parents.length
  def numLabelsCorrect: Int = _labels.count(_.valueIsTarget)
  def labelsAccuracy: Double = numLabelsCorrect.toDouble / _labels.length

  /** Returns the position in the sentence of the root token. */
  def rootChildIndex: Int = _parents.indexWhere(_.intValue == ParseTree.rootIndex)
  /** Return the vertex at the root of the parse tree.  The parent of this vertex is null.  The parentIndex of this position is -1. */
  def rootChild: ParseTreeVertex = _vertices(rootChildIndex)

  /** Returns the vertex index of the parent of the vertex at position childIndex */
  def parentIndex(childIndex:Int): Int = if (childIndex == ParseTree.rootIndex) ParseTree.noIndex else _parents(childIndex).value
  def targetParentIndex(childIndex:Int): Int = if (childIndex == ParseTree.rootIndex) ParseTree.noIndex else _parents(childIndex).target.value

  /** Returns the parent of the vertex at position childIndex */
  def parent(childIndex:Int) = _parents(childIndex)

  /** Returns the parent vertex of the given token */
  def parent(token:Token): ParseTreeVertex = { require(token.sentence eq sentence); tokenToVertex(token) }

  //TODO: all of the  following methods are inefficient if the parse tree is fixed, and various things can be precomputed.
  // see trait ImParseTree, which can be mixed in with ParseTree

  /** Return the vertex index of the first vertex whose parent is 'parentIndex' */
  protected def firstChild(parentIndex:Int): Int = {
    var i = 0
    while ( i < _parents.length) {
      if (_parents(i).value == parentIndex) return i
      i += 1
    }
    -1
  }

  /** Return a list of vertices who are the children of the vertex at vertex position 'parentIndex' */
  def children(parentIndex:Int): Seq[ParseTreeVertex] = {
    getChildrenIndices(parentIndex).map(_vertices(_))
  }

  val defaultFilter: Int => Boolean = {x => false}
  def getChildrenIndices(parentIndex:Int, filter : Int => Boolean = defaultFilter): Seq[Int] = {
    val result = new ArrayBuffer[Int]
    var i = 0
    while (i < _parents.length) {
      if (_parents(i).value == parentIndex) result += i
      i += 1
    }
    if(filter == defaultFilter) result
    else result.sorted.takeWhile( i => !filter(i))
  }

  def subtree(parentIndex:Int): Seq[Token] = {
    getSubtreeInds(parentIndex).map(sentence.tokens(_))
  }

  def getSubtreeInds(parentIndex: Int, filter : Int => Boolean = defaultFilter): Seq[Int] = {
    val result = new ArrayBuffer[Int]()
    result += parentIndex
    result ++= getChildrenIndices(parentIndex, filter).flatMap(getSubtreeInds(_)).distinct
    result
  }

  def leftChildren(parentIndex:Int): Seq[ParseTreeVertex] = {
    val result = new scala.collection.mutable.ArrayBuffer[ParseTreeVertex]
    var i = 0
    while (i < parentIndex) {
      if (_parents(i).value == parentIndex) result += _vertices(i)
      i += 1
    }
    result
  }
  def rightChildren(parentIndex:Int): Seq[ParseTreeVertex] = {
    val result = new scala.collection.mutable.ArrayBuffer[ParseTreeVertex]
    var i = parentIndex+1
    while (i < _parents.length) {
      if (_parents(i).value == parentIndex) result += _vertices(i)
      i += 1
    }
    result
  }
  /** Return a list of tokens who are the children of the token at sentence position 'parentIndex' and who also have the indicated label value. */
  def childrenLabeled(parentIndex:Int, labelIntValue:Int): Seq[ParseTreeVertex] = {
    val result = new scala.collection.mutable.ArrayBuffer[ParseTreeVertex]
    var i = 0
    while (i < _parents.length) {
      if (_parents(i).value == parentIndex && _labels(i).intValue == labelIntValue) result += _vertices(i)
      i += 1
    }
    result
  }
  def leftChildrenLabeled(parentIndex:Int, labelIntValue:Int): Seq[ParseTreeVertex] = {
    val result = new scala.collection.mutable.ArrayBuffer[ParseTreeVertex]
    var i = 0
    while (i < parentIndex) {
      if (_parents(i).value == parentIndex && _labels(i).intValue == labelIntValue) result += sentence.tokens(i)
      i += 1
    }
    result
  }
  def rightChildrenLabeled(parentIndex:Int, labelIntValue:Int): Seq[ParseTreeVertex] = {
    val result = new scala.collection.mutable.ArrayBuffer[ParseTreeVertex]
    var i = parentIndex+1
    while (i < _parents.length) {
      if (_parents(i).value == parentIndex && _labels(i).intValue == labelIntValue) result += _vertices(i)
      i += 1
    }
    result
  }
  //def childrenOfLabel(token:Token, labelIntValue:Int): Seq[Token] = childrenOfLabel(token.position - sentence.start, labelIntValue)
  //def childrenLabeled(index:Int, labelValue:DiscreteValue): Seq[Token] = childrenLabeled(index, labelValue.intValue)
  //def childrenOfLabel(token:Token, labelValue:DiscreteValue): Seq[Token] = childrenOfLabel(token.position - sentence.start, labelValue.intValue)
  /** Return the label on the edge from the child at sentence position 'index' to its parent. */
  def label(index:Int): ParseTreeLabel2 = _labels(index)
  def copy: ParseTree2 = {
    val newTree:ParseTree2 =
      if(this.isInstanceOf[MutableParseTree])
        new MutableParseTree(sentence, _parents.map(_.target.intValue), _labels.map(_.target.categoryValue))
      else
        new ImmutableParseTree(sentence, _parents.map(_.target.intValue), _labels.map(_.target.categoryValue))

    for (i <- 0 until sentence.length) {
      newTree._parents(i).set(this._parents(i).intValue)(null)
      newTree._labels(i).set(this._labels(i).intValue)(null)
    }
    newTree
  }
  /** Return the label on the edge from 'childToken' to its parent. */
  //def label(childToken:Token): ParseTreeLabel2 = { require(childToken.sentence eq sentence); label(childToken.position - sentence.start) }
  override def toString: String = {
    val tokenStrings = {
      if (_vertices.forall(_.token.posTag ne null))
        _vertices.map(v => v.tokens.map(_.string).mkString(" ") + "/" + v.token.posTag.categoryValue)
      else
        _vertices.map(_.tokens.map(_.string).mkString(" "))
    }
    val labelStrings = _labels.map(_.value.toString())
    val buff = new StringBuffer()
    for (i <- 0 until _vertices.length)
      buff.append(i + " " + _parents(i).intValue + " " + tokenStrings(i) + " " + labelStrings(i) + "\n")
    buff.toString
  }

  def toStringTex:String = {
    def texEdges(idx:Int, builder:StringBuilder):StringBuilder = this.children(idx) match {
      case empty if empty.isEmpty => builder
      case children => children.foreach { vertex =>
        val childIdx = vertex.token.positionInSentence
        val parentIdx = vertex.token.parseParentIndex
        val label = vertex.token.parseLabel.categoryValue
        builder.append("  \\depedge{%s}{%s}{%s}".format(parentIdx + 1, childIdx + 1, label)).append("\n") // latex uses 1-indexing
        texEdges(childIdx, builder)
      }
        builder
    }
    val sentenceString = this._vertices.map(_.tokens.map(_.string).mkString(" ")).mkString(""" \& """) + """\\"""

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

  def toImmutable:ImmutableParseTreeLike = this match {
    case t:ImmutableParseTreeLike => t
    case _:TokenParseTree =>
      val newTree = new ImmutableParseTree(sentence, parents.map(_.target.value), labels.map(_.target.categoryValue))
      for (i <- 0 until sentence.length) {
        newTree.parent(i).set(_parents(i).intValue)(null)
        newTree.label(i).set(_labels(i).intValue)(null)
      }
      newTree
    case _ => throw new IllegalArgumentException(s"There is no conversion from ${this.getClass.getName} into a mutable parse tree")
  }

  def toMutable:MutableParseTreeLike = this match {
    case t:MutableParseTreeLike => t
    case _:TokenParseTree =>
      val newTree = new MutableParseTree(sentence, parents.map(_.target.value), labels.map(_.target.categoryValue))
      for (i <- 0 until sentence.length) {
        newTree.parent(i).set(_parents(i).intValue)(null)
        newTree.label(i).set(_labels(i).intValue)(null)
      }
      newTree
    case _ => throw new IllegalArgumentException(s"There is no conversion from ${this.getClass.getName} into a mutable parse tree")
  }

  def toCollapsedParseTree = this match {
    case t: CollapsedParseTree => t
    case t: TokenParseTree => new CollapsedParseTree(t)
    case _ => throw new IllegalArgumentException(s"There is no conversion from ${this.getClass.getName} into a collapsed parse tree")
  }
}

class TokenParseTree(val sentence:Sentence, theTargetParents:Seq[Int], theTargetLabels:Seq[String]) extends ParseTree2 {
  override protected val _labels: Array[ParseTreeLabel2] =
    theTargetLabels.map(s => new ParseTreeLabel2(this, s)).toArray
  override protected val _parents: Array[ParseTreeParent] =
    theTargetParents.map(p => new ParseTreeParent(this,p)).toArray
  override protected val _vertices: Array[ParseTreeVertex] =
    sentence.tokens.map(t => new TokenParseTreeVertex(this,t)).toArray
}

//inefficient functions for retrieving children
class MutableParseTree(sentence:Sentence, targetParents:Seq[Int], targetLabels:Seq[String])
  extends TokenParseTree(sentence, targetParents, targetLabels) with MutableParseTreeLike {
  def this(sentence:Sentence) = this(sentence, Array.fill[Int](sentence.length)(ParseTree.noIndex), Array.tabulate(sentence.length)(i => CollapsedParseTreeLabelDomain.defaultCategory)) // Note: this puts in dummy target data which may be confusing
}
//efficient functions for retrieving children
class ImmutableParseTree(sentence:Sentence, targetParents:Seq[Int], targetLabels:Seq[String])
  extends TokenParseTree(sentence, targetParents, targetLabels) with ImmutableParseTreeLike

//collapses certain phrases into one vertex and prepositions become edges
class CollapsedParseTree(val parseTree:TokenParseTree) extends ParseTree2 with ImmutableParseTreeLike {
  override val sentence: Sentence = parseTree.sentence
  override protected val (_labels, _parents, _vertices): (Array[ParseTreeLabel2],Array[ParseTreeParent], Array[ParseTreeVertex]) = {
    var phraseTokens = mutable.Map[Token,Phrase]()
    val doc = sentence.document
    val cf = doc.coref
    if(cf != null) {
      def addPhrase(p:Phrase) = p.foreach(t => if(phraseTokens.get(t).fold(-1)(_.length) < p.length) phraseTokens += t -> p)
      val mentions = cf.mentions
      //Sometimes there are nested phrases, so the largest should be chosen
      mentions.withFilter(m => sentence.start <= m.phrase.start && sentence.end >= m.phrase.end).foreach(m => addPhrase(m.phrase))
      doc.attr.all[PhraseList].foreach(_.withFilter(p => sentence.start <= p.start && sentence.end >= p.end).foreach(addPhrase))
    }
    val vertices = ArrayBuffer[ParseTreeVertex]()
    val idxMap = sentence.tokens.foldLeft(mutable.HashMap[AnyRef,Int]())((map,t) => {
      if(!phraseTokens.contains(t)) {
        //collapse simple prepositions
        if(parseTree.label(t.positionInSentence).categoryValue != "prep" || parseTree.getChildrenIndices(t.positionInSentence).exists(c => !parseTree.label(c).categoryValue.matches("pobj|pcomp"))) {
          map += t -> map.size
          vertices += new TokenParseTreeVertex(this,t)
        }
      } else {
        val p = phraseTokens(t)
        if(p.headToken == t) {
          map += p -> map.size
          vertices += new PhraseParseTreeVertex(this,p)
        }
      }
      map
    })
    val aLength = idxMap.size
    var i = 0
    val parents = Array.ofDim[ParseTreeParent](aLength)
    val labels = Array.ofDim[ParseTreeLabel2](aLength)
    while(i < aLength) {
      val t = vertices(i).token
      val l = parseTree.label(t.positionInSentence).categoryValue
      var parent = parseTree.parent(t.positionInSentence).vertex
      //collapse prepositions
      labels(i) = if((l =="pobj"|| l =="pcomp") && !idxMap.contains(parent.token)) {
        val labelString = parent.token.lemmaString
        parent = parseTree.parent(parent.token.positionInSentence).vertex
        new ParseTreeLabel2(this, labelString)
      } else new ParseTreeLabel2(this, l)
      if(parent != null) {
        try {
          val parentIdx = idxMap.getOrElse(parent.token, idxMap(phraseTokens(parent.token)))
          parents(i) = new ParseTreeParent(this, parentIdx)
        } catch {
          case e:Throwable =>
            println(e.printStackTrace())
        }
      } else parents(i) = new ParseTreeParent(this, ParseTree.rootIndex)
      i += 1
    }
    (labels,parents,vertices.toArray)
  }
}   

trait MutableParseTreeLike extends ParseTree2 {
  /** Set the parent of the token at position 'child' to be at position 'parentIndex'.  A parentIndex of -1 indicates the root.  */
  def setParent(childIndex:Int, parentIndex:Int): Unit = _parents(childIndex).set(parentIndex)(null)
  def setTargetParent(childIndex:Int, parentIndex:Int): Unit = _parents(childIndex).target.set(parentIndex)(null)
  /** Set the parent of the token 'child' to be 'parent'. */
  def setParent(child:Token, parent:Token): Unit = {
    require(child.sentence eq sentence)
    val parentIdx = _vertices.indexWhere(_.tokens.contains(parent))
    val childIdx = _vertices.indexWhere(_.tokens.contains(child))
    _parents(childIdx).set(parentIdx)(null)
  }
  /** Make the argument the root of the tree.  This method does not prevent their being two roots. */
  def setRootChild(token:Token): Unit = setParent(_vertices.indexWhere(_.tokens.contains(token)), -1)

  /** Set the parent of the token 'child' to be 'parent'. */
  def setParent(child:ParseTreeVertex, parent:ParseTreeVertex): Unit = {
    require(child.token.sentence eq sentence)
    val parentIdx = _vertices.indexOf(parent)
    val childIdx = _vertices.indexOf(child)
    _parents(childIdx).set(parentIdx)(null)
  }
  /** Make the argument the root of the tree.  This method does not prevent their being two roots. */
  def setRootChild(root:ParseTreeVertex): Unit = setParent(_vertices.indexOf(root), -1)
}

//Mixin for immutable trees that provides a set of more efficient getters/functions
trait ImmutableParseTreeLike extends ParseTree2 {
  lazy val _children = {
    val cs = Array.tabulate(_parents.size)(_ => List[Int]())
    ((_parents.length -1) to 0 by -1).foreach(child => {
      val parentIdx = _parents(child).value
      if(parentIdx >= 0) cs(parentIdx) = child :: cs(parentIdx)
    })
    cs
  }

  /** Return the token at the root of the parse tree.  The parent of this token is null.  The parentIndex of this position is -1. */
  override lazy val rootChild: ParseTreeVertex = super.rootChild

  /** Returns the position in the sentence of the root token. */
  override lazy val rootChildIndex: Int = super.rootChildIndex

  /** Return the sentence index of the first token whose parent is 'parentIndex' */
  override protected def firstChild(parentIndex:Int): Int = {
    if(parentIndex < 0) rootChildIndex
    else _children(parentIndex).headOption.getOrElse(-1)
  }

  override def getChildrenIndices(parentIndex:Int, filter : Int => Boolean = defaultFilter): Seq[Int] = {
    if(filter == defaultFilter)
      _children(parentIndex)
    else
      _children(parentIndex).filter( i => !filter(i))
  }

  override def leftChildren(parentIndex:Int) = getChildrenIndices(parentIndex).withFilter(_ < parentIndex).map(_vertices(_))

  override def rightChildren(parentIndex:Int) = getChildrenIndices(parentIndex).withFilter(_ > parentIndex).map(_vertices(_))

  /** Return a list of tokens who are the children of the token at sentence position 'parentIndex' and who also have the indicated label value. */
  override def childrenLabeled(parentIndex:Int, labelIntValue:Int) =
    getChildrenIndices(parentIndex).
      withFilter(i => _labels(i).intValue == labelIntValue).
      map(_vertices(_))

  override def leftChildrenLabeled(parentIndex:Int, labelIntValue:Int) =
    getChildrenIndices(parentIndex).
      withFilter(i => i < parentIndex && _labels(i).intValue == labelIntValue).
      map(_vertices(_))

  override def rightChildrenLabeled(parentIndex:Int, labelIntValue:Int) =
    getChildrenIndices(parentIndex).
      withFilter(i => i > parentIndex && _labels(i).intValue == labelIntValue).
      map(_vertices(_))
}

// Example usages:
// token.sentence.attr[ParseTree].parent(token)
// sentence.attr[ParseTree].children(token)
// sentence.attr[ParseTree].setParent(token, parentToken)
// sentence.attr[ParseTree].label(token)
// sentence.attr[ParseTree].label(token).set("SUBJ")

// Methods also created in Token supporting:
// token.parseParent
// token.setParseParent(parentToken)
// token.parseChildren
// token.parseLabel
// token.leftChildren
