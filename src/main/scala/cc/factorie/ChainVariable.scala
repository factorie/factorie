/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

// Used by app.chain.Observation and app.chain.Lexicon.LexiconToken
trait AbstractChainLink[+This<:AbstractChainLink[This]] {
  this: This =>
  def hasNext: Boolean
  def hasPrev: Boolean
  def next: This
  def prev: This
  def position: Int
  def next(offset:Int): This = {
    if (offset < 0) return prev(-offset)
    var i = offset
    var result = this
    while (i > 0 && hasNext) {
      result = next
      i -= 1
    }
    if (i == 0) result
    else null.asInstanceOf[This]
  }
  def prev(offset:Int): This = {
    if (offset < 0) return next(-offset)
    var i = offset
    var result = this
    while (i > 0 && hasPrev) {
      result = prev
      i -= 1
    }
    if (i == 0) result
    else null.asInstanceOf[This]
  }
  def chainHead: This = { var result = this; while (result.hasPrev) result = prev; result }
  def chainLast: This = { var result = this; while (result.hasPrev) result = prev; result }
}

/** An element or "link" of a Chain, having methods "next", "prev", etc. */
trait ChainLink[This<:ChainLink[This,C],C<:Chain[C,This]] extends AbstractChainLink[This] with ThisType[This] {
  this: This =>
  private var _position: Int = -1
  private var _chain: C = null.asInstanceOf[C]
  // This method should never be called outside Chain.+=
  def _setChainPosition(c:C, p:Int): Unit = {
    require(_chain eq null)
    require(p >= 0)
    _chain = c
    _position = p
  }
  def chain: C = _chain
  def position: Int = _position
  
  def hasNext = if (_position == -1) throw new IllegalStateException("InChain position not yet set") else _chain != null && _position + 1 < _chain.length
  def next: This = if (_position == -1) throw new IllegalStateException("InChain position not yet set") else if (_position + 1 < _chain.length) chain(_position + 1) else null.asInstanceOf[This]
  def hasPrev = if (_position == -1) throw new IllegalStateException("InChain position not yet set") else _chain != null && _position > 0
  def prev: This = if (_position == -1) throw new IllegalStateException("InChain position not yet set") else if (_position > 0) chain(_position - 1) else null.asInstanceOf[This]
  override def next(n:Int): This = { 
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set")
    val i = _position + n
    if (i >= 0 && i < _chain.length) chain(i) else null.asInstanceOf[This]
  }
  override def prev(n:Int): This = {
    if (_position == -1) throw new IllegalStateException("VarInSeq position not yet set") 
    val i = _position - n
    if (i >= 0 && i < _chain.length) chain(i) else null.asInstanceOf[This]
  }
  def chainAfter: IndexedSeq[This] = _chain.links.drop(_position+1)
  def chainBefore: IndexedSeq[This] = _chain.links.take(_position)
  def prevWindow(n:Int): Seq[This] = {
    var i = math.max(_position-n,  0)
    val res = new collection.mutable.ListBuffer[This]
    while (i <= math.max(_position-1, 0)) {res.append(chain(i)) ;i += 1}
    res
  }
  def nextWindow(n:Int): Seq[This] = {
    var i = math.min(_position+1,  _chain.length-1)
    val res = new collection.mutable.ListBuffer[This]
    while (i <= math.min(_position+n, _chain.length-1)) {res.append(chain(i)) ;i += 1}
    res
  }
  def window(n:Int): Seq[This] = {
    for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1)) yield chain(i)
  }
  def windowWithoutSelf(n:Int): Seq[This] = {
    for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1); if (i != _position)) yield chain(i)
  }
  def between(other:This): Seq[This] = {
    require(other.chain == chain)
    if (other.position > _position)
      for (i <- _position until other.position) yield chain(i)
    else
      for (i <- other.position until _position) yield chain(i)
  } 
  def firstInSeq = chain(0)
  /*class InSubChain[+This2<:InSubChain[This,C2],+C2<:Chain[C2,This2]](val subchain:C) extends InChain[This2,C2] {
    this: This2 =>
    def elt: This = InChain.this
  }*/
}

/** A chain of elements, each of which has methods "next", "prev", etc */
trait Chain[This<:Chain[This,E],E<:ChainLink[E,This]] extends ThisType[This] with ElementType[E] {
  this: This =>
  private val _chainseq = new scala.collection.mutable.ArrayBuffer[E]
  def apply(i:Int): E = _chainseq(i)
  def length = _chainseq.length
  @inline final def links: IndexedSeq[E] = _chainseq
  def +=(e:E): this.type = {
    e._setChainPosition(this, _chainseq.length)
    _chainseq += e
    this
  }
  def ++=(es:Iterable[E]): this.type = { es.foreach(+=(_)); this }
  def asSeq: IndexedSeq[E] = _chainseq
}

/** A Chain that is also a Variable, with value IndexedSeq[ElementType] */
trait ChainVar[This<:ChainVar[This,E],E<:ChainLink[E,This]] extends Chain[This,E] with IndexedSeqVar[E] with VarAndValueGenericDomain[ChainVar[This,E],IndexedSeq[E]] {
  this: This =>
  def value: IndexedSeq[E] = links // TODO But this isn't actually immutable. :-(  Inefficient to copy whole seq though. 
}

class ChainVariable[This<:ChainVariable[This,E],E<:ChainLink[E,This]] extends ChainVar[This,E] {
  this: This =>
  def this(elements:Iterable[E]) = { this(); elements.foreach(+=(_)) }
}

/** A Chain which itself is also an element of an outer Chain */
trait ChainInChain[This<:ChainInChain[This,E,S],E<:ChainLink[E,This],S<:Chain[S,This]] extends ChainLink[This,S] with Chain[This,E] {
  this: This =>
}
