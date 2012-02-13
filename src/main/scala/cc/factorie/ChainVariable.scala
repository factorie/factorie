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

/*trait ElementType[+E<:AnyRef] {
  type ElementType = E
}*/

// TODO Consider getting rid of this, now that InChain is not longer covariant in C
trait ChainType[+C<:AnyRef] {
  type ChainType = C
}

// Used by app.chain.Observation and app.chain.Lexicon.LexiconToken
trait AbstractChainLink[+This<:AbstractChainLink[This]] {
  this: This =>
  def hasNext: Boolean
  def hasPrev: Boolean
  def next: This
  def prev: This
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
}

/** An element or "link" of a Chain, having methods "next", "prev", etc. */
trait ChainLink[This<:ChainLink[This,C],C<:Chain[C,This]] extends AbstractChainLink[This] with ThisType[This] with ChainType[C] {
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
  def seqAfter = chain.drop(_position+1)
  def seqBefore = chain.take(_position)
  def prevWindow(n:Int): Seq[This] = for (i <- math.max(_position-n, 0) to math.max(_position-1,0)) yield chain(i)
  def nextWindow(n:Int): Seq[This] = for (i <- math.min(_position+1, _chain.length-1) to math.min(_position+n, _chain.length-1)) yield chain(i)
  def prevWindowNum(n:Int): IndexedSeq[(Int,This)] = for (i <- math.max(_position-n, 0) until math.max(_position,0)) yield ((i-_position).abs->chain(i))
  def nextWindowNum(n:Int): IndexedSeq[(Int,This)] = for (i <- math.min(_position+1, _chain.length-1) until math.min(_position+n+1, _chain.length-1)) yield ((i-_position).abs->chain(i))
  def window(n:Int): Seq[This] = for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1)) yield chain(i)
  def windowWithoutSelf(n:Int): Seq[This] = for (i <- math.max(_position-n,0) to math.min(_position+n, _chain.length-1); if (i != _position)) yield chain(i)
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
trait Chain[This<:Chain[This,E],E<:ChainLink[E,This]] extends IndexedSeqEqualsEq[E] with ThisType[This] with ElementType[E] {
  this: This =>
  private val _chainseq = new scala.collection.mutable.ArrayBuffer[E]
  def apply(i:Int): E = _chainseq(i)
  def length = _chainseq.length
  def value = _chainseq // TODO But this isn't actually immutable. :-(  Inefficient to copy whole seq though. 
  def +=(e:E): Unit = {
    e._setChainPosition(this, _chainseq.length)
    _chainseq += e
  }
  def ++=(es:Iterable[E]): Unit = es.foreach(+=(_))
}

/** A Chain that is also a Variable, with value IndexedSeq[ElementType] */
trait ChainVar[This<:ChainVar[This,E],E<:ChainLink[E,This]] extends Chain[This,E] with IndexedSeqVar[E] with VarAndValueGenericDomain[ChainVar[This,E],IndexedSeq[E]] {
  this: This =>
}

class ChainVariable[This<:ChainVariable[This,E],E<:ChainLink[E,This]] extends ChainVar[This,E] {
  this: This =>
  def this(elements:Iterable[E]) = { this(); elements.foreach(+=(_)) }
}

/** A Chain which itself is also an element of an outer Chain */
trait ChainInChain[This<:ChainInChain[This,E,S],E<:ChainLink[E,This],S<:Chain[S,This]] extends ChainLink[This,S] with Chain[This,E] {
  this: This =>
}
