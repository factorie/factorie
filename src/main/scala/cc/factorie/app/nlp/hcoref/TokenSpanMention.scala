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

package cc.factorie.app.nlp.hcoref
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}
import cc.factorie.util.{Cubbie,CubbieRefs}
import cc.factorie.util.Attr

trait TokenSpanMention extends TokenSpan with Entity


abstract class TokenSpanMentionCubbie extends TokenSpanCubbie {
  // Unfortunately we can't inherit from both TokenSpanCubbie and EntityCubbie
  val entityRef = RefSlot("entityRef", () => newEntityCubbie)
  def newEntityCubbie: TokenSpanMentionCubbie
  def newTokenSpanMention(doc:Document, start:Int, length:Int): TokenSpanMention
  def storeTokenSpanMention(tsm:TokenSpanMention): this.type = {
    storeTokenSpan(tsm)
    entityRef := tsm.parentEntity.id
    this
  }
  def fetchTokenSpanMention(doc:Document): TokenSpanMention = {
    val tsm = newTokenSpanMention(doc, start.value, length.value)
    throw new Error("Not yet implemented")
    tsm.parentEntity.setParentEntity(null)(null)
    finishFetchTokenSpan(tsm)
    tsm
  }
}
