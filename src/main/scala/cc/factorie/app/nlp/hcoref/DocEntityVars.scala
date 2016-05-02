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
package cc.factorie.app.nlp.hcoref

import cc.factorie.app.nlp.coref.WithinDocEntity
import cc.factorie.variable.{BagOfWordsVariable, DenseDoubleBagVariable, DiffList}

/**
 * @author John Sullivan
 */
class DocEntityVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val nerType:BagOfWordsVariable, val mention:BagOfWordsVariable, val number:BagOfWordsVariable, val truth:BagOfWordsVariable) extends NodeVariables[DocEntityVars] with Canopy with GroundTruth {
  val getVariables = Seq(names, context, nerType, mention, number)

  def canopies = names.value.asHashMap.keySet

  def this() = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable())
  def this(names:BagOfWordsVariable, context:BagOfWordsVariable, gender:BagOfWordsVariable, mention:BagOfWordsVariable, number:BagOfWordsVariable) = this(names, context, gender, mention, number, new BagOfWordsVariable())

  def --=(other: DocEntityVars)(implicit d: DiffList) = {
    this.names.remove(other.names.value)(d)
    this.context.remove(other.context.value)(d)
    this.nerType.remove(other.nerType.value)(d)
    this.mention.remove(other.mention.value)(d)
    this.number.remove(other.number.value)(d)
    this.truth.remove(other.truth.value)(d)
  }


  def ++=(other: DocEntityVars)(implicit d: DiffList) = {
    this.names.add(other.names.value)(d)
    this.context.add(other.context.value)(d)
    this.nerType.add(other.nerType.value)(d)
    this.mention.add(other.mention.value)(d)
    this.number.add(other.number.value)(d)
    this.truth.add(other.truth.value)(d)
  }

  def --(other: DocEntityVars)(implicit d: DiffList) = new DocEntityVars(this.names -- other.names, this.context -- other.context, this.nerType -- other.nerType, this.mention -- other.mention, this.number -- other.number, this.truth -- other.truth)

  def ++(other: DocEntityVars)(implicit d: DiffList) = new DocEntityVars(this.names ++ other.names, this.context ++ other.context, this.nerType ++ other.nerType, this.mention ++ other.mention, this.number ++ other.number, this.truth ++ other.truth)
}


class DenseDocEntityVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val nerType:BagOfWordsVariable, val contextVec:DenseDoubleBagVariable, val number:BagOfWordsVariable, val truth:BagOfWordsVariable) extends NodeVariables[DenseDocEntityVars] with Canopy with GroundTruth {
  val getVariables = Seq(names, context, nerType, contextVec, number)

  def canopies = names.value.asHashMap.keySet

  def this() = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new DenseDoubleBagVariable(50), new BagOfWordsVariable(), new BagOfWordsVariable())
  def this(names:BagOfWordsVariable, context:BagOfWordsVariable, nerType:BagOfWordsVariable, contextVec:DenseDoubleBagVariable, number:BagOfWordsVariable) = this(names, context, nerType, contextVec, number, new BagOfWordsVariable())

  def --=(other: DenseDocEntityVars)(implicit d: DiffList) = {
    this.names.remove(other.names.value)(d)
    this.context.remove(other.context.value)(d)
    this.nerType.remove(other.nerType.value)(d)
    this.contextVec.remove(other.contextVec.value)(d)
    this.number.remove(other.number.value)(d)
    this.truth.remove(other.truth.value)(d)
  }


  def ++=(other: DenseDocEntityVars)(implicit d: DiffList) = {
    this.names.add(other.names.value)(d)
    this.context.add(other.context.value)(d)
    this.nerType.add(other.nerType.value)(d)
    this.contextVec.add(other.contextVec.value)(d)
    this.number.add(other.number.value)(d)
    this.truth.add(other.truth.value)(d)
  }

  def --(other: DenseDocEntityVars)(implicit d: DiffList) = new DenseDocEntityVars(this.names -- other.names, this.context -- other.context, this.nerType -- other.nerType, this.contextVec -- other.contextVec, this.number -- other.number, this.truth -- other.truth)

  def ++(other: DenseDocEntityVars)(implicit d: DiffList) = new DenseDocEntityVars(this.names ++ other.names, this.context ++ other.context, this.nerType ++ other.nerType, this.contextVec ++ other.contextVec, this.number ++ other.number, this.truth ++ other.truth)
}

object DocEntityVars {
  def fromWithinDocEntity(e:WithinDocEntity):DocEntityVars = {
    val nameBag = new BagOfWordsVariable()
    val contextBag = new BagOfWordsVariable()
    val nerBag = new BagOfWordsVariable()
    val mentionBag = new BagOfWordsVariable()
    val numberBag = new BagOfWordsVariable()

    e.mentions.foreach { mention =>
      contextBag ++= mention.phrase.contextWindow(5).groupBy(_.lemmaString).mapValues(_.size.toDouble)
      //nameBag += mention.phrase.string
      //todo filter nominal mentions
      nameBag ++= mention.phrase.tokens.map(_.string)
      Option(mention.phrase.head.nerTag) match {
        case Some(tag) => nerBag += tag.baseCategoryValue
        case None => ()
      }
      Option(mention.phrase.number) match {
        case Some(number) => numberBag += number.categoryValue
        case None => ()
      }
    }
    // each other entity in the document
    e.document.coref.entities.filterNot(_.uniqueId == e.uniqueId).foreach { entity =>
      mentionBag += entity.canonicalName
    }
    new DocEntityVars(nameBag, contextBag, nerBag, mentionBag, numberBag)
  }
}

