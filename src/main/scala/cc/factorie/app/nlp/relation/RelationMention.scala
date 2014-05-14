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
package cc.factorie.app.nlp.relation

import cc.factorie.util.Attr
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.coref._
import cc.factorie.variable.StringVar

/** A binary relation between two Mentions.  Any labels or attributes of the relation are attached on this.attr. */
class RelationMention(val mention1:Mention, val mention2:Mention) extends Attr

/** An attr of PhraseRelation that represents the relation type simply as a String,
    in the style of "Universal Schema".
    @author Andrew McCallum */
case class UniversalRelationMentionType(relation:RelationMention, value:String) extends StringVar 


// TODO Might we want something like this too?

/** A binary relation between two Phrases.  Any labels or attributes of the relation are attached on this.attr.
    in the style of "Universal Schema".
    @author Andrew McCallum */
class PhraseRelation(val phrase1:Phrase, val phrase2:Phrase) extends Attr

/** An attr of PhraseRelation that represents the relation type simply as a String. */
case class UniversalPhraseRelationType(relation:PhraseRelation, value:String) extends StringVar 



