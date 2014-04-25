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



