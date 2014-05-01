package cc.factorie.app.nlp.coref
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase._
import cc.factorie.app.nlp.ner._

/** A dead-simple deterministic coreference system that operates only on named entities
    and resolves coreference only by exact string match. */
object DeterministicNamedCoref extends DocumentAnnotator {
  def prereqAttrs: Seq[Class[_]] = ConllProperNounPhraseFinder.prereqAttrs
  def postAttrs = Seq(classOf[WithinDocCoref])
  def tokenAnnotationString(token:Token): String = ???
  def process(document: Document) = {
    val phrases = ConllProperNounPhraseFinder(document)
    val coref = new WithinDocCoref(document)
    for (phrase <- phrases) {
      val targetString = phrase.tokensString(" ")
      // Find an entity whose canonical mention is an exact string match
      val entityOption = coref.entities.find(_.canonicalMention.string == targetString)
      if (entityOption.isDefined) coref.addMention(phrase, entityOption.get)
      else { val entity = coref.newEntity(); val mention = coref.addMention(phrase, entity); entity.canonicalMention = mention }
    }
    document.attr += coref
    document
  }

}
