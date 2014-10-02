package cc.factorie.app.nlp.hcoref

import cc.factorie.variable.{BagOfWordsVariable, DiffList}
import cc.factorie.app.nlp.coref.WithinDocEntity

/**
 * @author John Sullivan
 */
class DocEntityVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val gender:BagOfWordsVariable, val mention:BagOfWordsVariable, val number:BagOfWordsVariable) extends NodeVariables[DocEntityVars] with Canopy {
  val getVariables = Seq(names, context, gender, mention, number)

  def canopies = names.value.asHashMap.keySet

  def this() = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable())

  def --=(other: DocEntityVars)(implicit d: DiffList) = {
    this.names.remove(other.names.value)(d)
    this.context.remove(other.context.value)(d)
    this.gender.remove(other.gender.value)(d)
    this.mention.remove(other.mention.value)(d)
    this.number.remove(other.number.value)(d)
  }


  def ++=(other: DocEntityVars)(implicit d: DiffList) = {
    this.names.add(other.names.value)(d)
    this.context.add(other.context.value)(d)
    this.gender.add(other.gender.value)(d)
    this.mention.add(other.mention.value)(d)
    this.number.add(other.number.value)(d)
  }

  def --(other: DocEntityVars)(implicit d: DiffList) = new DocEntityVars(this.names -- other.names, this.context -- other.context, this.gender -- other.gender, this.mention -- other.mention, this.number -- other.number)

  def ++(other: DocEntityVars)(implicit d: DiffList) = new DocEntityVars(this.names ++ other.names, this.context ++ other.context, this.gender ++ other.gender, this.mention ++ other.mention, this.number ++ other.number)
}

object DocEntityVars {
  def fromWithinDocEntity(e:WithinDocEntity):DocEntityVars = {
    val nameBag = new BagOfWordsVariable()
    val contextBag = new BagOfWordsVariable()
    val genderBag = new BagOfWordsVariable()
    val mentionBag = new BagOfWordsVariable()
    val numberBag = new BagOfWordsVariable()

    e.mentions.foreach { mention =>
      contextBag ++= mention.phrase.contextWindow(5).groupBy(_.lemmaString).mapValues(_.size.toDouble)
      //nameBag += mention.phrase.string
      //todo filter nominal mentions
      nameBag ++= mention.phrase.tokens.map(_.string)
      Option(mention.phrase.gender) match {
        case Some(gender) => genderBag += gender.categoryValue
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
    new DocEntityVars(nameBag, contextBag, genderBag, mentionBag, numberBag)
  }
}

