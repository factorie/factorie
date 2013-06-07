package cc.factorie.app.nlp.lexicon

import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.TokenSpan
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer,NoopLemmatizer}
import scala.collection.mutable.{ArrayBuffer,HashMap}
import scala.io.Source
import java.io.File

// Several standard lexicons available through the factorie-nlp-lexicons.jar

// To read from .jar in classpath use source = io.Source.fromInputStream(getClass.getResourceAsStream(_))
// To read from directory in filesystem use source = io.Source.fromFile(new File(_))
class ResourceLexicons(val source: String=>io.Source, val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer) {
  class WordLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.WordLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= source(dir + "/" + name + ".txt") } catch { case e => { e.printStackTrace; throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }
  class PhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.PhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) { this ++= source(dir + "/" + name + ".txt") }
  object iesl {
    private implicit val dir = "iesl"
    object Continents extends PhraseLexicon("continents") // TODO Rename this to continent
    object Country extends PhraseLexicon("country")
    object City extends PhraseLexicon("city")
    object USState extends PhraseLexicon("us-state")
    object PlaceSuffix extends WordLexicon("place-suffix")

    object JobTitle extends PhraseLexicon("jobtitle") // TODO Rename file to job-title
    object Money extends WordLexicon("money")

    object Company extends PhraseLexicon("company")
    object OrgSuffix extends WordLexicon("org-suffix")

    object Month extends WordLexicon("month")
    object Day extends WordLexicon("day")
    
    object PersonHonorific extends WordLexicon("person-honorific")
    object PersonFirstHighest extends WordLexicon("person-first-highest")
    object PersonFirstHigh extends WordLexicon("person-first-high")
    object PersonFirstMedium extends WordLexicon("person-first-medium")
    object PersonFirst extends UnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)
    
    object PersonLastHighest extends WordLexicon("person-last-highest")
    object PersonLastHigh extends WordLexicon("person-last-high")
    object PersonLastMedium extends WordLexicon("person-last-medium")
    object PersonLast extends UnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)
  }
  
  object uscensus {
    private implicit val dir = "uscensus"
    object PersonFirstFemale extends WordLexicon("person-first-female")
    object PersonFirstMale extends WordLexicon("person-first-male")
    object PersonLast extends WordLexicon("person-last")
  }
  
  object wikipedia {
    private implicit val dir = "wikipedia"
    object Person extends PhraseLexicon("person")
    object Organization extends PhraseLexicon("organization")
    object Location extends PhraseLexicon("location")
    object Event extends PhraseLexicon("events") // TODO Change this name to event
    object Battle extends PhraseLexicon("battle")
    object Competition extends PhraseLexicon("competition")
  }
}

object ClasspathResourceLexicons extends ResourceLexicons(s => io.Source.fromInputStream(StopWords.getClass.getResourceAsStream(s)))
