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
package cc.factorie.app.nlp.lexicon

import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer}
import scala.io.Source
import java.io.File
import java.util.jar.JarFile
import cc.factorie.util.ClasspathURL

// Several standard lexicons available through the factorie-nlp-lexicons.jar

// To read from .jar in classpath use source = io.Source.fromInputStream(getClass.getResourceAsStream(_))
// To read from directory in filesystem use source = io.Source.fromFile(new File(_))
class ResourceLexicons(val sourceFactory: String=>io.Source, val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer) {
  /** deprecated **/
  class ChainWordLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.ChainWordLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    this ++= sourceFactory(dir + "/" + name + ".txt")
  }
  /** deprecated **/
  class ChainPhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.ChainPhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => { throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }

  class PhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.PhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => {throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }

  class TriePhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.TriePhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => {throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }

  object iesl {
    private implicit val dir = "iesl"

    object Continents extends TriePhraseLexicon("continents")
    object Country extends TriePhraseLexicon("country")
    object City extends TriePhraseLexicon("city")
    object USState extends TriePhraseLexicon("us-state")
    object PlaceSuffix extends TriePhraseLexicon("place-suffix")
    object AllPlaces extends TrieUnionLexicon("place-suffix", Continents, Country, City, USState)

    object JobTitle extends TriePhraseLexicon("jobtitle") // TODO Rename file to job-title
    object Money extends TriePhraseLexicon("money")

    object TestCompany extends ChainPhraseLexicon("company")
    object Company extends TriePhraseLexicon("company")
    object OrgSuffix extends TriePhraseLexicon("org-suffix")

    object Month extends TriePhraseLexicon("month")
    object Day extends TriePhraseLexicon("day")

    object PersonHonorific extends TriePhraseLexicon("person-honorific")
    object PersonFirstHighest extends TriePhraseLexicon("person-first-highest")
    object PersonFirstHigh extends TriePhraseLexicon("person-first-high")
    object PersonFirstMedium extends TriePhraseLexicon("person-first-medium")
    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLastHighest extends TriePhraseLexicon("person-last-highest")
    object PersonLastHigh extends TriePhraseLexicon("person-last-high")
    object PersonLastMedium extends TriePhraseLexicon("person-last-medium")
    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

    object Say extends TriePhraseLexicon("say")

    object Demonym extends TriePhraseLexicon("demonyms") {
      try {
        for (line <- sourceFactory(dir + "/demonyms.txt").getLines(); entry <- line.trim.split("\t")) this += entry
      } catch { case e:java.io.IOException => { throw new Error("Could not find "+dir+"/demonyms\n") } }
    }

    // Map from Chilean->Chile and Chileans->Chile
    object DemonymMap extends scala.collection.mutable.HashMap[String,String] {
      try {
        for (line <- sourceFactory(dir + "/demonyms.txt").getLines()) {
          val entries = line.trim.split("\t")
          val value = entries.head
          entries.foreach(e => this.update(e, value))
        }
      } catch { case e:java.io.IOException => { throw new Error("Could not find "+dir+"/demonyms\n") } }
    }
  }

  // TODO Move these here
  object ssdi {
    private implicit val dir = "ssdi"

    object PersonFirstHighest extends TriePhraseLexicon("person-first-highest")
    object PersonFirstHigh extends TriePhraseLexicon("person-first-high")
    object PersonFirstMedium extends TriePhraseLexicon("person-first-medium")
    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLastHighest extends TriePhraseLexicon("person-last-highest")
    object PersonLastHigh extends TriePhraseLexicon("person-last-high")
    object PersonLastMedium extends TriePhraseLexicon("person-last-medium")
    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)
  }

  object uscensus {
    private implicit val dir = "uscensus"
    object PersonFirstFemale extends TriePhraseLexicon("person-first-female")
    object PersonFirstMale extends TriePhraseLexicon("person-first-male")
    object PersonLast extends TriePhraseLexicon("person-last")
  }

  object wikipedia {
    private implicit val dir = "wikipedia"

    object Battle extends TriePhraseLexicon("battle")
    object BattleRedirect extends TriePhraseLexicon("battle-redirect")
    object BattleAndRedirect extends TrieUnionLexicon("battle-and-redirect", Battle, BattleRedirect)
    object BattleDisambiguation extends TriePhraseLexicon("battle-disambiguation")
    //object BattleParen extends TriePhraseLexicon("battle-paren")
    //object BattleRedirectParen extends TriePhraseLexicon("battle-redirect-paren")

    object Book extends TriePhraseLexicon("book")
    object BookRedirect extends TriePhraseLexicon("book-redirect")
    object BookAndRedirect extends TrieUnionLexicon("book-and-redirect", Book, BookRedirect)
    object BookDisambiguation extends TriePhraseLexicon("book-disambiguation")
    //object BookParen extends TriePhraseLexicon("book-paren")
    //object BookRedirectParen extends TriePhraseLexicon("book-redirect-paren")


    object Business extends TriePhraseLexicon("business")
    object BusinessRedirect extends TriePhraseLexicon("business-redirect")
    object BusinessAndRedirect extends TrieUnionLexicon("business-and-redirect", Business, BusinessRedirect)
    object BusinessDisambiguation extends TriePhraseLexicon("business-disambiguation")
    //object BusinessParen extends TriePhraseLexicon("business-paren")
    //object BusinessRedirectParen extends TriePhraseLexicon("business-redirect-paren")

    object Competition extends TriePhraseLexicon("competition")
    object CompetitionRedirect extends TriePhraseLexicon("competition-redirect")
    object CompetitionAndRedirect extends TrieUnionLexicon("competition-and-redirect", Competition, CompetitionRedirect)
    object CompetitionDisambiguation extends TriePhraseLexicon("competition-disambiguation")
    //object CompetitionParen extends TriePhraseLexicon("competition-paren")
    //object CompetitionRedirectParent extends TriePhraseLexicon("competition-redirect-paren")

    object Event extends TriePhraseLexicon("events") // TODO Change this name to event
    object EventRedirect extends TriePhraseLexicon("events-redirect")
    object EventAndRedirect extends TrieUnionLexicon("events-and-redirect", Event, EventRedirect)
    object EventDisambiguation extends TriePhraseLexicon("event-disambiguation")
    //object EventParen extends TriePhraseLexicon("events-paren")
    //object EventRedirectParen extends TriePhraseLexicon("events-redirect-paren")

    object Film extends TriePhraseLexicon("film")
    object FilmRedirect extends TriePhraseLexicon("film-redirect")
    object FilmAndRedirect extends TrieUnionLexicon("film-and-redirect", Film, FilmRedirect)
    object FilmDisambiguation extends TriePhraseLexicon("film-disambiguation")
    //object FilmParen extends TriePhraseLexicon("film-paren")
    //object FilmRedirectParen extends TriePhraseLexicon("film-redirect-paren")

    object Location extends TriePhraseLexicon("location")
    object LocationRedirect extends TriePhraseLexicon("location-redirect")
    object LocationAndRedirect extends TrieUnionLexicon("location-and-redirect", Location, LocationRedirect)
    object LocationDisambiguation extends TriePhraseLexicon("location-disambiguation")
    //object LocationParen extends TriePhraseLexicon("location-paren")
    //object LocationRedirectParen extends TriePhraseLexicon("location-redirect-paren")

    object ManMadeThing extends TriePhraseLexicon("man_made_thing")
    object ManMadeThingRedirect extends TriePhraseLexicon("man_made_thing-redirect")
    object ManMadeThingAndRedirect extends TrieUnionLexicon("man_made_thing-and-redirect", ManMadeThing, ManMadeThingRedirect)
    object ManMadeThingDisambiguation extends TriePhraseLexicon("man_made_thing-disambiguation")
    //object ManMadeThingParen extends TriePhraseLexicon("man_made_thing-paren")
    //object ManMadeThingRedirectParen extends TriePhraseLexicon("man_made_thing-redirect-paren")

    object Organization extends TriePhraseLexicon("organization")
    object OrganizationRedirect extends TriePhraseLexicon("organization-redirect")
    object OrganizationAndRedirect extends TrieUnionLexicon("organization-and-redirect", Organization, OrganizationRedirect)
    object OrganizationDisambiguation extends TriePhraseLexicon("organization-disambiguation")
    //object OrganizationParen extends TriePhraseLexicon("organization-paren")
    //object OrganizationRedirectParen extends TriePhraseLexicon("organization-redirect-paren")

    object Person extends TriePhraseLexicon("person")
    object PersonRedirect extends TriePhraseLexicon("person-redirect")
    object PersonAndRedirect extends TrieUnionLexicon("person-and-redirect", Person, PersonRedirect)
    object PersonDisambiguation extends TriePhraseLexicon("person-disambiguation")
    //object PersonParen extends TriePhraseLexicon("person-paren") // paren lines need more processing to be useful
    //object PersonRedirectParen extends TriePhraseLexicon("person-redirect-paren")

    object Song extends TriePhraseLexicon("song")
    object SongRedirect extends TriePhraseLexicon("song-redirect")
    object SongAndRedirect extends TrieUnionLexicon("song-and-redirect", Song, SongRedirect)
    object SongDisambiguation extends TriePhraseLexicon("song-disambiguation")
    //object SongParen extends TriePhraseLexicon("song-paren") // paren lines need more processing to be useful
    //object SongRedirectParen extends TriePhraseLexicon("song-redirect-paren")

  }

  /**
   * Mandarin Chinese lexicons collected from various sources around the WWW
   * @author Kate Silverstein
   */
  object mandarin {
    private implicit val dir = "mandarin"

    object SurnamePinyin extends TriePhraseLexicon("surname-pinyin")
    object GivenNamePinyin extends TriePhraseLexicon("givenname-pinyin")
  }
}

/** Static access through classpath or file location (specified as Java System Property)
    @author Andrew McCallum */
object ClasspathResourceLexicons extends ResourceLexicons(string => { io.Source.fromURL(ClasspathURL.fromDirectory[Lexicon](string))(io.Codec.UTF8) })
