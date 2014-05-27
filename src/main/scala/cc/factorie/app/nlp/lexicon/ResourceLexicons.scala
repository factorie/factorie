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
  class WordLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.WordLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    this ++= sourceFactory(dir + "/" + name + ".txt")
  }
  class PhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.PhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => { throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }

  class HashyLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.HashyLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => {throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }

  object iesl {
    private implicit val dir = "iesl"

    object Continents extends HashyLexicon("continents")
    object Country extends HashyLexicon("country")
    object City extends HashyLexicon("city")
    object USState extends HashyLexicon("us-state")
    object PlaceSuffix extends HashyLexicon("place-suffix")
    object AllPlaces extends HashyUnionLexicon("place-suffix", Continents, Country, City, USState)

    object JobTitle extends HashyLexicon("jobtitle") // TODO Rename file to job-title
    object Money extends HashyLexicon("money")

    object Company extends HashyLexicon("company")
    object OrgSuffix extends HashyLexicon("org-suffix")

    object Month extends HashyLexicon("month")
    object Day extends HashyLexicon("day")

    object PersonHonorific extends HashyLexicon("person-honorific")
    object PersonFirstHighest extends HashyLexicon("person-first-highest")
    object PersonFirstHigh extends HashyLexicon("person-first-high")
    object PersonFirstMedium extends HashyLexicon("person-first-medium")
    object PersonFirst extends HashyUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLastHighest extends HashyLexicon("person-last-highest")
    object PersonLastHigh extends HashyLexicon("person-last-high")
    object PersonLastMedium extends HashyLexicon("person-last-medium")
    object PersonLast extends HashyUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

    object Say extends HashyLexicon("say")

    object Demonym extends PhraseLexicon(dir+"/demonyms") {
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

    object Battle extends HashyLexicon("battle")
    object BattleRedirect extends HashyLexicon("battle-redirect")
    object BattleAndRedirect extends HashyUnionLexicon("battle-and-redirect", Battle, BattleRedirect)
    object BattleDisambiguation extends HashyLexicon("battle-disambiguation")
    //object BattleParen extends PhraseLexicon("battle-paren")
    //object BattleRedirectParen extends PhraseLexicon("battle-redirect-paren")

    object Book extends HashyLexicon("book")
    object BookRedirect extends HashyLexicon("book-redirect")
    object BookAndRedirect extends HashyUnionLexicon("book-and-redirect", Book, BookRedirect)
    object BookDisambiguation extends HashyLexicon("book-disambiguation")
    //object BookParen extends PhraseLexicon("book-paren")
    //object BookRedirectParen extends HashyLexicon("book-redirect-paren")

    object Business extends HashyLexicon("business")
    object BusinessRedirect extends HashyLexicon("business-redirect")
    object BusinessAndRedirect extends HashyUnionLexicon("business-and-redirect", Business, BusinessRedirect)
    object BusinessDisambiguation extends HashyLexicon("business-disambiguation")
    //object BusinessParen extends PhraseLexicon("business-paren")
    //object BusinessRedirectParen extends PhraseLexicon("business-redirect-paren")

    object Competition extends HashyLexicon("competition")
    object CompetitionRedirect extends HashyLexicon("competition-redirect")
    object CompetitionAndRedirect extends HashyUnionLexicon("competition-and-redirect", Competition, CompetitionRedirect)
    object CompetitionDisambiguation extends HashyLexicon("competition-disambiguation")
    //object CompetitionParen extends PhraseLexicon("competition-paren")
    //object CompetitionRedirectParent extends PhraseLexicon("competition-redirect-paren")

    object Event extends HashyLexicon("events") // TODO Change this name to event
    object EventRedirect extends HashyLexicon("events-redirect")
    object EventAndRedirect extends HashyUnionLexicon("events-and-redirect", Event, EventRedirect)
    object EventDisambiguation extends HashyLexicon("event-disambiguation")
    //object EventParen extends PhraseLexicon("events-paren")
    //object EventRedirectParen extends HashyLexicon("events-redirect-paren")

    object Film extends HashyLexicon("film")
    object FilmRedirect extends HashyLexicon("film-redirect")
    object FilmAndRedirect extends HashyUnionLexicon("film-and-redirect", Film, FilmRedirect)
    object FilmDisambiguation extends HashyLexicon("film-disambiguation")
    //object FilmParen extends PhraseLexicon("film-paren")
    //object FilmRedirectParen extends PhraseLexicon("film-redirect-paren")

    object Location extends HashyLexicon("location")
    object LocationRedirect extends HashyLexicon("location-redirect")
    object LocationAndRedirect extends HashyUnionLexicon("location-and-redirect", Location, LocationRedirect)
    object LocationDisambiguation extends HashyLexicon("location-disambiguation")
    //object LocationParen extends PhraseLexicon("location-paren")
    //object LocationRedirectParen extends PhraseLexicon("location-redirect-paren")

    object ManMadeThing extends HashyLexicon("man_made_thing")
    object ManMadeThingRedirect extends HashyLexicon("man_made_thing-redirect")
    object ManMadeThingAndRedirect extends HashyUnionLexicon("man_made_thing-and-redirect", ManMadeThing, ManMadeThingRedirect)
    object ManMadeThingDisambiguation extends HashyLexicon("man_made_thing-disambiguation")
    //object ManMadeThingParen extends PhraseLexicon("man_made_thing-paren")
    //object ManMadeThingRedirectParen extends PhraseLexicon("man_made_thing-redirect-paren")

    object Organization extends HashyLexicon("organization")
    object OrganizationRedirect extends HashyLexicon("organization-redirect")
    object OrganizationAndRedirect extends HashyUnionLexicon("organization-and-redirect", Organization, OrganizationRedirect)
    object OrganizationDisambiguation extends HashyLexicon("organization-disambiguation")
    //object OrganizationParen extends PhraseLexicon("organization-paren")
    //object OrganizationRedirectParen extends PhraseLexicon("organization-redirect-paren")

    object Person extends HashyLexicon("person")
    object PersonRedirect extends HashyLexicon("person-redirect")
    object PersonAndRedirect extends HashyUnionLexicon("person-and-redirect", Person, PersonRedirect)
    object PersonDisambiguation extends HashyLexicon("person-disambiguation")
    //object PersonParen extends PhraseLexicon("person-paren") // paren lines need more processing to be useful
    //object PersonRedirectParen extends PhraseLexicon("person-redirect-paren")

    object Song extends HashyLexicon("song")
    object SongRedirect extends HashyLexicon("song-redirect")
    object SongAndRedirect extends HashyUnionLexicon("song-and-redirect", Song, SongRedirect)
    object SongDisambiguation extends HashyLexicon("song-disambiguation")
    //object SongParen extends PhraseLexicon("song-paren") // paren lines need more processing to be useful
    //object SongRedirectParen extends PhraseLexicon("song-redirect-paren")

  }
}

/** Static access through classpath or file location (specified as Java System Property)
    @author Andrew McCallum */
object ClasspathResourceLexicons extends ResourceLexicons(string => { io.Source.fromURL(ClasspathURL.fromDirectory[Lexicon](string))(io.Codec.UTF8) })
