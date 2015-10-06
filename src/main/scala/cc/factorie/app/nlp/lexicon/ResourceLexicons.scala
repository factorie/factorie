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

import cc.factorie.app.nlp.lexicon.{iesl => Iesl, uscensus => Uscensus, wikipedia => Wikipedia, ssdi => Ssdi}
import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.nlp.lemma.{Lemmatizer,LowercaseLemmatizer}
import scala.io.Source
import java.io.File
import cc.factorie.util.{ModelProvider, ClasspathURL}

import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions

class LexiconModelProvider[Lexicon](relativeRoot:File)(implicit ct:ClassTag[Lexicon]) extends ModelProvider[Lexicon, Source] {
  final def lexiconName = ct.runtimeClass.getName.zipWithIndex.flatMap {
    case (u, 0) => u.toLower.toString
    case (u, _) if u.isUpper => "-" + u.toLower
    case (l, _) => l.toString}.mkString("") + ".txt"

  final def provide: Source = Source.fromFile(relativeRoot.toPath.resolve(lexiconName).toFile)
}

trait LexiconsProvider {
  implicit def provide[L : ClassTag]:ModelProvider[L, Source]
}

object LexiconsProvider {

  private def lexiconNamePieces[L:ClassTag]:Seq[String] = {
    val arr = classTag[L].runtimeClass.getName.split("""\.""").map(_.toLowerCase.stripSuffix("$"))
    val fileName = arr.last.zipWithIndex.flatMap {
      case (u, 0) => u.toLower.toString
      case (u, _) if u.isUpper => "-" + u.toLower
      case (l, _) => l.toString
    }.mkString("") + ".txt"
    arr.init ++ Seq(fileName)
  }

  private def fullLexiconName[L:ClassTag] = lexiconNamePieces[L].mkString("/")
  private def shortLexiconName[L:ClassTag] = lexiconNamePieces[L].takeRight(2).mkString("/")


  def fromFile(f:File, useFullPath:Boolean = false):LexiconsProvider = new LexiconsProvider {
    override implicit def provide[L : ClassTag]: ModelProvider[L, Source] = new ModelProvider[L, Source] {
      def provide: Source = Source.fromFile(f.toPath.resolve(if(useFullPath) fullLexiconName[L] else shortLexiconName[L]).toFile)
    }
  }

  implicit def provideFile(f:File):LexiconsProvider = fromFile(f,false)

  @deprecated("This exists to preserve legacy functionality", "10/05/15")
  def classpath:LexiconsProvider = new LexiconsProvider {
    implicit def provide[L : ClassTag]: ModelProvider[L, Source] = new ModelProvider[L, Source] {
      def provide: Source = Source.fromURL(ClasspathURL.fromDirectory[Lexicon](fullLexiconName[L]))(io.Codec.UTF8)
    }
  }
}



trait ProvidedLexicon[L] {
  this: MutableLexicon =>

  def provider:ModelProvider[L, Source]

  this.++=(provider.provide)
}

abstract class ProvidedTriePhraseLexicon[L]()(implicit val provider:ModelProvider[L, Source], ct:ClassTag[L]) extends TriePhraseLexicon(ct.runtimeClass.getName) with ProvidedLexicon[L]

class StaticLexicons()(implicit lp:LexiconsProvider) {

  import lp._

  object iesl {

    object Continents extends Iesl.Continents()(lp.provide[Iesl.Continents])
    object Country extends Iesl.Country()(lp.provide[Iesl.Country])
    object City extends Iesl.City()(lp.provide[Iesl.City])
    object UsState extends Iesl.UsState()(lp.provide[Iesl.UsState])
    object PlaceSuffix extends Iesl.PlaceSuffix()(lp.provide[Iesl.PlaceSuffix])
    object JobTitle extends Iesl.JobTitle()(lp.provide[Iesl.JobTitle])
    object Money extends Iesl.Money()(lp.provide[Iesl.Money])
    object Company extends Iesl.Company()(lp.provide[Iesl.Company])
    object OrgSuffix extends Iesl.OrgSuffix()(lp.provide[Iesl.OrgSuffix])
    object Month extends Iesl.Month()(lp.provide[Iesl.Month])
    object Day extends Iesl.Day()(lp.provide[Iesl.Day])
    object PersonHonorific extends Iesl.PersonHonorific()(lp.provide[Iesl.PersonHonorific])
    object PersonFirstHighest extends Iesl.PersonFirstHighest()(lp.provide[Iesl.PersonFirstHighest])
    object PersonFirstHigh extends Iesl.PersonFirstHigh()(lp.provide[Iesl.PersonFirstHigh])
    object PersonFirstMedium extends Iesl.PersonFirstMedium()(lp.provide[Iesl.PersonFirstMedium])
    object PersonLastHighest extends Iesl.PersonLastHighest()(lp.provide[Iesl.PersonLastHighest])
    object PersonLastHigh extends Iesl.PersonLastHigh()(lp.provide[Iesl.PersonLastHigh])
    object PersonLastMedium extends Iesl.PersonLastMedium()(lp.provide[Iesl.PersonLastMedium])
    object Say extends Iesl.Say()(lp.provide[Iesl.Say])
    object Demonym extends Iesl.Demonym()(lp.provide[Iesl.Demonym])
    object DemonymMap extends Iesl.DemonymMap()(lp.provide[Iesl.Demonym])

    object AllPlaces extends TrieUnionLexicon("places", Continents, Country, City, UsState)

    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

  }

  object ssdi {
    object PersonFirstHighest extends Ssdi.PersonFirstHighest()(lp.provide[Ssdi.PersonFirstHighest])
    object PersonFirstHigh extends Ssdi.PersonFirstHigh()(lp.provide[Ssdi.PersonFirstHigh])
    object PersonFirstMedium extends Ssdi.PersonFirstMedium()(lp.provide[Ssdi.PersonFirstMedium])
    object PersonLastHighest extends Ssdi.PersonLastHighest()(lp.provide[Ssdi.PersonLastHighest])
    object PersonLastHigh extends Ssdi.PersonLastHigh()(lp.provide[Ssdi.PersonLastHigh])
    object PersonLastMedium extends Ssdi.PersonLastMedium()(lp.provide[Ssdi.PersonLastMedium])

    object PersonFirst extends TrieUnionLexicon("person-first", PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

    object PersonLast extends TrieUnionLexicon("person-last", PersonLastHighest, PersonLastHigh, PersonLastMedium)

  }

  object uscensus {

    object PersonFirstFemale extends Uscensus.PersonFirstFemale()(lp.provide[Uscensus.PersonFirstFemale])
    object PersonFirstMale extends Uscensus.PersonFirstMale()(lp.provide[Uscensus.PersonFirstMale])
    object PersonLast extends Uscensus.PersonLast()(lp.provide[Uscensus.PersonLast])

  }

  object wikipedia {
    object Battle extends Wikipedia.Battle()(lp.provide[Wikipedia.Battle])
    object BattleRedirect extends Wikipedia.BattleRedirect()(lp.provide[Wikipedia.BattleRedirect])
    object BattleAndRedirect extends Wikipedia.BattleAndRedirect()(lp.provide[Wikipedia.BattleAndRedirect])
    object BattleDisambiguation extends Wikipedia.BattleDisambiguation()(lp.provide[Wikipedia.BattleDisambiguation])
    object Book extends Wikipedia.Book()(lp.provide[Wikipedia.Book])
    object BookRedirect extends Wikipedia.BookRedirect()(lp.provide[Wikipedia.BookRedirect])
    object BookAndRedirect extends Wikipedia.BookAndRedirect()(lp.provide[Wikipedia.BookAndRedirect])
    object BookDisambiguation extends Wikipedia.BookDisambiguation()(lp.provide[Wikipedia.BookDisambiguation])
    object Business extends Wikipedia.Business()(lp.provide[Wikipedia.Business])
    object BusinessRedirect extends Wikipedia.BusinessRedirect()(lp.provide[Wikipedia.BusinessRedirect])
    object BusinessAndRedirect extends Wikipedia.BusinessAndRedirect()(lp.provide[Wikipedia.BusinessAndRedirect])
    object BusinessDisambiguation extends Wikipedia.BusinessDisambiguation()(lp.provide[Wikipedia.BusinessDisambiguation])
    object Competition extends Wikipedia.Competition()(lp.provide[Wikipedia.Competition])
    object CompetitionRedirect extends Wikipedia.CompetitionRedirect()(lp.provide[Wikipedia.CompetitionRedirect])
    object CompetitionAndRedirect extends Wikipedia.CompetitionAndRedirect()(lp.provide[Wikipedia.CompetitionAndRedirect])
    object CompetitionDisambiguation extends Wikipedia.CompetitionDisambiguation()(lp.provide[Wikipedia.CompetitionDisambiguation])
    object Event extends Wikipedia.Event()(lp.provide[Wikipedia.Event])
    object EventRedirect extends Wikipedia.EventRedirect()(lp.provide[Wikipedia.EventRedirect])
    object EventAndRedirect extends Wikipedia.EventAndRedirect()(lp.provide[Wikipedia.EventAndRedirect])
    object EventDisambiguation extends Wikipedia.EventDisambiguation()(lp.provide[Wikipedia.EventDisambiguation])
    object Film extends Wikipedia.Film()(lp.provide[Wikipedia.Film])
    object FilmRedirect extends Wikipedia.FilmRedirect()(lp.provide[Wikipedia.FilmRedirect])
    object FilmAndRedirect extends Wikipedia.FilmAndRedirect()(lp.provide[Wikipedia.FilmAndRedirect])
    object FilmDisambiguation extends Wikipedia.FilmDisambiguation()(lp.provide[Wikipedia.FilmDisambiguation])
    object Location extends Wikipedia.Location()(lp.provide[Wikipedia.Location])
    object LocationRedirect extends Wikipedia.LocationRedirect()(lp.provide[Wikipedia.LocationRedirect])
    object LocationAndRedirect extends Wikipedia.LocationAndRedirect()(lp.provide[Wikipedia.LocationAndRedirect])
    object LocationDisambiguation extends Wikipedia.LocationDisambiguation()(lp.provide[Wikipedia.LocationDisambiguation])
    object ManMadeThing extends Wikipedia.ManMadeThing()(lp.provide[Wikipedia.ManMadeThing])
    object ManMadeThingRedirect extends Wikipedia.ManMadeThingRedirect()(lp.provide[Wikipedia.ManMadeThingRedirect])
    object ManMadeThingAndRedirect extends Wikipedia.ManMadeThingAndRedirect()(lp.provide[Wikipedia.ManMadeThingAndRedirect])
    object ManMadeThingDisambiguation extends Wikipedia.ManMadeThingDisambiguation()(lp.provide[Wikipedia.ManMadeThingDisambiguation])
    object Organization extends Wikipedia.Organization()(lp.provide[Wikipedia.Organization])
    object OrganizationRedirect extends Wikipedia.OrganizationRedirect()(lp.provide[Wikipedia.OrganizationRedirect])
    object OrganizationAndRedirect extends Wikipedia.OrganizationAndRedirect()(lp.provide[Wikipedia.OrganizationAndRedirect])
    object OrganizationDisambiguation extends Wikipedia.OrganizationDisambiguation()(lp.provide[Wikipedia.OrganizationDisambiguation])
    object Person extends Wikipedia.Person()(lp.provide[Wikipedia.Person])
    object PersonRedirect extends Wikipedia.PersonRedirect()(lp.provide[Wikipedia.PersonRedirect])
    object PersonAndRedirect extends Wikipedia.PersonAndRedirect()(lp.provide[Wikipedia.PersonAndRedirect])
    object PersonDisambiguation extends Wikipedia.PersonDisambiguation()(lp.provide[Wikipedia.PersonDisambiguation])
    object Song extends Wikipedia.Song()(lp.provide[Wikipedia.Song])
    object SongRedirect extends Wikipedia.SongRedirect()(lp.provide[Wikipedia.SongRedirect])
    object SongAndRedirect extends Wikipedia.SongAndRedirect()(lp.provide[Wikipedia.SongAndRedirect])
    object SongDisambiguation extends Wikipedia.SongDisambiguation()(lp.provide[Wikipedia.SongDisambiguation])

  }

  object mandarin {

  }

}

/*

// Several standard lexicons available through the factorie-nlp-lexicons.jar

// To read from .jar in classpath use source = io.Source.fromInputStream(getClass.getResourceAsStream(_))
// To read from directory in filesystem use source = io.Source.fromFile(new File(_))
class ResourceLexicons(val tokenizer:StringSegmenter = cc.factorie.app.strings.nonWhitespaceSegmenter, val lemmatizer:Lemmatizer = LowercaseLemmatizer)(implicit mp:ModelProvider[ResourceLexicons, Source]) {

  class TriePhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.TriePhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => {throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }
  /*
  class TriePhraseLexicon(name:String)(implicit dir:String) extends cc.factorie.app.nlp.lexicon.TriePhraseLexicon(dir+"/"+name, tokenizer, lemmatizer) {
    try { this ++= sourceFactory(dir + "/" + name + ".txt") } catch { case e:java.io.IOException => {throw new Error("Could not find "+dir+"/"+name+"\n") } }
  }
  */

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
object ClasspathResourceLexicons extends ResourceLexicons(ModelProvider.classpathRelative[Lexicon, Source])(io.Codec.UTF8) })
*/
