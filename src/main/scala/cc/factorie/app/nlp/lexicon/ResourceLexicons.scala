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
  object iesl {
    private implicit val dir = "iesl"
      
    object Continents extends PhraseLexicon("continents") // TODO Rename this to continent
    object Country extends PhraseLexicon("country")
    object City extends PhraseLexicon("city")
    object USState extends PhraseLexicon("us-state")
    object PlaceSuffix extends WordLexicon("place-suffix")
    object AllPlaces extends UnionLexicon("place-suffix", Continents, Country, City, USState)

    object JobTitle extends PhraseLexicon("jobtitle") // TODO Rename file to job-title
    object Money extends WordLexicon("money")

    object Company extends PhraseLexicon("company")
    object OrgSuffix extends PhraseLexicon("org-suffix")

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

    object Say extends WordLexicon("say")
    
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
      
    object Battle extends PhraseLexicon("battle")
    object BattleRedirect extends PhraseLexicon("battle-redirect")
    object BattleAndRedirect extends UnionLexicon("battle-and-redirect", Battle, BattleRedirect)
    object BattleDisambiguation extends PhraseLexicon("battle-disambiguation")
    //object BattleParen extends PhraseLexicon("battle-paren")
    //object BattleRedirectParen extends PhraseLexicon("battle-redirect-paren")
    
    object Book extends PhraseLexicon("book")
    object BookRedirect extends PhraseLexicon("book-redirect")
    object BookAndRedirect extends UnionLexicon("book-and-redirect", Book, BookRedirect)
    object BookDisambiguation extends PhraseLexicon("book-disambiguation")
    //object BookParen extends PhraseLexicon("book-paren")
    //object BookRedirectParen extends PhraseLexicon("book-redirect-paren")
    
    object Business extends PhraseLexicon("business")
    object BusinessRedirect extends PhraseLexicon("business-redirect")
    object BusinessAndRedirect extends UnionLexicon("business-and-redirect", Business, BusinessRedirect)
    object BusinessDisambiguation extends PhraseLexicon("business-disambiguation")
    //object BusinessParen extends PhraseLexicon("business-paren")
    //object BusinessRedirectParen extends PhraseLexicon("business-redirect-paren")

    object Competition extends PhraseLexicon("competition")
    object CompetitionRedirect extends PhraseLexicon("competition-redirect")
    object CompetitionAndRedirect extends UnionLexicon("competition-and-redirect", Competition, CompetitionRedirect)
    object CompetitionDisambiguation extends PhraseLexicon("competition-disambiguation")
    //object CompetitionParen extends PhraseLexicon("competition-paren")
    //object CompetitionRedirectParent extends PhraseLexicon("competition-redirect-paren")

    object Event extends PhraseLexicon("events") // TODO Change this name to event
    object EventRedirect extends PhraseLexicon("events-redirect")
    object EventAndRedirect extends UnionLexicon("events-and-redirect", Event, EventRedirect)
    object EventDisambiguation extends PhraseLexicon("event-disambiguation")
    //object EventParen extends PhraseLexicon("events-paren")
    //object EventRedirectParen extends PhraseLexicon("events-redirect-paren")

    object Film extends PhraseLexicon("film")
    object FilmRedirect extends PhraseLexicon("film-redirect")
    object FilmAndRedirect extends UnionLexicon("film-and-redirect", Film, FilmRedirect)
    object FilmDisambiguation extends PhraseLexicon("film-disambiguation")
    //object FilmParen extends PhraseLexicon("film-paren")
    //object FilmRedirectParen extends PhraseLexicon("film-redirect-paren")

    object Location extends PhraseLexicon("location")
    object LocationRedirect extends PhraseLexicon("location-redirect")
    object LocationAndRedirect extends UnionLexicon("location-and-redirect", Location, LocationRedirect)
    object LocationDisambiguation extends PhraseLexicon("location-disambiguation")
    //object LocationParen extends PhraseLexicon("location-paren")
    //object LocationRedirectParen extends PhraseLexicon("location-redirect-paren")

    object ManMadeThing extends PhraseLexicon("man_made_thing")
    object ManMadeThingRedirect extends PhraseLexicon("man_made_thing-redirect")
    object ManMadeThingAndRedirect extends UnionLexicon("man_made_thing-and-redirect", ManMadeThing, ManMadeThingRedirect)
    object ManMadeThingDisambiguation extends PhraseLexicon("man_made_thing-disambiguation")
    //object ManMadeThingParen extends PhraseLexicon("man_made_thing-paren")
    //object ManMadeThingRedirectParen extends PhraseLexicon("man_made_thing-redirect-paren")

    
    object Organization extends PhraseLexicon("organization")
    object OrganizationRedirect extends PhraseLexicon("organization-redirect")
    object OrganizationAndRedirect extends UnionLexicon("organization-and-redirect", Organization, OrganizationRedirect)
    object OrganizationDisambiguation extends PhraseLexicon("organization-disambiguation")
    //object OrganizationParen extends PhraseLexicon("organization-paren")
    //object OrganizationRedirectParen extends PhraseLexicon("organization-redirect-paren")
    
    object Person extends PhraseLexicon("person")
    object PersonRedirect extends PhraseLexicon("person-redirect")
    object PersonAndRedirect extends UnionLexicon("person-and-redirect", Person, PersonRedirect)
    object PersonDisambiguation extends PhraseLexicon("person-disambiguation")
    //object PersonParen extends PhraseLexicon("person-paren") // paren lines need more processing to be useful
    //object PersonRedirectParen extends PhraseLexicon("person-redirect-paren")

    object Song extends PhraseLexicon("song")
    object SongRedirect extends PhraseLexicon("song-redirect")
    object SongAndRedirect extends UnionLexicon("song-and-redirect", Song, SongRedirect)
    object SongDisambiguation extends PhraseLexicon("song-disambiguation")
    //object SongParen extends PhraseLexicon("song-paren") // paren lines need more processing to be useful
    //object SongRedirectParen extends PhraseLexicon("song-redirect-paren")
    
  }
}

/** Static access through classpath or file location (specified as Java System Property)
    @author Andrew McCallum */
object ClasspathResourceLexicons extends ResourceLexicons(string => { io.Source.fromURL(ClasspathURL.fromDirectory[Lexicon](string)) })
