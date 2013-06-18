package cc.factorie.app.nlp.coref

import java.io.File
import cc.factorie.app.nlp.mention.Mention
import collection.mutable

/**
 * User: apassos
 * Date: 5/30/13
 * Time: 10:07 PM
 */


object CorefGazetteers extends CorefGazetteers(null)

class CorefGazetteers(lexDir: String) {
  private def load(name: String): mutable.Set[String] = {
    if (lexDir eq null) {
      //println("class is " + cc.factorie.app.nlp.Lexicon.getClass.getName + " resource is " + name)
      val res = cc.factorie.app.nlp.lexicon.Lexicon.getClass.getResource(name)
      assert(res ne null, "To load from a jar the factorie nlp resources jar must be in the classpath")
      val src = io.Source.fromURL(res)
      assert(src ne null)
      new mutable.LinkedHashSet[String]  ++= src.getLines()
    } else {
      new mutable.LinkedHashSet[String]  ++= io.Source.fromFile(new File(lexDir + name)).getLines()
    }
  }
  def loadInto(map: collection.mutable.HashMap[String,mutable.Set[String]], names: Seq[String], dir: String) {
    names.foreach(lexName => {
      val name = normalizeName(lexName)
      if(!map.contains(name))
        map += (name -> load(dir+lexName).map(_.toLowerCase))
      else
        map(name) ++= load(dir+lexName).map(_.toLowerCase)
    })
  }


  def normalizeName(str: String): String = {
    str.replaceAll(".txt", "").replaceAll("-paren", "").replaceAll("-redirect","")
  }

  //make a big hashmap from filename to a set of the strings in that file
  val lexHash = collection.mutable.HashMap[String,mutable.Set[String]]()
  loadInto(lexHash, CorefGazetteersData.ieslLexiconsToLoad, "iesl/")

  val censusHash = collection.mutable.HashMap[String,mutable.Set[String]]()
  loadInto(censusHash, CorefGazetteersData.censusLexiconsToLoad, "uscensus/")

  val wikiLexHash = collection.mutable.HashMap[String,mutable.Set[String]]()
  loadInto(wikiLexHash, CorefGazetteersData.wikiLexiconsToLoad, "wikipedia/")

  //these are things  used elsewhere in the coref code
  val honors =  lexHash("person-honorific").toSet
  val cities = lexHash("city").toSet
  val countries = lexHash("country").toSet
  val lastNames = (lexHash("person-last-high") ++ lexHash("person-last-highest") ++ censusHash("person-last")  ++ wikiLexHash("person").map(x => x.split(" ").last)).toSet
  val maleFirstNames = censusHash("person-first-male").toSet
  val femaleFirstNames = censusHash("person-first-female") .toSet

  val sayWords = lexHash("say").toSet
  val orgClosings = lexHash("org-suffix").toSet
  val demonyms = lexHash("demonyms").toSet
  val demonymMap = demonyms.flatMap(d => {
      val a = d.trim.split("\t")
      a.map(_ -> a.head)
    }).toMap

  //these are things used in entity type classification
  val firstNames = (maleFirstNames ++ femaleFirstNames /*++ wikiLexHash("person").map(_.split(" ").head)*/).toSet
  val events =  (wikiLexHash("events") ++ wikiLexHash("battles") ++ wikiLexHash("competition")).toSet
  val placeWords = (cities ++ lexHash("country") ++ lexHash("continents") ++ lexHash("us-state") ++ lexHash("place-suffix")  ++ wikiLexHash("location") ).toSet
  val orgWords = (lexHash("company") ++ lexHash("org-suffix") ++ wikiLexHash("organization")   ++ wikiLexHash("business")  ).toSet
  val timeWords = (lexHash("day") ++ lexHash("month") ).toSet
  val personFirstWords = (firstNames ++  lexHash("person-first-high") ++  lexHash("person-first-highest") ++ lexHash("jobtitle") ++ lexHash("person-honorific") /*++ wikiLexHash("person").map(_.split(" ").last)*/).toSet
  val personLastWords = (lastNames  ++ firstNames ++ honors ).toSet
  val personFullNames = (wikiLexHash("person") ).toSet
  val properWords = (wikiLexHash("book") ++ wikiLexHash("battles") ++ wikiLexHash("man_made_thing") ++ wikiLexHash("film") ++ wikiLexHash("songs") ).toSet

  def hasSpeakWord(m: Mention, size: Int): Boolean = {
    val until = m.span.head.position
    val from = math.max(until - size, 0)
    m.document.tokens.slice(from, until).exists(t => sayWords.contains(t.string.trim.toLowerCase))
  }

  val morph = if (lexDir eq null) {
    val fmap = (s : String) => {
      val res = cc.factorie.app.nlp.lexicon.Lexicon.getClass.getResource("morph/en" + s)
      assert(res ne null, "To load from a jar the factorie nlp resources jar must be in the classpath")
      io.Source.fromURL(res)
    }
    new cc.factorie.app.nlp.morph.MorphologicalAnalyzer1(fmap)
  } else new cc.factorie.app.nlp.morph.MorphologicalAnalyzer1(lexDir)
  def isPlural(s: String): Boolean   = morph.isPlural(s)
  def isSingular(s: String): Boolean  = morph.isSingular(s)
}


object CorefGazetteersData {
  val ieslLexiconsToLoad = Seq(
    "city.txt",
    "company.txt",
    "continents.txt",
    "country.txt",
    "day.txt",
    "demonyms.txt",
    "jobtitle.txt",
    "month.txt",
    "org-suffix.txt",
    "person-first-high.txt",
    "person-first-highest.txt",
    "person-first-medium.txt",
    "person-honorific.txt",
    "person-improper.txt",
    "person-last-high.txt",
    "person-last-highest.txt",
    "person-last-medium.txt",
    "place-suffix.txt",
    "say.txt",
    "us-state.txt"
  )

  val censusLexiconsToLoad = Seq(
    "person-first-female.txt",	"person-first-male.txt"	,"person-last.txt"
  )

  val wikiLexiconsToLoad = Seq(
    "battles-paren.txt",
    "battles-redirect-paren.txt",
    "battles-redirect.txt",
    "battles.txt",
    "book-paren.txt",
    "book-redirect-paren.txt",
    "book-redirect.txt",
    "book.txt",
    "business-paren.txt",
    "business-redirect-paren.txt",
    "business-redirect.txt",
    "business.txt",
    "competition-paren.txt",
    "competition-redirect-paren.txt",
    "competition-redirect.txt",
    "competition.txt",
    "events-paren.txt",
    "events-redirect-paren.txt",
    "events-redirect.txt",
    "events.txt",
    "film-paren.txt",
    "film-redirect-paren.txt",
    "film-redirect.txt",
    "film.txt",
    "location-paren.txt",
    //"location-redirect-paren.txt",
    //"location-redirect.txt",
    "location.txt",
    "man_made_thing-paren.txt",
    "man_made_thing-redirect-paren.txt",
    "man_made_thing-redirect.txt",
    "man_made_thing.txt",
    //"organization-paren.txt",
    // "organization-redirect-paren.txt",
    // "organization-redirect.txt",
    "organization.txt",
    "person-paren.txt",
    //"person-redirect-paren.txt",
    //"person-redirect.txt",
    "person.txt",
    "songs-paren.txt",
    "songs-redirect-paren.txt",
    "songs-redirect.txt",
    "songs.txt"
  )
}

