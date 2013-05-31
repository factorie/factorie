package cc.factorie.app.nlp.coref

import java.io.File
import cc.factorie.app.nlp.mention.Mention

/**
 * User: apassos
 * Date: 5/30/13
 * Time: 10:07 PM
 */

// TODO: make this read from the lexicons jar if possible
class CorefGazetteers(lexDir: String) {
  //make a big hashmap from filename to a set of the strings in that file
  val lexHash = collection.mutable.HashMap[String,Set[String]]()
  val dirname = lexDir + "/iesl/"
  val files = (new File(dirname)).list()
  private def load(gaz: File): Set[String] = {
    io.Source.fromFile(gaz).getLines().toSet
  }
  files.map(f => new File(dirname + f)).foreach(f => {
    println("reading from " + f.getPath)
    val name = f.getName.replaceAll(".txt","")
    lexHash += ((name,load(f).map(_.toLowerCase)))
  })

  val wikiLexHash = collection.mutable.HashMap[String,Set[String]]()
  val dirname2 = lexDir + "/wikipedia/"
  val wikiFiles = (new File(dirname2)).list()
  wikiFiles.foreach(f => {
    println("reading from " + f)
    //todo: choose which file extensions to use, and strip these off. right now we're not using the redirect ones (by choice)
    //(the redirect ones may have better coverage, but lower precision. need to evaluate
    val name = f.replaceAll(".txt","").replaceAll("-paren.txt","")
    if(!wikiLexHash.contains(name))
      wikiLexHash += ((name,load(dirname2 + f).map(_.toLowerCase)))
    else
      wikiLexHash(name) ++=  load(dirname2 + f).map(_.toLowerCase)
  })

  //these are things  used elsewhere in the coref code
  val honors =  lexHash("person-honorific")
  val cities = lexHash("city")
  val countries = lexHash("country")
  val lastNames = lexHash("person-last-high") ++ lexHash("person-last-highest")
  val maleFirstNames = lexHash("maleFirstNames")
  val femaleFirstNames = lexHash("femaleFirstNames")
  val sayWords = lexHash("sayWords")
  val orgClosings = lexHash("org-suffix")
  val demonyms = lexHash("demonyms")
  val demonymMap = demonyms.flatMap(d => {
      val a = d.trim.split("\t")
      a.map(_ -> a.head)
    }).toMap


  //these are things used in entity type classification
  val firstNames = maleFirstNames ++ femaleFirstNames ++ wikiLexHash("person").map(_.split(" ").head)
  val events =  wikiLexHash("events") ++ wikiLexHash("battles") ++ wikiLexHash("competition")
  val placeWords = cities ++ lexHash("country") ++ lexHash("continents") ++ lexHash("us-state") ++ lexHash("place-suffix")  ++ wikiLexHash("location")
  val orgWords = lexHash("company") ++ lexHash("org-suffix") ++ wikiLexHash("organization")   ++ wikiLexHash("business")
  val timeWords = lexHash("day") ++ lexHash("month")
  val personFirstWords = firstNames ++  lexHash("person-first-high") ++  lexHash("person-first-highest") ++ lexHash("jobtitle") ++ lexHash("person-honorific") ++ wikiLexHash("person").map(_.split(" ").last)
  val personLastWords = lastNames  ++ firstNames ++ honors
  val personFullNames = wikiLexHash("person")
  val properWords = wikiLexHash("book") ++ wikiLexHash("battles") ++ wikiLexHash("man_made_thing") ++ wikiLexHash("film") ++ wikiLexHash("songs")

  def hasSpeakWord(m: Mention, size: Int): Boolean = {
    val until = m.span.head.position
    val from = math.max(until - size, 0)
    m.document.tokens.slice(from, until).exists(t => sayWords.contains(t.string.trim.toLowerCase))
  }

//  private def load(f: String): Set[String] = {
//    val stream = this.getClass.getResourceAsStream(f)
//    println("using " + stream.toString)
//    scala.io.Source.fromInputStream(stream).getLines().toSet
//  }

  private def load(f: String): Set[String] = {
    scala.io.Source.fromFile(f).getLines().toSet
  }


  val morph = new cc.factorie.app.nlp.morph.MorphologicalAnalyzer1(lexDir + "/morph/en/")
  def isPlural(s: String): Boolean   = morph.isPlural(s)
  def isSingular(s: String): Boolean  = morph.isSingular(s)

}


object LexiconInfo{
  val ieslLexiconsToLoad = Seq(
  "city",
  "company",
  "continents",
  "country",
  "day",
  "demonyms.txt",
  "femaleFirstNames.txt",
  "improper-person-names",
  "jobtitle",
  "maleFirstNames.txt",
  "month",
  "org-suffix",
  "person-first-high",
  "person-first-highest",
  "person-first-medium",
  "person-honorific",
  "person-last-high",
  "person-last-highest",
  "person-last-medium",
  "place-suffix",
  "pluralNouns.txt",
  "sayWords.txt",
  "singularNouns.txt",
  "us-state"
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
  "location-redirect-paren.txt",
  "location-redirect.txt",
  "location.txt",
  "man_made_thing-paren.txt",
  "man_made_thing-redirect-paren.txt",
  "man_made_thing-redirect.txt",
  "man_made_thing.txt",
  "organization-paren.txt",
  "organization-redirect-paren.txt",
  "organization-redirect.txt",
  "organization.txt",
  "person-paren.txt",
  "person-redirect-paren.txt",
  "person-redirect.txt",
  "person.txt",
  "songs-paren.txt",
  "songs-redirect-paren.txt",
  "songs-redirect.txt",
  "songs.txt"
  )
}
