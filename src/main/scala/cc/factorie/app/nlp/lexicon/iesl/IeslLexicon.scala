package cc.factorie.app.nlp.lexicon.iesl

import cc.factorie.app.nlp.lexicon.{TriePhraseLexicon, ProvidedTriePhraseLexicon}
import cc.factorie.util.ModelProvider

import scala.io.Source

/**
 * @author johnsullivan
 */
class Continents()(implicit mp: ModelProvider[Continents, Source]) extends ProvidedTriePhraseLexicon[Continents]
class Country()(implicit mp: ModelProvider[Country, Source]) extends ProvidedTriePhraseLexicon[Country]
class City()(implicit mp: ModelProvider[City, Source]) extends ProvidedTriePhraseLexicon[City]
class UsState()(implicit mp: ModelProvider[UsState, Source]) extends ProvidedTriePhraseLexicon[UsState]
class PlaceSuffix()(implicit mp: ModelProvider[PlaceSuffix, Source]) extends ProvidedTriePhraseLexicon[PlaceSuffix]
class JobTitle()(implicit mp: ModelProvider[JobTitle, Source]) extends ProvidedTriePhraseLexicon[JobTitle]
class Money()(implicit mp: ModelProvider[Money, Source]) extends ProvidedTriePhraseLexicon[Money]
class Company()(implicit mp: ModelProvider[Company, Source]) extends ProvidedTriePhraseLexicon[Company]
class OrgSuffix()(implicit mp: ModelProvider[OrgSuffix, Source]) extends ProvidedTriePhraseLexicon[OrgSuffix]
class Month()(implicit mp: ModelProvider[Month, Source]) extends ProvidedTriePhraseLexicon[Month]
class Day()(implicit mp: ModelProvider[Day, Source]) extends ProvidedTriePhraseLexicon[Day]
class PersonHonorific()(implicit mp: ModelProvider[PersonHonorific, Source]) extends ProvidedTriePhraseLexicon[PersonHonorific]
class PersonFirstHighest()(implicit mp: ModelProvider[PersonFirstHighest, Source]) extends ProvidedTriePhraseLexicon[PersonFirstHighest]
class PersonFirstHigh()(implicit mp: ModelProvider[PersonFirstHigh, Source]) extends ProvidedTriePhraseLexicon[PersonFirstHigh]
class PersonFirstMedium()(implicit mp: ModelProvider[PersonFirstMedium, Source]) extends ProvidedTriePhraseLexicon[PersonFirstMedium]
class PersonLastHighest()(implicit mp: ModelProvider[PersonLastHighest, Source]) extends ProvidedTriePhraseLexicon[PersonLastHighest]
class PersonLastHigh()(implicit mp: ModelProvider[PersonLastHigh, Source]) extends ProvidedTriePhraseLexicon[PersonLastHigh]
class PersonLastMedium()(implicit mp: ModelProvider[PersonLastMedium, Source]) extends ProvidedTriePhraseLexicon[PersonLastMedium]
class Say()(implicit mp: ModelProvider[Say, Source]) extends ProvidedTriePhraseLexicon[Say]
class Demonym()(implicit mp: ModelProvider[Demonym, Source]) extends TriePhraseLexicon(classOf[Demonym].getName) {
  mp.provide.getLines().flatMap(_.trim.split("\t")).foreach(this.+=)
}

class DemonymMap()(implicit mp:ModelProvider[Demonym, Source]) extends scala.collection.mutable.HashMap[String,String] {
  mp.provide.getLines().foreach { line =>
    val entries = line.trim.split("\t")
    val value = entries.head
    entries.foreach(e => this.update(e, value))
  }
}
