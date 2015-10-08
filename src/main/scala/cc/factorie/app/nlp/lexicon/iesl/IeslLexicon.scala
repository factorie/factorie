package cc.factorie.app.nlp.lexicon.iesl

import cc.factorie.app.nlp.lexicon.{TriePhraseLexicon, ProvidedTriePhraseLexicon}
import cc.factorie.util.ModelProvider

import scala.io.Source

/**
 * @author johnsullivan
 */
class Continents()(implicit mp: ModelProvider[Continents]) extends ProvidedTriePhraseLexicon[Continents]
class Country()(implicit mp: ModelProvider[Country]) extends ProvidedTriePhraseLexicon[Country]
class City()(implicit mp: ModelProvider[City]) extends ProvidedTriePhraseLexicon[City]
class UsState()(implicit mp: ModelProvider[UsState]) extends ProvidedTriePhraseLexicon[UsState]
class PlaceSuffix()(implicit mp: ModelProvider[PlaceSuffix]) extends ProvidedTriePhraseLexicon[PlaceSuffix]
class JobTitle()(implicit mp: ModelProvider[JobTitle]) extends ProvidedTriePhraseLexicon[JobTitle]
class Money()(implicit mp: ModelProvider[Money]) extends ProvidedTriePhraseLexicon[Money]
class Company()(implicit mp: ModelProvider[Company]) extends ProvidedTriePhraseLexicon[Company]
class OrgSuffix()(implicit mp: ModelProvider[OrgSuffix]) extends ProvidedTriePhraseLexicon[OrgSuffix]
class Month()(implicit mp: ModelProvider[Month]) extends ProvidedTriePhraseLexicon[Month]
class Day()(implicit mp: ModelProvider[Day]) extends ProvidedTriePhraseLexicon[Day]
class PersonHonorific()(implicit mp: ModelProvider[PersonHonorific]) extends ProvidedTriePhraseLexicon[PersonHonorific]
class PersonFirstHighest()(implicit mp: ModelProvider[PersonFirstHighest]) extends ProvidedTriePhraseLexicon[PersonFirstHighest]
class PersonFirstHigh()(implicit mp: ModelProvider[PersonFirstHigh]) extends ProvidedTriePhraseLexicon[PersonFirstHigh]
class PersonFirstMedium()(implicit mp: ModelProvider[PersonFirstMedium]) extends ProvidedTriePhraseLexicon[PersonFirstMedium]
class PersonLastHighest()(implicit mp: ModelProvider[PersonLastHighest]) extends ProvidedTriePhraseLexicon[PersonLastHighest]
class PersonLastHigh()(implicit mp: ModelProvider[PersonLastHigh]) extends ProvidedTriePhraseLexicon[PersonLastHigh]
class PersonLastMedium()(implicit mp: ModelProvider[PersonLastMedium]) extends ProvidedTriePhraseLexicon[PersonLastMedium]
class Say()(implicit mp: ModelProvider[Say]) extends ProvidedTriePhraseLexicon[Say]
class Demonym()(implicit mp: ModelProvider[Demonym]) extends TriePhraseLexicon(classOf[Demonym].getName) {
  synchronized {
    Source.fromInputStream(mp.provide)(io.Codec.UTF8).getLines().flatMap(_.trim.split("\t")).foreach(this.+=)
  }
}

class DemonymMap()(implicit mp:ModelProvider[Demonym]) extends scala.collection.mutable.HashMap[String,String] {
  synchronized {
    Source.fromInputStream(mp.provide)(io.Codec.UTF8).getLines().foreach { line =>
      val entries = line.trim.split("\t")
      val value = entries.head
      entries.foreach(e => this.update(e, value))
    }
  }
}
