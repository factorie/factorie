package cc.factorie.app.nlp.lexicon.iesl.es

import cc.factorie.app.nlp.lexicon.{TriePhraseLexicon, ProvidedTriePhraseLexicon}
import cc.factorie.util.ModelProvider

import scala.io.Source

class Continents()(implicit mp: ModelProvider[Continents]) extends ProvidedTriePhraseLexicon[Continents]
class Day()(implicit mp: ModelProvider[Day]) extends ProvidedTriePhraseLexicon[Day]
class Month()(implicit mp: ModelProvider[Month]) extends ProvidedTriePhraseLexicon[Month]
class PersonFirst()(implicit mp: ModelProvider[PersonFirst]) extends ProvidedTriePhraseLexicon[PersonFirst]
class Location()(implicit mp: ModelProvider[Location]) extends ProvidedTriePhraseLexicon[Location]
class Miscellaneous()(implicit mp: ModelProvider[Miscellaneous]) extends ProvidedTriePhraseLexicon[Miscellaneous]
class Person()(implicit mp: ModelProvider[Person]) extends ProvidedTriePhraseLexicon[Person]
class Organization()(implicit mp: ModelProvider[Organization]) extends ProvidedTriePhraseLexicon[Organization]
