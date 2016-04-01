package cc.factorie.app.nlp.lexicon.iesl.es

import cc.factorie.app.nlp.lexicon.{TriePhraseLexicon, ProvidedTriePhraseLexicon}
import cc.factorie.util.ModelProvider

import scala.io.Source

class Continents()(implicit mp: ModelProvider[Continents]) extends ProvidedTriePhraseLexicon[Continents]
class Day()(implicit mp: ModelProvider[Day]) extends ProvidedTriePhraseLexicon[Day]
class Month()(implicit mp: ModelProvider[Month]) extends ProvidedTriePhraseLexicon[Month]
class PersonFirst()(implicit mp: ModelProvider[PersonFirst]) extends ProvidedTriePhraseLexicon[PersonFirst]
class PersonLast()(implicit mp: ModelProvider[PersonLast]) extends ProvidedTriePhraseLexicon[PersonLast]
class Location()(implicit mp: ModelProvider[Location]) extends ProvidedTriePhraseLexicon[Location]
class Miscellaneous()(implicit mp: ModelProvider[Miscellaneous]) extends ProvidedTriePhraseLexicon[Miscellaneous]
class Person()(implicit mp: ModelProvider[Person]) extends ProvidedTriePhraseLexicon[Person]
class PersonHonorific()(implicit mp: ModelProvider[PersonHonorific]) extends ProvidedTriePhraseLexicon[PersonHonorific]
class Organization()(implicit mp: ModelProvider[Organization]) extends ProvidedTriePhraseLexicon[Organization]
class OrgSuffix()(implicit mp: ModelProvider[OrgSuffix]) extends ProvidedTriePhraseLexicon[OrgSuffix]
class Demonym()(implicit mp: ModelProvider[Demonym]) extends ProvidedTriePhraseLexicon[Demonym]

