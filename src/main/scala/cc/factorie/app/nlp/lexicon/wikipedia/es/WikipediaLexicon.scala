package cc.factorie.app.nlp.lexicon.wikipedia.es

import cc.factorie.app.nlp.lexicon.ProvidedTriePhraseLexicon
import cc.factorie.util.ModelProvider

class Book()(implicit mp:ModelProvider[Book]) extends ProvidedTriePhraseLexicon[Book]
class Film()(implicit mp:ModelProvider[Film]) extends ProvidedTriePhraseLexicon[Film]
class Event()(implicit mp:ModelProvider[Event]) extends ProvidedTriePhraseLexicon[Event]
class Business()(implicit mp:ModelProvider[Business]) extends ProvidedTriePhraseLexicon[Business]
class Location()(implicit mp:ModelProvider[Location]) extends ProvidedTriePhraseLexicon[Location]
class LocationRedirect()(implicit mp:ModelProvider[LocationRedirect]) extends ProvidedTriePhraseLexicon[LocationRedirect]
class Organization()(implicit mp:ModelProvider[Organization]) extends ProvidedTriePhraseLexicon[Organization]
class OrganizationRedirect()(implicit mp:ModelProvider[OrganizationRedirect]) extends ProvidedTriePhraseLexicon[OrganizationRedirect]
class Person()(implicit mp:ModelProvider[Person]) extends ProvidedTriePhraseLexicon[Person]
class PersonRedirect()(implicit mp:ModelProvider[PersonRedirect]) extends ProvidedTriePhraseLexicon[PersonRedirect]

