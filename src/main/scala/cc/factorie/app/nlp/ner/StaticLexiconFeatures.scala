package cc.factorie.app.nlp.ner

import cc.factorie.app.nlp.Token
import cc.factorie.variable.CategoricalVectorVar
import cc.factorie.app.nlp.lexicon.StaticLexicons
import cc.factorie.app.nlp.lexicon.LexiconsProvider
import cc.factorie.app.nlp.lemma.LowercaseTokenLemma

class StaticLexiconFeatures(lexicon:StaticLexicons) extends NerLexiconFeatures {
    //this block serves to initialize all of the lexicons used by the model before processing
  lexicon.synchronized {
    lexicon.iesl.Month.toString()
    lexicon.iesl.Day.toString()

    lexicon.iesl.PersonFirst.toString()
    lexicon.iesl.PersonFirstHigh.toString()
    lexicon.iesl.PersonFirstHighest.toString()
    lexicon.iesl.PersonFirstMedium.toString()

    lexicon.iesl.PersonLast.toString()
    lexicon.iesl.PersonLastHigh.toString()
    lexicon.iesl.PersonLastHighest.toString()
    lexicon.iesl.PersonLastMedium.toString()

    lexicon.iesl.PersonHonorific.toString()

    lexicon.iesl.Company.toString()
    lexicon.iesl.JobTitle.toString()
    lexicon.iesl.OrgSuffix.toString()

    lexicon.iesl.Country.toString()
    lexicon.iesl.City.toString()
    lexicon.iesl.PlaceSuffix.toString()
    lexicon.iesl.UsState.toString()
    lexicon.iesl.Continents.toString()

    lexicon.wikipedia.Person.toString()
    lexicon.wikipedia.Event.toString()
    lexicon.wikipedia.Location.toString()
    lexicon.wikipedia.Organization.toString()
    lexicon.wikipedia.ManMadeThing.toString()
    lexicon.iesl.Demonym.toString()

    lexicon.wikipedia.Book.toString()
    lexicon.wikipedia.Business.toString()
    lexicon.wikipedia.Film.toString()

    lexicon.wikipedia.LocationAndRedirect.toString()
    lexicon.wikipedia.PersonAndRedirect.toString()
    lexicon.wikipedia.OrganizationAndRedirect.toString()
    
    lexicon.spanish.Continents.toString()
    lexicon.spanish.Month.toString()
    lexicon.spanish.Day.toString()
    lexicon.spanish.PersonFirst.toString()
    lexicon.spanish.Book.toString()    
    lexicon.spanish.Event.toString()    
    lexicon.spanish.Business.toString()  
    lexicon.spanish.Film.toString()
    
    lexicon.spanish.Person.toString()
    lexicon.spanish.Location.toString()
    lexicon.spanish.Organization.toString()
    lexicon.spanish.LocationAndRedirect.toString()
    lexicon.spanish.PersonAndRedirect.toString()
    lexicon.spanish.OrganizationAndRedirect.toString()
     
  }

  
  def addLexiconFeatures(tokenSequence: IndexedSeq[Token], vf: (Token => CategoricalVectorVar[String])) {
    lexicon.iesl.Month.tagText(tokenSequence,vf,"MONTH")
    lexicon.iesl.Day.tagText(tokenSequence,vf,"DAY")

    lexicon.iesl.PersonFirst.tagText(tokenSequence,vf,"PERSON-FIRST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSequence,vf,"PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSequence,vf,"PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSequence,vf,"PERSON-FIRST-MEDIUM")

    lexicon.iesl.PersonLast.tagText(tokenSequence,vf,"PERSON-LAST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSequence,vf,"PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastHighest.tagText(tokenSequence,vf,"PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastMedium.tagText(tokenSequence,vf,"PERSON-LAST-MEDIUM")

    lexicon.iesl.PersonHonorific.tagText(tokenSequence,vf,"PERSON-HONORIFIC")

    lexicon.iesl.Company.tagText(tokenSequence,vf,"COMPANY")
    lexicon.iesl.JobTitle.tagText(tokenSequence,vf,"JOB-TITLE")
    lexicon.iesl.OrgSuffix.tagText(tokenSequence,vf,"ORG-SUFFIX")

    lexicon.iesl.Country.tagText(tokenSequence,vf,"COUNTRY")
    lexicon.iesl.City.tagText(tokenSequence,vf,"CITY")
    lexicon.iesl.PlaceSuffix.tagText(tokenSequence,vf,"PLACE-SUFFIX")
    lexicon.iesl.UsState.tagText(tokenSequence,vf,"USSTATE")
    lexicon.iesl.Continents.tagText(tokenSequence,vf,"CONTINENT")

    lexicon.wikipedia.Person.tagText(tokenSequence,vf,"WIKI-PERSON")
    lexicon.wikipedia.Event.tagText(tokenSequence,vf,"WIKI-EVENT")
    lexicon.wikipedia.Location.tagText(tokenSequence,vf,"WIKI-LOCATION")
    lexicon.wikipedia.Organization.tagText(tokenSequence,vf,"WIKI-ORG")
    lexicon.wikipedia.ManMadeThing.tagText(tokenSequence,vf,"MANMADE")
    lexicon.iesl.Demonym.tagText(tokenSequence,vf,"DEMONYM")

    lexicon.wikipedia.Book.tagText(tokenSequence,vf,"WIKI-BOOK")
    lexicon.wikipedia.Business.tagText(tokenSequence,vf,"WIKI-BUSINESS")
    lexicon.wikipedia.Film.tagText(tokenSequence,vf,"WIKI-FILM")

    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSequence,vf,"WIKI-LOCATION-REDIRECT")
    lexicon.wikipedia.PersonAndRedirect.tagText(tokenSequence,vf,"WIKI-PERSON-REDIRECT")
    lexicon.wikipedia.OrganizationAndRedirect.tagText(tokenSequence,vf,"WIKI-ORG-REDIRECT")

    
    lexicon.spanish.Continents.tagText(tokenSequence,vf,"ES-CONTINENT")
    lexicon.spanish.Month.tagText(tokenSequence,vf,"ES-MONTH")
    lexicon.spanish.Day.tagText(tokenSequence,vf,"ES-DAY")
    lexicon.spanish.PersonFirst.tagText(tokenSequence,vf,"ES-PERSON-FIRST")
    lexicon.spanish.Book.tagText(tokenSequence,vf,"ES-WIKI-BOOK")
    lexicon.spanish.Event.tagText(tokenSequence,vf,"ES-WIKI-EVENT")
    lexicon.spanish.Film.tagText(tokenSequence,vf,"ES-WIKI-FILM")
    lexicon.spanish.Business.tagText(tokenSequence,vf,"ES-WIKI-BUSINESS")
    
    lexicon.spanish.Person.tagText(tokenSequence,vf,"ES-WIKI-PERSON")
    lexicon.spanish.Location.tagText(tokenSequence,vf,"ES-WIKI-LOCATION")
    lexicon.spanish.Organization.tagText(tokenSequence,vf,"ES-WIKI-ORG")
    lexicon.spanish.LocationAndRedirect.tagText(tokenSequence,vf,"ES-WIKI-LOCATION-REDIRECT")
    lexicon.spanish.PersonAndRedirect.tagText(tokenSequence,vf,"ES-WIKI-PERSON-REDIRECT")
    lexicon.spanish.OrganizationAndRedirect.tagText(tokenSequence,vf,"ES-WIKI-ORG-REDIRECT")

  }

}

object StaticLexiconFeatures{
  def apply(): StaticLexiconFeatures = {
    new StaticLexiconFeatures(new StaticLexicons()(LexiconsProvider.classpath()))
  }
}