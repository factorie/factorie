package cc.factorie.app.nlp.lexicon

// Several standard lexicons available through the factorie-nlp-lexicons.jar

object Continents extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/continents.txt")) }
object Country extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/country.txt")) }
object City extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/city.txt")) }
object USState extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/us-state.txt")) }
object PlaceSuffix extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/place-suffix.txt")) }

object JobTitle extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/jobtitle.txt")) } // TODO Change this name here and in the .jar to job-title
object Money extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/money.txt")) }

object Company extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/company.txt")) }
object OrgSuffix extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/org-suffix.txt")) }

object Month extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/month.txt")) }
object Day extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/day.txt")) }

/** A lexicon of honorifics, such as "Mrs." "Senator" and "Dr." */
object PersonHonorific extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-honorific.txt")) }

object PersonFirstHighest extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-first-highest.txt")) }
object PersonFirstHigh extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-first-high.txt")) }
object PersonFirstMedium extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-first-medium.txt")) }
object PersonFirst extends UnionLexicon(PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

object PersonLastHighest extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-last-highest.txt")) }
object PersonLastHigh extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-last-high.txt")) }
object PersonLastMedium extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("iesl/person-last-medium.txt")) }
object PersonLast extends UnionLexicon(PersonLastHighest, PersonLastHigh, PersonLastMedium)

// TODO Change the names in the .jar to match these
object PersonFirstFemale extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("uscensus/person-first-female.txt")) }
object PersonFirstMale extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("uscensus/person-first-male.txt")) }
object PersonLastFromCensus extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("uscensus/person-last.txt")) }


// TODO Change the names in the .jar to match these
object WikipediaPerson extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/person.txt")) }
object WikipediaOrganization extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/organization.txt")) }
object WikipediaLocation extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/location.txt")) }
object WikipediaEvent extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/events.txt")) } // TODO rename this to "event.txt"
object WikipediaBattle extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/battle.txt")) }
object WikipediaCompetition extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("wikipedia/competition.txt")) }

