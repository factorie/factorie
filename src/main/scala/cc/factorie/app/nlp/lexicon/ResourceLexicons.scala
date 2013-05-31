package cc.factorie.app.nlp.lexicon

// Several standard lexicons available through the factorie-nlp-lexicons.jar

object Continents extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/continents.txt")) }
object Country extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/country.txt")) }
object City extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/city.txt")) }
object USState extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/us-state.txt")) }
object PlaceSuffix extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/place-suffix.txt")) }

object JobTitle extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/job-title.txt")) } // TODO Change this name in the .jar

object Company extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/company.txt")) }
object OrgSuffix extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/org-suffix.txt")) }

object Month extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/month.txt")) }
object Day extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/day.txt")) }

/** A lexicon of honorifics, such as "Mrs." "Senator" and "Dr." */
object PersonHonorific extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc.factorie.app.nlp.lexicon.iesl.person-honorific.txt")) }

object PersonFirstHighest extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-first-highest.txt")) }
object PersonFirstHigh extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-first-high.txt")) }
object PersonFirstMedium extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-first-medium.txt")) }
object PersonFirst extends UnionLexicon(PersonFirstHighest, PersonFirstHigh, PersonFirstMedium)

object PersonLastHighest extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-last-highest.txt")) }
object PersonLastHigh extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-last-high.txt")) }
object PersonLastMedium extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/iesl/person-last-medium.txt")) }
object PersonLast extends UnionLexicon(PersonLastHighest, PersonLastHigh, PersonLastMedium)

// TODO Change the names in the .jar to match these
object PersonFirstFemale extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/uscensus/person-first-female.txt")) }
object PersonFirstMale extends WordLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/uscensus/person-first-male.txt")) }

// TODO Change the names in the .jar to match these
object Event extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/wiki/event.txt")) }
object Battle extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/wiki/battle.txt")) }
object Competition extends PhraseLexicon { this ++= io.Source.fromInputStream(getClass.getResourceAsStream("cc/factorie/app/nlp/lexicon/wiki/competition.txt")) }

