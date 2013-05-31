package cc.factorie.app.nlp.lexicon
import cc.factorie.app.nlp.lemma._
import cc.factorie.app.strings._

object NumberWords extends PhraseLexicon(nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++= 
"""zero
one
two
three
four
five
six
seven
eight
nine
ten
tens
eleven
twelve
thirteen
fourteen
fifteen
sixteen
seventeen
eighteen
nineteen
twenty
thirty
forty
fifty
sixty
seventy
eighty
ninety
hundred
hundreds
thousand
thousands
million
millions
billion
billions
trillion
trillions
quadrillion
quintillion
sextillion
septillion
zillion
umpteen
multimillion
multibillion
"""
}


object DeterminerWords extends PhraseLexicon(nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++= 
"""the
a
this
an
that
some
all
these
no
any
those
another
both
each
every
either
neither
"""
}
