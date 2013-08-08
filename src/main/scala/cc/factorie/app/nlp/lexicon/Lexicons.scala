package cc.factorie.app.nlp.lexicon
import cc.factorie.app.nlp.lemma._
import cc.factorie.app.strings._

object NumberWords extends WordLexicon("NumberWords", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
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


object Determiner extends WordLexicon("Determiner", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
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

object Pronoun extends PhraseLexicon("Pronoun", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++=
"""all
another
any
anybody
anyone
anything
both
each
each other
either
everybody
everyone
everything
few
he
her
hers
herself
him
himself
his
I
it
its
itself
many
me
mine
myself
neither
no_one
nobody
none
nothing
one
one another
other
ours
ourselves
several
she
some
somebody
someone
something
such
that
theirs
them
themselves
these
they
this
those
us
we
what
whatever
which
whichever
who
whoever
whom
whomever
whose
you
yours
yourself
yourselves"""    
}

object PersonPronoun extends WordLexicon("PersonPronoun", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++=
"""anybody
anyone
everybody
everyone
he
her
hers
herself
him
himself
his
I
me
mine
myself
nobody
ours
ourselves
she
somebody
someone
theirs
them
themselves
they
us
we
who
whoever
whom
whomever
whose
you
yours
yourself
yourselves"""    
}

object PosessiveDeterminer extends WordLexicon("PosessiveDeterminer", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++=
"""my
your
his
her
its
their"""
}
