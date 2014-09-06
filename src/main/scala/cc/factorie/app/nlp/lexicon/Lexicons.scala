/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.lexicon
import cc.factorie.app.nlp.lemma._
import cc.factorie.app.strings._

object NumberWords extends PhraseLexicon("NumberWords", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
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
dozen
dozens
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


object Determiner extends PhraseLexicon("Determiner", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
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

object PersonPronoun extends PhraseLexicon("PersonPronoun", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
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

object PosessiveDeterminer extends PhraseLexicon("PosessiveDeterminer", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++=
"""my
your
his
her
its
their"""
}

/** A non-exhaustive list of common English prepositions. */
object Preposition extends PhraseLexicon("Preposition", nonWhitespaceClassesSegmenter, LowercaseLemmatizer) {
  this ++=
"""about
above
across
after
against
around
at
before
behind
below
beneath
beside
besides
between
beyond
by
down
during
except
for
from
in
inside
into
like
near
of
off
on
out
outside
over
since
through
throughout
till
to
toward
under
until
up
upon
with
without"""
}
