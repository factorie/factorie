---
title: Factorie 0.10.1 released
layout: default
group: news
categories: news
---

## Factorie 0.10.1 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

After a long delay, the latest version of Factorie has been released. Numerous major features have been added, and significant renames and refactoring has been performed. 

* Executable Jar: **[factorie-0.10.1.jar](http://factorie.googlecode.com/files/factorie-0.10.1.jar)**
* Source files: [factorie-0.10.1-src.tar.gz](http://factorie.googlecode.com/files/factorie-0.10.1-src.tar.gz)

Here's an incomplete changelog:

## New in version 0.10.1:

	
* Many renames, new features and refactors; the list below is partially complete.

* Initial support for sparse value iteration in factor/families

* Data representation for app.nlp like Tokens, ParseTrees, Spans, Sentences, etc.

* Initial version for POS, NER, within-doc coref for app.nlp

* Additional vectors that mix sparse and dense representations (SparseOuterVector) in factorie.la

* Added Families that represent sets of factors. Templates are a type of Family now.

* Initial support for MaxLikelihood and Piecewise Training using the new BP framework

* Added a more flexible, modular BP framework

* DiscreteVector and CategoricalVector

	The old names "DiscretesValue", "DiscretesVariable", etc were
deemed too easily misread (easy to miss the little "s" in the middle)
and have been renamed "DiscreteVectorValue", "DiscreteVectorVariable",
etc.

* Factors independent of Templates

* Models independent of Templates

* Redesigned cc.factorie.generative package

New in version 0.10.0:
---

* Variable 'value' methods:

	All variables must now have a 'value' method with return type
'this.Value'. By default this type is Any. If you want to override
use the VarAndValueType trait, which sets the covariant types
'VariableType' and 'ValueType'. 'Value' is magically defined from
these to be psuedo-invariant.

	The canonical representation of DiscreteVariable (and
CategoricalVariable) values used to be an Int. Now it is a
DiscreteValue (or CategoricalValue) object, which is a wrapper around
an integer (and its corresponding categorical value). These objects
are created automatically in the DiscreteDomain (or
CategoricalDomain), and are guaranteed to be unique for each integer
value, and thus can be compared by pointer equality.

	For example, if 'label' is a CategoricalVariable[String]
label.value is a CategoricalValue.
label.intValue == label.value.index, is an integer
label.categoryValue == label.value.category, is a String

* Discrete variables and vectors

	DiscreteValues has been renamed DiscretesValue. Similarily there are
now classes DiscretesVariable, CategoricalsValue and
CategoricalsVariable. These plural names refer to vector values and
their variables. For example, CategoricalsVariable is a superclass of
the BinaryFeatureVectorVariable.

	The singular DiscreteValue, DiscreteVariable, CategoricalValue and
CategoricalVariable hold single values (i.e. which could be mapped to
single integers), but are subclasses their plural counterparts, with
values that are singleton vectors.

	The domain of the plural types (i.e. vectors, not necessarily
singleton vectors) are DiscretesDomain and CategoricalsDomain. The
length of these vectors are determined by an inner DiscreteDomain or
CategoricalDomain. Hence to create a domain for vectors of length 10:

		new DiscretesDomain {
		  val dimensionDomain = new DiscreteDomain { def count = 10 }
		}

* TrueSetting renamed to TargetValue

	Now that all variables have a 'value', the name 'setting' is
deprecated. Also, "true" and "truth" were deemed confusable with
boolean values, and are now deprecated. The preferred alternative is
"target". Hence, the "TrueSetting" trait has been renamed
"TargetValue", and various methods renamed:
setToTruth => setToTarget
valueIsTruth => valueIsTarget
trueIntValue => targetIntValue

* Domains:

	Previously there was a one-to-one correspondence between variable
classes and domains; the variable looked up its domain in a global
hashtable whose keys were the variable classes. Furthermore Domain
objects were often created for the user auto-magically. This scheme
lacked flexibility and was sometimes confusing. The one-to-one
correspondence has now been removed. The 'domain' method in Variable
is now abstract. Some subclasses of Variable define this method, such
as RealVariable; others still leave it abstract. For example, in
subclasses of DiscreteVariable and CategoricalVariable you must define
the 'domain' method. In these cases you must also create your domain
objects explicitly. Thus we have sacrificed a little brevity for
clarity and flexibility. Here is an example of typical code for
creating class labels:

		object MyLabelDomain extends CategoricalDomain[String]
		class MyLabel(theValue:String) extends CategoricalVariable(theValue) {
		  def domain = MyLabelDomain
		}

	or

		class MyLabel(theValue:String, val domain = MyLabelDomain) extends CategoricalVariable(theValue)

	The type argument for domains used to be the variable class; now it is
the 'ValueType' type of the domain (and its variables).

	Templates now automatically gather the domains of the neighbor
variables. VectorTemplates also gather the domains of their
statistics values. [TODO: Discuss the dangers of this automatic
mechanism and consider others mechanisms.]



* Template statistics:

	Previously the constructor arguments of Stat objects were Variables.
They have now been changed to Variable *values* instead. Furthermore,
whereas the old Template.statistics method took as arguments a list
of variables, the new Template.statistics method takes a "Values"
object, which is a simple Tuple-style case class containing variable values.

	For example, old code:

		new Template2[Label,Label] extends DotStatistics1[BooleanVariable] {
		  def statistics(y1:Label, y2:Label) =
		    Stat(new BooleanVariable(y1.intValue == y2.intValue)
		}

	might be re-written as:

		new Template2[Label,Label] extends DotStatistics1[BooleanValue] {
		  def statistics(values:Values) = Stat(values._1 == values._2)
		}

* VectorTemplate

	VectorStatistics1, VectorStatistics2, VectorStatistics3 used to take
VectorVar type arguments. They now take DiscretesValue type
arguments. The method 'statsize' has been renmed
'statisticsVectorLength' for clarity.

* Generative modeling package

	The probability calculations and sampling routines are no longer
implemented in the variable, but in templates instead. Each
GeneratedVar must have a value "generativeTemplate" and a method
"generativeFactor". Many changes have been made to the generative
modeling package, but they are not yet finished or usable. The code
is being checked in now in order to facilitate others' work on the
undirected models.

New in Version 0.9.0:
---

Rudimentary optimize package includes ConjugateGradient and
LimitedMemoryBFGS.

LogLinearMaximumLikelihood sets parameters by BFGS on likelihood
gradient calculated by belief propagation on trees. Additional
inference methods to come soon.

Belief propagation now works.

Variables no longer use their own "set" method to initialize their
values. This means that if you are relying on "override def set" to
do some coordination during object initialization, you must separately
set up this coordination in your own constructors.

Rename Factor neighbor variables from "n1" to "_1" to better match
Scala's Tuples.

Support for generative models has been completely overhauled, and is
now in its own separate package: cc.factorie.generative.

Many variables have been renamed to better match standard names in
statistics, including EnumVariable => CategoricalVariable.

