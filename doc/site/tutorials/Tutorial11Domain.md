---
title: "Domain"
layout: default
group: tutorial
weight: 21
---

Domains Tutorial
================

Examples of FACTORIE Domains, which specify the range of values a Variable can take.

```scala
package cc.factorie.tutorial
object TutorialDomain extends App {
  import org.junit.Assert._
  import cc.factorie._
  import cc.factorie.variable.{ DoubleDomain, DoubleVariable, DiscreteDomain, DiffList }
```

In FACTORIE every Variable has a Domain that specifies the range of values the Variable can take.
Domains have a type member 'Value' which indicates the type of these values.
All domains are subclasses of the trait Domain[A], where A is a lower bound on the domain's Value type.

For example, the domain of an DoubleVariable is the object DoubleDomain, and DoubleDomain.Value is Double.

```scala
  val dv = new DoubleVariable(3.1)
  val dd = dv.domain // equals DoubleDomain
  val d: DoubleDomain.Value = 3.14

```

Many Domains, including DoubleDomain do not have significant functionality.
But we could create a subclass of the DoubleDomain trait expressing a restricted range of values
between a newline defined minValue and maxValue.

```scala
  class LimitedDoubleDomain(override val minValue: Double, override val maxValue: Double) extends DoubleDomain
```

We can then define a subclass of DoubleVariable that uses this domain and requires its initial and set values to be within the range.
Note that it is possible for different instances of the same Variable class to have different domains.

```scala
  class LimitedDoubleVariable(initialValue: Double, override val domain: LimitedDoubleDomain) extends DoubleVariable(initialValue) {
    checkValue(initialValue)
    def checkValue(x: Double): Unit = require(initialValue <= domain.maxValue && initialValue >= domain.minValue)
    override def set(x: Double)(implicit d: DiffList): Unit = { checkValue(x); super.set(x) }
  }
  val ldd1 = new LimitedDoubleDomain(0.0, 1.0)
  val ldd9 = new LimitedDoubleDomain(0.0, 9.0)
  val ldva = new LimitedDoubleVariable(0.5, ldd1)
  val ldvb = new LimitedDoubleVariable(3.5, ldd9)

```

Other domains that have insignificant functionality include IntegerDomain, StringDomain, RefDomain,
SeqDomain, SetDomain, TensorDomain, MassesDomain, and ProportionsDomain.
In their corresponding variable traits and classes (IntegerVar, StringVariable etc)
the domain method is pre-defined to return these domain objects.

Unlike the domain classes above,
significant functionality is provided by DiscreteDomain and its subclasses (which include CategoricalDomain,
BooleanDomain and HashDomain).

DiscreteDomain is the domain of DiscreteVariable, which takes on a finite N number of values.
Each value is associated with an integer index from 0 to N-1.
The value, however, is not the integer itself, but an instance of DiscreteValue,
which in turn is a subclass of BinarySingletonTensor1.  Therefore DiscreteDomain inherits from TensorDomain.

DiscreteDomain instances can be initialized with this size, N, as an argument to their constructor.
DiscreteDomain instances are also responsible for constructing all the DiscreteValue instances that are their values.

```scala
  val dieDomain = new DiscreteDomain(6)
  assertEquals(dieDomain.size, 6)

```

DiscreteDomain instances inherit from IndexedSeq[DiscreteValue],
so a DiscreteDomain can also be understood as a collection
of DiscreteValues, where each such value is accessible by its index.

```scala
  val ddv2 = dieDomain(2)
  assertEquals(ddv2.intValue, 2)
  assertEquals(22.0, ddv2 dot new la.DenseTensor1(Array(0.0, 11.0, 22.0, 33.0, 44.0, 55.0)), 0.01)

```

Alternatively to setting the size of a DiscreteDomain with a constant integer at the DiscreteDomain's construction,
it can also have its size delegated to match a proxy object's size.
(The proxy object must be a subclass of scala.collection.Iterable.)

```scala
  val names = new scala.collection.mutable.ArrayBuffer[String] ++= List("Alan", "Barbara", "Carol", "Denis")
  val students = new DiscreteDomain(names)
  assertEquals(4, students.size)
  //& 4
  names += "Ernie"
  assertEquals(students.size, 5)
}

```
This allows a DiscreteDomain to grow in size after its construction. 

CategoricalDomain 

VectorDomain and CategoricalVectorDomain 
