---
title: "Factor"
layout: default
group: tutorial
weight: 30
---


Factor Tutorial
==============

A Factor is a container for an ordered set of neighboring variables,
and can return "scores" and "statistics" based on values for those neighbors.

```scala
package cc.factorie.tutorial
object TutorialFactors extends App {
  import org.junit.Assert._
  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.variable._
  import scala.Tuple2
  import cc.factorie.model._
  import scala.Tuple2
    
    // First we'll create some variables for us as Factor neighbors. 
    val v1 = new IntegerVariable(1)
    val v2 = new DoubleVariable(2.0)
    val v3 = new IntegerVariable(3)
    
```

There are classes Factor1, Factor2, Factor3, Factor4 for Factors with 1-4 neighboring variables.

(If you want a Factor that has more than 4 neighbors,
and many of your neighbors are of the same Variable class,
there is a "var-args" construction available, which will be explained below.
You could also create your own subclass of the trait "Factor".)
    

```scala
    // Here is a factor with one neighbor.
    val f1 = new Factor1(v1) {
      def score(i:Int) = if (i == 1) 0.0 else -1.0
    }

```

The only method that is abstract in Factor1-Factor4 classes is the "score" method.
Its arguments are the values of the neighboring variables, in order.
It must return a Double.
NegInfinity indicates "impossible values on neighbors".
More positive scores indicate higher likelihood.
In terms of probabilities these scores can be interpreted as "unnormalized log-probabilities".
If you do have a normalized probability for a factor, the score should be `math.log(probability)`.
    

```scala
    // Naturally, then we can get the factor's score for particular values of the neighboring variable(s).
    println("First factor score for value 0 is "+f1.score(0))
    assertEquals(-1.0, f1.score(0), 0.01)
    
    // We can also ask for the score resulting from the neighbors current values.
    println("First factor score for current neighbor value is "+f1.currentScore)
    assertEquals(0.0, f1.currentScore, 0.1)
    
    // Here is a Factor with two neighbors.
    // The second neighbor is a DoubleVariable and has value type Double.
    val f2 = new Factor2(v1, v2) {
      def score(i:Int, j:Double) = i * j
    }
    println("Second factor score is "+f2.currentScore)
    assertEquals(2.0, f2.currentScore, 0.01)

    // Given a factor I can get its list of neighbors, or individual neighbors
    println("Second factor neighbors are "+f2.variables)
    println("Second factor first neighbor is "+f2.variable(0))
    assert(f2.variable(0) == v1)
    
    // We can also ask if a Factor has a variable among its neighbors
    if (f2.touches(v3)) println("Second factor has v3 as a neighbor.")
    assert(!f2.touches(v3))
    
    // Factors can be given a customized named used in their toString method
    val f2b = new Factor2(v1, v2) {
      def score(i:Int, j:Double) = i * j
      override def factorName = "Factor2B"
    }
    println("Factor with custom name "+f2b)
    assertEquals("Factor2B", f2b.factorName)
    
    // We can get an Assignment object that captures the current values of the neighbors.
    val a2: Assignment2[IntegerVariable,DoubleVariable] = f2.currentAssignment
    v2 := 4.4 // Here we give v2 a new value.
    println("Second neighbor of second factor had value "+a2(v2))
    assertEquals(2.0, a2(v2), 0.01)
    
    // We can ask for a factor's score using the values in an Assignment
    // rather than the neighbors current values
    println("Second factor's score from old assignment is "+f2.assignmentScore(a2))
    assertEquals(2.0, f2.assignmentScore(a2), 0.01)
    
    // The Assignment object could contain values for more variables than the neighbors
    val as = new HashMapAssignment(v1, v2, v3)
    as.update[IntegerVariable](v1, 44)
    println("Second factor's score from a new assignment is "+f2.assignmentScore(as))
    assertEquals(44*4.4, f2.assignmentScore(as), 0.01)
    
    // All factors also have "statistics"
    // This is some arbitrary object that holds information sufficient to obtain a score.
    // (Although this sufficiency is not enforced.)
    val s2 = f2.statistics(77,8.8)
    
    // As with "score" we can get the statistics corresponding to the neighbors current values
    val s2c = f2.currentStatistics
    
    // The default statistics are a Tuple containing the values of the neighbors.
    // However the known return type is Any, so it must be cast to use.
    // (This lack of typing is because we want the method to be flexibly override-able in subclasses.)
    if (s2.asInstanceOf[(Int, Double)]._2 == 4.4)
      println("The second factor's second value is 4.4")
    assertEquals(8.8, s2.asInstanceOf[(Int, Double)]._2, 0.01)

    // Some subclasses of Factor override the method to return more specific types.
    // If the Tuple statistics are sufficient for your needs, 
    // and you want the "statistics" method's return type to be the correct Tuple
    // use the classes TupleFactor*
    val f3 = new TupleFactorWithStatistics3(v1, v2, v3) {
      def score(i:Int, j:Double, k:Int) = if (i == k) j else 0.0
    }
    println("The current value of the third neighbor is "+f3.currentStatistics._3)
    assertEquals(3.0, f3.currentStatistics._3, 0.01)
    
    // The various Factor subclasses containing "WithStatistics" in the class name
    // all contain a new final definition of the "statistics" method.
    // For example, if I subclass TupleFactorWithStatistics3, I cannot redefine the "statistics" method.
    val f3b = new TupleFactorWithStatistics3(v1, v2, v3) {
      def score(i:Int, j:Double, k:Int) = if (i == k) j else 0.0
      // If uncommented, the next line would cause a compilation error:
      //def statistics(i:Int, j:Double, k:Int) = Tuple(i, k)
    }
    
    // In the plain "Factor1-4" classes you can override statistics to return any type you like,
    // but you have to set the type member StatisticsType to match.
    val f4 = new Factor2(v1, v3) {
      type StatisticsType = Boolean
      override def statistics(i:Int, j:Int): Boolean = i == j
      def score(i:Int, j:Int) = if (statistics(i, j)) 1.0 else -1.0
    }
    
    // Factors with Tensor-valued statistics are widely-used.
    
    // First let's make some Tensor-valued variables.
    object LabelDomain extends CategoricalDomain(List("politics", "sports", "arts"))
    class Label(initialValue:String) extends CategoricalVariable(initialValue) { def domain = LabelDomain }
    val label1 = new Label("arts")
    val label2 = new Label("arts")
    val label3 = new Label("arts")
    // Remember, BooleanVariables have value BooleanValue, which inherits from SingletonBinaryTensor1
    val flag1 = new BooleanVariable(false) 
    val flag2 = new BooleanVariable(true)
    
    // Here is a factor whose statistics are an outer product of the Tensor values of both neighbors.
    // The score is the dot product between this tensor an a Tensor of "weightsSet" parameters.
    val f5 = new Factor2(label1, label2) {
      type StatisticsType = Tensor
      val weights = new DenseTensor2(Array(Array(2.0, 1.0, 0.0), Array(1.0, 2.0, 1.0), Array(0.0, 1.0, 2.0)))
      // Label#Value is a CategoricalValue[String], and a subclass of SingletonBinaryTensor1
      override def statistics(lv1:Label#Value, lv2:Label#Value): Tensor = lv1 outer lv2
      def score(lv1:Label#Value, lv2:Label#Value): Double = weights dot statistics(lv1, lv2) 
    }
```

It is up to you to make the dimensions of the weightsSet Tensor match the dimensions of the statistics Tensor
There are no compile-time checks for this.
    

Since Factors with Tensor statistics are common, there are Factor classes that pre-define this,
avoiding the need to assign StatisticsType.

The method "score" is pre-defined to gather these Tensor statistics and call "scoreStatistics(Tensor)".
(This enforces that the Tensor statistics are sufficient to calculate the score.)
In TensorFactor2 only "statistics" and "scoreStatistics" are abstract.

```scala
    val f6 = new TensorFactor2(flag1, flag2) {
      val weights = new DenseTensor2(Array(Array(3.0, 1.0), Array(2.0, 4.0)))
      override def statistics(fv1:BooleanValue, fv2:BooleanValue): Tensor = fv1 outer fv2
      def statisticsScore(tensor:Tensor): Double = weights dot tensor
    }
    
```

Because it is also common that the statistics are the outer product of the neighbors Tensor values,
there are also TensorFactor* subclasses that pre-define "statistics" in this way.
Only the "scoreStatistics(Tensor)" method is abstract.

```scala
    val f7 = new TensorFactorWithStatistics2(flag1, flag2) {
      val weights = new DenseTensor2(Array(Array(3.0, 1.0), Array(2.0, 4.0)))
      def statisticsScore(tensor:Tensor): Double = weights dot tensor
    }
    
```

Because it is furthermore common that the score be
a dot product of the Tensor statistics and a "weight" parameter Tensor,
there are Factor subclasses that pre-define scoreStatistics to perform this dot-product.
Only the "weightsSet" method is abstract.

```scala
    val f8 = new DotFactorWithStatistics2(flag1, flag2) {
      val weights = new DenseTensor2(Array(Array(3.0, 1.0), Array(2.0, 4.0)))
    }
    
```

If you want the score to be such a dot-product, but you want the statistics Tensor
to be something other than the outer product of the neighbor values,
you can use the DotFactor{1,2,3,4} classes.
Here only the "statistics" and "weightsSet" methods are abstract.

```scala
    val f9 = new DotFactor4(label1, label2, label3, flag1) {
      val weights = new DenseTensor2(Array(Array(2.0, 1.0, 0.0), Array(1.0, 2.0, 1.0), Array(0.0, 1.0, 2.0)))
      override def statistics(lv1:Label#Value, lv2:Label#Value, lv3:Label#Value, fv2:BooleanValue): Tensor = 
        if (flag1.booleanValue) lv1 outer lv2 else lv1 outer lv3
    }
   
    // Naturally, we can define a class of Factors all sharing the same "score" method
    class MyFactor(n1:IntegerVariable, n2:IntegerVariable) extends Factor2(n1, n2) {
      def score(i:Int, j:Int) = if (i == j) 1.0 else -1.0
    }
    
```

Factor equality is based on the Factor class and the identity of its neighboring variables.
That is, if I create two factors, having the same class and same neighbors,
the two factors will be equal.
(This helps us de-duplicate Factors coming from Templates.
We will describe Templates in a later tutorial.)

```scala
    val mf1 = new MyFactor(v1, v3)
    val mf2 = new MyFactor(v1, v3)
    if (mf1 == mf2) println("Two factors are equal.")
    assert(mf1 == mf2)

```

Let's create a Factor representing a simple log-linear classifier for classifying blog posts.
into the classes "politics", "sports", "arts"
The input is a feature vector of three boolean values indicating whether or not
a particular word was present in the document: "beat", "beautiful", "election".

We can use the Label variable from above.
Here let's define a Variable Article for representing the feature vector.
WordDomain is the object WordDomain extends CategoricalDomain(List("beat", "beautiful", "election"))

```scala
    object WordDomain extends CategoricalDomain(List("beat", "beautiful", "election"))
    // TODO Consider interface improvements to CategoricalVectorDomain initialization.
    object ArticleDomain extends CategoricalVectorDomain[String] { override def dimensionDomain = WordDomain }
    class Article(ws:Iterable[String]) extends BinaryFeatureVectorVariable[String](ws) {
      def domain = ArticleDomain
    }

    class MyClassifier(label:Label, article:Article) extends DotFactorWithStatistics2(label, article) {
      val weights = new DenseTensor2(LabelDomain.size, WordDomain.size)
      weights(LabelDomain.index("politics"), WordDomain.index("beat")) = 3.0
      weights(LabelDomain.index("politics"), WordDomain.index("beautiful")) = 2.0
      weights(LabelDomain.index("politics"), WordDomain.index("election")) = 5.0
      weights(LabelDomain.index("sports"), WordDomain.index("beat")) = 4.0
      weights(LabelDomain.index("sports"), WordDomain.index("beautiful")) = 1.0
      weights(LabelDomain.index("sports"), WordDomain.index("election")) = -1.0
      weights(LabelDomain.index("arts"), WordDomain.index("beat")) = -2.0
      weights(LabelDomain.index("arts"), WordDomain.index("beautiful")) = 5.0
      weights(LabelDomain.index("arts"), WordDomain.index("election")) = 1.0
    }
    
    val a1 = new Article("beat election".split(" "))
    val l1 = new Label("politics")
    val cf = new MyClassifier(l1, a1)
    
    // Any now we can do simple exhaustive inference
    var maxScore = Double.NegativeInfinity
    var maxLabeling = LabelDomain.head
    for (labeling <- LabelDomain) {
      l1.set(labeling)(null)
      val score = cf.currentScore
      if (score > maxScore) {
        maxScore = score
        maxLabeling = labeling
      }
    }
    println("When scoring "+a1+" the highest scoring label value is "+maxLabeling)
}
```

Of course there is much more rich and efficient support for classification
(including pre-built large-vocabulary document classification) available in FACTORIE.
The above is a simple demonstration.

Note that all MyClassifier Factors share the same weightsSet Tensor,
yet weightsSet is inefficiently created and filled separate for each MyClassifier instance.
FACTORIE has special support for representing commonalities between Factors
that belong to the same "Family".
