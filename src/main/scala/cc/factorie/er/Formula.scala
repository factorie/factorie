/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.er
import cc.factorie._
import scala.collection.mutable.{ArrayStack,HashSet,HashMap,ListBuffer}


// Define functions for clauses in first-order logic
// Example usage: Forany[Person] { p => p.smokes ==> p.cancer }



/** Could also be known as a BooleanExpression.  See also IntExpression below. */
trait Formula[X<:Variable] {
  def eval(x:FormulaArgs) : Boolean
  def manifests : List[Manifest[_<:FormulaArg]]
  def getters : List[GetterHead[X,FormulaArg]]
  def ==>(f:Formula[X]) = Implies(this, f)
  def ^(f:Formula[X]) = And(this, f)
  def v(f:Formula[X]) = Or(this, f)
  def unary_! = Not(this) // provides usage "!a"
  def <==>(f:Formula[X]) = BooleanEquals(this, f)
}
// TODO Why is this a case class?
case class BooleanTerm[X<:Variable,A<:FormulaArg](g1:Getter[A]{type A=X})(implicit ma:Manifest[A]) extends Formula[X] {
  var extraNeighborCount = 0
  var manifests = ma.asInstanceOf[Manifest[FormulaArg]] :: Nil
  var getters = g1.asInstanceOf[GetterHead[X,FormulaArg]] :: Nil
  // this BooleanTerm has one value (of the last getter) needed for evaluation of the Formula, 
  // but it may have other mutable variables in its chain of getters, in particular a Relation.
  // We need to make sure that these variables will be neighbors in the Template created from this Formula, in the correct order
  var g: Getter[_] = g1
  //println("Term ma "+ma)
  while (g.getPrefix != null) { 
    if (g.extraManifest != null) {
      //println("Term adding extraManifest "+g.extraManifest)
      manifests = g.extraManifest.asInstanceOf[Manifest[FormulaArg]] :: manifests
      getters = g.extraGetter.asInstanceOf[GetterHead[X,FormulaArg]] :: getters
    }
    g = g.getPrefix
  }
  def eval(x:FormulaArgs): Boolean = {
    if (extraNeighborCount != 0) for (i <- 0 until extraNeighborCount) x.pop
    x.pop.asInstanceOf[BooleanVar].booleanValue
  }
}

/*abstract class Term2[X<:Variable,A<:FormulaArg,B<:FormulaArg](g1:GetterHead[X,A], g2:Getter0[X,B])(implicit ma:Manifest[A], mb:Manifest[B]) extends Formula[X] {
 // def eval(x:Args) is missing
 def manifests = List(ma,mb)
 val getters = {
 val pos1 = g1.argPosition; val pos2 = g2.argPosition; val max = if (pos1 > pos2) pos1 else pos2
 val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](max)
 for (i <- 0 until max)
 if (g1.argPosition == i && g2.argPosition == i)
 a(i) = List(g1.asInstanceOf[Getter0[Variable,CategoricalVariable]], g2.asInstanceOf[Getter0[Variable,CategoricalVariable]])
 else if (g1.argPosition == i)
 a(i) = List(g1.asInstanceOf[Getter0[Variable,CategoricalVariable]])
 else if (g2.argPosition == i)
 a(i) = List(g2.asInstanceOf[Getter0[Variable,CategoricalVariable]])
 a
 }
 }*/

abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
  def manifests = c1.manifests
  def getters = c1.getters
}
case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
  def eval(x:FormulaArgs) = ! f1.eval(x)
}
case class True[X<:Variable](f1:Formula[X]) extends Formula1(f1) { // noop, but forces implicit conversion to BooleanTerm
  def eval(x:FormulaArgs) = f1.eval(x)
}
abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
  def manifests = c1.manifests ++ c2.manifests
  def getters = c1.getters ++ c2.getters
}
case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) && c2.eval(x) 
}
case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) || c2.eval(x) 
}
case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
  def eval(x:FormulaArgs) = (! c1.eval(x)) || c2.eval(x)
}
case class BooleanEquals[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) == c2.eval(x)
}
case class Forall[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
  def eval(x:FormulaArgs) = throw new Error("Not yet implemented") // Need to think carefully about this
}
case class Forsome[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
  def eval(x:FormulaArgs) = throw new Error("Not yet implemented") // Need to think carefully about this
}


/** An expression whose value is an integer.  Type argument X is the type of the root of the expression. */
trait IntExpression[X<:Variable] {
  def eval(x:FormulaArgs) : Int
  def manifests : List[Manifest[_<:FormulaArg]]
  def getters : List[GetterHead[X,FormulaArg]]
  def ===(f:IntExpression[X]) = IntEquals(this, f)
  def >(f:IntExpression[X]) = GreaterThan(this, f)
  def <(f:IntExpression[X]) = LessThan(this, f)
  // To avoid implicit conversion ambiguity, none of the operator names here should conflict with the operator names of Formula
}

case class IntTerm[X<:Variable,A<:FormulaArg](g1:GetterHead[X,A])(implicit ma:Manifest[A]) extends IntExpression[X] {
  def eval(x:FormulaArgs): Int = x.pop.intValue
  var manifests = List(ma.asInstanceOf[Manifest[FormulaArg]])
  var getters = List(g1.asInstanceOf[GetterHead[X,FormulaArg]])
  /*val getters = {
   val pos = g.argPosition
   val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](pos+1)
   a(pos+1) = List(g.asInstanceOf[Getter0[Variable,CategoricalVariable]]) // TODO can we get rid of the cast with covariant typing?
   a
   }*/
} 

abstract class IntIntExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntExpression[X] {
  def manifests = c1.manifests ++ c2.manifests
  def getters = c1.getters ++ c2.getters
}
abstract class IntBoolExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends Formula[X] {
  def manifests = c1.manifests ++ c2.manifests
  def getters = c1.getters ++ c2.getters
  /*val getters = {
   val pos1 = c1.getters.size; val pos2 = c2.getters.size; val max = if (pos1 > pos2) pos1 else pos2
   val a = new Array[Seq[Getter0[Variable,CategoricalVariable]]](max+1)
   for (i <- 0 until max)
   if (c1.getters.length > i && c2.getters.length > i)
   a(i) = c1.getters(i) ++ c2.getters(i)
   else if (c1.getters.length > i)
   a(i) = c1.getters(i)
   else
   a(i) = c2.getters(i)
   a
   }*/
}
case class IntEquals[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) == c2.eval(x) 
}
case class GreaterThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) > c2.eval(x) 
}
case class LessThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
  def eval(x:FormulaArgs) = c1.eval(x) < c2.eval(x) 
}



/** The form of Template statistics used by a logical Formula. */
trait LogicStatistics extends DotStatistics1[BooleanVar] {
  // Should a non-zero weight instead be spread across each of the two possibilities?
  def *(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = math.log(w); this }
}


/** Create a Formula starting from a Getter */
object Forany {
  def apply[X<:Variable {type GetterType <: Getter[X]}](x2c:X#GetterType {type A = X}=>Formula[X])(implicit m:Manifest[X]): Template with LogicStatistics = {
    type I = FormulaArg
    val getterRoot: X#GetterType { type A = X } = newGetterUnit[X](m)
    val formula = x2c(getterRoot)
    val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
    //assert(formula.getters.length == 1, formula.getters.length)
    val getters = formula.getters
    val size = manifests.length
    size match {
      case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
        override def unroll1(n1:I) = { val roots = getters(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
        def statistics(n1:I) = { val s = new FormulaArgs; s+=n1; Stat(BooleanObservation(formula.eval(s))) }
      }.init
      case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
        def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
        def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
        def statistics(n1:I, n2:I) = { val s = new ArrayStack[I]; s+=n2; s+=n1; Stat(BooleanObservation(formula.eval(s))) }
      }.init
      case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
        def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
        def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) } 
        def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) } 
        def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[I]; s+=n3; s+=n2; s+=n1; Stat(BooleanObservation(formula.eval(s))) }
      }.init
      case 4 => new Template4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with LogicStatistics {
        def unroll1(n1:I) = { val roots = getters(0).reverse(n1); for (root <- roots; n2 <- getters(1).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
        def unroll2(n2:I) = { val roots = getters(1).reverse(n2); for (root <- roots; n1 <- getters(0).forward(root); n3 <- getters(2).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
        def unroll3(n3:I) = { val roots = getters(2).reverse(n3); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n4 <- getters(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
        def unroll4(n4:I) = { val roots = getters(3).reverse(n4); for (root <- roots; n1 <- getters(0).forward(root); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
        def statistics(n1:I, n2:I, n3:I, n4:I) = { val s = new ArrayStack[I]; s+=n4; s+=n3; s+=n2; s+=n1; Stat(BooleanObservation(formula.eval(s))) }
      }.init
    }
  }
}
/*
 object Forany2 {
 def apply[X1<:Variable,X2<:Variable](x2c:(Arg[X1],Arg[X2])=>Formula) : Template with LogicStatistics = {
 type I = SingleIndexedVariable
 val formula = x2c (Arg[X1](0), Arg[X2](1))
 val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
 assert(formula.getters.length == 2)
 val gettersPerArg = formula.getters
 val size = manifests.length
 size match {
 case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
 // TODO Is this case possible?
 override def unroll1(n1:I) = { if (gettersPerArg.exists(!_.apply(0).reverse(n1).isEmpty)) Factor(n1) else Nil }
 def statistics(n1:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n1; Stat(Bool(formula.eval(s))) }
 }.init
 case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
 //val argPositions = new Array[Int](2) ....
 def unroll1(n1:I) = { for (getters <- gettersPerArg; root <- getters(0).reverse(n1); n2 <- getters(1).forward(root)) yield Factor(n1,n2) }
 def unroll2(n2:I) = { for (getters <- gettersPerArg; root <- getters(1).reverse(n2); n1 <- getters(0).forward(root)) yield Factor(n1,n2) }
 def statistics(n1:I, n2:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
 }.init
 case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
 def unroll1(n1:I) = { for (getters <- gettersPerArg; root <- getters(0).reverse(n1); n2 <- getters(1).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) }
 def unroll2(n2:I) = { for (getters <- gettersPerArg; root <- getters(1).reverse(n2); n1 <- getters(0).forward(root); n3 <- getters(2).forward(root)) yield Factor(n1,n2,n3) }
 def unroll3(n3:I) = { for (getters <- gettersPerArg; root <- getters(2).reverse(n3); n1 <- getters(0).forward(root); n2 <- getters(1).forward(root)) yield Factor(n1,n2,n3) }
 def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
 }.init
 }
 }
 }
 */


// TODO Consider using something like the following?
//type VariableWithGetter[D] = Variable { type GetterType <: Getter[D] }
//type HasGetterType[D] = { type GetterType <: Getter[D] }

