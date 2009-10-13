package cc.factorie

import cc.factorie.er._
import scala.reflect.Manifest
import scala.collection.mutable.ArrayStack

/** Provides syntactic sugar making it easy to create factor templates from clauses in first-order logic. 

To use, first "import cc.factorie.logic._".  This functionality is
currently defined as an object in order to provide the necessary
implicit conversion functions, but after the release of Scala 2.8, it will be
defined in a package and package object.
*/

object logic {
  // Defining in this way so that "import cc.factorie.logic._" will work, and we get the necessary implicit def getter2formula

  /** The collection of arguments to the boolean expression; the variables neighboring the factor */
  type Args = ArrayStack[SingleIndexedVariable]

  /** A boolean expression */
  trait Formula[X<:Variable] {
    def eval(x:Args) : Boolean
    def manifests : Seq[Manifest[_<:Variable]]
    def getters : Seq[Getter0[X,SingleIndexedVariable]] // used to be [X,Bool]
    def ==>(f:Formula[X]) = Implies(this, f)
    def |=>(f:Formula[X]) = Implies(this, f) // TODO perhaps this would be better to make sure it has lower precedence than === !!  http://www.scala-lang.org/docu/files/ScalaReference.pdf
    def ^(f:Formula[X]) = And(this, f)
    def v(f:Formula[X]) = Or(this, f)
    def ===(f:Formula[X]) = Equals(this, f)
  }
  
  /** A boolean expression consisting of a single boolean variable */
  case class Term[X<:Variable,A<:Bool](g1:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends Formula[X] {
    def eval(x:Args) : Boolean = x.pop.asInstanceOf[Bool].value // { val as = g1.forward(x).toList; assert(as.size == 1); as(0).value }
    def manifests = List(ma)
    def getters = List(g1.asInstanceOf[Getter0[X,SingleIndexedVariable]])
  }
  
  implicit def getter2Formula[X<:Variable,A<:Bool](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : Formula[X] = new Term(g)(mx,ma)

  abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests
    def getters = c1.getters
  }
  case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:Args) = ! f1.eval(x)
  }
  abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x) && c2.eval(x) 
  }
  case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x)|| c2.eval(x) 
  }
  case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:Args) = (! c1.eval(x)) || c2.eval(x)
  }
  case class Equals[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x) == c2.eval(x)
  }  
  

  /** An expression whose value is an integer */
  trait IntExpression[X<:Variable] {
    def eval(x:Args) : Int
    def manifests : Seq[Manifest[_<:Variable]]
    def getters : Seq[Getter0[X,SingleIndexedVariable]]
    def ===(f:IntExpression[X]) = IntEquals(this, f)
    def >(f:IntExpression[X]) = GreaterThan(this, f)
    def <(f:IntExpression[X]) = LessThan(this, f)
  }

  case class IntTerm[X<:Variable,A<:SingleIndexedVariable](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends IntExpression[X] {
    def eval(x:Args) : Int = x.pop.index
    def manifests = List(ma)
    def getters = List(g.asInstanceOf[Getter0[X,SingleIndexedVariable]])
  } 
  
  implicit def getter2IntTerm[X<:Variable,A<:SingleIndexedVariable](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : IntExpression[X] = new IntTerm(g)(mx,ma)

  abstract class IntIntExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntExpression[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  abstract class IntBoolExpression2[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  case class IntEquals[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x) == c2.eval(x) 
  }
  case class GreaterThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x) > c2.eval(x) 
  }
  case class LessThan[X<:Variable](c1:IntExpression[X], c2:IntExpression[X]) extends IntBoolExpression2[X](c1,c2) {
    def eval(x:Args) = c1.eval(x) < c2.eval(x) 
  }

  
  trait LogicStatistics extends ExpStatistics1[Bool] with PerceptronLearning {
  	def %(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }

  case class Forany2 {
    def apply[X1<:Variable,X2<:Variable](x2c:(Arg[X1],Arg[X2])=>Formula[X2]) : Template with LogicStatistics = {
      type I = SingleIndexedVariable
      //val formula = x2c(Arg[X1],Arg[X2])
      null
    }
  }
  object Forany {
    def apply[X<:Variable](x2c:Arg[X]=>Formula[X]) : Template with LogicStatistics = {
      type I = SingleIndexedVariable 
      val formula = x2c(Arg[X](0))
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]]
      val accessors = formula.getters
      val size = manifests.length
      size match {
        case 1 => new Template1[I]()(manifests(0)) with LogicStatistics {
        	override def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
          def statistics(n1:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 2 => new Template2[I,I]()(manifests(0), manifests(1)) with LogicStatistics {
        	def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
          def statistics(n1:I, n2:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 3 => new Template3[I,I,I]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
          def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
          def statistics(n1:I, n2:I, n3:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
        case 4 => new Template4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) with LogicStatistics {
        	def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:I) = { val roots = accessors(3).reverse(n4); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def statistics(n1:I, n2:I, n3:I, n4:I) = { val s = new ArrayStack[SingleIndexedVariable]; s+=n4; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }.init
      }
    }
  }

}
