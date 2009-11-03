package cc.factorie

import cc.factorie.er1._
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
  trait Formula {
    def eval(x:Args) : Boolean
    def manifests : Seq[Manifest[_<:Variable]]
    /** The Array is for each Arg position; the Seq is for the collection of terms using that Arg. */
    def getters : Array[Seq[Getter0[Variable,SingleIndexedVariable]]] // used to be [X,Bool]
    def ==>(f:Formula) = Implies(this, f)
    //def |=>(f:Formula) = Implies(this, f) // TODO perhaps this would be better to make sure it has lower precedence than === !!  http://www.scala-lang.org/docu/files/ScalaReference.pdf
    def ^(f:Formula) = And(this, f)
    def v(f:Formula) = Or(this, f)
    def <==>(f:Formula) = Equals(this, f)
  }
  
  /** A boolean expression consisting of a single boolean variable */
  case class Term[X<:Variable,A<:Bool](g1:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends Formula {
    def eval(x:Args) : Boolean = x.pop.asInstanceOf[Bool].value // { val as = g1.forward(x).toList; assert(as.size == 1); as(0).value }
    def manifests = List(ma)
    val getters = {
      val pos = g1.argPosition
      val a = new Array[Seq[Getter0[Variable,SingleIndexedVariable]]](pos+1)
      a(pos) = List(g1.asInstanceOf[Getter0[Variable,SingleIndexedVariable]]) // TODO can we get rid of the cast with covariant typing?
      a
    }
  }
  
  abstract class Term2[A<:Variable,B<:Variable](g1:Getter0[_,A], g2:Getter0[_,B])(implicit ma:Manifest[A], mb:Manifest[B]) extends Formula {
    // def eval(x:Args) is missing
    def manifests = List(ma,mb)
    val getters = {
      val pos1 = g1.argPosition; val pos2 = g2.argPosition; val max = if (pos1 > pos2) pos1 else pos2
      val a = new Array[Seq[Getter0[Variable,SingleIndexedVariable]]](max)
      for (i <- 0 until max)
        if (g1.argPosition == i && g2.argPosition == i)
        	a(i) = List(g1.asInstanceOf[Getter0[Variable,SingleIndexedVariable]], g2.asInstanceOf[Getter0[Variable,SingleIndexedVariable]])
        else if (g1.argPosition == i)
          a(i) = List(g1.asInstanceOf[Getter0[Variable,SingleIndexedVariable]])
        else if (g2.argPosition == i)
        	a(i) = List(g2.asInstanceOf[Getter0[Variable,SingleIndexedVariable]])
      a
    }
  }
  
  implicit def getter2Formula[X<:Variable,A<:Bool](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : Formula = new Term(g)(mx,ma)

  //implicit def relation2Formula
  
  abstract class FormulaOp1(c1:Formula) extends Formula {
    def manifests = c1.manifests
    def getters = c1.getters
  }
  case class Not(f1:Formula) extends FormulaOp1(f1) {
    def eval(x:Args) = ! f1.eval(x)
  }
  abstract class FormulaOp2(c1:Formula, c2:Formula) extends Formula {
    def manifests = c1.manifests ++ c2.manifests
    val getters = {
      val pos1 = c1.getters.size; val pos2 = c2.getters.size; val max = if (pos1 > pos2) pos1 else pos2
      val a = new Array[Seq[Getter0[Variable,SingleIndexedVariable]]](max)
      for (i <- 0 until max)
        if (c1.getters.length > i && c2.getters.length > i)
        	a(i) = c1.getters(i) ++ c2.getters(i)
        else if (c1.getters.length > i)
          a(i) = c1.getters(i)
        else
        	a(i) = c2.getters(i)
      a
    }
  }
  case class And(c1:Formula, c2:Formula) extends FormulaOp2(c1,c2) {
    def eval(x:Args) = c1.eval(x) && c2.eval(x) 
  }
  case class Or(c1:Formula, c2:Formula) extends FormulaOp2(c1,c2) {
    def eval(x:Args) = c1.eval(x)|| c2.eval(x) 
  }
  case class Implies(c1:Formula, c2:Formula) extends FormulaOp2(c1,c2) {
    def eval(x:Args) = (! c1.eval(x)) || c2.eval(x)
  }
  case class Equals(c1:Formula, c2:Formula) extends FormulaOp2(c1,c2) {
    def eval(x:Args) = c1.eval(x) == c2.eval(x)
  }  
  

  /** An expression whose value is an integer */
  trait IntExpression {
    def eval(x:Args) : Int
    def manifests : Seq[Manifest[_<:Variable]]
    def getters : Array[Seq[Getter0[Variable,SingleIndexedVariable]]]
    def ====(f:IntExpression) = IntEquals(this, f)
    def >(f:IntExpression) = GreaterThan(this, f)
    def <(f:IntExpression) = LessThan(this, f)
  }

  case class IntTerm[X<:Variable,A<:SingleIndexedVariable](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends IntExpression {
    def eval(x:Args) : Int = x.pop.index
    def manifests = List(ma)
    val getters = {
      val pos = g.argPosition
      val a = new Array[Seq[Getter0[Variable,SingleIndexedVariable]]](pos+1)
      a(pos+1) = List(g.asInstanceOf[Getter0[Variable,SingleIndexedVariable]]) // TODO can we get rid of the cast with covariant typing?
      a
    }

  } 
  
  implicit def getter2IntTerm[X<:Variable,A<:SingleIndexedVariable](g:Getter0[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : IntExpression = new IntTerm(g)(mx,ma)

  abstract class IntIntExpression2(c1:IntExpression, c2:IntExpression) extends IntExpression {
    def manifests = c1.manifests ++ c2.manifests
    def getters = c1.getters ++ c2.getters
  }
  abstract class IntBoolExpression2(c1:IntExpression, c2:IntExpression) extends Formula {
    def manifests = c1.manifests ++ c2.manifests
    val getters = {
      val pos1 = c1.getters.size; val pos2 = c2.getters.size; val max = if (pos1 > pos2) pos1 else pos2
      val a = new Array[Seq[Getter0[Variable,SingleIndexedVariable]]](max+1)
      for (i <- 0 until max)
        if (c1.getters.length > i && c2.getters.length > i)
        	a(i) = c1.getters(i) ++ c2.getters(i)
        else if (c1.getters.length > i)
          a(i) = c1.getters(i)
        else
        	a(i) = c2.getters(i)
      a
    }
  }
  case class IntEquals(c1:IntExpression, c2:IntExpression) extends IntBoolExpression2(c1,c2) {
    def eval(x:Args) = c1.eval(x) == c2.eval(x) 
  }
  case class GreaterThan(c1:IntExpression, c2:IntExpression) extends IntBoolExpression2(c1,c2) {
    def eval(x:Args) = c1.eval(x) > c2.eval(x) 
  }
  case class LessThan(c1:IntExpression, c2:IntExpression) extends IntBoolExpression2(c1,c2) {
    def eval(x:Args) = c1.eval(x) < c2.eval(x) 
  }

  
  trait LogicStatistics extends DotStatistics1[Bool] {
  	def %(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }

  object Forany {
    def apply[X<:Variable](x2c:Arg[X]=>Formula) : Template with LogicStatistics = {
      type I = SingleIndexedVariable 
      val formula = x2c(Arg[X](0))
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[I]]];
      assert(formula.getters.length == 1)
      val accessors = formula.getters(0)
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


}
