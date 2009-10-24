package cc.factorie
import scala.collection.mutable.ArrayStack
import scala.reflect.Manifest
import cc.factorie.er2._

object logic2 {
  import cc.factorie.er2

  trait Formula[X<:Variable] {
    def eval(x:ArrayStack[Bool]) : Boolean
    def manifests : Seq[Manifest[_<:Variable]]
    def accessors : Seq[Accessor[X,Bool]]
    def -->(f:Formula[X]) = Implies(this, f)
    def ^(f:Formula[X]) = And(this, f)
    def v(f:Formula[X]) = Or(this, f)
    def !: = Not(this)
  }
  case class Term[X<:Variable,A<:Bool](g1:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends Formula[X] {
    def eval(x:ArrayStack[Bool]) = x.pop.value
    def manifests = List(ma)
    def accessors = List(g1.asInstanceOf[Accessor[X,Bool]])
  }
  implicit def accessor2formula[X<:Variable,A<:Bool](g:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : Formula[X] = new Term(g)(mx,ma)
  abstract class Formula1[X<:Variable](c1:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests
    def accessors = c1.accessors
  }
  case class Not[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[Bool]) = ! f1.eval(x)
  }
  abstract class Formula2[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula[X] {
    def manifests = c1.manifests ++ c2.manifests
    def accessors = c1.accessors ++ c2.accessors
  }
  case class And[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[Bool]) = c1.eval(x) && c2.eval(x) 
  }
  case class Or[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[Bool]) = c1.eval(x)|| c2.eval(x) 
  }
  case class Implies[X<:Variable](c1:Formula[X], c2:Formula[X]) extends Formula2[X](c1,c2) {
    def eval(x:ArrayStack[Bool]) = (! c1.eval(x)) || c2.eval(x)
  }
  case class Forall[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[Bool]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }
  case class Forsome[X<:Variable](f1:Formula[X]) extends Formula1(f1) {
    def eval(x:ArrayStack[Bool]) = throw new Error("Not yet implemented") // Need to think carefully about this
  }

  trait LogicStatistics extends ExpStatistics1[Bool] with DenseWeightedLinearTemplate {
  	def %(w:Double) : this.type = { this.weights(0) = 0.0; this.weights(1) = Math.log(w); this }
  }
  object Forany {
    def apply[X<:AccessorUnit with Variable](x2c:X#AccessorUnitType=>Formula[X])(implicit m:Manifest[X#AccessorUnitType]) : LogicStatistics = {
      val formula = x2c(m.erasure.getConstructors()(0).newInstance().asInstanceOf[X#AccessorUnitType])
      val manifests = formula.manifests.asInstanceOf[Seq[Manifest[Bool]]]
      val accessors = formula.accessors
      val size = manifests.length
      println("Forany leaf size = "+size)
      size match {
        case 1 => new Template1[Bool]()(manifests(0).asInstanceOf[Manifest[Bool]]) with LogicStatistics {
          override def unroll1(n1:Bool) = {
            val roots = accessors(0).reverse(n1)
            if (!roots.isEmpty) Factor(n1) else Nil
            //println("Forany case1  "+accessors(0).reverse(n1))
            //roots.flatMap(accessors(0).forward(_)).map(Factor(_))
          }
          // TODO think about this: what happens when there are multiple factors returned above.  Is root(n1) different for each?
          def statistics(n1:Bool) = { val s = new ArrayStack[Bool]; s+=n1; Stat(Bool(formula.eval(s))) }
          /** Set the weight parameters to make possible worlds in which the template result is true be w-times more likely than worlds in which it is false. */
        }
        case 2 => new Template2[Bool,Bool]()(manifests(0), manifests(1)) with LogicStatistics {
          def unroll1(n1:Bool) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
          def unroll2(n2:Bool) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
          def statistics(n1:Bool, n2:Bool) = { val s = new ArrayStack[Bool]; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }
        case 3 => new Template3[Bool,Bool,Bool]()(manifests(0), manifests(1), manifests(2)) with LogicStatistics {
          def unroll1(n1:Bool) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll2(n2:Bool) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
          def unroll3(n3:Bool) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
          def statistics(n1:Bool, n2:Bool, n3:Bool) = { val s = new ArrayStack[Bool]; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }
        case 4 => new Template4[Bool,Bool,Bool,Bool]()(manifests(0), manifests(1), manifests(2), manifests(3)) with LogicStatistics {
          def unroll1(n1:Bool) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll2(n2:Bool) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll3(n3:Bool) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def unroll4(n4:Bool) = { val roots = accessors(3).reverse(n4); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
          def statistics(n1:Bool, n2:Bool, n3:Bool, n4:Bool) = { val s = new ArrayStack[Bool]; s+=n4; s+=n3; s+=n2; s+=n1; Stat(Bool(formula.eval(s))) }
        }
      }
    }
  }

}
