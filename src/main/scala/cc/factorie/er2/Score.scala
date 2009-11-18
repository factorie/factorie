package cc.factorie.er2
import scala.reflect.Manifest
import cc.factorie._

case class Score[X<:Variable](sns:ScoreNeighbor0[X]*) {
  def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
  def accessors : Seq[Accessor[X,CategoricalValues]] = sns.flatMap(_.accessors)
}
trait ScoreNeighbor0[X<:Variable] {
  def manifests : Iterable[Manifest[CategoricalValues]]
  def accessors : Iterable[Accessor[X,CategoricalValues]]
}
class ScoreNeighbor[X<:Variable,A<:CategoricalValues](a1:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends ScoreNeighbor0[X] {
  def manifests = List(ma.asInstanceOf[Manifest[CategoricalValues]])
  def accessors = List(a1.asInstanceOf[Accessor[X,CategoricalValues]])
}

// The following implicit conversion is in cc.factorie.er2.er2:
//  implicit def accessor2scoreneighbor[X<:Variable,A<:CategoricalValues](a:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]): ScoreNeighbor0[X] = new ScoreNeighbor(a)(mx,ma)


object For {
  def apply[X<:AccessorType with Variable](x2c:X#AccessorType=>Score[X])(implicit m:Manifest[X#AccessorType]) = {
  	//val score = x2c(m.erasure.getConstructors()(0).newInstance().asInstanceOf[X#AccessorUnitType])
    val score = x2c(AccessorUnit[X](m))
    val manifests = score.manifests.toList.asInstanceOf[List[Manifest[CategoricalValues]]]
    val accessors = score.accessors
    val size = manifests.length
    type I = CategoricalValues
    size match {
    	case 1 => new TemplateWithDotStatistics1[I]()(manifests(0)) {
    		override def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
    	}
    	case 2 => new TemplateWithDotStatistics2[I,I]()(manifests(0), manifests(1)) {
    		def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
    		def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
    	}
    	case 3 => new TemplateWithDotStatistics3[I,I,I]()(manifests(0), manifests(1), manifests(2)) {
    		def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
    		def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
    		def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
    	}
    	case 4 => new TemplateWithDotStatistics4[I,I,I,I]()(manifests(0), manifests(1), manifests(2), manifests(3)) {
    		def unroll1(n1:I) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    		def unroll2(n2:I) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    		def unroll3(n3:I) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n4 <- accessors(3).forward(root)) yield Factor(n1,n2,n3,n4) } 
    		def unroll4(n4:I) = { val roots = accessors(3).reverse(n4); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3,n4) } 
    	}
    }
  }
}
