package cc.factorie.er2
import scala.reflect.Manifest
import cc.factorie._

case class Score[X<:Variable](sns:ScoreNeighbor0[X]*) {
  def manifests : Seq[Manifest[_<:Variable]] = sns.flatMap(_.manifests)
  def accessors : Seq[Accessor[X,IndexedVariable]] = sns.flatMap(_.accessors)
}
trait ScoreNeighbor0[X<:Variable] {
  def manifests : Iterable[Manifest[IndexedVariable]]
  def accessors : Iterable[Accessor[X,IndexedVariable]]
}
class ScoreNeighbor[X<:Variable,A<:IndexedVariable](a1:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) extends ScoreNeighbor0[X] {
  def manifests = List(ma.asInstanceOf[Manifest[IndexedVariable]])
  def accessors = List(a1.asInstanceOf[Accessor[X,IndexedVariable]])
}
//implicit def accessor2scoreneighbor[X<:Variable,A<:IndexedVariable](a:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]) : ScoreNeighbor0[X] = new ScoreNeighbor(a)(mx,ma)



object For {
  def apply[X<:AccessorUnit with Variable](x2c:X#AccessorUnitType=>Score[X])(implicit m:Manifest[X#AccessorUnitType]) = {
    val score = x2c(m.erasure.getConstructors()(0).newInstance().asInstanceOf[X#AccessorUnitType])
    val manifests = score.manifests.toList.asInstanceOf[List[Manifest[IndexedVariable]]]
    val accessors = score.accessors
    val size = manifests.length
    size match {
      case 1 => new TemplateWithExpStatistics1[IndexedVariable]()(manifests(0)) with PerceptronLearning {
        override def unroll1(n1:IndexedVariable) = { val roots = accessors(0).reverse(n1); if (!roots.isEmpty) Factor(n1) else Nil }
      }
      case 2 => new TemplateWithExpStatistics2[IndexedVariable,IndexedVariable]()(manifests(0),manifests(1)) with PerceptronLearning {
        def unroll1(n1:IndexedVariable) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root)) yield Factor(n1,n2) }
        def unroll2(n2:IndexedVariable) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root)) yield Factor(n1,n2) }
      }
      case 3 => new TemplateWithExpStatistics3[IndexedVariable,IndexedVariable,IndexedVariable]()(manifests(0),manifests(1),manifests(2)) with PerceptronLearning {
        def unroll1(n1:IndexedVariable) = { val roots = accessors(0).reverse(n1); for (root <- roots; n2 <- accessors(1).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
        def unroll2(n2:IndexedVariable) = { val roots = accessors(1).reverse(n2); for (root <- roots; n1 <- accessors(0).forward(root); n3 <- accessors(2).forward(root)) yield Factor(n1,n2,n3) } 
        def unroll3(n3:IndexedVariable) = { val roots = accessors(2).reverse(n3); for (root <- roots; n1 <- accessors(0).forward(root); n2 <- accessors(1).forward(root)) yield Factor(n1,n2,n3) } 
      }
    }
  }
}
