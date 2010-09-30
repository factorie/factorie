package cc.factorie

import org.scalatest.junit._
import org.junit.Test

/**
 * @author sriedel
 */
class TemplateTestSuite extends JUnitSuite  {

  implicit def template2initialized1[S1<:VectorVar](t:VectorStatistics1[S1])(implicit m:Manifest[S1]): InitializedTemplate = new InitializedTemplate(t.init)
  implicit def template2initialized(t:Template): InitializedTemplate = new InitializedTemplate(t)
  
  @Test
  def testCreateDiff {
    //this test just shows how variables create diff objects that point to them
    val b = new BooleanVariable(true)
    val diff = new DiffList
    b.set(false)(diff)
    assert(diff(0).variable === b)
  }

  @Test
  def testFactorsOfDiffList {
    val template = new TemplateWithDotStatistics1[BooleanVariable]
    val b = new BooleanVariable(true)
    val diff = new DiffList
    b.set(false)(diff)
    val factors = template.factors(diff)
    assert(factors.head.template === template)
    assert(factors.head.variables.head == b)
    assert(factors.head.statistics.asInstanceOf[TemplateWithDotStatistics1[BooleanVariable]#Stat]._1.value == false)
  }

  @Test
  def testCascadeUnroll {
    object Aggregate extends BooleanVariable {
      val b1 = new BooleanVariable {
        override def unrollCascade: scala.Iterable[Variable] = Seq(Aggregate)
      }
    }
    val diff = new DiffList
    val template = new TemplateWithDotStatistics1[BooleanVariable]
    Aggregate.b1.set(true)(diff)
    val factors = template.factors(diff)
    assert(factors.exists(factor => factor.variables.head == Aggregate.b1))
    assert(factors.exists(factor => factor.variables.head == Aggregate))
  }

  @Test
  def testVarArgs {
    class Aggregate extends BooleanVariable {
      class Member extends BooleanVariable {
        def owner = Aggregate.this
      }
      val members = for (i <- 0 until 10) yield new Member 
    }
    val aggregate = new Aggregate
    val template = new Template2[Aggregate,Vars[Aggregate#Member]] with DotStatistics1[RealVariable] {
      def unroll2(v: Vars[Aggregate#Member]) = error("Not needed")
      def unroll1(v: Aggregate) = Factor(v,Vars(v.members))
      override def unroll2s(v: Aggregate#Member) = Factor(v.owner,Vars(v.owner.members))

      def statistics(v1: Aggregate, v2: Vars[Aggregate#Member]) = {
        new Stat(new RealVariable(v1.members.filter(_.value).size))
      }
    }
    val diff = new DiffList
    aggregate.members(0).set(true)(diff)
    aggregate.members(2).set(true)(diff)
    val factors = template.factors(diff).toSeq
    assert(factors.size === 1)
    assert(factors(0).variables(0) === aggregate)
    assert(factors(0).variables(1) === Vars(aggregate.members))


  }
  

}