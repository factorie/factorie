package cc.factorie

import org.scalatest.junit._
import org.junit.Test
import junit.framework.TestCase

/**
 * @author sriedel
 */
class TemplateTestSuite extends JUnitSuite  {

  //implicit def template2initialized1[S1<:DiscretesValue](t:VectorStatistics1[S1])(implicit m:Manifest[S1]): InitializedTemplate = new InitializedTemplate(t.init)
  //implicit def template2initialized(t:Template): InitializedTemplate = new InitializedTemplate(t)
  
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
    val template = new DotTemplateWithStatistics1[BooleanVariable] { lazy val weights = new la.DenseTensor1(BooleanDomain.size) }
    val b = new BooleanVariable(true)
    val diff = new DiffList
    b.set(false)(diff)
    val factors = template.factors(diff)
    assert(factors.head.family === template)
    assert(factors.head.variables.head == b)
    // TODO Uncomment this next line
    //assert(factors.head.statistics.asInstanceOf[TemplateWithDotStatistics1[BooleanVariable]#Stat]._1 == false)
  }

  @Test
  def testCascadeUnroll {
    object Aggregate extends BooleanVariable {
      val b1 = new BooleanVariable {
        override def unrollCascade: scala.Iterable[Variable] = Seq(Aggregate)
      }
    }
    val diff = new DiffList
    val template = new DotTemplateWithStatistics1[BooleanVariable] { lazy val weights = new la.DenseTensor1(BooleanDomain.size) }
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
    val template = new DotTemplate2[Aggregate,Vars[Aggregate#Member]] {
      lazy val weights = new la.DenseTensor1(1)
      def unroll2(v: Vars[Aggregate#Member]) = sys.error("Not needed")
      def unroll1(v: Aggregate) = Factor(v,Vars(v.members))
      override def unroll2s(v: Aggregate#Member) = Factor(v.owner,Vars(v.owner.members))
      def statistics(v1:Aggregate#Value, v2:Vars[Aggregate#Member]#Value) = 
        new RealVariable(v2.filter(_.booleanValue).size).value // TODO Just create a RealValue; don't bother with a RealVariable
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


/**
 * @author brian martin
 * @date 2/9/2012
 */
//class SettingIteratorTests extends TestCase {
//  val v1 = new BooleanVariable(true)
//  val v2 = new BooleanVariable(true)
//  val v3 = new BooleanVariable(true)
//
//  //TODO: test fixed assignments
//
//  def testLimitedSettingsIterator1 {
//    val template = new TemplateWithDotStatistics1[BooleanVariable] { def statisticsDomains = Tuple1(BooleanDomain) }
//    val factor = template.unroll1(v1).head
//    assert(factor.valuesIterator(Set(factor._1.asInstanceOf[Variable])).size == 2)
//    println("Template1 valuesIterator:")
//    factor.valuesIterator(Set(factor._1.asInstanceOf[Variable])).foreach(println(_))
//    println("--------------------------------")
//
//    template.addLimitedDiscreteValues(Seq(BooleanDomain.head.intValue))
//    template.isLimitingValuesIterator = true
//    assert(factor.valuesIterator(Set(factor._1.asInstanceOf[Variable])).size == 1)
//    println("Template1 limitedValuesIterator:")
//    factor.valuesIterator(Set(factor._1.asInstanceOf[Variable])).foreach(println(_))
//    println("--------------------------------")
//  }
//
//  def testLimitedSettingsIterator2 {
//    val template = new TemplateWithDotStatistics2[BooleanVariable, BooleanVariable] {
//      def statisticsDomains = ((BooleanDomain, BooleanDomain))
//      def unroll1(v: BooleanVariable) = Factor(v1, v2)
//      def unroll2(v: BooleanVariable) = sys.error("Not needed")
//    }
//
//    val factor = template.unroll1(v1).head
//    assert(factor.valuesIterator(factor.variables.toSet).size == 4)
//    println("Template2 valuesIterator:")
//    factor.valuesIterator(factor.variables.toSet).foreach(println(_))
//    println("--------------------------------")
//
//    template.addLimitedDiscreteValues(Seq((0,0),(1,1)))
//    template.isLimitingValuesIterator = true
//
//    assert(factor.valuesIterator(factor.variables.toSet).size == 2)
//    println("Template2 limitedValuesIterator:")
//    factor.valuesIterator(factor.variables.toSet).foreach(println(_))
//    println("--------------------------------")
//  }
//
//  def testLimitedSettingsIterator3 {
//    val template = new TemplateWithDotStatistics3[BooleanVariable, BooleanVariable, BooleanVariable] {
//      def statisticsDomains = ((BooleanDomain, BooleanDomain, BooleanDomain))
//      def unroll1(v: BooleanVariable) = Factor(v1, v2, v3)
//      def unroll2(v: BooleanVariable) = sys.error("Not needed")
//      def unroll3(v: BooleanVariable) = sys.error("Not needed")
//    }
//
//    var factor = template.unroll1(v1).head
//    println("Template3 valuesIterator:")
//    factor.valuesIterator(factor.variables.toSet).foreach(println(_))
//    assert(factor.valuesIterator(factor.variables.toSet).size == 8)
//    println("--------------------------------")
//
//    template.addLimitedDiscreteValues(Seq((0,0,0),(1,1,1)))
//    template.isLimitingValuesIterator = true
//
//    println("limiting factor? : " + factor.isLimitingValuesIterator)
//    println("Template3 limitedValuesIterator:")
//    factor.valuesIterator(factor.variables.toSet).foreach(println(_))
//    assert(factor.valuesIterator(factor.variables.toSet).size == 2)
//    println("--------------------------------")
//  }
//
//}
