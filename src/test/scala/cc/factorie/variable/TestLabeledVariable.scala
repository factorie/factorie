package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestLabeledVariable extends JUnitSuite with cc.factorie.util.FastLogging {


  // LabeledCategoricalVariable
  @Test
  def testLabeledCategoricalVariable(): Unit = {
    object GenderDomain extends CategoricalDomain[String] {
      value("male")
      value("female")
      freeze()
    }
    val v = new LabeledCategoricalVariable[String]("male") {
      override def domain: CategoricalDomain[String] = GenderDomain
    }

    assert(v.target.value == GenderDomain.value("male"))

    v.set(GenderDomain.value("female"))(null)
    assert(v.value == GenderDomain.value("female"))
    assert(!v.valueIsTarget)

    v.set(GenderDomain.value("male"))(null)
    assert(v.value == GenderDomain.value("male"))
    assert(v.valueIsTarget)

  }

  // LabeledIntegerVariable
  @Test
  def testLabeledIntegerVariable(): Unit = {
    val v = new LabeledIntegerVariable(2)
    v.set(0)(null)

    assert(v.intValue == 0)
    assert(v.target.intValue == 2)
    assert(!v.valueIsTarget)

    v.set(2)(null)
    assert(v.intValue == 2)
    assert(v.valueIsTarget)
  }

  // LabeledBooleanVariable
  @Test
  def testLabeledBooleanVariable(): Unit = {
    val v = new LabeledBooleanVariable(true) {}
    assert(v.target.booleanValue)

    v.set(false)(null)
    assert(!v.booleanValue)
    assert(!v.valueIsTarget)

    v.set(true)(null)
    assert(v.booleanValue)
    assert(v.valueIsTarget)
  }

}
