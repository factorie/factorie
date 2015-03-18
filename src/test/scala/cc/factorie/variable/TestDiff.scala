package cc.factorie.variable

import org.junit.Test
import org.scalatest.junit._


class TestDiff extends JUnitSuite with cc.factorie.util.FastLogging {

  @Test
  def testCreateDiff() {
    //this test just shows how variables create diff objects that point to them
    val b = new BooleanVariable(true)
    val diff = new DiffList
    b.set(false)(diff)
    assert(diff(0).variable === b)
  }

  @Test
  def testDiffUndoAndRedo() {
    val i = new IntegerVariable(2)

    val d = new DiffList
    i.set(3)(d) // This method will create a Diff object and append it to the DiffList d.
    assert(3 == i.value)

    d.undo()
    assert(2 == i.value)

    d.redo()
    assert(3 == i.value)
  }
}
