package cc.factorie


import org.specs.runner.{JUnit, JUnit4}
import org.specs.Specification

/**
 * @author Sebastian Riedel
 */
class CoordinatedLabelTest extends JUnit4(CoordinatedLabelSpec)
object CoordinatedLabelSpec extends Specification with JUnit {

  "A CoordinatedLabel" should {
    "change its value when the set method is called" in {
      implicit val diffList = new DiffList
      val label = new CoordinatedLabel("ORG") {}
      label.set("MISC")
      label.value must_== "MISC"
    }

    "create a diff object that undoes a set action when the set method is called" in {
      implicit val diffList = new DiffList
      val label = new CoordinatedLabel("ORG") {}
      label.set("MISC")
      diffList(0).undo
      label.value must_== "ORG"
    }

  }

}