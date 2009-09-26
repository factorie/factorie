package cc.factorie


import org.specs.runner.{JUnit4, JUnit}
import org.specs.Specification

/**
 * @author Sebastian Riedel
 */
class DomainTest extends JUnit4(DomainSpec)
object DomainSpec extends Specification with JUnit {

  "The Domain helper object" should {
    "provide the information whether a class or trait has its domain defined in subclasses or not" in {
      @DomainInSubclasses
      trait TraitWithoutDomain extends Variable
      @DomainInSubclasses
      class ClassWithoutDomain extends TraitWithoutDomain
      class ClassWithDomain extends ClassWithoutDomain

      Domain.domainInSubclassesByAnnotation(classOf[TraitWithoutDomain]) must_== true
      Domain.domainInSubclassesByAnnotation(classOf[ClassWithoutDomain]) must_== true
      Domain.domainInSubclassesByAnnotation(classOf[ClassWithDomain]) must_== false
    }
  }

}