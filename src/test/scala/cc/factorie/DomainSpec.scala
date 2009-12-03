package cc.factorie


import org.specs.runner.{JUnit4, JUnit}
import org.specs.Specification

/**
 * @author Sebastian Riedel
 */
class DomainTest extends JUnit4(DomainSpec)
object DomainSpec extends Specification with JUnit {

  "The Domain helper object" should {
    "throw an exception when returning domains if and only if asked to return domains of classes not annotated" +
            " with @DomainInSubclasses" in {
      @DomainInSubclasses
      trait TraitWithoutDomain extends Variable
      @DomainInSubclasses
      class ClassWithoutDomain extends TraitWithoutDomain
      class ClassWithDomain extends ClassWithoutDomain

      Domain[TraitWithoutDomain] must throwA[Error]
      Domain[ClassWithoutDomain] must throwA[Error]
      Domain[ClassWithDomain] must not(throwA[Error])


    }
    "return the same domain for subclasses of variables (unless @DomainInSubclass is used)" in {
      @DomainInSubclasses
      trait TraitWithoutDomain extends Variable
      @DomainInSubclasses
      class ClassWithoutDomain extends TraitWithoutDomain
      class ClassWithDomain extends ClassWithoutDomain
      class SubclassWithSameDomain extends ClassWithDomain
      
      Domain[ClassWithDomain] must_== Domain[SubclassWithSameDomain]

    }
  }

}