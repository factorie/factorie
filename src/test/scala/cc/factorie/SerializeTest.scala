package cc.factorie

import la.{DenseTensor1, UniformTensor1}
import org.scalatest.junit.JUnitSuite
import cc.factorie.app.bib.parser.Dom.Name
import org.junit.Test
import java.io._
import collection.mutable.ArrayBuffer

class SerializeTests extends JUnitSuite {

  @Test def testModelSerializationWithDomains(): Unit = {
    object domain1 extends CategoricalDomain[String]
    val words = "The quick brown fox jumped over the lazy dog".split(" ")
    words.foreach(domain1.index(_))

    class Model1(d: CategoricalDomain[String]) extends Model {
      object family1 extends DotFamilyWithStatistics1[CategoricalVariable[String]] {
        lazy val weights = new DenseTensor1(d.length)
      }
      override def families = Seq(family1)
      def factors(v: Variable) = Nil
    }
    val model = new Model1(domain1)
    model.family1.weights(6) = 12

    val fileName1 = java.io.File.createTempFile("foo", "domain")
    val domainFile = new File(fileName1.getAbsolutePath)
    val domainCubbie = new CategoricalDomainCubbie(domain1)
    BinaryCubbieFileSerializer.serialize(domainCubbie, domainFile)

    val fileName2 = java.io.File.createTempFile("foo", "model")
    val modelFile = new File(fileName2.getAbsolutePath)
    val modelCubbie = new ModelCubbie(model)
    BinaryCubbieFileSerializer.serialize(modelCubbie, modelFile)

    object domain2 extends CategoricalDomain[String]
    val model2 = new Model1(domain2)

    val domainFile2 = new File(fileName1.getAbsolutePath)
    val domainCubbie2 = new CategoricalDomainCubbie(domain2)
    BinaryCubbieFileSerializer.deserialize(domainCubbie2, domainFile2)

    val modelFile2 = new File(fileName2.getAbsolutePath)
    val modelCubbie2 = new ModelCubbie(model2)
    BinaryCubbieFileSerializer.deserialize(modelCubbie2, modelFile2)
  }

  @Test def testInstanceSerialize(): Unit = {
    import app.classify._
    val fileName = java.io.File.createTempFile("FactorieTestFile", "serialize-instances").getAbsolutePath
    val ll = new LabelList[app.classify.Label, Features](_.features)
    val labelDomain = new CategoricalDomain[String] { }
    val featuresDomain = new CategoricalTensorDomain[String] { }
    for (i <- 1 to 100) {
      val labelName = (i % 2).toString
      val features = new BinaryFeatures(labelName, i.toString, featuresDomain, labelDomain)
      (1 to 100).shuffle.take(50).map(_.toString).foreach(features +=)
      ll += new app.classify.Label(labelName, features, labelDomain)
    }
    val llFile = new File(fileName)
    val llCubbie = new LabelListCubbie(featuresDomain, labelDomain, true)
    llCubbie.store(ll)
    BinaryCubbieFileSerializer.serialize(llCubbie, llFile)

    val newllCubbie = new LabelListCubbie(featuresDomain, labelDomain, true)
    BinaryCubbieFileSerializer.deserialize(newllCubbie, llFile)
    val newll = newllCubbie.fetch()

    assert(newll.zip(ll).forall({
      case (newl, oldl) =>
        newl.labelName == oldl.labelName &&
        newl.features.tensor.activeElements.sameElements(oldl.features.tensor.activeElements)
    }))
  }

  @Test def test(): Unit = {
    val fileName = java.io.File.createTempFile("FactorieTestFile", "serialize-model").getAbsolutePath
    val fileName2 = java.io.File.createTempFile("FactorieTestFile", "serialize-domain").getAbsolutePath
    // Read data and create Variables
    val sentences = for (string <- data.toList) yield {
      val sentence = new Sentence
      var beginword = true
      for (c <- string.toLowerCase) {
        if (c >= 'a' && c <= 'z') {
          sentence += new Token(c, beginword)
          beginword = false
        } else
          beginword = true
      }
      for (token <- sentence.links) {
        if (token.hasPrev) token += (token.prev.char + "@-1") else token += "START@-1"
        if (token.hasNext) token += (token.next.char + "@1") else token += "END@+1"
      }
      sentence
    }
    println("TokenDomain.dimensionDomain.size=" + TokenDomain.dimensionDomain.size)

    val model = new SegmenterModel
    model.bias.weights += new UniformTensor1(model.bias.weights.dim1, 1.0)
    model.obs.weights += new la.UniformTensor2(model.obs.weights.dim1, model.obs.weights.dim2, 1.0)

    val modelFile = new File(fileName)

    BinaryCubbieFileSerializer.serialize(new ModelCubbie(model), modelFile)

    val deserializedModel = new SegmenterModel
    BinaryCubbieFileSerializer.deserialize(new ModelCubbie(deserializedModel), modelFile)

    val domainFile = new File(fileName2)

    BinaryCubbieFileSerializer.serialize(new CategoricalDomainCubbie(TokenDomain.dimensionDomain), domainFile)

    println("Original model family weights: ")
    model.families.foreach({case f: DotFamily => println(f.weights)})
    println("Deserialized model family weights: ")
    deserializedModel.families.foreach({case f: DotFamily => println(f.weights)})

    assert(
      deserializedModel.families.map({case f: DotFamily => f.weights})
        .zip(model.families.map({case f: DotFamily => f.weights}))
        .forall({case (a, b) => a.activeElements.toSeq.sameElements(b.activeElements.toSeq)}))

    println("Original domain:")
    println(TokenDomain.dimensionDomain.toSeq.mkString(","))
    println("Deserialized domain:")
    val newDomain = new CategoricalTensorDomain[String] { }
    val cubbie = new CategoricalDomainCubbie(newDomain.dimensionDomain)
    BinaryCubbieFileSerializer.deserialize(cubbie, domainFile)
    println(newDomain.dimensionDomain.toSeq.mkString(","))

    assert(TokenDomain.dimensionDomain.toSeq.map(_.category).sameElements(newDomain.dimensionDomain.toSeq.map(_.category)))
  }

  class Label(b: Boolean, val token: Token) extends LabeledBooleanVariable(b)
  object TokenDomain extends CategoricalTensorDomain[String]
  class Token(val char: Char, isWordStart: Boolean) extends BinaryFeatureVectorVariable[String] with ChainLink[Token, Sentence] {
    def domain = TokenDomain
    val label = new Label(isWordStart, this)
    this += char.toString
    if ("aeiou".contains(char)) this += "VOWEL"
  }
  class Sentence extends Chain[Sentence, Token]

  class SegmenterModel extends ModelWithContext[Seq[Label]] {
    object bias extends DotFamilyWithStatistics1[Label] {
      factorName = "Label"
      lazy val weights = new la.DenseTensor1(BooleanDomain.size)
    }
    object obs extends DotFamilyWithStatistics2[Label, Token] {
      factorName = "Label,Token"
      lazy val weights = new la.DenseTensor2(BooleanDomain.size, TokenDomain.dimensionSize)
    }
    override def families: Seq[Family] = Seq(bias, obs)
    def factorsWithContext(label: Seq[Label]): Iterable[Factor] = {
      Seq.empty[Factor]
    }
    def factors(v:Variable) = throw new Error("Not yet implemented.")
  }

  val data = Array(
    "Free software is a matter of the users' freedom to run, copy, distribute, study, change and improve the software. More precisely, it refers to four kinds of freedom, for the users of the software.",
    "The freedom to run the program, for any purpose.",
    "The freedom to study how the program works, and adapt it to your needs.",
    "The freedom to redistribute copies so you can help your neighbor.",
    "The freedom to improve the program, and release your improvements to the public, so that the whole community benefits.",
    "A program is free software if users have all of these freedoms. Thus, you should be free to redistribute copies, either with or without modifications, either gratis or charging a fee for distribution, to anyone anywhere. Being free to do these things means (among other things) that you do not have to ask or pay for permission.",
    "You should also have the freedom to make modifications and use them privately in your own work or play, without even mentioning that they exist. If you do publish your changes, you should not be required to notify anyone in particular, or in any particular way.",
    "In order for the freedoms to make changes, and to publish improved versions, to be meaningful, you must have access to the source code of the program. Therefore, accessibility of source code is a necessary condition for free software.",
    "Finally, note that criteria such as those stated in this free software definition require careful thought for their interpretation. To decide whether a specific software license qualifies as a free software license, we judge it based on these criteria to determine whether it fits their spirit as well as the precise words. If a license includes unconscionable restrictions, we reject it, even if we did not anticipate the issue in these criteria. Sometimes a license requirement raises an issue that calls for extensive thought, including discussions with a lawyer, before we can decide if the requirement is acceptable. When we reach a conclusion about a new issue, we often update these criteria to make it easier to see why certain licenses do or don't qualify.",
    "In order for these freedoms to be real, they must be irrevocable as long as you do nothing wrong; if the developer of the software has the power to revoke the license, without your doing anything to give cause, the software is not free.",
    "However, certain kinds of rules about the manner of distributing free software are acceptable, when they don't conflict with the central freedoms. For example, copyleft (very simply stated) is the rule that when redistributing the program, you cannot add restrictions to deny other people the central freedoms. This rule does not conflict with the central freedoms; rather it protects them.",
    "Thus, you may have paid money to get copies of free software, or you may have obtained copies at no charge. But regardless of how you got your copies, you always have the freedom to copy and change the software, even to sell copies.",
    "Rules about how to package a modified version are acceptable, if they don't effectively block your freedom to release modified versions. Rules that ``if you make the program available in this way, you must make it available in that way also'' can be acceptable too, on the same condition. (Note that such a rule still leaves you the choice of whether to publish the program or not.) It is also acceptable for the license to require that, if you have distributed a modified version and a previous developer asks for a copy of it, you must send one.",
    "Sometimes government export control regulations and trade sanctions can constrain your freedom to distribute copies of programs internationally. Software developers do not have the power to eliminate or override these restrictions, but what they can and must do is refuse to impose them as conditions of use of the program. In this way, the restrictions will not affect activities and people outside the jurisdictions of these governments.",
    "Finally, note that criteria such as those stated in this free software definition require careful thought for their interpretation. To decide whether a specific software license qualifies as a free software license, we judge it based on these criteria to determine whether it fits their spirit as well as the precise words. If a license includes unconscionable restrictions, we reject it, even if we did not anticipate the issue in these criteria. Sometimes a license requirement raises an issue that calls for extensive thought, including discussions with a lawyer, before we can decide if the requirement is acceptable. When we reach a conclusion about a new issue, we often update these criteria to make it easier to see why certain licenses do or don't qualify.",
    "The GNU Project was launched in 1984 to develop a complete Unix-like operating system which is free software: the GNU system.")

}
