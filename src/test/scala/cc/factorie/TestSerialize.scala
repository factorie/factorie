package cc.factorie

import app.chain.ChainModel
import cc.factorie.la._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import java.io._
import cc.factorie.app.nlp
import cc.factorie.util.{TensorCubbie, BinarySerializer}
import scala.language.postfixOps
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.ner.NerLabel
import cc.factorie.variable._
import cc.factorie.model._

class TestSerialize extends JUnitSuite  with cc.factorie.util.FastLogging{

 class MyChainNerFeatures(val token: nlp.Token, override val domain: CategoricalVectorDomain[String])
   extends BinaryFeatureVectorVariable[String] {
   override def skipNonCategories = true
 }

 class OntoNerLabel(val token: nlp.Token, ta: String, val domain: CategoricalDomain[String]) extends NerLabel(ta) {
   type ContainedVariableType = this.type
 }

  @Test def testTensorSerialization(): Unit = {
    val random = new scala.util.Random(0)
    val tensorFile = java.io.File.createTempFile("FactorieTestFile", "serialize-tensor").getAbsolutePath
    val tensor = new SparseIndexedTensor2(100, 20)
    for (i <- 0 until tensor.length) tensor(i) = random.nextDouble()
    BinarySerializer.serialize(tensor, tensorFile)
    val newTensor  = BinarySerializer.deserialize[SparseIndexedTensor2](tensorFile)
    assert(tensor.toSeq.sameElements(newTensor.toSeq))

    val tensors = Seq(new DenseTensor3(100,1,4) , new SparseIndexedTensor2(100, 20))
    for (t <- tensors; i <- 0 until t.length) t(i) = random.nextDouble()
    BinarySerializer.serialize(tensors, tensorFile)
    val newTensors = BinarySerializer.deserialize[Seq[Tensor]](tensorFile)
    assert(tensors.zip(newTensors).forall({case (t1, t2) => t1.toSeq.sameElements(t2.toSeq)}))
  }

  @Test def testOutOfOrderDomainSerialization(): Unit = {
    val random = new scala.util.Random(0)
    val file = java.io.File.createTempFile("foo", "multi")
      object MyChainNerFeaturesDomain extends CategoricalVectorDomain[String]
      MyChainNerFeaturesDomain.dimensionDomain ++= Seq("A","B","C")

      object OntoNerLabelDomain extends CategoricalDomain[String]
      OntoNerLabelDomain ++= Seq("Hello","GoodBye")

      val model = makeModel(MyChainNerFeaturesDomain, OntoNerLabelDomain)
      model.bias.weights.value := Array.fill[Double](model.bias.weights.value.length)(random.nextDouble())
      model.obs.weights.value := Array.fill[Double](model.obs.weights.value.length)(random.nextDouble())
      model.markov.weights.value := Array.fill[Double](model.markov.weights.value.length)(random.nextDouble())

      BinarySerializer.serialize(model, MyChainNerFeaturesDomain, OntoNerLabelDomain, file)

      object featDomain2 extends CategoricalVectorDomain[String]
      object labelDomain2 extends CategoricalDomain[String]
      val model2 = makeModel(featDomain2, labelDomain2)

      BinarySerializer.deserialize(model2, featDomain2, labelDomain2, file)

      assertSameWeights(model2, model)
  }


 @Test def testChainModelSerialization(): Unit = {
   val random = new scala.util.Random(0)

   val f = File.createTempFile("FactorieTestFile", "serialize-chain-model")
   val modelFileOutput = new FileOutputStream(f)

   logger.debug("creating toy model with random weights")

   object MyChainNerFeaturesDomain extends CategoricalVectorDomain[String]
   MyChainNerFeaturesDomain.dimensionDomain ++= Seq("A","B","C")

   object OntoNerLabelDomain extends CategoricalDomain[String]
   OntoNerLabelDomain ++= Seq("Hello","GoodBye")

   val model = makeModel(MyChainNerFeaturesDomain, OntoNerLabelDomain)
   model.bias.weights.value:= Array.fill[Double](model.bias.weights.value.length)(random.nextDouble())
   model.obs.weights.value:= Array.fill[Double](model.obs.weights.value.length)(random.nextDouble())
   model.markov.weights.value:= Array.fill[Double](model.markov.weights.value.length)(random.nextDouble())
   logger.debug("serializing chain model")
   model.serialize(modelFileOutput)
   modelFileOutput.flush()
   modelFileOutput.close()


   val modelFileInput = new FileInputStream(f)

   val deserialized = deserializeChainModel(modelFileInput)

   assertSameWeights(model, deserialized)

   logger.debug("successfully deserialized")
 }

 def getWeights(model: Parameters): Seq[Tensor] = model.parameters.tensors.toSeq

 def assertSameWeights(model1: Parameters, model2: Parameters): Unit = {
   val weights1 = getWeights(model1)
   val weights2 = getWeights(model2)
   assert(weights1.size == weights2.size,
     "Number of families didn't match: model1 had %d, model2 had %d" format (weights1.size, weights2.size))
   for ((w1, w2) <- weights1.zip(weights2)) {
     logger.debug("# active elements in w1: " + w1.activeDomainSize)
     logger.debug("# active elements in w2: " + w2.activeDomainSize)
     assert(w1.activeDomainSize == w2.activeDomainSize)
     for (((a1, a2), (b1, b2)) <- w1.activeElements.toSeq.zip(w2.activeElements.toSeq)) {
       assert(a1 == b1, "Index %d from w1 not equal to %d from w2" format (a1, b1))
       assert(a2 == b2, "Value %f at index %d from w1 not equal to value %f at index %d from w2" format (a2, a1, b2, b1))
     }
   }
 }

 def makeModel(featuresDomain: CategoricalVectorDomain[String],
   labelDomain: CategoricalDomain[String]): ChainModel[OntoNerLabel, MyChainNerFeatures, nlp.Token] = {
   object model extends ChainModel[OntoNerLabel, MyChainNerFeatures, nlp.Token](
     labelDomain, featuresDomain, l => l.token.attr[MyChainNerFeatures], l => l.token, t => t.attr[OntoNerLabel])
   model.useObsMarkov = false
   model
 }

 def deserializeChainModel(iStream: InputStream): ChainModel[OntoNerLabel, MyChainNerFeatures, nlp.Token] = {
   object MyChainNerFeaturesDomain extends CategoricalVectorDomain[String]
   object OntoNerLabelDomain extends CategoricalDomain[String]
   val model = makeModel(MyChainNerFeaturesDomain, OntoNerLabelDomain)
   model.deserialize(iStream)
   model
 }

 @Test def testModelSerializationWithDomains(): Unit = {
   object domain1 extends CategoricalDomain[String]
   val words = "The quick brown fox jumped over the lazy dog".split(" ")
   words.foreach(domain1.index(_))

   class Model1(d: CategoricalDomain[String]) extends Model with Parameters {
     val family1 = new DotFamilyWithStatistics1[CategoricalVariable[String]] {
       val weights = Weights(new DenseTensor1(d.length))
     }
     def families: Seq[DotFamily] = Seq(family1)
     def factors(v: Iterable[Var]) = Nil
   }
   val model = new Model1(domain1)
   model.family1.weights.value(6) = 12

   val fileName1 = java.io.File.createTempFile("foo", "domain")
   val domainFile = new File(fileName1.getAbsolutePath)
   val domainCubbie = new CategoricalDomainCubbie(domain1)
   BinarySerializer.serialize(domainCubbie, domainFile)

   val fileName2 = java.io.File.createTempFile("foo", "model")
   val modelFile = new File(fileName2.getAbsolutePath)
   val modelCubbie = new WeightsSetCubbie(model.parameters)
   BinarySerializer.serialize(modelCubbie, modelFile)

   object domain2 extends CategoricalDomain[String]
   val model2 = new Model1(domain2)

   val domainFile2 = new File(fileName1.getAbsolutePath)
   val domainCubbie2 = new CategoricalDomainCubbie(domain2)
   BinarySerializer.deserialize(domainCubbie2, domainFile2)

   val modelFile2 = new File(fileName2.getAbsolutePath)
   val modelCubbie2 = new WeightsSetCubbie(model2.parameters)
   BinarySerializer.deserialize(modelCubbie2, modelFile2)

   assertSameWeights(model, model2)
 }

 @Test def testMultipleSerialization(): Unit = {
   val random = new scala.util.Random(0)
   val file = java.io.File.createTempFile("foo", "multi")
   object MyChainNerFeaturesDomain extends CategoricalVectorDomain[String]
   MyChainNerFeaturesDomain.dimensionDomain ++= Seq("A","B","C")

   object OntoNerLabelDomain extends CategoricalDomain[String]
   OntoNerLabelDomain ++= Seq("Hello","GoodBye")

   val model = makeModel(MyChainNerFeaturesDomain, OntoNerLabelDomain)
   model.bias.weights.value := Array.fill[Double](model.bias.weights.value.length)(random.nextDouble())
   model.obs.weights.value := Array.fill[Double](model.obs.weights.value.length)(random.nextDouble())
   model.markov.weights.value := Array.fill[Double](model.markov.weights.value.length)(random.nextDouble())

   BinarySerializer.serialize(MyChainNerFeaturesDomain, OntoNerLabelDomain, model, file)

   object featDomain2 extends CategoricalVectorDomain[String]
   object labelDomain2 extends CategoricalDomain[String]
   val model2 = makeModel(featDomain2, labelDomain2)

   BinarySerializer.deserialize(featDomain2, labelDomain2, model2,  file)

   assertSameWeights(model2, model)
 }

 // NOTE: this is a hack to get around broken Manifest <:< for singleton types
 // this is fixed in 2.10 so once we upgrade we can remove this hack (that assumes all params are covariant!)
 def checkCompat(m1: Manifest[_], m2: Manifest[_]): Boolean =
   m2.runtimeClass.isAssignableFrom(m1.runtimeClass) && m1.typeArguments.zip(m2.typeArguments).forall({case (l, r) => checkCompat(l, r)})

 @Test def testClassifierPosSerialization() {
   val model = new app.nlp.pos.POS1
   val fileName = java.io.File.createTempFile("FactorieTestFile", "classifier-pos").getAbsolutePath
   model.serialize(fileName)
   val otherModel = new app.nlp.pos.POS1(new File(fileName))
 }

 @Test def testInstanceSerialize(): Unit = {
   implicit val random = new scala.util.Random(0)
   import app.classify._
   val fileName = java.io.File.createTempFile("FactorieTestFile", "serialize-instances").getAbsolutePath
   val ll = new ArrayBuffer[app.classify.Label]()
   val labelDomain = new CategoricalDomain[String] { }
   val featuresDomain = new CategoricalVectorDomain[String] { }
   for (i <- 1 to 100) {
     val labelName = (i % 2).toString
     val features = new BinaryFeatures(labelName, i.toString, featuresDomain, labelDomain)
     (1 to 100).shuffle.take(50).map(_.toString).foreach(features +=)
     ll += new app.classify.Label(labelName, features, labelDomain)
   }
   val llFile = new File(fileName)
   val llCubbie = new LabelListCubbie(featuresDomain, labelDomain, true)
   llCubbie.store(ll)
   BinarySerializer.serialize(llCubbie, llFile)

   val newllCubbie = new LabelListCubbie(featuresDomain, labelDomain, true)
   BinarySerializer.deserialize(newllCubbie, llFile)
   val newll = newllCubbie.fetch()

   assert(newll.zip(ll).forall({
     case (newl, oldl) =>
       newl.labelName == oldl.labelName &&
       newl.features.value.activeElements.sameElements(oldl.features.value.activeElements)
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
   logger.debug("TokenDomain.dimensionDomain.size=" + TokenDomain.dimensionDomain.size)

   val model = new SegmenterModel
   model.bias.weights.value += new UniformTensor1(model.bias.weights.value.dim1, 1.0)
   model.obs.weights.value += new la.UniformTensor2(model.obs.weights.value.dim1, model.obs.weights.value.dim2, 1.0)

   val modelFile = new File(fileName)

   BinarySerializer.serialize(new WeightsSetCubbie(model.parameters), modelFile)

   val deserializedModel = new SegmenterModel
   BinarySerializer.deserialize(new WeightsSetCubbie(deserializedModel.parameters), modelFile)

   val domainFile = new File(fileName2)

   BinarySerializer.serialize(new CategoricalVectorDomainCubbie(TokenDomain), domainFile)

   logger.debug("Original model family weightsSet: ")
   getWeights(model).foreach(s => logger.debug(s.toString))
   logger.debug("Deserialized model family weightsSet: ")
   getWeights(deserializedModel).foreach(s => logger.debug(s.toString))

   assertSameWeights(model, deserializedModel)

   logger.debug("Original domain:")
   logger.debug(TokenDomain.dimensionDomain.toSeq.mkString(","))
   logger.debug("Deserialized domain:")
   val newDomain = new CategoricalVectorDomain[String] { }
   val cubbie = new CategoricalVectorDomainCubbie(newDomain)
   BinarySerializer.deserialize(cubbie, domainFile)
   logger.debug(newDomain.dimensionDomain.toSeq.mkString(","))

   assert(TokenDomain.dimensionDomain.toSeq.map(_.category).sameElements(newDomain.dimensionDomain.toSeq.map(_.category)))
 }

 class Label(b: Boolean, val token: Token) extends LabeledBooleanVariable(b)
 object TokenDomain extends CategoricalVectorDomain[String]
 class Token(val char: Char, isWordStart: Boolean) extends BinaryFeatureVectorVariable[String] with ChainLink[Token, Sentence] {
   def domain = TokenDomain
   val label = new Label(isWordStart, this)
   this += char.toString
   if ("aeiou".contains(char)) this += "VOWEL"
 }
 class Sentence extends Chain[Sentence, Token]

 class SegmenterModel extends Model with Parameters {
   val bias = new DotFamilyWithStatistics1[Label] {
     factorName = "Label"
     val weights = Weights(new la.DenseTensor1(BooleanDomain.size))
   }
   val obs = new DotFamilyWithStatistics2[Label, Token] {
     factorName = "Label,Token"
     val weights = Weights(new la.DenseTensor2(BooleanDomain.size, TokenDomain.dimensionSize))
   }
   def factors(label: Iterable[Var]): Iterable[Factor] = {
     Seq.empty[Factor]
   }
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
