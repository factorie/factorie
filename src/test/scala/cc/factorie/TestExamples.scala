package cc.factorie
import org.junit.Test
import java.io._

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 10/10/12
 * Time: 3:29 PM
 * To change this template use File | Settings | File Templates.
 */

class TestExamples {

  val emptyArgs = Array[String]()

  // Returns the name of a new temporary file with the specified
  def dummyFileWithContents(prefix: String, content: String): String = {
    val name = java.io.File.createTempFile("FactorieTestFile", prefix).getAbsolutePath
    val writer = new BufferedWriter(new FileWriter(name))
    writer.write(content)
    writer.close()
    name
  }

  def dummyDirectoryWithFileWithContents(prefix: String, content: String, ext: String = ".txt"): String = {
    val dir = java.io.File.createTempFile("FactorieTestFile", prefix)
    new File(dir.getAbsolutePath + "2").mkdirs()
    val n1 = dir.getAbsolutePath + "2" + java.io.File.separator + "f1" + ext
    val writer = new BufferedWriter(new FileWriter(n1))
    writer.write(content)
    writer.close()
    dir.getAbsolutePath+"2"
  }

  val dummyNERFile = dummyFileWithContents("train", "A NN C I-PER\nA NNS D O\nA NNP C I-LOC")

  @Test def testChainNER1ML() {
    cc.factorie.tutorial.ChainNERExample.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testCoref1() {
    cc.factorie.tutorial.Coref1.main(emptyArgs)
  }

  @Test def testCorefMentions() {
    cc.factorie.tutorial.CorefMentions.main(emptyArgs)
  }

  @Test def testDirichletDemo() {
    cc.factorie.tutorial.DirichletDemo.main(emptyArgs)
  }

  val dummyDir1 = dummyDirectoryWithFileWithContents("documentDir1", "I am a file\n")
  val dummyDir2 = dummyDirectoryWithFileWithContents("documentDir2", "I am a other file\n")

  @Test def testDocumentClassifier1() {
    cc.factorie.tutorial.DocumentClassifier1.main(Array(dummyDir1, dummyDir2))
  }

  val posFile = dummyFileWithContents("POS", "\nNN WORD=Hello\nNN WORD=World\n")

  @Test def testForwardBackwardPOS() {
    cc.factorie.tutorial.ForwardBackwardPOS.main(Array("--train", posFile, "--dev", posFile, "--test", posFile))
  }

  @Test def testGaussianDemo() {
    cc.factorie.tutorial.GaussianDemo.main(emptyArgs)
  }

  @Test def testGaussianMixtureDemo() {
    cc.factorie.tutorial.GaussianMixtureDemo.main(emptyArgs)
  }

  @Test def testMultivariateGaussianDemo() {
    cc.factorie.tutorial.MultivariateGaussianDemo.main(emptyArgs)
  }

  @Test def testMultivariateGaussianMixtureDemo() {
    cc.factorie.tutorial.MultivariateGaussianMixtureDemo.main(emptyArgs)
  }

  @Test def testGrid() {
    cc.factorie.tutorial.Grid.main(emptyArgs)
  }

  @Test def testSimpleLDA() {
    cc.factorie.tutorial.SimpleLDA.main(Array(dummyDir1))
  }

  @Test def testEfficientLDA() {
    cc.factorie.tutorial.EfficientLDA.main(Array(dummyDir1))
  }

  @Test def testTopicsOverTime() {
    cc.factorie.tutorial.TopicsOverTime.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testMultinomialDemo() {
    cc.factorie.tutorial.MultinomialDemo.main(emptyArgs)
  }

  @Test def testTutorial10() {
    cc.factorie.tutorial.TutorialVariables.main(emptyArgs)
  }

  @Test def testTutorial11() {
    cc.factorie.tutorial.TutorialDomain.main(emptyArgs)
  }

  @Test def testTutorial20() {
    cc.factorie.tutorial.TutorialFactors.main(emptyArgs)
  }

  @Test def testTutorial21() {
    cc.factorie.tutorial.TutorialFamily.main(emptyArgs)
  }

  @Test def testTutorial30() {
    cc.factorie.tutorial.TutorialModel.main(emptyArgs)
  }

  @Test def testTutorial40() {
    cc.factorie.tutorial.Tutorial60Learning.main(emptyArgs)
  }

  @Test def testTutorial040() {
    cc.factorie.tutorial.Tutorial090ParallelismAndHyperparameters.main(emptyArgs)
  }

  @Test def testWordSegmenter() {
    cc.factorie.tutorial.WordSegmenter.main(emptyArgs)
  }
}
