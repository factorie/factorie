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

  @Test def testCards4() {
    cc.factorie.tutorial.Cards4.main(emptyArgs)
  }

  val dummyNERFile = dummyFileWithContents("train", "A NN C I-PER\nA NNS D O\nA NNP C I-LOC")

  //@Test def testChainNer1() { cc.factorie.tutorial.ChainNER1a.main(Array(dummyNERFile, dummyNERFile)) }

  @Test def testChainNER1ML() {
    cc.factorie.tutorial.ChainNER1ML.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testChainNER2() {
    cc.factorie.tutorial.ChainNER2.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testChainNER2b() {
    cc.factorie.tutorial.ChainNER2b.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testChainNER4() {
    cc.factorie.tutorial.ChainNER4.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testCoref1() {
    cc.factorie.tutorial.Coref1.main(emptyArgs)
  }

  @Test def testCoref4() {
    // Uncomment after removing mongo dependency
    // cc.factorie.example.Coref4.main(emptyArgs)
  }

  @Test def testCorefMentions() {
    cc.factorie.tutorial.CorefMentionsDemo.main(emptyArgs)
  }

  @Test def testDirichletDemo() {
    cc.factorie.tutorial.DirichletDemo.main(emptyArgs)
  }

  val dummyDir1 = dummyDirectoryWithFileWithContents("documentDir1", "I am a file\n")
  val dummyDir2 = dummyDirectoryWithFileWithContents("documentDir2", "I am a other file\n")

  @Test def testDocumentClassifier1() {
    cc.factorie.tutorial.DocumentClassifier1.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testDocumentClassifier2() {
    cc.factorie.tutorial.DocumentClassifier2.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testDocumentClassifier3() {
    cc.factorie.tutorial.DocumentClassifier3.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testDocumentClassifier4() {
    cc.factorie.tutorial.DocumentClassifier4.main(Array(dummyDir1, dummyDir2))
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

  @Test def testLDA2() {
    cc.factorie.tutorial.LDA2.main(Array(dummyDir1))
  }

  @Test def testLDA3() {
    cc.factorie.tutorial.LDA3.main(Array(dummyDir1))
  }

  val saxDir = dummyDirectoryWithFileWithContents("saxDir", "<abstract>Hello</abstract><year>2012</year>", ext=".xml")

  @Test def testLDA4() {
    // TODO: uncomment this after figuring out the specific XML format for fuse docs
    // cc.factorie.example.LDA4.main(Array(saxDir))
  }

  @Test def testLDA5() {
    cc.factorie.tutorial.LDA5.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testMultinomialDemo() {
    cc.factorie.tutorial.MultinomialDemo.main(emptyArgs)
  }

  //@Test def testPerceptronPOS() {
  //  cc.factorie.example.PerceptronPOS.main(Array("--train", posFile, "--dev", posFile, "--test", posFile))
  //}

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
    cc.factorie.tutorial.Tutorial060Learning.main(emptyArgs)
  }

  @Test def testTutorial040() {
    cc.factorie.tutorial.Tutorial090ParallelismAndHyperparameters.main(emptyArgs)
  }

  @Test def testVarArgs() {
    cc.factorie.tutorial.VarArgsDemo.main(emptyArgs)
  }

  @Test def testWordSegmenter() {
    cc.factorie.tutorial.WordSegmenterDemo.main(emptyArgs)
  }

  @Test def testWordSegmenter2() {
    cc.factorie.tutorial.WordSegmenter2.main(emptyArgs)
  }
}
