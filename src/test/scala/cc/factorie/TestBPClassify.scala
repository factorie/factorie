package cc.factorie

/**
 * Created by IntelliJ IDEA.
 * User: gdruck
 * Date: Sep 2, 2010
 * Time: 12:11:28 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.mutable.{ArrayBuffer}
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.io.Source

import java.io.File

import cc.factorie._
import cc.factorie.la._
import cc.factorie.optimize._
import scala.math


object TestBPClassify {
  class Document(file: File) extends BinaryVectorVariable[String] {
    var label = new Label(file.getParentFile.getName, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryVectorVariable
    "\\w+".r.findAllIn(Source.fromFile(file).mkString).foreach(regexMatch => this += regexMatch.toString)
  }
  class Label(name: String, val document: Document) extends LabelVariable(name)

  val model = new Model(

    /**Bias term just on labels */
    new InitializedTemplate(new TemplateWithDotStatistics1[Label]),

    /**Factor between label and observed document */
    new InitializedTemplate(new TemplateWithDotStatistics2[Label, Document] {
      def unroll1(label: Label) = Factor(label, label.document)

      def unroll2(token: Document) = throw new Error("Document values shouldn't change")
    }
    ))

  val objective = new Model(new InitializedTemplate(new LabelTemplate[Label]))

  def main(args: Array[String]): Unit = {
    if (args.length < 2)
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var documents = new ArrayBuffer[Document];
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        documents += new Document(file)
      }
    }

    // Make a test/train split
    val (testSet, trainSet) = documents.shuffle.split(0.5)
    //val trainSet = documents
    //val testSet = documents
    var trainVariables = trainSet.map(_ label)
    var testVariables = testSet.map(_ label)
    (trainVariables ++ testVariables).foreach(_.setRandomly())

    // Train and test
    val trainer = new SimpleMaxEntTrainer(model)
    trainer.process(trainVariables)

    val inferencer = new BPInferencer[LabelVariable[String]](model)
    val lattice: BPLattice = inferencer.inferTreewise(testVariables.asInstanceOf[Seq[LabelVariable[String]]])

    var trueSumLogZ = 0.0

    println("CHECKING BP MARGINALS")
    testVariables.foreach(v => {
      val bpMarginal = lattice.marginal(v)

      val trueMarginal = new Array[Double](v.domainSize) // TODO Are we concerned about all this garbage collection?
      forIndex(trueMarginal.length)(i => {
        v.set(i)(null)
        // compute score of variable with value 'i'
        trueMarginal(i) = model.score(v)
      })

      var logZ = Math.NEG_INF_DOUBLE
      forIndex(trueMarginal.length)(i => {
        logZ = Maths.sumLogProb(logZ,trueMarginal(i))
      })
      trueSumLogZ += logZ

      Maths.expNormalize(trueMarginal)

      forIndex(trueMarginal.length)(i => {
        if (Math.abs(trueMarginal(i) - bpMarginal(i)) > 1e-6) {
          throw new RuntimeException("BP MARGINALS INCORRECT! " + trueMarginal(i) + " " + bpMarginal(i))
        }
      })
    })
    println("DONE CHECKING BP MARGINALS")

    val bpSumLogZ = lattice.sumLogZ
    if (Math.abs(trueSumLogZ - lattice.sumLogZ) > 1e-6) {
      throw new RuntimeException("BP LOGZ INCORRECT! " + trueSumLogZ + " " + bpSumLogZ)
    }


    val predictor = new VariableSettingsMaximizer[Label](model)
    predictor.process(trainVariables)
    predictor.process(testVariables)
    println("Train accuracy = " + cc.factorie.defaultObjective.aveScore(trainVariables))
    println("Test  accuracy = " + cc.factorie.defaultObjective.aveScore(testVariables))

  }

}


class SimpleMaxEntTrainer(model: Model) {
  type TemplatesToUpdate = DotTemplate
  var gaussianPriorVariance = 1.0

  def process[V <: DiscreteVariableWithTrueSetting](variables: Seq[V], numIterations: Int = Math.MAX_INT): Unit = {
    // Data structure for holding per-template constraints and expectations
    class SuffStats extends HashMap[TemplatesToUpdate, Vector] {
      override def default(template: TemplatesToUpdate) = {
        template.freezeDomains
        val vector: Vector = template.weights match {
          case w: SparseVector => new SparseVector(w.domainSize)
          case w: DenseVector => new DenseVector(w.domainSize)
        }
        this(template) = vector
        vector
      }
      // To help make sure sort order of vectors matches
      def sortedKeys = keys.toSeq.sortWith(_.hashCode > _.hashCode)
    }
    val constraints = new SuffStats
    // Add all model dot templates to constraints
    model.templatesOf[TemplatesToUpdate].foreach(t => constraints(t) = constraints.default(t))
    // Gather constraints
    variables.foreach(_.setToTruth(null))
    model.factorsOf[TemplatesToUpdate](variables).foreach(f => constraints(f.template) += f.statistic.vector)

    def templates = constraints.sortedKeys

    // Currently only supports iid single DiscreteVariables
    val optimizable = new OptimizableTemplates(templates) with OptimizableByValueAndGradient {
      // Cached values
      private var oValue = Math.NaN_DOUBLE
      private var oGradient: Array[Double] = new Array[Double](numOptimizableParameters)
      // Flush cache when parameters change
      override def setOptimizableParameters(a: Array[Double]): Unit = {oValue = Math.NaN_DOUBLE; super.setOptimizableParameters(a)}

      override def optimizableParameter_=(index: Int, d: Double): Unit = {oValue = Math.NaN_DOUBLE; super.optimizableParameter_=(index, d)}
      // Calculation of value and gradient
      def setOptimizableValueAndGradient: Unit = {
        val expectations = new SuffStats
        oValue = 0.0
        java.util.Arrays.fill(oGradient, 0.0)
        variables.foreach(v => {
          val distribution = new Array[Double](v.domainSize) // TODO Are we concerned about all this garbage collection?
          forIndex(distribution.length)(i => {
            v.set(i)(null)
            // compute score of variable with value 'i'
            distribution(i) = model.score(v)
          })

          Maths.expNormalize(distribution)

          forIndex(distribution.length)(i => {
            v.set(i)(null)
            // put negative expectations into 'expectations' StatMap
            model.factorsOf[TemplatesToUpdate](v).foreach(f => expectations(f.template) += f.statistic.vector * -distribution(i))
          })

          oValue += Math.log(distribution(v.trueIntValue))
        })
        val invVariance = -1.0 / gaussianPriorVariance
        model.templatesOf[TemplatesToUpdate].foreach {
          t =>
            oValue += 0.5 * t.weights.dot(t.weights) * invVariance
            // sum positive constraints into (previously negated) expectations
            expectations(t) += constraints(t)
            // subtract weights due to regularization
            expectations(t) += t.weights * invVariance
        }
        // constraints.keys.foreach(t => expectations(t) += constraints(t))
        oGradient = (new ArrayFromVectors(expectations.sortedKeys.map(expectations(_)))).getVectorsInArray(oGradient)
      }

      def optimizableValue: Double = {
        if (oValue.isNaN) setOptimizableValueAndGradient
        oValue
      }

      def getOptimizableGradient(a: Array[Double] = null): Array[Double] = {
        if (oValue.isNaN) setOptimizableValueAndGradient
        if (a == null) {
          var b = new Array[Double](numOptimizableParameters);
          Array.copy(oGradient, 0, b, 0, oGradient.length);
          b
        }
        else {Array.copy(oGradient, 0, a, 0, oGradient.length); a}
      }
    }

    val optimizer = new LimitedMemoryBFGS(optimizable)
    optimizer.optimize(numIterations)
  }
}