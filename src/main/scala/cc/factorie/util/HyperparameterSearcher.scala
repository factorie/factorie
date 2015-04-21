/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.util

import scala.util.Random
import scala.concurrent._
import java.text.SimpleDateFormat
import java.io.{FileOutputStream, OutputStreamWriter}
import cc.factorie.variable.Proportions

/**
 * User: apassos
 * Date: 6/9/13
 * Time: 7:20 AM
 */

trait ParameterSampler[T] {
  def sample(rng: scala.util.Random): T
  def buckets: Array[(T,Double,Double,Double,Int)]
  def valueToBucket(v: T): Int
  def accumulate(value: T, d: Double) {
    val v = math.max(0, math.min(valueToBucket(value), buckets.length-1))
    val (_, sum, sumSq, max, count) = buckets(v)
    buckets(v) = (value, sum+d, sumSq+d*d, math.max(max, d), count+1)
  }
}

// Samples uniformly one of the values in the sequence.
class SampleFromSeq[T](seq: Seq[T]) extends ParameterSampler[T] {
  val buckets = seq.map(s => (s,0.0,0.0,0.0,0)).toArray
  def valueToBucket(v: T) = buckets.toSeq.map(_._1).indexOf(v)
  def sample(rng: Random) = seq(rng.nextInt(seq.length))
}

// Samples non-uniformly one of the values in the sequence.
class SampleFromProportions[T](seq: Seq[T], prop: Proportions) extends ParameterSampler[T] {
  val buckets = seq.map(s => (s,0.0,0.0,0.0,0)).toArray
  def valueToBucket(v: T) = (0 until buckets.length).filter(i => buckets(i)._1 == v).head
  def sample(rng: Random) = seq(prop.sampleIndex(rng))
}

// Samples uniformly a Double in the range
class UniformDoubleSampler(lower: Double, upper: Double, numBuckets: Int = 10) extends ParameterSampler[Double] {
  val dif = upper - lower
  val buckets = (0 to numBuckets).map(i => (0.0, 0.0, 0.0, 0.0,0)).toArray
  def valueToBucket(d: Double) = (numBuckets*(d - lower)/dif).toInt
  def sample(rng: Random) = rng.nextDouble()*dif + lower
}

// Samples Doubles in the range such that their logarithm is uniform.
// Useful for learning rates, variances, alphas, and other things which
// vary in order of magnitude.
class LogUniformDoubleSampler(lower: Double, upper: Double, numBuckets: Int = 10) extends ParameterSampler[Double] {
  val inner = new UniformDoubleSampler(math.log(lower), math.log(upper), numBuckets)
  def valueToBucket(v: Double) = inner.valueToBucket(math.log(v))
  val buckets = (0 to numBuckets).map(i => (0.0, 0.0, 0.0, 0.0, 0)).toArray
  def sample(rng: Random) = math.exp(inner.sample(rng))
}

/**
 * A container for a hyperparameter which will be optimized by random sampling.
 * @param option The CmdOption for the parameter
 * @param sampler A sampler which can return values for the parameter
 */
case class HyperParameter[T](option: CmdOption[T], sampler: ParameterSampler[T]) {
  def set(rng: Random) { option.setValue(sampler.sample(rng)) }
  def accumulate(objective: Double) { sampler.accumulate(option.value, objective) }
  def report() {
    println("Parameter " + option.name + "      mean   stddev  count")
    for ((value, sum, sumSq, max, count) <- sampler.buckets) {
      val mean = sum/count
      val stdDev = math.sqrt(sumSq/count - mean*mean)
      value match {
        case v: Double => println(f"${v.toDouble}%2.15f  $mean%2.4f  $stdDev%2.4f max $max ($count)")
        case _ => println(f"${value.toString}%20s $mean%2.2f $stdDev%2.2f max $max ($count)")
      }
    }
    println()
  }
}

/**
 * A container for a hyperparameter for which a job will be run for every value.
 * @param option The CmdOption for the parameter
 * @param params Range of settings to use for the parameter
 */
case class DistributorParameter[T](option: CmdOption[T], params: Seq[T]) {
  var i = 0
  val numParams = params.length
  def set: Unit = {
    if(i > numParams) throw new Error("Trying to set param more times than there are values")
    else {
      option.setValue(params(i))
      i += 1
    }
  }
  def reset: Unit = {i = 0}
  def numSettings: Int = params.length
}

/**
 *
 * @param cmds The options to be passed to the slaves
 * @param parameters The hyperparameters we want to tune, assumed to be a subset of cmds
 * @param executor A function which will train the model and return its quality
 * @param numTrials How many executors should start
 * @param numToFinish How many executors we wait for (to account for stragglers)
 * @param secondsToSleep How many seconds do we sleep for between polling the executors.
 */
class HyperParameterSearcher(cmds: CmdOptions,
                             parameters: Seq[HyperParameter[_]],
                             executor: Array[String] => Future[Double],
                             numTrials: Int,
                             numToFinish: Int,
                             secondsToSleep: Int = 60) {
  private def sampledParameters(rng: Random): Array[String] = {
    parameters.foreach(_.set(rng))
    cmds.values.flatMap(_.unParse).toArray
  }

  // the contract is that optimize will also set the appropriate values in cmds
  def optimize(rng: Random = new Random(0)): Array[String] = {
    val settings = (0 until numTrials).map(i => sampledParameters(rng))
    println("Starting hyperparameter optimization")
    val futures = settings.map(s => (s,executor(s)))
    var finished = false
    while (!finished) {
      Thread.sleep(secondsToSleep*1000)
      val values = futures.filter(_._2.isCompleted).map(_._2.value.get.getOrElse(Double.NegativeInfinity))
      val finiteValues = values.filterNot(_.isInfinite)
      finished = values.length >= numToFinish
      if (values.length > 0)
        println(s"Finished jobs: ${values.length} failed jobs: ${values.count(_.isInfinite)}  best value: ${values.max} mean value: ${finiteValues.sum/finiteValues.length}")
      else
        println(s"Finished jobs: ${values.length} failed jobs: ${values.count(_.isInfinite)}")
    }
    for ((setting,future) <- futures; if future.isCompleted; if future.value.get.isSuccess; if !future.value.get.get.isInfinite) {
      cmds.parse(setting)
      for (p <- parameters) p.accumulate(future.value.get.get)
    }
    for (p <- parameters) p.report()
    val top10 = futures.filter(_._2.isCompleted).map(i => (i._1,i._2.value.get.get)).sortBy(i => i._2).reverse.take(10).reverse
    println("Top configurations: ")
    for ((setting,value) <- top10) {
      cmds.parse(setting)
      println(f"$value%2.4f  configuration: ${parameters.map(p => p.option.name +":"+ p.option.value).mkString(" ")}")
    }
    val bestConfig = futures.filter(_._2.isCompleted).maxBy(_._2.value.get.get)._1
    cmds.parse(bestConfig)
    parameters.flatMap(_.option.unParse).toArray
  }
}

/**
 * Run a job using the given executor on each set of the given parameters, where a set is defined as parameters
 * with the same index from each list (each list of parameters should be the same length).
 *
 * @param cmds The options to be passed to the slaves
 * @param parameters The parameters we want to vary between jobs, assumed to be a subset of cmds
 * @param executor The main function that we want to run
 * @param secondsToSleep How many seconds do we sleep for between polling the executors.
 */
// TODO HyperParameterSearcher should probably inherit from this or something like it
class JobDistributor(cmds: CmdOptions,
                     parameters: Seq[DistributorParameter[_]],
                     executor: Array[String] => Future[Double],
                     secondsToSleep: Int = 60) {
  // the contract is that distribute will also set the appropriate values in cmds
  def distribute: Seq[Double] = {
    val numParams = parameters.head.numSettings
    assert(parameters.map(_.numSettings).filterNot(_ == numParams).isEmpty, "All parameter lists must be of the same length")

    val settings = (0 until numParams).map(i => {parameters.foreach(_.set); cmds.values.flatMap(_.unParse).toArray})

    println("Starting job distributor")
    val futures = settings.map(s => (s,executor(s)))
    var finished = false
    var numSuccessfullyFinished = 0
    var values = Seq[Double]()
    while (!finished) {
      Thread.sleep(secondsToSleep*1000)
      values = futures.filter(_._2.isCompleted).map(_._2.value.get.getOrElse(Double.NegativeInfinity))
      numSuccessfullyFinished = values.count(!_.isInfinite)
      finished = values.length >= numParams
      println(s"Finished jobs: ${values.length} failed jobs: ${values.count(_.isInfinite)} remaining jobs: ${numParams-values.length}")
    }
    // return number of jobs successfully completed
    values
  }
}

// I created this because the return type of a reflection invocation is Object
// and I don't want to assume things about how the scala doubles are boxed.
case class BoxedDouble(d: Double)

/**
 * Main class which implements training of a model.
 * Can't assume anything about the environment.
 * Users should implement evaluateParameters.
 */
trait HyperparameterMain {
  def evaluateParameters(args: Array[String]): Double
  final def main(args: Array[String]) { evaluateParameters(args) }
  final def actualMain(args: Array[String]) = BoxedDouble(evaluateParameters(args))
}

/**
 * Base class for executors which run their own JVMs.
 */
trait Executor {
  def serializeArgs(args: Array[String]) = args.mkString("::")
  val classpath =
    ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile).mkString(":")
  def execute(args: Array[String]): Future[Double]
}

/**
 * A general executor for job queues.
 * @param memory How many gigabytes of RAM to use.
 * @param className The class which will be run.
 */
abstract class JobQueueExecutor(memory: Int, className: String, cores: Int = 1) extends Executor {
  /**
   * Runs a job in the queue
   * @param script the file name of the shell script to be run
   * @param logFile the file on which to write the output
   */
  def runJob(script: String, logFile: String)
  val date = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new java.util.Date())
  val prefix = s"hyper-search-$date"
  new java.io.File(prefix).mkdirs()
  println(s"QSubExecutor saving logs in $prefix.")
  var id = 0
  def execute(args: Array[String]) = {
    id += 1
    val thisId = id
    val as = serializeArgs(args)
    import scala.concurrent.ExecutionContext.Implicits.global
    Future {
      import sys.process._
      val thisPrefix = s"$prefix/job-$thisId"
      val outFile = thisPrefix+"-out"
      val jvmCommand = s"java -Xmx${memory}g -classpath '$classpath' cc.factorie.util.QSubExecutor --className=$className  '--classArgs=$as' --outFile=$outFile"
      val cmdFile = thisPrefix+"-cmd.sh"
      val s = new OutputStreamWriter(new FileOutputStream(cmdFile))
      s.write(jvmCommand + "\n")
      s.close()
      Thread.sleep(1000)
      blocking { try { runJob(cmdFile, thisPrefix+"-log.txt") } catch { case c: RuntimeException => () } }
      var done = false
      var tries = 0
      while (!done && tries < 10) {
        tries += 1
        if (new java.io.File(outFile).exists() && io.Source.fromFile(outFile).getLines().toSeq.size > 0) done = true
        else blocking { Thread.sleep(1000) }
      }
      if (new java.io.File(outFile).exists && io.Source.fromFile(outFile).getLines().toSeq.size > 0)
        io.Source.fromFile(outFile).getLines().toSeq.head.toDouble
      else {
        println("Job " + thisId + " failed. See log file " + thisPrefix+"-log.txt for more information.")
        Double.NegativeInfinity
      }
    }
  }
}

/**
 * An executor that uses qsub as it is set up in UMass.
 * @param memory How many gigabytes of RAM to use.
 * @param className The class which will be run.
 */
class QSubExecutor(memory: Int, className: String, cores: Int = 1) extends JobQueueExecutor(memory, className, cores) {
  import sys.process._
  def runJob(script: String, logFile: String) { s"qsub -pe blake $cores -sync y -l mem_token=${memory}G -cwd -j y -o $logFile -S /bin/sh $script".!! }
}

/**
 * I wish I could make this object private. It is the main function
 * actually running in the slaves started by QSubActorExecutor.
 */
object QSubExecutor {
  object opts extends CmdOptions {
    val className = new CmdOption("className", "", "STRING", "Class to run")
    val classArgs = new CmdOption("classArgs", "", "STRING", "Arguments to pass it")
    val outFile = new CmdOption("outFile", "", "STRING", "File on which to write the final double")
  }
  def main(args: Array[String]) {
    opts.parse(args)
    val cls = Class.forName(opts.className.value)
    val mainMethod = cls.getMethods.filter(_.getName == "actualMain").head
    val argsArray = opts.classArgs.value.split("::").toArray
    println("Using args \n" + argsArray.mkString("\n"))
    val result = mainMethod.invoke(null, argsArray).asInstanceOf[BoxedDouble].d
    println("---- END OF JOB -----")
    println("Result was: " + result)
    //import sys.process._
    //(("echo " + result.toString) #> new java.io.File(opts.outFile.value)).!
    val resFile = new OutputStreamWriter(new FileOutputStream(opts.outFile.value))
    resFile.write(result.toString + "\n")
    resFile.write("END OF RESULTS\n")
    resFile.close()
    println(s"Done, file ${opts.outFile.value} written")
  }
}
