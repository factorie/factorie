/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.chain
import cc.factorie._

object Chain {
  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val writeSequences = new CmdOption("write-sequences", "sequences", "FILE", "Filename in which to save the sequences' labels and features.")

      val readSequences = new CmdOption("read-sequences", "sequences", "FILE", "Filename from which to read the sequences' labels and features in one-line-per-token format.")
      val readTrainingSequences = new CmdOption("read-training-sequences", "sequences", "FILE", "Filename from which to read the training sequences' labels and features.")
      val readValidationSequences = new CmdOption("read-validation-sequences", "sequences", "FILE", "Filename from which to read the validation sequences' labels and features.")
      val readTestingSequences = new CmdOption("read-testing-sequences", "sequences", "FILE", "Filename from which to read the testing sequences' labels and features.")
      val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the sequences that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
      val validationPortion = new CmdOption("validation-portion", 0.0, "FRACTION", "The fraction of the sequences that should be used for validation")
      val crossValidation = new CmdOption("cross-validation", 1, "N", "The number of folds for cross-validation (DEFAULT=1)")

      val readTextFiles = new CmdOption("read-text-files", Seq("textfile"), "FILE.txt...", "Files from which to read sequences; tokens become features; directory name is the label.")
      val readTextLines = new CmdOption("read-text-lines", "textfile", "FILE.txt", "Filename from which to read the sequences' labels and features; first word is the label value")
      val readTextTokenSegmenter = new CmdOption("read-text-token-segmenter", "cc.factorie.app.nlp.segment.Tokenizer", "StringSegmenter", "Scala expression providing a subclass of StringSegmenter to turn text into tokens.")
      val readTextSkipHeader = new CmdOption("read-text-skip-header", false, "true|false", "Skip text up until double newline.")
      val readTextSkipHTML = new CmdOption("read-text-skip-html", false, "true|false", "Exclude HTML tags.")
      val readTextEncoding = new CmdOption("read-text-encoding", "UTF-8", "ENCODING", "The name of the encoding to use, e.g. UTF-8.")
      
      val writeSGML = new CmdOption("write-sgml", "output.sgml", "FILE.sgml", "File in which to write the inferred output, with SGML markup.")
      val writeOWPL = new CmdOption("write-owpl", "output.owpl", "FILE.owpl", "File in which to write the inferred output, with one-word-per-line (e.g. CoNLL-2003 format).")

      val writeVocabulary = new CmdOption("write-vocabulary", "vocabulary", "FILE", "Filename in which to save the vocabulary.")
      val readVocabulary = new CmdOption("read-vocabulary", "vocabulary", "FILE", "Filename from which to load the vocabulary.")

      val writeClassifications = new CmdOption("write-classifications", "classifications", "FILE", "Filename in which to save the classifications.")

      val writeChainModel = new CmdOption("write-chain-model", "chain-model", "FILE", "Filename in which to save the chain model.")
      val readChainModel = new CmdOption("read-chain-model", "chain-model", "FILE", "Filename from which to read the chain model.")
      val localRandomSeed = new CmdOption("random-seed", -1, "N", "The random seed for randomly selecting a proportion of the instance list for training")

      val trainer = new CmdOption("trainer", "ChainLikelihoodTrainer", "ChainTrainer", "Scala expression providing ChainTrainer class.")
      // TODO Consider enabling the system to use multiple ChainTrainers at the same time, and compare results

      val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.")
      val printInfoGain = new CmdOption("print-infogain", false, "true|false", "Print the training features with highest information gain.")
    }
    opts.parse(args)

    object FeaturesDomain extends CategoricalTensorDomain[String]
    object LabelDomain extends CategoricalDomain[String]
  }
}