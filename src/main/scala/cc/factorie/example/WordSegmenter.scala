package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.Sorting
import factorie.util.Implicits._

object WordSegmenterDemo { 

	// The variable types:
	class Label(b:Boolean, val token:Token) extends EnumVariable(b) 
	class Token(val char:Char, isWordStart:Boolean) extends VectorVariable[String] with VarInSeq[Token] {
		this += char.toString
		if ("aeiou".contains(char)) this += "VOWEL"
		val label = new Label(isWordStart, this)
	}
	//Domain[Token] = new IndexedDomain[Token] { override def allocSize = 100000 }
	//object Token extends IndexedDomain[Token] { override def allocSize = 100000 }

	// The factor templates that define the model
	val model = new Model
	/** Bias term just on labels */
	model += new TemplateWithExpStatistics1[Label] with PerceptronLearning
	/** Factor between label and observed token */
	model += new TemplateWithExpStatistics2[Label,Token] with SparsePerceptronLearning {
		def unroll1 (label:Label) = Factor(label, label.token)
		def unroll2 (token:Token) = throw new Error("Token values shouldn't change")
	}
	/** A token bi-gram conjunction  */
	model += new TemplateWithExpStatistics3[Label,Token,Token] with SparsePerceptronLearning {
		def unroll1 (label:Label) = if (label.token.hasPrev) Factor(label, label.token, label.token.prev) else Nil
		def unroll2 (token:Token) = throw new Error("Token values shouldn't change")
		def unroll3 (token:Token) = throw new Error("Token values shouldn't change")
	}
	/** Factor between two successive labels */
	model += new TemplateWithExpStatistics2[Label,Label] with PerceptronLearning {
		def unroll1 (label:Label) = if (label.token.hasNext) Factor(label, label.token.next.label) else Nil
		def unroll2 (label:Label) = if (label.token.hasPrev) Factor(label.token.prev.label, label) else Nil
	}
	/** Skip edge */
	val skipTempalte = new Template2[Label,Label] with ExpStatistics1[Bool] with PerceptronLearning {
		def unroll1 (label:Label) =  
			// could cache this search in label.similarSeq for speed
		  for (other <- label.token.seq; if label.token.char == other.char) yield 
		  	if (label.token.position < other.position) Factor(label, other.label) else Factor(other.label,label)
		def unroll2 (label:Label) = Nil // We handle symmetric case above
		def statistics(label1:Label, label2:Label) = Stat(Bool(label1==label2))
	}.init

	val objective = new Model(new TrueEnumTemplate[Label])



	def main(args: Array[String]) : Unit = {
	  implicit val random = new scala.util.Random 
		var startTime:Long = 0
  
		// Read data and create Variables
	  class Sentence extends VariableSeq[Token]
    val sentences = for (string <- data) yield {
      val sentence = new Sentence
      var beginword = true
      for (c <- string.toLowerCase) {
      	if (c >= 'a' && c <= 'z') {
      		sentence += new Token(c, beginword)
      		beginword = false
      	} else
      		beginword = true
      }
      for (token <- sentence) {
        if (token.hasPrev) token += (token.prev.char+"@-1") else token += "START@-1"
				if (token.hasNext) token += (token.next.char+"@1") else token += "END@+1"
      }
      sentence
    }

		// Make a test/train split
		val (testSet, trainSet) = sentences.shuffle(random).split(0.5) //RichSeq.split(RichSeq.shuffle(instances), 0.5)
		var trainVariables = trainSet.flatMap(_.map(_.label))
		var testVariables = testSet.flatMap(_.map(_.label))

		testVariables.foreach(_.setRandomly)
		Console.println ("Initial test accuracy = "+ objective.aveScore(testVariables))

		// Sample and Learn!
		var sampler = new GibbsPerceptronLearner(model, objective)
		sampler.learningRate = 1.0
		for (i <- 0 until 10) {
			sampler.sampleAndLearn (trainVariables, 2)
			sampler.learningRate *= 0.8
			sampler.sample (testVariables, 2)
			Console.println ("Train accuracy = "+ objective.aveScore(trainVariables))
			Console.println ("Test  accuracy = "+ objective.aveScore(testVariables))
			if (startTime == 0) startTime = System.currentTimeMillis // do the timing only after HotSpot has warmed up
		}

		// Show the parameters
		//model.templatesOf[LogLinearScoring].foreach(t => Console.println(t.weights.toList))
		println("Finished in "+(System.currentTimeMillis-startTime)+" milliseconds.")
		0;
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
		"The GNU Project was launched in 1984 to develop a complete Unix-like operating system which is free software: the GNU system."
	)


}
