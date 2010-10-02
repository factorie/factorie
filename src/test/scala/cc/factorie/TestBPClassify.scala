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

import junit.framework._
import cc.factorie._
import cc.factorie.la._
import cc.factorie.optimize._
import scala.math

object TestBPClassify {
  def main(args: Array[String]): Unit = {
    val t = new TestBPClassify
    t.main(args)
  }
}


class TestBPClassify extends TestCase {
  class Document(contents: String, labelStr:String) extends BinaryFeatureVectorVariable[String] {
    def this(file:File) = this(Source.fromFile(file).mkString, file.getParentFile.getName) // Could also append ".skipHeader"
    var label = new Label(labelStr, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryVectorVariable
    "[A-Za-z]+".r.findAllIn(contents).foreach(regexMatch => this += regexMatch.toString)
  }
  class Label(name: String, val document: Document) extends LabelVariable(name)

  val model = new Model (
    /**Bias term just on labels */
    new InitializedTemplate(new TemplateWithDotStatistics1[Label]),
    /**Factor between label and observed document */
    new InitializedTemplate(new TemplateWithDotStatistics2[Label, Document] {
      def unroll1(label: Label) = Factor(label, label.document)
      def unroll2(token: Document) = throw new Error("Document values shouldn't change")
    })
  )

  val objective = new Model(new InitializedTemplate(new Label01LossTemplate[Label]))
  
  def testMain: Unit = main(new Array[String](0))

  def main(args: Array[String]): Unit = {
    var documents = new ArrayBuffer[Document]

    if (args.length >= 2) {
      // Read data and create Variables
      for (directory <- args) {
        var docCount = 0
        println(directory)
        val directoryFile = new File(directory)
        if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
          documents += new Document(file)
          if (docCount < 50) { documents.last.values.take(30).foreach((w:String) => print(w+" ")); println }
          docCount += 1
        }
      }
    } else {
      // Get test data from "Data" object below
      for ((classname,documentStrings) <- data)
        for (str <- documentStrings) documents += new Document(str, classname)
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
    val lattice = inferencer.inferTreewise(testVariables.asInstanceOf[Seq[LabelVariable[String]]])

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

      var logZ = Double.NegativeInfinity
      forIndex(trueMarginal.length)(i => {
        logZ = maths.sumLogProb(logZ,trueMarginal(i))
      })
      trueSumLogZ += logZ

      maths.expNormalize(trueMarginal)

      forIndex(trueMarginal.length)(i => {
        if (math.abs(trueMarginal(i) - bpMarginal.get(i)) > 1e-6) {
          throw new RuntimeException("BP MARGINALS INCORRECT! " + trueMarginal(i) + " " + bpMarginal.get(i))
        }
      })
    })
    println("DONE CHECKING BP MARGINALS")

    val bpSumLogZ = lattice.sumLogZ
    if (math.abs(trueSumLogZ - lattice.sumLogZ) > 1e-6) {
      throw new RuntimeException("BP LOGZ INCORRECT! " + trueSumLogZ + " " + bpSumLogZ)
    }


    val predictor = new VariableSettingsGreedyMaximizer[Label](model)
    predictor.processAll(trainVariables)
    predictor.processAll(testVariables)
    println("Train accuracy = " + cc.factorie.defaultObjective.aveScore(trainVariables))
    println("Test  accuracy = " + cc.factorie.defaultObjective.aveScore(testVariables))

  }


  val data = Map(
"comp.sys.ibm.pc.hardware" ->
Seq(
"I ve acquired an old Logitech Series button mouse and m told that this is a bus Does anyone want to unload pc clone card for email replies bobmon cs",
"I think it would be a great idea to have new group created comp sys ibm pc flame therapy anybody agree",
"In C zwC LK acsu buffalo edu v kcbp ubvmsd cc MITCH writes Now does anyone know a way to back up the masters of Word Perfect for Windows and",
"In article hxg nxl rpi edu wen yingyang ral John Wen writes From Subject PC keyboard Summary location of cap lock and ctrl keys on Keywords key Date Mon Apr",
"I need a device either an ISA board or subsystem which will take two RGB video signals and combine them according to template The can be as simple rectangular window",
"Well my inch VGA x interlacing year old no brand monitor just bit the bullet I pressed power switch and a few seconds later light went out with POP Gawd",
"It is model number D a interlaced dp BTW if you have to contact the company that would really be helpful Thanks for replying I was beginning believe never going",
"In article pm pINNp geraldo cc utexas edu jbodnar ccwf John Bodnar writes According to e p unl edgar pearlstein Here s another one My computer arrived with the following",
"I was wondering if anyone out there has had the same problem am having with my Gateway DX VL Bus system ATI Graphics Ultra Pro LB When have computer in",
"I have the Mb SCSI disk drive currently used for my Mac but it can be use PC also In good condition rarely and no bad track Full high fast",
"In article bu edu nshah acs writes I have a gateway local bus system It has slots for SIMMS that either to use or MB simms My question just received",
"INABU ibm rz tu clausthal de Arnd Burghardt writes Hi folks Yesterday i visited the CEBIT hannover germany where Intel was presenting Pentium processor They had four in words machines",
"HUAYONG YANG yang titan ucs umass edu wrote Most if not all credit card companies offer to double the warranty up one year namely you make a purchase by get",
"Since I ve been seeing all kinds of complaints regarding Gateways lately on here thought post my recent pleasant experiences My machine DX V this past Friday This was two",
"Where can I get the Winmarks benchmark to run on my PC via ftp would be best Roger bix ruzun NET uzun crash cts com",
"In article jmgree starbase spd louisville edu Jude M Greer writes I was wondering if anyone out there has had the same problem am having with my Gateway DX VL",
"I have Mx ns pin SIMM for Sale It is in perfect condition will not work my system because it requires SIMMS would like to get what paid insured shipping",
"Another thing why a SCSI interface By giving the MO floppy it could reduce price of and make easily installed in existing devices pc clone you mean thats not even",
"Greg Spath GKS psuvm psu edu wrote In article C uEoM EvF odin corp sgi com mikey Mike Yang says So by going mailorder through Gateway I save Plus get",
"Andrew BW Colfelt colfelt ucsu Colorado EDU wrote Shadow mask is when you put your face into main memory Keep day job",
"Thanks to all who responded my original post I got the number for Western Digital tech support and determined that need upgrade BIOS Super It will handle hard drives with",
"Help I need to implement COM and on a board that m designing finding it dificult track down definition hardware is of have the IO adresses fact shares IRQ with",
"Bruce Maynard drac uumeme chi il us wrote In article MAL psuvm psu edu Mitch Lewars writes Can someone give me the specs on a null modem cable I ferget",
"Hello I am looking for a PC card with the following features Controller IDE AT Bus HardDiskDrive FloppyDiskDrives Standard KB VGA Graphics INCLUDING FEATURE CONNECTOR important There are some manufacturors",
"Hi All I have heard that somewhere there exist programmable keyboards eg one can program displays on the keys to show some specific characters et c Does it mean is",
"ST brownvm brown edu wrote As far as I can tell the MD is an offshoot of technology that already exists It MO drive drives exist for computers They re",
"In article C CMD zz newcastle ac uk Tor Olav Berntzen writes Another thing why a SCSI interface for MDs By giving the MO floppy it could reduce price of",
"Please help if you can with the following strange problem The Maxtor drive in my clone would from time to for no obvious reason spin down completely one tell by",
"Could someone tell me if the ATI graphic ultra pro is supported in a version of vpic now If so where it located thanks Robert email replies would be appreciated",
"Just a quick THANKS to the many who explained backing up of my masters Apparently they are NOT copy protected I just used program that is unable handle high density",
"I would like advice on how to configure my accept Com currently IRQ and LPT Bus Mouse Sound Card no idea what do can be set any using for MOuse",
"Hi I recently switched my old Amiga with a DX My computer configuration is Mhz kB EISA Micronics Motherboard ASIC MB Ram ST N Harddisk SCSI UltraStor F Cache Controller",
"AY In many recent advertisements I have seen both DX and based systems Does the first really exists does it imply that all circuitry on motherboard with works at speed",
"MG joohwee students neural iss nus sg wrote I went buying SIMMs recently and the sales person told me that are chip one cannot use them interchan If you have",
"KM Is the DX anything more than a myth I haven t heard of it from any source that trust and sure don see ads for machines in Computer Shopper",
"B BK Is it possible to plug in ns or SIMMs into a motherboard saying wants simms You shouldn t have troubles I heard of machines having problems with slower",
"Is it possible to plug an ordinary ISA card into a VESA localbus slot I am running out of slots and have one spare Doug Rabson IOC Ltd Email dfr",
"Hello Can anyone out there tell me if it is possible to put ordinary standard SIMM RAM chips ns in a COMPAQ PROLINEA or do require special Please also email",
"What ways are there to hook up an appletalk network use Apple LaserWriter Is a way I can AppleShare File Server also The less memory used the better Thanks Any",
"How is the CMOS backed up Dry cell batteries or ni cad Your may be dead mwallack kean ucs mun ca wrote A friend s computer recently failed to recognize",
"Anonymous I saw a posting about the choice between DX and was wondering although is faster because of path to it s external cache shouldn t be as that one",
"Is it possible to connect a atari monochrome monitor some kind of VGA card If someone have done this please let me know how Thanx l Max Brante m max",
"robert desonia hal k ann arbor mi us Robert Desonia writes I heard the rumor as well but story differed Intel was not coming out with tripling clock a clone",
"Hi I have got a Quantum ProDrive AT IDE harddisk and would like to format it When trying no low level just FDISK DOS FORMAT somehow messed up the parameters",
"In article pifisINNhsr dns NMSU Edu jdiers dante nmsu edu DIERS writes I own a Stealth card from diamond When using the X x mil win driver and work but",
"This may be the dumbest question of year but is there a way to piggyback or expand slot motherboard all bit get usual My case has slots for and I",
"Please reply via e mail since this is job related I have a Colorado Jumbo back up system at one of my places employment and it has eaten two tapes",
"In article bu edu lcai acs says I just received my Gateway DX mini desktop system The first thing noticed when plugged in the power cord is noise that comes",
"In Apr odin diku dk phantom Haktan Bulut writes Hi I recently switched my old Amiga with a DX My computer configuration is Mhz kB EISA Micronics Motherboard ASIC MB",
"In article Apr cbfsb cb att com rmm cbnewsg richard m maniscalco writes pm pINNp geraldo cc utexas edu jbodnar ccwf John Bodnar According to e p unl edgar pearlstein"
),
"comp.sys.mac.hardware" ->
Seq(
"In article davea xetron com David P Alverson writes I believe Apple has a patent on the region features of QuickDraw A mac clone would have to implement regions This",
"My wife has one of these I have not had much chance to fiddle with it but in comparison our Laserwriters Canon engines she complains that the print is too",
"Hi I am looking into buying a Floptical Drive and was wondering what experience people have with the drives from Iomega PLI MASS MicroSystems or Procom These seem to be",
"In article Apr physc byu edu goblec writes I just tried running my Bernoulli Box off a Centris and the driver software only seems to work when cache is If",
"Article crossposted from comp sys hp Author was Gordon Lang Posted on Apr GMT ibm pc hardware I need a device either an ISA board or subsystem which will take",
"Hey All Does anyone know if I can ftp to get the newest version of Radiusware and soft pivot from Radius bought a monitor but it has an old this",
"I have the Mb SCSI disk drive currently used for my Mac but it can be use PC also In good condition rarely and no bad track Full high fast",
"This model is one of the two low cost laser printers that Apple just introduced I m thinking getting to use at home Have any you had experience with this",
"We re all set to buy one of these for the office use scanning in color photographs and optical character recognition ve played with original grayscale OneScanner were very pleased",
"I saw once an article about a new line of Macs configured to work more optimally as file servers Anyone know any details",
"In article C vr z EB usenet ucs indiana edu kssimon silver kenneth steven simon wrote hades coos dartmouth Brian V Hughes writes To my knowledge there is no way",
"Another thing why a SCSI interface By giving the MO floppy it could reduce price of and make easily installed in existing devices pc clone you mean thats not even",
"FOR SALE Apple Macintosh SE MB RAM HD System Installed RasterOps bit video card for Monitor Targus carrying case I m after offers in the region of pounds north London",
"A couple of questions for the multimedia set Does anybody have a phone or fax number e mail address name principal in CEDAR Technologies Dublin New Hampshire All I is",
"I had asked everyone about problems installing a meg simm and an in my Centris but the folks at local Apple store called Cupertino found that you can t have",
"In article Apr cbnews cb att com jbr joseph a brownlee writes Could someone post definitive answer about the VRAM configuration for Q and presumably C as well There seems",
"On a related note will the work on Centris with internal video and give multiple resolutions This I m VERY curious about Thanks Kevin Tieskoetter Technical Support Drake Looniversity MicroFrontier",
"Does NuTek or anyone at have an email address If not why Good Things Books by Robert Heinlein Music Enya Computers Apple Humor Dave Barry Thursday nights on NBC and",
"In article Mar news media mit edu fredm Fred G Martin writes part of posting removed the Sony CPD has better video circuitry than either other two monitors It can",
"In article pgifo efb msuinfo cl msu edu gary ah cal writes As I said know a multisession CD ROM is necessary if you do more photos BUT what it",
"Approximately four months ago I purchased a Quantum LPS HD from La Cie for After two the drive started having problems First there were intermittent freezes then corrupted files and",
"Is there a Wyse Terminal Emulator or comms toolbox kit available on the net somewhere Thanks Vince",
"Spring has Sprung in Cambridge so it time for the monthly Flea at MIT to start up buyers discount with hardcopy of this notice COMPUTERS ELECTRONICS HAM RADIO FLEA all",
"In article Mar leland Stanford EDU tedebear Theodore Chen writes there isn t any copyright equivalent of the res ipsa doctrine but s something kind similar to show infringement one",
"Hi folks what exactly is the maximum memory I can put in a Quadra My manual says MB with x SIMMs but MacWarehouse and like advertise to give it total",
"I have MANY questions for all you experts out there pertaining to apple s built in video Do macs that the ability use VGA monitors If so if not which",
"Title says it all I d be particularly interested in the performance difference Just how much faster is Centris over LCIII Tom UUCP humu nctams pnet tomj ARPA nosc mil",
"Well here are the results of Mathematica test which I posted to this newsgroup The was following command Plot D x y PlotPoints just curious how fast plot would be",
"Please excuse and redirect me if this has already been answered but is there a small utility that switches the functionality of caps lock key ctrl on powerbook keyboard I",
"I have a friend who has MAC LC or II think and her family an extra LaserJet IIIp sitting around Is there any way to connect these two make them",
"In article C s Fy murdoch acc Virginia EDU jht e faraday clas Jason Harvey Titus writes I had asked everyone about problems installing a meg simm and an in",
"Could someone please tell me what a LaserWriter IINTX upgrade kit is Its small box which has bag inn it seemingly containing chips look like ROMS and manual The installation",
"Gene s stuff for sale NEW PRICES The following items are Qty Description List Price SuperMac ColorLink SX T bit NuBUS BASE This card is primo selling mailorder It suports",
"Excerpts from cmu comp sys mac Apr Re SE Serial Port Speed by Samuel John Kass andrew Sorry I got a bit technical To answer your question Mac will have",
"Just wondering if anyone had info experience with a video fpu for mac LC just thinking of adding second monitor most likely grayscale Bret Oeltjen exp Pi i University Minnesota",
"In article Apr nctams uucp tomj pnet cts com Tom Jenkins writes Title says it all I d be particularly interested in the performance difference Just how much faster is",
"In article pqprtINNf escargot xx rmit OZ AU s minyos Douglas Barry Mcpherson writes Could someone please tell me what a LaserWriter IINTX upgrade kit is Its small box which",
"ronaldw sco COM Ronald A Wong writes In article C vr z EB usenet ucs indiana edu kssimon silver kenneth steven simon wrote The program PowerStrip which is freeware has",
"Help I have an ADB graphicsd tablet which want to connect my Quadra Unfortunately the has only one port and it seems would give up mouse Please can someone help",
"Excerpts from netnews comp sys mac misc Apr Re HELP INSTALL RAM ON CEN by Jason Harvey Titus farad From jht e faraday clas Virginia EDU Subject CENTRIS Date Mon",
"You will need Driver ver to work with Quadra Centris can download it from iomega BBS Dominic Cheng d cheng descartes uwaterloo ca Computer Science University of Waterloo Ontario Canada",
"I have been playing with my Centris for almost a week now must say this machine is really fast The hardware turn on feature annoying but got PowerKey from Sophisicated",
"In article hansg risken vd volvo se Hans Granqvist writes Is it wise to even think about removing the annoying fan from my Classic I have no warranty void And",
"Could someone direct me to information on SCSI performance for each Mac Max throughput etc Kurt Tiedtke ktiedtke jarthur claremont edu Please email Thanks",
"In a previous article jcav ellis uchicago edu JohnC says This model is one of the two low cost laser printers that Apple just introduced I m thinking getting to",
"In article Apr alijku edvz uni linz ac at Norbert Mueller K writes Newsgroups comp sys mac hardware prudhom iros Serge Prud homme IRO UMontreal CA Any info on the",
"In article C BJ E imag fr you write hillman plk af mil wrote deathbird CMU EDU Donpaul Stephens kind of slated wouldn t say Who is going to throw",
"In article osu INN r tamsun tamu edu mclean math Robert Mclean wrote My MacPlus is having problems which seem temperature related After using it for a while freezes The",
"In article Afi sHS VohMrYlEe andrew cmu edu Donpaul C Stephens deathbird CMU EDU wrote What is the difference I want a double spin CD ROM drive by May looking",
"In article p e tINNojp MINERVA CIS YALE EDU bell peter yale edu Peter Bell writes My advisor has decided to get a mac for the lab now that we"
))

}






class SimpleMaxEntTrainer(model: Model) {
  type TemplatesToUpdate = DotTemplate
  var gaussianPriorVariance = 1.0

  def process[V <: DiscreteVariableWithTrueSetting](variables: Seq[V], numIterations: Int = Int.MaxValue): Unit = {
    // Data structure for holding per-template constraints and expectations
    class SuffStats extends HashMap[TemplatesToUpdate, Vector] {
      override def default(template: TemplatesToUpdate) = {
        template.freezeDomains
        val vector: Vector = template.weights match {
          case w: SparseVector => new SparseVector(w.length)
          case w: DenseVector => new DenseVector(w.length)
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
    model.factorsOf[TemplatesToUpdate](variables).foreach(f => constraints(f.template) += f.statistics.vector)

    def templates = constraints.sortedKeys

    // Currently only supports iid single DiscreteVariables
    val optimizable = new OptimizableTemplates(templates) with OptimizableByValueAndGradient {
      // Cached values
      private var oValue = Double.NaN
      private var oGradient: Array[Double] = new Array[Double](numOptimizableParameters)
      // Flush cache when parameters change
      override def setOptimizableParameters(a: Array[Double]): Unit = {oValue = Double.NaN; super.setOptimizableParameters(a)}

      override def optimizableParameter_=(index: Int, d: Double): Unit = {oValue = Double.NaN; super.optimizableParameter_=(index, d)}
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

          maths.expNormalize(distribution)

          forIndex(distribution.length)(i => {
            v.set(i)(null)
            // put negative expectations into 'expectations' StatMap
            model.factorsOf[TemplatesToUpdate](v).foreach(f => expectations(f.template) += f.statistics.vector * -distribution(i))
          })

          oValue += math.log(distribution(v.trueIntValue))
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

      def getOptimizableGradient(a: Array[Double]) = {
        if (oValue.isNaN) setOptimizableValueAndGradient
        Array.copy(oGradient, 0, a, 0, oGradient.length)
      }
    }

    val optimizer = new LimitedMemoryBFGS(optimizable)
    optimizer.optimize(numIterations)
  }
}

