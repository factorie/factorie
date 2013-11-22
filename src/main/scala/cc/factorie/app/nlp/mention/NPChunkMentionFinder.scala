package cc.factorie.app.nlp.mention

import java.io.{File, FileOutputStream, FileInputStream}
import cc.factorie.app.nlp._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import cc.factorie.app.nlp.phrase.{ChunkerOpts, CRFChunker}
import cc.factorie.app.nlp.load.{BILOU2LayerChunkTag, BILOUChunkTag}
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}
import cc.factorie.app.nlp.coref.ForwardCorefTrainer.opts


/**
 * User: cellier
 * Date: 10/28/13
 * Time: 11:24 PM
 * Finite State machine implementation to grab NP spans
 * TODO: MentionEntityType / run ner over them
 */

class ChunkBasedMentionList extends MentionList

class NPChunkBasedMentionFinder(val chunk:CRFChunker) extends DocumentAnnotator {
  def this(filename:String) = this( {val chunk = new CRFChunker()
    chunk.deserialize(new FileInputStream(new java.io.File("CRFChunker.factorie")))
    chunk
  })
  def prereqAttrs = Seq(classOf[Token],classOf[BILOUChunkTag])
  def postAttrs = Seq(classOf[ChunkBasedMentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  def getNPChunkSpans(document: Document): Seq[(String,TokenSpan)] ={
    val spans = ArrayBuffer[(String,TokenSpan)]()
    document.sections.foreach{s => s.tokens.map{
      t =>
        if (t.attr[BILOUChunkTag].categoryValue != "O") {
          if(t.attr[BILOUChunkTag].categoryValue == "U-NP") spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, 1))
          else if(t.attr[BILOUChunkTag].categoryValue == "B-NP"){
            if(t.hasNext) {
              var lookFor = t.next
              while (lookFor.hasNext && lookFor.attr[BILOUChunkTag].categoryValue.matches("(I|L)-NP")) lookFor = lookFor.next
              spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, lookFor.positionInSection - t.positionInSection))
            } else  spans += (t.attr[BILOUChunkTag].categoryValue -> new TokenSpan(s, t.positionInSection, 1))
          }
        }  else {
          if ( t.string.length > 2 && !t.containsLowerCase && upperCase.findFirstIn(t.string).nonEmpty && (t.getNext ++ t.getPrev).exists(i => i.containsLowerCase)) {
            spans += ("ORG" -> new TokenSpan(s, t.positionInSection, 1))
          } else if (t.posTag.categoryValue == "NNP") {
            spans += ("MISC" -> new TokenSpan(s, t.positionInSection, 1))
          }
        }
      }
    }
    spans.toSeq
  }

  def getChunkMentions(document: Document): Seq[Mention] = {
    getNPChunkSpans(document).map{labelSpan =>
      val label = labelSpan._1
      val s = labelSpan._2
      val m = new Mention(s, s.length-1)
      m.attr += new MentionType(m, "NAM")
      m
    }
  }

  def process(document: Document) = {
    chunk.process(document)
    val mentions = getChunkMentions(document)
    document.attr += new MentionList() ++= mentions.sortBy(m => (m.head.stringStart, m.length))
    document
  }

  def tagBILOUtoMentions(sentences: Iterable[Sentence], mentions: ArrayBuffer[Mention]): (ListBuffer[Mention])={
    sentences.map(s=>s.tokens.map(t => t.attr += new BILOU2LayerChunkTag(t,"O:O")))
    val skippedMentions = ListBuffer[Mention]()
    //Negative length for descending sort
    mentions.sortBy(m => m.tokens.length).foreach{mention =>
      if(mention.forall(t => t.attr[BILOU2LayerChunkTag].categoryValue.split(':')(0) == "O")){
        mention.tokens.map{t =>
          if(mention.tokens.length == 1)  t.attr += new BILOU2LayerChunkTag(t, "U-NP:O")
          else if(t == mention.head) t.attr += new BILOU2LayerChunkTag(t, "B-NP:O")
          else if(t == mention.last) t.attr += new BILOU2LayerChunkTag(t,"L-NP:O")
          else t.attr += new BILOU2LayerChunkTag(t,"I-NP:O")
        }
      }
      else if(mention.forall(t => t.attr[BILOU2LayerChunkTag].categoryValue.split(':')(1) == "O")){
        mention.tokens.map{t =>
          val innerMention = t.attr[BILOU2LayerChunkTag].categoryValue.split(':')(0)
          if(mention.tokens.length == 1)  t.attr += new BILOU2LayerChunkTag(t, innerMention+":U-NP")
          else if(t == mention.head) t.attr += new BILOU2LayerChunkTag(t, innerMention+":B-NP")
          else if(t == mention.last) t.attr += new BILOU2LayerChunkTag(t,innerMention+":L-NP")
          else t.attr += new BILOU2LayerChunkTag(t,innerMention+":I-NP")
        }
      } else
        skippedMentions += mention

    }

    skippedMentions
  }
}

object NPChunkBasedMentionFinder extends NPChunkBasedMentionFinder(CRFChunker){

}


object CreateTrainingData{
  def main(args: Array[String]){
    implicit val random = new scala.util.Random(0)
    val opts = new ChunkerOpts
    opts.parse(args)
    assert(opts.trainFile.wasInvoked)
    val trainDocs = ConllCorefLoader.loadWithParse(opts.trainFile.value)
    val testDocs = ConllCorefLoader.loadWithParse(opts.testFile.value)
    val chunker = new CRFChunker()
    chunker.deserialize(new FileInputStream(new java.io.File("2011CM.factorie")))
    val mentionfinder = NPChunkBasedMentionFinder
    println("Train: " + trainDocs.length+" documents, " + trainDocs.map(d => d.attr[MentionList].length).sum.toFloat / trainDocs.length + " mentions/doc")
    val trainSentences = trainDocs.flatMap(_.sentences)
    val missedMentions = trainDocs.flatMap(d=>mentionfinder.tagBILOUtoMentions(d.sentences,d.attr[MentionList]))
    println("Mentions Tagged")
    val totalMentions= trainDocs.map(d => d.attr[MentionList].length).sum.toFloat
    println("Total Mentions: " + totalMentions)
    println("Missed " + missedMentions.length + " Mentions")
    println("Tagged Percent: " + (totalMentions-missedMentions.length)/totalMentions)

    val p1 = new java.io.PrintWriter(new File("src/main/resources/2LayerTaggedMentionsTrain.txt"))
    try {
      trainSentences.foreach{s => s.tokens.map{t =>  p1.println(t.string + " " + t.attr[PennPosTag].categoryValue + " " + t.attr[BILOU2LayerChunkTag].categoryValue)};p1.println()}
    } finally { p1.close() }
  }
}

object ChunkingExperiments{
  def main(args:Array[String]){
    opts.parse(args)
    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    map(classOf[MentionList]) = () => NPChunkBasedMentionFinder
    val options = new ForwardCoref().options

    val (testDocs,testMap) = MentionAlignment.makeLabeledData(opts.testFile.value,null,opts.portion.value,options.useEntityType, options, map.toMap)

    //val (testDocs,testMap) = MentionAlignment.makeLabeledData(opts.testFile.value,null,opts.portion.value,options.useEntityType, options, map.toMap)

    //ChunkBasedMentionFinder.initChunker("2011CM.factorie")
    //println("2011 Exact Boundaries with no Headword")
    //ForwardCorefTrainer.evaluateParameters(args:+"--alignment-width=0")

  }
}



