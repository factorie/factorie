/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.pos

import java.io._
import java.util.{HashMap, HashSet}

import cc.factorie.app.chain.Observations._
import cc.factorie.app.nlp.{Document, Sentence, Token}
import cc.factorie.util.{BinarySerializer, ClasspathURL}

/**
 * Created by Oskar Singer on 10/6/14.
 */

class CtbChainPosTagger extends ChainPosTagger((t:Token) => new CtbPosTag(t, 0)) {

  private var prefixMap = new HashMap[Char, HashSet[String]]
  private var suffixMap = new HashMap[Char, HashSet[String]]

  def this(url: java.net.URL) = {
    this()
    deserialize(url.openConnection().getInputStream)
  }

  override def train(trainSentences:Seq[Sentence],
                     testSentences:Seq[Sentence],
                     lrate:Double = 0.1,
                     decay:Double = 0.01,
                     cutoff:Int = 2,
                     doBootstrap:Boolean = true,
                     useHingeLoss:Boolean = false,
                     numIterations: Int = 5,
                     l1Factor:Double = 0.000001,
                     l2Factor:Double = 0.000001)(implicit random: scala.util.Random): Unit = {
    initPrefixAndSuffixMaps(trainSentences.flatMap(_.tokens))
    super.train(trainSentences, testSentences, lrate, decay, cutoff, doBootstrap, useHingeLoss, numIterations, l1Factor, l2Factor)
  }

  def initPOSFeatures(sentence: Sentence): Unit = {
    import cc.factorie.app.chineseStrings._

    for (token <- sentence.tokens) {
      if(token.attr[PosFeatures] ne null)
        token.attr.remove[PosFeatures]

      val features = token.attr += new PosFeatures(token)
      val rawWord = token.string
      val prefix = rawWord(0)
      val suffix = rawWord(rawWord.size - 1)

      features += "W="+rawWord

      (0 to 4).foreach {
        i =>
          features += "SUFFIX" + i + "=" + rawWord.takeRight(i)
          features += "PREFIX" + i + "=" + rawWord.take(i)
      }

      if(prefixMap.containsKey(prefix)) {
        val prefixLabelSet = prefixMap.get(prefix)
        val prefixCTBMorph = posDomain.categories.map{
          category =>

            val hasCategory = {
              if(prefixLabelSet.contains(category))
                "TRUE"
              else
                "FALSE"
            }

            "PRE_" + category + "_" + hasCategory
        }

        features ++= prefixCTBMorph
      }

      if(suffixMap.containsKey(suffix)) {
        val suffixLabelSet = suffixMap.get(suffix)
        val suffixCTBMorph = posDomain.categories.map{
          category =>

            val hasCategory = {
              if(suffixLabelSet.contains(category))
                "TRUE"
              else
                "FALSE"
            }

            "SUF_" + category + "_" + hasCategory
        }

        features ++= suffixCTBMorph
      }

      if (hasPunctuation(rawWord)) features += "PUNCTUATION"
      /*
            if (hasNumeric(rawWord)) features += "NUMERIC"
            if (hasChineseNumeric(rawWord)) features += "CHINESE_NUMERIC"
            if (hasAlpha(rawWord)) features += "ALPHA"
      */
    }

    addNeighboringFeatureConjunctions(sentence.tokens,
      (t: Token) => t.attr[PosFeatures],
      "W=[^@]*$",
      List(-2),
      List(-1),
      List(1),
      List(-2,-1),
      List(-1,0))
  }

  def initPrefixAndSuffixMaps(tokens: Seq[Token]): Unit = {
    prefixMap.clear()
    suffixMap.clear()

    tokens.map(
      token => (token.string, token.attr[LabeledCtbPosTag].categoryValue)
    ).foreach{
      case (word, label) =>

        val prefix = word(0)
        val suffix = word(word.size - 1)

        val prefixLabelSet = prefixMap.get(prefix)

        if(prefixLabelSet != null) {
          if(!prefixLabelSet.contains(label)) {
            prefixLabelSet.add(label)
          }
        } else {
          val labelSet = new HashSet[String]

          labelSet.add(label)
          prefixMap.put(prefix, labelSet)
        }

        val suffixLabelSet = suffixMap.get(suffix)

        if(suffixLabelSet != null) {
          if(!suffixLabelSet.contains(label)) {
            suffixLabelSet.add(label)
          }
        } else {
          val labelSet = new HashSet[String]

          labelSet.add(label)
          suffixMap.put(suffix, labelSet)
        }
    }

    println("PREFIX MAP SIZE: " + prefixMap.size())
    println("SUFFIX MAP SIZE: " + suffixMap.size())
  }

  override def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(new BufferedOutputStream(stream))
    val out = new ObjectOutputStream(dstream)
    out.writeObject(prefixMap)
    out.writeObject(suffixMap)
    BinarySerializer.serialize(PosFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.serialize(model, dstream)
    dstream.close()
    out.close()
  }
  override def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(new BufferedInputStream(stream))
    val in = new ObjectInputStream(dstream)
    prefixMap = in.readObject().asInstanceOf[HashMap[Char, HashSet[String]]]
    suffixMap = in.readObject().asInstanceOf[HashMap[Char, HashSet[String]]]
    BinarySerializer.deserialize(PosFeaturesDomain.dimensionDomain, dstream)
    BinarySerializer.deserialize(model, dstream)
    dstream.close()
    in.close()
  }
}
object CtbChainPosTagger extends CtbChainPosTagger(ClasspathURL[CtbChainPosTagger](".factorie"))

object CtbChainPosTrainer extends ChainPosTrainer[CtbPosTag, CtbChainPosTagger](
  () => new CtbChainPosTagger(),
  (dirName: String) => {
    val directory = new File(dirName)

    val documents =
      (for{
        file <- directory.listFiles
        if file.isFile
        document = new Document
        line <- scala.io.Source.fromFile(file, "utf-8").getLines
        if line.size > 0 && line(0) != '<'
        sentence = new Sentence(document)
        (word, label) <- line.split(' ').map( pair => {val (word, label) = pair.splitAt(pair.lastIndexOf('_')); (word, label.slice(1,label.size))} )
        token = new Token(sentence, word)
        labeledTag = token.attr += new LabeledCtbPosTag(token, label)
      } yield document
        ).toIndexedSeq.distinct

    documents
  }
)