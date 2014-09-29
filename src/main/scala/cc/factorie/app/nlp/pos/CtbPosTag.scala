package cc.factorie.app.nlp.pos

import cc.factorie.app.nlp._
import cc.factorie.variable._

/**
 * Created by oskar on 9/23/14.
 */
object CtbPosDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "VA",
    "VC",
    "VE",
    "VV",
    "NR",
    "NT",
    "NN",
    "LC",
    "PN",
    "DT",
    "CD",
    "OD",
    "M",
    "X",
    "AD",
    "P",
    "CC",
    "CS",
    "DEC",
    "DEG",
    "DER",
    "DEV",
    "SP",
    "AS",
    "ETC",
    "SP",
    "MSP",
    "IJ",
    "ON",
    "PU",
    "JJ",
    "FW",
    "LB",
    "SB",
    "BA",
    "URL"
  )
  freeze()

  def isNoun(pos:String): Boolean = pos(0) == 'N'
  def isProperNoun(pos:String) = { pos == "NR" }
  def isVerb(pos:String) = pos(0) == 'V'
  def isAdjective(pos:String) = pos(0) == 'J'
  def isPersonalPronoun(pos: String) = pos == "PRP"
}

class CtbPosTag(token: Token, initialIndex: Int) extends PosTag(token, initialIndex) {
  def this(token: Token, initialCategory: String) = {
    this(token, CtbPosDomain.index(initialCategory.split('-')(0)))
  }
  final def domain = CtbPosDomain
  def isNoun = domain.isNoun(categoryValue)
  def isProperNoun = domain.isProperNoun(categoryValue)
  def isVerb = domain.isVerb(categoryValue)
  def isAdjective = domain.isAdjective(categoryValue)
  def isPersonalPronoun = domain.isPersonalPronoun(categoryValue)
}

class LabeledCtbPosTag(token: Token, targetValue: String) extends CtbPosTag(token, targetValue) with CategoricalLabeling[String]
