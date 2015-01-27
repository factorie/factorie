package cc.factorie.app.bib.hcoref

import cc.factorie.util.namejuggler._

/**
 * @author John Sullivan
 */
object Canopies {

  def fromString(rawNameString:String):String = StringUtils.toOptionNonempty(rawNameString.trim.replace(", , ",", ")) match {
    case Some(nonEmptyNameString) =>
      try{
        val personName = PersonNameWithDerivations(nonEmptyNameString).inferFully
        val res = OptionUtils.merge[NonemptyString](personName.firstInitial, personName.longestSurName, {(initial, surname) => NonemptyString(initial.s.substring(0, 1).toLowerCase + surname.s)}).map(_.s).getOrElse("")
        res
      } catch {
        case e:Exception =>
          println("Failed on %s" format rawNameString)
          //e.printStackTrace()
          ""
      }
    case None => ""
  }

  def streamCanopies(ncs:Iterator[AuthorNodeCubbie]):Iterator[(String, String)] = ncs.map { nc =>
    fromString(nc.fullName.value) -> nc.id.toString
  }

}

