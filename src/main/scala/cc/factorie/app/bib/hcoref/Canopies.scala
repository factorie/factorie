package cc.factorie.app.bib.hcoref

import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.OptionUtils
import edu.umass.cs.iesl.scalacommons.StringUtils
import scala.collection.mutable

/**
 * @author John Sullivan
 */
class IndexedCounter[A] extends Iterable[(A, (Int, Int))] {

  private var index = 0
  private val indexer = mutable.HashMap[A, (Int, Int)]()

  private def default:(Int, Int) = {
    val tIdx = index
    index += 1
    tIdx -> 0
  }

  def increment(a:A) {
    val (idx, cnt) = indexer.getOrElse(a, default)
    indexer(a) = idx -> (cnt + 1)
  }

  def get(a:A):Option[(Int, Int)] = indexer.get(a)

  def getIndex(a:A):Option[Int] = get(a).map(_._1)
  def getCount(a:A):Option[Int] = get(a).map(_._2)

  def iterator = indexer.toIterator
}

object Canopies {
  private val canopyIndexer = new IndexedCounter[String] //mutable.HashMap[String, Int]().withDefaultValue(0)

  def fromString(rawNameString:String):String = StringUtils.toOptionNonempty(rawNameString.trim.replace(", , ",", ")) match {
    case Some(nonEmptyNameString) =>
      try{
        val personName = PersonNameWithDerivations(nonEmptyNameString).inferFully
        val res = OptionUtils.merge[NonemptyString](personName.firstInitial, personName.longestSurName, {(initial, surname) => NonemptyString(initial.s.substring(0, 1).toLowerCase + surname.s)}).map(_.s).getOrElse("")
        canopyIndexer increment res
        res
      } catch {
        case e:Exception =>
          println("Failed on %s" format rawNameString)
          //e.printStackTrace()
          ""
      }
    case None => ""
  }

  val getIndex = canopyIndexer.getIndex _
}
