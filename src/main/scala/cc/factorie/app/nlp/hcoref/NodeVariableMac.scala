package cc.factorie.app.nlp.hcoref

import scala.language.experimental.macros
import scala.reflect.macros.Context
import cc.factorie.variable.BagOfWordsVariable

/**
 * @author John Sullivan
 */
class NodeVariableMac[Vars <: NodeVariables[Vars]] {

  def method_impl(c:Context)(bag: c.Expr[BagOfWordsVariable]): c.Expr[Vars] = {
    import c.universe._
    val Literal(Constant(b:BagOfWordsVariable)) = bag.tree
  }

}
