package cc.factorie.app.nlp

import cc.factorie.util.CmdOptions

/**
 * Created with IntelliJ IDEA.
 * User: belanger
 * Date: 10/4/13
 * Time: 12:41 PM
 * To change this template use File | Settings | File Templates.
 */
trait SharedNLPCmdOptions extends CmdOptions  {
  val targetAccuracy = new CmdOption("target-accuracy", "", "FLOAT", "target accuracy for this NLP model. It will throw an exception if you don't hit this")
}
