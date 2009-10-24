package cc.factorie
import scala.reflect.Manifest

abstract class ContrastiveDivergence[C](model:Model, val objective:Model)(implicit mc:Manifest[C]) extends MHSampler[C](model)(mc) {
  throw new Error("Not yet implemented")
}
