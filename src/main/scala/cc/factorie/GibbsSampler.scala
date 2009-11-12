package cc.factorie
import scala.reflect.Manifest 
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue}
import cc.factorie.util.Implicits._

// How to think about Proposals and MCMC:
// Variables know their own range of values.  This needs to be coded on a per-variable basis
// Scored preferences about different values are known only by using the model.
// Sometimes we want to sample more than one variable together.  One variable cannot know how to do this on its own.
// Sometimes we want to sample conditioned on other fixed values.  One variable cannot know about this either.  It must be something like a Template
// Sometimes we want sampling to chain: sample v1, then v2 conditioned on the value of v1, etc.
// Making proposals is sometimes keyed by a single variable, a list of variables, or nothing (proposer itself maintains context of what to change next)
// Perhaps proposers should be in a list of Template-like objects; given a variable, first Template in the list to claim it gets to make the change.
// To facilitate ease of use, variable classes could provide something like:
//   class Label[T] { def defaultSampler = LabelSampler; def sample(model:Model) = defaultSampler.sample(this,model) }
//   object LabelSampler extends Sampler1[Label]
    

/** GibbsSampler for a subclass of Variable with IterableSettings */
class GibbsSampler1[V1<:Variable with IterableSettings](model:Model, objective:Model) extends SamplerOverSettings[V1](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
	def settings(v:V1) : SettingIterator = v.settings
}

/** GibbsSampler for generic "Variable with IterableSettings" */
class GibbsSampler(model:Model, objective:Model) extends GibbsSampler1[Variable with IterableSettings](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
}

// TODO Not yet tested
/** GibbsSampling over a block size of 2.  Override "block" method to locate the second variable from the first. */
abstract class GibbsSampler2[V1<:Variable with IterableSettings,V2<:Variable with IterableSettings](model:Model, objective:Model) extends SamplerOverSettings[V1](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
  /** Override this method to define the block, returning the second variable V2 to be paired with V1 */
  def block(v1:V1): V2
  /** Return an iterator over the cross product of the settings of V1 and V2. */
  def settings(v1:V1) : SettingIterator = new SettingIterator {
    val v2 = block(v1)
    assert (v2 != v1)
    val s1 = v1.settings
    val s2 = v2.settings
    var first = true // indicates that we haven't yet called next for the first time
    def next(difflist:DiffList): DiffList = {
      val d = newDiffList
      if (first) { s1.next(d); s2.next(d) }
      else if (s2.hasNext) s2.next(d) 
      else { s1.next(d); s2.reset; s2.next(d) }
      first = false
      d
    }
    def hasNext = s1.hasNext || s2.hasNext
    def reset = { s1.reset; s2.reset; first = true }
  }
}

// TODO Not yet tested
/** GibbsSampling over a block size of 3.  Override "block" method to locate the second and third variables from the first. */
abstract class GibbsSampler3[V1<:Variable with IterableSettings,V2<:Variable with IterableSettings,V3<:Variable with IterableSettings](model:Model, objective:Model) extends SamplerOverSettings[V1](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
  /** Override this method to define the block, returning the other variables (V2,V3) to be jointly varied with V1 */
  def block(v1:V1): (V2,V3)
  /** Return an iterator over the cross product of the settings of V1, V2 and V3. */
  def settings(v1:V1) : SettingIterator = new SettingIterator {
    val (v2,v3) = block(v1)
    assert (v2 != v1); assert (v3 != v1); assert (v2 != v3)
    val s1 = v1.settings
    val s2 = v2.settings
    val s3 = v3.settings
    var first = true // indicates that we haven't yet called next for the first time
    def next(difflist:DiffList): DiffList = {
      val d = newDiffList
      if (first) { s1.next(d); s2.next(d); s3.next(d) }
      else if (s3.hasNext) s3.next(d)
      else if (s2.hasNext) { s2.next(d); s3.reset; s3.next(d) }
      else { s1.next(d); s2.reset; s2.next(d); s3.reset; s3.next(d) }
      first = false
      d
    }
    def hasNext = s1.hasNext || s2.hasNext || s3.hasNext
    def reset = { s1.reset; s2.reset; s3.reset; first = true }
  }
}
