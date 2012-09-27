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

package cc.factorie.example
import cc.factorie._
import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}


abstract class PairwiseTemplate extends Template3[PairwiseMention, PairwiseMention, PairwiseLabel] with Statistics[(BooleanValue,CorefAffinity)] {
  override def statistics(m1:PairwiseMention#Value, m2:PairwiseMention#Value, l:PairwiseLabel#Value) = {
    val mention1 = m1
    val mention2 = m2
    val coref: Boolean = l.booleanValue
    (null, null)
  }
}
abstract class PairwiseTransitivityTemplate extends Template3[PairwiseLabel,PairwiseLabel,PairwiseLabel] with Statistics[BooleanValue] {
  //def unroll1(p:PairwiseBoolean) = for (m1 <- p.edge.src.edges; m2 <- p.edge.dst.edges; if (m1)
}



object CorefAffinityDimensionDomain extends EnumDomain {
  val Bias, ExactMatch, SuffixMatch, EntityContainsMention, EditDistance2, EditDistance4, NormalizedEditDistance9, NormalizedEditDistance5, Singleton = Value
}
object CorefAffinityDomain extends CategoricalTensorDomain[String] {
  override lazy val dimensionDomain = CorefAffinityDimensionDomain
}
class CorefAffinity extends BinaryFeatureVectorVariable[String] {
  def domain = CorefAffinityDomain
}

class EntityMentionModel extends TemplateModel(
  new DotTemplate1[EntityRef] {
    lazy val weights = new la.DenseTensor1(CorefAffinityDimensionDomain.dimensionSize)
    //println("*** EntityMentionModel index="+CorefAffinityDomain.dimensionDomain.index("ExactMatch"))
    //weights.update(CorefAffinityDimensionDomain.Bias, -1)
    weights(CorefAffinityDimensionDomain.Bias) = -1
    weights(CorefAffinityDimensionDomain.ExactMatch) = 10
    weights(CorefAffinityDimensionDomain.SuffixMatch) = 2
    weights(CorefAffinityDimensionDomain.EntityContainsMention) = 3
    weights(CorefAffinityDimensionDomain.EditDistance2) = 4
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance9) = -10
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance5) = -2
    weights(CorefAffinityDimensionDomain.Singleton) = -1
    override def statistics(e:EntityRef#Value) = {
      val mention: Entity = e._1
      val entity: Entity = e._2
      val affinity = new CorefAffinity
      if (mention.string == entity.string) affinity += CorefAffinityDimensionDomain.ExactMatch
      if (mention.string.takeRight(4) == entity.string.takeRight(4)) affinity += CorefAffinityDimensionDomain.SuffixMatch
      if (entity.string.contains(mention.string)) affinity += CorefAffinityDimensionDomain.EntityContainsMention
      val editDistance = entity.string.editDistance(mention.string)
      val normalizedEditDistance = editDistance / entity.string.length
      if (editDistance <= 2) affinity += CorefAffinityDimensionDomain.EditDistance2
      if (editDistance <= 4) affinity += CorefAffinityDimensionDomain.EditDistance4
      if (normalizedEditDistance > .5) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance5
      if (normalizedEditDistance > .9) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance9
      if (entity.childEntities.size == 1) affinity += CorefAffinityDimensionDomain.Singleton
      val result = affinity.value
      val str = result.toString
      //println("### EntityMentionModel Stat="+str)
      result
    }
  }
)

class PairwiseModel extends TemplateModel(
  new DotTemplate1[EntityRef] {
    lazy val weights = new la.DenseTensor1(CorefAffinityDimensionDomain.dimensionSize)
    //println("*** PairwiseModel index="+CorefAffinityDomain.dimensionDomain.index("ExactMatch"))
    weights(CorefAffinityDimensionDomain.Bias) = -1
    weights(CorefAffinityDimensionDomain.ExactMatch) = 10
    weights(CorefAffinityDimensionDomain.SuffixMatch) = 2
    weights(CorefAffinityDimensionDomain.EntityContainsMention) = 3
    weights(CorefAffinityDimensionDomain.EditDistance2) = 4
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance9) = -10
    weights(CorefAffinityDimensionDomain.NormalizedEditDistance5) = -2
    weights(CorefAffinityDimensionDomain.Singleton) = -1
    override def statistics(e:EntityRef#Value) = {
      val mention: Entity = e._1
      val entity: Entity = e._2
      val affinity = new CorefAffinity
      if (mention.string == entity.string) affinity += CorefAffinityDimensionDomain.ExactMatch
      if (mention.string.takeRight(4) == entity.string.takeRight(4)) affinity += CorefAffinityDimensionDomain.SuffixMatch
      if (entity.string.contains(mention.string)) affinity += CorefAffinityDimensionDomain.EntityContainsMention
      val editDistance = entity.string.editDistance(mention.string)
      val normalizedEditDistance = editDistance / entity.string.length
      if (editDistance <= 2) affinity += CorefAffinityDimensionDomain.EditDistance2
      if (editDistance <= 4) affinity += CorefAffinityDimensionDomain.EditDistance4
      if (normalizedEditDistance > .5) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance5
      if (normalizedEditDistance > .9) affinity += CorefAffinityDimensionDomain.NormalizedEditDistance9
      if (entity.childEntities.size == 1) affinity += CorefAffinityDimensionDomain.Singleton
      val result = affinity.value
      val str = result.toString
      //println("### PairwiseModel Stat="+str)
      result
    }
  }
)

/*class PairwiseModel2 extends TemplateModel(
  new Template4[EntityRef2,EntityRef2,Mention,Mention] with DotStatistics1[DiscreteVectorValue] {
    def unroll1 (er:EntityRef2) = for (other <- er.value.mentions; if (other.entityRef.value == er.value)) yield 
        if (er.mention.hashCode > other.hashCode) Factor(er, other.entityRef, er.mention, other.entityRef.mention)
        else Factor(er, other.entityRef, other.entityRef.mention, er.mention)
    def unroll2 (er:EntityRef2) = Nil // symmetric
    def unroll3 (mention:Mention) = throw new Error
    def unroll4 (mention:Mention) = throw new Error
    def statistics (values:Values) = throw new Error // Stat(new MentionAffinity(values._3, values._4).value)
  }
)*/


