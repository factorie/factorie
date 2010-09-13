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



package cc.factorie.application
import cc.factorie._
import scala.reflect.Manifest
import cc.factorie.er._
import cc.factorie.DomainInSubclasses

/** Variables and factors for independent classification of feature vectors with String-valued features. 
 
    @author Andrew McCallum
    @since 0.8
 */
object FeatureVectorClassification {
 @DomainInSubclasses
 abstract class Instance[L<:Label[This,L],This<:Instance[L,This]](val name:String) extends BinaryFeatureVectorVariable[String] {
   this: This =>
   type VariableType <: Instance[L,This]
   type GetterType <: InstanceGetter[L,This]
   class GetterClass extends InstanceGetter[L,This]
   def newGetter = new InstanceGetter[L,This]
   def label: L 
 }

 @DomainInSubclasses
 abstract class Label[I<:Instance[This,I],This<:Label[I,This]](labelString:String, val _instance:I) extends cc.factorie.LabelVariable(labelString) {
   this: This =>
   type GetterType <: LabelGetter[I,This];
   class GetterClass extends LabelGetter[I,This]
   type VariableType <: Label[I,This]
   def newGetter = new LabelGetter[I,This]
   def instance: I = _instance // Why was this necessary?  Why didn't simply (val instance:I) above work?
 }
 

 class InstanceGetter[L<:Label[ThisInstance,L],ThisInstance<:Instance[L,ThisInstance]] extends Getter[ThisInstance] {
   def newLabelGetter = new LabelGetter[ThisInstance,L]
   def label = initOneToOne[L](newLabelGetter, instance => instance.label, (label:L) => label.instance)
 }
 
 class LabelGetter[I<:Instance[ThisLabel,I],ThisLabel<:Label[I,ThisLabel]] extends Getter[ThisLabel] {
   def newInstanceGetter = new InstanceGetter[ThisLabel,I]
   def instance = initOneToOne[I](newInstanceGetter, label => label.instance, (instance:I) => instance.label)
 }
 
 
 /**Bias term just on labels */
 class LabelTemplate[L<:Label[_,L]](implicit lm:Manifest[L]) extends TemplateWithDotStatistics1[L]()(lm) 

 /**Factor between label and observed instance vector */
 class LabelInstanceTemplate[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L],im:Manifest[I]) extends TemplateWithDotStatistics2[L,I]()(lm,im) {
   def unroll1(label: L) = Factor(label,label.instance)
   def unroll2(instance: I) = throw new Error("Instance BinaryFeatureVectorVariable shouldn't change")
 }

 /**Factor between label and observed instance vector */
 class SparseLabelInstanceTemplate[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L],im:Manifest[I]) extends TemplateWithDotStatistics2[L,I]()(lm,im) with SparseWeights {
   def unroll1(label: L) = Factor(label,label.instance)
   def unroll2(instance: I) = throw new Error("Instance BinaryFeatureVectorVariable shouldn't change")
 }
 
 def newModel[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L],im:Manifest[I]) =
   new Model(
     new LabelTemplate[L],
     new LabelInstanceTemplate[L,I]
   )

 def newObjective[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L]) = new LabelTemplate[L]()(lm)
}
