/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
//package cc.factorie.app.nlp.coref
//
//import cc.factorie.util.{F1Evaluation,FastLogging}
//
///** A generic trait for coreference solution containers that can be evaluated. */
//trait EvaluatableCoref[EntityIdType,MentionIdType] {
//  def evalEntityIds: Iterable[EntityIdType]
//  def evalMentionIds: Iterable[MentionIdType]
//  def evalMentionIds(entityId:EntityIdType): Iterable[MentionIdType]
//  def evalIntersectionSize(entityId1:EntityIdType, entityId2:EntityIdType): Int
//  def evalEntityId(mentionId:MentionIdType): EntityIdType
//}
//
//
//
//trait CorefEvaluation {
//  def apply[E,M](predicted: EvaluatableCoref[E,M], truth: EvaluatableCoref[E,M]): F1Evaluation
//}
//
//
//// TODO Move BCubed, BCubedNoSingletons, MUC and CEAF here from cc.factorie.util.coref. -akm
