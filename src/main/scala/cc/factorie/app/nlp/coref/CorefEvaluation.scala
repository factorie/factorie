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
