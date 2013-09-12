package cc.factorie.app

package object nlp {

  /** Mapping from annotation class (usually stored in an attr) and the DocumentAnnotor from which it can be obtained. */
  type DocumentAnnotatorMap = collection.Map[Class[_], () => DocumentAnnotator]

}