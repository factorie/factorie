package cc.factorie.er2
import scala.reflect.Manifest

// Because we are lacking Scala 2.8 package objects
object er2 {

  // I'm not sure I like these type names though.
  type Faccessor[A,B] = A=>Iterable[B];
  type Baccessor[A,B] = B=>Iterable[A];
  type AccessorArgs[A,B,C] = (Accessor[A,B], B=>Iterable[C], C=>Iterable[B])
  
  // Support for Forany[Token] { token => Score(token, token.label) }
  implicit def accessor2scoreneighbor[X<:Variable,A<:CategoricalValues](a:Accessor[X,A])(implicit mx:Manifest[X], ma:Manifest[A]): ScoreNeighbor0[X] = 
    new ScoreNeighbor(a)(mx,ma)

}
