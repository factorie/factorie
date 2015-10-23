package cc.factorie

package object model {
  /** An iterable collection for efficiently holding a single Factor.
    Used in various Template classes and in an implicit conversion from Factor to IterableSingleFactor
    so that unroll1,2,3,4 methods (which are supposed to return Iterable[Factor] can be written
    by end users to return a single Factor (which is then implicitly converted to this class).
    @author Andrew McCallum */
  implicit class IterableSingleFactor[F<:Factor](val factor:F) extends Iterable[F] {
    def iterator = Iterator.single(factor)
    override def size = 1
    override def head = factor
  }
}
