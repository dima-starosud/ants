package ungeneric

import scala.language.higherKinds

sealed trait Constrained[C[_ <: R], R] extends Any {
  type Value <: R
  def value: C[Value]
}

object Constrained {
  private final class ConstrainedImpl[C[_ <: R], R](override val value: C[R])
    extends AnyVal with Constrained[C, R] {
    override type Value = R
  }

  def apply[C[_ <: R], R](c: C[R]): Constrained[C, R] = new ConstrainedImpl[C, R](c)
}
