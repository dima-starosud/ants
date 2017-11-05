package ungeneric

sealed trait Forall[C[_]] extends Any {
  def apply[T]: C[T]
}

object Forall {
  def apply[C[_]]: ForallBuilder[C] = new ForallBuilder[C]

  implicit def genericImplicitHelper[C[_]](
    /** doesn't [always] work due to some bugs :( */
    gb: ForallBuilder[C])(implicit value: C[gb.Any]
  ): Forall[C] = gb(value)

  implicit def genericImplicit[C[_]](
    implicit helper: ForallBuilder[C] => Forall[C]
  ): Forall[C] = helper(Forall[C])

  final class ForallBuilder[C[_]] {
    type Any
    type Type = C[Any]

    def apply(value: Type): Forall[C] = new Forall[C] {
      override def apply[T]: C[T] = value.asInstanceOf[C[T]]
    }
  }
}
