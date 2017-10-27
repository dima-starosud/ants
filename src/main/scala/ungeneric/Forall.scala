package ungeneric

sealed trait Forall[C[_]] extends Any {
  def apply[T]: C[T]
}

object Forall {
  def apply[C[_]]: ForallBuilder[C] = new ForallBuilder[C]

  protected type HackToOvercome_SI_5643_SI_9625

  implicit def genericImplicitHelper[C[_]](
    gb: ForallBuilder[C])(implicit value: C[HackToOvercome_SI_5643_SI_9625]
  ): Forall[C] = gb(value.asInstanceOf[C[gb.Any]])

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
