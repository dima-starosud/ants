package ants.testing

final case class C[T](tittle: String)

sealed trait FallbackImplicit {
  implicit def ct[T]: C[T] = C[T]("T")
}

object Testing extends FallbackImplicit {
  implicit def cint: C[Int] = C[Int]("Int")

  import ants.Effect._
  import ungeneric.Forall

  println(implicitly[Forall[C]].apply)

  println(implicitly[Forall[Lambda[X => Eff[ENil, X] => Eff[Option :-: ENil, X]]]].apply)

  val z: Eff[ENil, Int] = null
  val y: Eff[Option :-: ENil, Int] = z
}
