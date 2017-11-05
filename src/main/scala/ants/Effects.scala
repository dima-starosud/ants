package ants

import shapeless.ops.coproduct.Basis
import shapeless.{:+:, CNil, Coproduct}

final case class Effects[C <: CP1, T](value: C#Apply[T]) {
  def apply[S <: CP1](f: C#Apply1 => S#Apply1): Effects[S, T] = {
    val g = f.asInstanceOf[C#Apply[T] => S#Apply[T]]
    Effects[S, T](CP1.lift2()  g(value))
  }

  def embed[S <: CP1](implicit basis: Basis[S#Apply1, C#Apply1]): Effects[S, T] =
    apply(basis inverse Right(_))
}

object Effects {

}

sealed trait CP1 {
  type Apply[A] <: Coproduct
  type Apply1 <: Coproduct
}
final abstract class CP1Nil extends CP1 {
  type Apply[A] = CNil
  type Apply1 = CNil
}
final abstract class :-:[H[_], T <: CP1] extends CP1 {
  type Apply[A] = H[A] :+: T#Apply[A]
  type Apply1 = CP1.HK[H] :+: T#Apply1
}

object CP1 {
  final abstract class HK[_[_]]

  // almost safe...
  def lift1[F[_], A <: CP1, T](
    a: F[A#Apply1]
  ): F[A#Apply[T]] = a.asInstanceOf

  def lift2[F[_, _], A <: CP1, B <: CP1, T](
    a: F[A#Apply1, B#Apply1]
  ): F[A#Apply[T], B#Apply[T]] = a.asInstanceOf

  def lift3[F[_, _, _], A <: CP1, B <: CP1, C <: CP1, T](
    a: F[A#Apply1, B#Apply1, C#Apply1]
  ): F[A#Apply[T], B#Apply[T], C#Apply[T]] = a.asInstanceOf
}
