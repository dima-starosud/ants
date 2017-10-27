package ants

import cats.free.Free
import cats.~>
import shapeless.ops.coproduct.Basis
import shapeless.{:+:, CNil, Coproduct}
import ungeneric.Forall

object Effect {
  type Effects = {type λ[_] <: Coproduct}
  type ENil = {type λ[T] = CNil}
  type :-:[F[_], G <: Effects] = {
    type λ[T] = F[T] :+: G#λ[T]
  }

  type Eff[E <: Effects, T] = Free[E#λ, T]

  implicit def embed[Sub[_] <: Coproduct, Super[_] <: Coproduct, T](
    free: Free[Sub, T]
  )(
    implicit basis: Forall[Lambda[X => Basis[Super[X], Sub[X]]]]
  ): Free[Super, T] = {
    free.compile(new (Sub ~> Super) {
      override def apply[A](fa: Sub[A]): Super[A] = {
        basis[A].inverse(Right(fa))
      }
    })
  }
}
