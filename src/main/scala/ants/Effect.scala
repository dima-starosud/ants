package ants

import cats.free.Free
import shapeless.ops.coproduct.Basis
import shapeless.{:+:, CNil, Coproduct}

object Effect {
  type Effects = {type λ[_] <: Coproduct}
  type UNil = {type λ[T] = CNil}
  type :-:[F[_], G <: Effects] = {
    type λ[T] = F[T] :+: G#λ[T]
  }

  type Eff[E <: Effects, T] = Free[E#λ, T]

  implicit def embed[Sub[_] <: Coproduct, Super[_] <: Coproduct, T](
    free: Free[Sub, T])(implicit basis: Basis[Super[T], Sub[T]]
  ): Free[Super, T] = {
    print(free + "" + basis)
    ???
    //    free.compile(λ[Sub ~> Super](basis inverse Right(_)))
  }

  implicit def three[A, B, C](implicit a: A, b: B, c: C): (A, B, C) = (a, b, c)
}
