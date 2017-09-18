package ants.free

import cats._
import cats.free.Free

sealed trait BehaviorOp[In, T] {
  def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[T]
}

final case class Self[In]() extends BehaviorOp[In, ActorRef[In]] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[ActorRef[In]] = v.self
}

final case class Receive[In]() extends BehaviorOp[In, In] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[In] = v.receive
}

final case class Cast[In, In1](actorRef: ActorRef[In1], message: In1) extends BehaviorOp[In, Unit] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[Unit] = v.cast(actorRef, message)
}

final case class Spawn[In, In1](behavior: Free[BehaviorOp[In1, ?], _]) extends BehaviorOp[In, ActorRef[In1]] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[ActorRef[In1]] = v.spawn(behavior)
}

final case class Stop[In]() extends BehaviorOp[In, Nothing] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[Nothing] = v.stop
}

final case class Recur[In]() extends BehaviorOp[In, Nothing] {
  override def visit[F[_]](v: BehaviorOp.Visitor[In, F]): F[Nothing] = v.recur
}


final case class ActorRef[In1](impl: Any) extends AnyVal {
  def ![In](message: In1): Free[BehaviorOp[In, ?], Unit] = BehaviorOp.cast(this, message)
}


object BehaviorOp {
  trait Visitor[In, F[_]] extends (BehaviorOp[In, ?] ~> F) {
    final override def apply[A](fa: BehaviorOp[In, A]): F[A] = fa.visit(this)

    def self: F[ActorRef[In]]
    def receive: F[In]
    def cast[In1](actorRef: ActorRef[In1], message: In1): F[Unit]
    def spawn[In1](BehaviorOp: Free[BehaviorOp[In1, ?], _]): F[ActorRef[In1]]
    def stop: F[Nothing]
    def recur: F[Nothing]
  }

  def self[In]: Free[BehaviorOp[In, ?], ActorRef[In]] =
    Free.liftF[BehaviorOp[In, ?], ActorRef[In]](Self())

  def receive[In]: Free[BehaviorOp[In, ?], In] =
    Free.liftF[BehaviorOp[In, ?], In](Receive())

  def cast[In, In1](actorRef: ActorRef[In1], message: In1): Free[BehaviorOp[In, ?], Unit] =
    Free.liftF[BehaviorOp[In, ?], Unit](Cast(actorRef, message))

  def spawn[In, In1](BehaviorOp: Free[BehaviorOp[In1, ?], _]): Free[BehaviorOp[In, ?], ActorRef[In1]] =
    Free.liftF[BehaviorOp[In, ?], ActorRef[In1]](Spawn(BehaviorOp))

  def stop[In]: Free[BehaviorOp[In, ?], Nothing] =
    Free.liftF[BehaviorOp[In, ?], Nothing](Stop())

  def recur[In]: Free[BehaviorOp[In, ?], Nothing] =
    Free.liftF[BehaviorOp[In, ?], Nothing](Recur())
}
