package ants

import cats._
import cats.free.Free

sealed trait ProcessDef[In, T] {
  def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[T]
}

final case class Self[In]() extends ProcessDef[In, ProcessId[In]] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[ProcessId[In]] = v.self
}

final case class Receive[In]() extends ProcessDef[In, In] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[In] = v.receive
}

final case class Cast[In, In1](pid: ProcessId[In1], message: In1) extends ProcessDef[In, Unit] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[Unit] = v.cast(pid, message)
}

final case class Spawn[In, In1](
  process: Free[ProcessDef[In1, ?], Nothing]
) extends ProcessDef[In, ProcessId[In1]] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[ProcessId[In1]] = v.spawn(process)
}

final case class Stop[In]() extends ProcessDef[In, Nothing] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[Nothing] = v.stop
}

final case class Recur[In]() extends ProcessDef[In, Nothing] {
  override def visit[F[_]](v: ProcessDef.Visitor[In, F]): F[Nothing] = v.recur
}


trait ProcessId[In1] extends Any

object ProcessId {
  implicit class ProcessIdMethods[In1](val processId: ProcessId[In1]) extends AnyVal {
    def ![In](message: In1): Free[ProcessDef[In, ?], Unit] = ProcessDef.cast(processId, message)
  }
}


object ProcessDef {
  trait Visitor[In, F[_]] extends (ProcessDef[In, ?] ~> F) {
    final override def apply[A](fa: ProcessDef[In, A]): F[A] = fa.visit(this)

    def self: F[ProcessId[In]]
    def receive: F[In]
    def cast[In1](pid: ProcessId[In1], message: In1): F[Unit]
    def spawn[In1](process: Free[ProcessDef[In1, ?], Nothing]): F[ProcessId[In1]]
    def stop: F[Nothing]
    def recur: F[Nothing]
  }

  def self[In]: Free[ProcessDef[In, ?], ProcessId[In]] =
    Free.liftF[ProcessDef[In, ?], ProcessId[In]](Self())

  def receive[In]: Free[ProcessDef[In, ?], In] =
    Free.liftF[ProcessDef[In, ?], In](Receive())

  def cast[In, In1](pid: ProcessId[In1], message: In1): Free[ProcessDef[In, ?], Unit] =
    Free.liftF[ProcessDef[In, ?], Unit](Cast(pid, message))

  def spawn[In, In1](process: Free[ProcessDef[In1, ?], Nothing]): Free[ProcessDef[In, ?], ProcessId[In1]] =
    Free.liftF[ProcessDef[In, ?], ProcessId[In1]](Spawn(process))

  def stop[In]: Free[ProcessDef[In, ?], Nothing] =
    Free.liftF[ProcessDef[In, ?], Nothing](Stop())

  def recur[In]: Free[ProcessDef[In, ?], Nothing] =
    Free.liftF[ProcessDef[In, ?], Nothing](Recur())
}
