package ants

import akka.typed.ActorContext
import cats._
import cats.data.Kleisli
import cats.effect.IO
import cats.free.Free

sealed trait Behavior[In, T] {
  def visit[F[_]](v: Behavior.Visitor[In, F]): F[T]
}
final case class Self[In]() extends Behavior[In, ActorRef[In]] {
  override def visit[F[_]](v: Behavior.Visitor[In, F]): F[ActorRef[In]] = v.self
}
final case class Receive[In]() extends Behavior[In, In] {
  override def visit[F[_]](v: Behavior.Visitor[In, F]): F[In] = v.receive
}
final case class Cast[In, In1](actorRef: ActorRef[In1], message: In1) extends Behavior[In, Unit] {
  override def visit[F[_]](v: Behavior.Visitor[In, F]): F[Unit] = v.cast(actorRef, message)
}
final case class Spawn[In, In1](behavior: Free[Behavior[In1, ?], _]) extends Behavior[In, ActorRef[In1]] {
  override def visit[F[_]](v: Behavior.Visitor[In, F]): F[ActorRef[In1]] = v.spawn(behavior)
}
final case class Stop[In]() extends Behavior[In, Unit] {
  override def visit[F[_]](v: Behavior.Visitor[In, F]): F[Unit] = v.stop
}

final case class ActorRef[In1](impl: Any) extends AnyVal {
  def ![In](message: In1): Free[Behavior[In, ?], Unit] = Behavior.cast(this, message)
}

object Behavior {
  trait Visitor[In, F[_]] extends (Behavior[In, ?] ~> F) {
    final override def apply[A](fa: Behavior[In, A]): F[A] = fa.visit(this)

    def self: F[ActorRef[In]]
    def receive: F[In]
    def cast[In1](actorRef: ActorRef[In1], message: In1): F[Unit]
    def spawn[In1](behavior: Free[Behavior[In1, ?], _]): F[ActorRef[In1]]
    def stop: F[Unit]
  }

  def self[In]: Free[Behavior[In, ?], ActorRef[In]] =
    Free.liftF[Behavior[In, ?], ActorRef[In]](Self())

  def receive[In]: Free[Behavior[In, ?], In] =
    Free.liftF[Behavior[In, ?], In](Receive())

  def cast[In, In1](actorRef: ActorRef[In1], message: In1): Free[Behavior[In, ?], Unit] =
    Free.liftF[Behavior[In, ?], Unit](Cast(actorRef, message))

  def spawn[In, In1](behavior: Free[Behavior[In1, ?], _]): Free[Behavior[In, ?], ActorRef[In1]] =
    Free.liftF[Behavior[In, ?], ActorRef[In1]](Spawn(behavior))

  def stop[In]: Free[Behavior[In, ?], Unit] =
    Free.liftF[Behavior[In, ?], Unit](Stop())
}

object MatriXXXx {
  final case class Pos(x: Int, y: Int) {
    def +(dir: Direction) = Pos(x + dir.dx, y + dir.dy)
  }

  final case class Direction(dx: Int, dy: Int) {
    def rotate = Direction(-dy, dx)
  }

  final case class Pass(from: ActorRef[PassResult], dir: Direction)

  sealed trait PassResult
  final case object Accepted extends PassResult
  final case object Denied extends PassResult

  def available(
    visitedCallback: () => Unit,
    cells: Direction => ActorRef[Pass],
  ): Free[Behavior[Pass, ?], Unit] =
    for {
      e <- Behavior.receive
      _ <- e.from ! Accepted
      _ = visitedCallback()
      _ <- Behavior.spawn(busy(e.dir, cells))
      _ <- Visited
    } yield {
    }

  def busy(
    dir: Direction,
    cells: Direction => ActorRef[Pass],
  ): Free[Behavior[PassResult, ?], Unit] = {
    lazy val retry: Seq[Direction] => Free[Behavior[PassResult, ?], Unit] = {
      case Seq() =>
        Behavior.stop
      case dir +: dirs =>
        for {
          self <- Behavior.self
          _ <- cells(dir) ! Pass(self, dir)
          msg <- Behavior.receive
          _ <- msg match {
            case Accepted => retry(Seq.empty)
            case Denied => retry(dirs)
          }
        } yield {
        }
    }

    retry(Seq.iterate(dir, 4)(_.rotate))
  }

  lazy val Visited: Free[Behavior[Pass, ?], Unit] = Behavior.receive.flatMap(_.from ! Denied)

  import akka.typed.scaladsl.{Actor => ActorDsl}


  final class TooToo[In]
    extends Behavior.Visitor[In, Kleisli[IO, ActorContext[In], ?]] {

    override def self: Kleisli[IO, ActorContext[In], ActorRef[In]] = Kleisli {
      as => IO(ActorRef(as.asScala.self))
    }
    override def receive: Kleisli[IO, ActorContext[In], In] = Kleisli {
      as =>
        ActorDsl.immutable[In] { (_, msg) =>
          ???
        }
        as.asScala
        ???
    }
    override def cast[In1](actorRef: ActorRef[In1], message: In1): Kleisli[IO, ActorContext[In], Unit] = ???
    override def spawn[In1](behavior: Free[Behavior[In1, ?], _]): Kleisli[IO, ActorContext[In], ActorRef[In1]] = ???
    override def stop: Kleisli[IO, ActorContext[In], Unit] = ???
  }
  //
  //  def circle(xs: Int, ys: Int): Behavior[Nothing] = Actor.deferred[Nothing] { ctx =>
  //    lazy val cells: Map[Pos, ActorRef[Pass]] = {
  //      val xss = Seq.range(1, xs + 1)
  //      val yss = Seq.range(1, ys + 1)
  //
  //      val active =
  //        for {
  //          x <- xss
  //          y <- yss
  //        } yield {
  //          val pos = Pos(x, y)
  //          val ref = ctx.spawnAnonymous(
  //            available(() => println(pos), d => cells(pos + d)))
  //          pos -> ref
  //        }
  //
  //      def sc[T] = Stream.continually[T] _
  //      val border =
  //        for {
  //          (x, y) <- xss.zip(sc(0)) ++ xss.zip(sc(ys + 1)) ++ sc(0).zip(yss) ++ sc(xs + 1).zip(yss)
  //        } yield {
  //          Pos(x, y) -> ctx.spawnAnonymous(Visited)
  //        }
  //
  //      (active ++ border).toMap
  //    }
  //
  //    val initiator = ctx.spawnAnonymous(Actor.immutable[PassResult] {
  //      (_, e) =>
  //        e match {
  //          case Accepted =>
  //            println("Successfully started...")
  //          case Denied =>
  //            println("Something went wrong :(")
  //        }
  //        Actor.stopped
  //    })
  //
  //    cells(Pos(1, 1)) ! Pass(initiator, Direction(1, 0))
  //
  //    Actor.empty
  //  }
  //
  //  def runCircle(xs: Int, ys: Int): ActorSystem[Nothing] =
  //    ActorSystem[Nothing](Matrix.circle(xs, ys), "Matrix-circle")
}
