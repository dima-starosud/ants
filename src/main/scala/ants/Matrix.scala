package ants

import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, Behavior}

object Matrix {
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

  final case class Active(pos: Pos, cells: Pos => ActorRef[Pass]) {

    lazy val Available: Behavior[Pass] = Actor.immutable {
      (ctx, e) =>
        e.from ! Accepted
        println(pos) // TODO delegate
        ctx.spawnAnonymous(Busy(e.dir))
        Visited
    }

    def Busy(dir: Direction): Behavior[PassResult] = {
      lazy val retry: Seq[Direction] => Behavior[PassResult] = {
        case Seq() =>
          Actor.stopped
        case dir +: dirs =>
          Actor.deferred { ctx =>
            cells(pos + dir) ! Pass(ctx.self, dir)
            Actor.immutable {
              case (_, Accepted) => retry(Seq.empty)
              case (_, Denied) => retry(dirs)
            }
          }
      }

      retry(Seq.iterate(dir, 4)(_.rotate))
    }
  }

  lazy val Visited: Behavior[Pass] = Actor.immutable {
    (_, e) =>
      e.from ! Denied
      Actor.same
  }

  def circle(
    xs: Int, ys: Int,
  ): Behavior[Nothing] = Actor.deferred[Nothing] { ctx =>

    lazy val cells: Map[Pos, ActorRef[Pass]] = {
      val visited = ctx.spawnAnonymous(Visited)
      for {
        x <- Seq.range(1, xs + 1)
        y <- Seq.range(1, ys + 1)
      } yield {
        val pos = Pos(x, y)
        val ref = ctx.spawnAnonymous(
          Active(pos, cells.getOrElse(_, visited)).Available)
        pos -> ref
      }
    }.toMap

    val initiator = ctx.spawnAnonymous(Actor.immutable[PassResult] {
      (_, e) =>
        e match {
          case Accepted =>
            println("Successfully started...")
          case Denied =>
            println("Something went wrong :(")
        }
        Actor.stopped
    })

    cells(Pos(1, 1)) ! Pass(initiator, Direction(1, 0))

    Actor.empty
  }
}
