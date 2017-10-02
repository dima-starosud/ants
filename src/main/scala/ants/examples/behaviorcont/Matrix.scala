package ants.examples.behaviorcont

import ants.{BehaviorCont, Bottom}
import cats.implicits._
import BehaviorCont.{ContActorRef, behaviorContMonad}

object Matrix {
  final case class Pos(x: Int, y: Int) {
    def +(dir: Direction) = Pos(x + dir.dx, y + dir.dy)
  }

  final case class Direction(dx: Int, dy: Int) {
    def rotate = Direction(-dy, dx)
  }

  final case class Pass(from: ContActorRef[PassResult], dir: Direction)

  sealed trait PassResult
  final case object Accepted extends PassResult
  final case object Denied extends PassResult

  def available(
    visitedCallback: () => Unit,
    cells: Direction => ContActorRef[Pass],
  ): BehaviorCont[Pass, Bottom] = {
    BehaviorCont.receive[Pass]
      .flatMap {
        e =>
          visitedCallback()
          BehaviorCont.spawn(busy(e.dir, cells))
      }
      .flatMap(_ => Visited)
  }

  def UNUSED(x: Any*): Unit = Function.const(())(x)

  def busy(
    dir: Direction,
    cells: Direction => ContActorRef[Pass],
  ): BehaviorCont[PassResult, Bottom] = {
    lazy val retry: Seq[Direction] => BehaviorCont[PassResult, Bottom] = {
      case Seq() =>
        BehaviorCont.stop
      case dir +: dirs =>
        BehaviorCont.self[PassResult]
          .flatMap(cells(dir) ! Pass(_, dir))
          .flatMap(_ => BehaviorCont.receive[PassResult])
          .flatMap {
            case Accepted => retry(Seq.empty)
            case Denied => retry(dirs)
          }
    }

    retry(Seq.iterate(dir, 4)(_.rotate))
  }


  lazy val Visited: BehaviorCont[Pass, Bottom] = {
    BehaviorCont.receive[Pass]
      .flatMap(e => e.from ! Denied)
      .flatMap(_ => BehaviorCont.recur)
  }

  type Cells = Pos => ContActorRef[Pass]

  def cells(xs: Int, ys: Int)(cells: Cells): BehaviorCont[Bottom, Cells] = {
    val xss = Seq.range(1, xs + 1)
    val yss = Seq.range(1, ys + 1)

    val active =
      for {
        x <- xss
        y <- yss
      } yield {
        val pos = Pos(x, y)
        val ref = BehaviorCont.spawn(
          available(() => println(pos), d => cells(pos + d)))
        pos -> ref
      }

    val sc = Stream.continually[Int] _
    val border =
      for {
        (x, y) <- xss.zip(sc(0)) ++ xss.zip(sc(ys + 1)) ++ sc(0).zip(yss) ++ sc(xs + 1).zip(yss)
      } yield {
        Pos(x, y) -> BehaviorCont.spawn(Visited)
      }

    (active ++ border)
      .map { case (p, bc) => bc.map(p -> _) }
      .toVector
      .sequence
      .map(_.toMap)
  }


  //  def circle(xs: Int, ys: Int): Behavior[Bottom] = Actor.deferred[Bottom] { ctx =>
  //    lazy val cells: Map[Pos, ContActorRef[Pass]] = {
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
  //      val sc = Stream.continually[Int] _
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
  //  def runCircle(xs: Int, ys: Int): ActorSystem[Bottom] =
  //    ActorSystem[Bottom](Matrix.circle(xs, ys), "Matrix-circle")
}
